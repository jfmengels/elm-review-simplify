module Simplify.Infer exposing
    ( Constraint(..)
    , Inferred
    , Resources
    , empty
    , get
    , getConstraint
    , getInt
    , inferForIfCondition
    )

import AssocList
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers


type Inferred
    = Inferred (AssocList.Dict Expression Constraint)


type Constraint
    = Equals Expression
    | NotEquals Expression


type alias Resources a =
    { a
        | lookupTable : ModuleNameLookupTable
        , inferredConstants : ( Inferred, List Inferred )
    }


empty : Inferred
empty =
    Inferred AssocList.empty


get : Expression -> Inferred -> Maybe Expression
get expr (Inferred inferred) =
    case AssocList.get expr inferred of
        Just (Equals value) ->
            Just value

        _ ->
            Nothing


getConstraint : Expression -> Inferred -> Maybe Constraint
getConstraint expr (Inferred inferred) =
    AssocList.get expr inferred


inferForIfCondition : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred -> List ( Range, Inferred )
inferForIfCondition condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer [ condition ] constraintTrue inferred )
    , ( falseBranchRange, infer [ condition ] constraintFalse inferred )
    ]


infer : List Expression -> Constraint -> Inferred -> Inferred
infer nodes constraint acc =
    case nodes of
        [] ->
            acc

        node :: rest ->
            let
                dict : Inferred
                dict =
                    injectConstraint node constraint acc
            in
            case node of
                Expression.FunctionOrValue _ _ ->
                    infer rest constraint dict

                Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
                    infer
                        rest
                        constraint
                        (infer [ Node.value expression ] (inverseConstraint constraint) dict)

                Expression.OperatorApplication "&&" _ left right ->
                    if constraint == constraintTrue then
                        infer (Node.value left :: Node.value right :: rest) constraint dict

                    else
                        -- TODO Add inverse constraint
                        infer rest constraint dict

                Expression.OperatorApplication "||" _ left right ->
                    if constraint == constraintFalse then
                        infer (Node.value left :: Node.value right :: rest) constraint dict

                    else
                        -- TODO Add inverse constraint
                        infer rest constraint dict

                Expression.OperatorApplication "==" _ left right ->
                    case Node.value right of
                        Expression.Integer _ ->
                            if constraint == constraintTrue then
                                injectConstraint (Node.value left) (Equals (Node.value right)) dict

                            else if constraint == constraintFalse then
                                injectConstraint (Node.value left) (NotEquals (Node.value right)) dict

                            else
                                infer rest constraint dict

                        _ ->
                            infer rest constraint dict

                Expression.OperatorApplication "/=" _ left right ->
                    -- TODO Also do left
                    case Node.value right of
                        Expression.Integer int ->
                            if constraint == constraintTrue then
                                injectConstraint
                                    (Node.value left)
                                    (NotEquals (Expression.Floatable (Basics.toFloat int)))
                                    dict

                            else if constraint == constraintFalse then
                                injectConstraint
                                    (Node.value left)
                                    (Equals (Expression.Floatable (Basics.toFloat int)))
                                    dict

                            else
                                infer rest constraint dict

                        _ ->
                            infer rest constraint dict

                _ ->
                    infer rest constraint dict


constraintTrue : Constraint
constraintTrue =
    Equals (Expression.FunctionOrValue [ "Basics" ] "True")


constraintFalse : Constraint
constraintFalse =
    Equals (Expression.FunctionOrValue [ "Basics" ] "False")


inverseConstraint : Constraint -> Constraint
inverseConstraint constraint =
    case constraint of
        Equals (Expression.FunctionOrValue [ "Basics" ] "True") ->
            constraintFalse

        Equals (Expression.FunctionOrValue [ "Basics" ] "False") ->
            constraintTrue

        Equals value ->
            NotEquals value

        NotEquals value ->
            Equals value


injectConstraint : Expression -> Constraint -> Inferred -> Inferred
injectConstraint expression constraint (Inferred inferred) =
    inferred
        |> AssocList.foldl
            (\expr v acc ->
                case expr of
                    _ ->
                        AssocList.insert expr v acc
            )
            AssocList.empty
        |> AssocList.insert expression constraint
        |> Inferred


getInt : Resources a -> Node Expression -> Maybe Int
getInt resources baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.Integer n ->
            Just n

        Expression.Hex n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getInt resources expr)

        Expression.FunctionOrValue _ name ->
            case
                ModuleNameLookupTable.moduleNameFor resources.lookupTable node
                    |> Maybe.andThen (\moduleName -> get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
            of
                Just (Expression.Integer int) ->
                    Just int

                _ ->
                    Nothing

        _ ->
            Nothing
