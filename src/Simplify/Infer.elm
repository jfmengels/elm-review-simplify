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
    | Is Bool


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
        Just (Is bool) ->
            Just
                (Expression.FunctionOrValue [ "Basics" ]
                    (if bool then
                        "True"

                     else
                        "False"
                    )
                )

        Just (Equals value) ->
            Just value

        _ ->
            Nothing


getConstraint : Expression -> Inferred -> Maybe Constraint
getConstraint expr (Inferred inferred) =
    AssocList.get expr inferred


inferForIfCondition : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred -> List ( Range, Inferred )
inferForIfCondition condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer [ condition ] (Is True) inferred )
    , ( falseBranchRange, infer [ condition ] (Is False) inferred )
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
                    if constraint == Is True then
                        infer (Node.value left :: Node.value right :: rest) constraint dict

                    else
                        -- TODO Add inverse constraint
                        infer rest constraint dict

                Expression.OperatorApplication "||" _ left right ->
                    if constraint == Is False then
                        infer (Node.value left :: Node.value right :: rest) constraint dict

                    else
                        -- TODO Add inverse constraint
                        infer rest constraint dict

                Expression.OperatorApplication "==" _ left right ->
                    infer rest
                        constraint
                        (dict
                            |> inferOnEquality left right constraint
                            |> inferOnEquality right left constraint
                        )

                Expression.OperatorApplication "/=" _ left right ->
                    let
                        inversedConstraint : Constraint
                        inversedConstraint =
                            inverseConstraint constraint
                    in
                    infer rest
                        constraint
                        (dict
                            |> inferOnEquality left right inversedConstraint
                            |> inferOnEquality right left inversedConstraint
                        )

                _ ->
                    infer rest constraint dict


inferOnEquality : Node Expression -> Node Expression -> Constraint -> Inferred -> Inferred
inferOnEquality (Node _ expr) (Node _ other) constraint dict =
    case expr of
        Expression.Integer int ->
            case constraint of
                Is True ->
                    injectConstraint
                        other
                        (Equals (Expression.Floatable (Basics.toFloat int)))
                        dict

                Is False ->
                    injectConstraint
                        other
                        (NotEquals (Expression.Floatable (Basics.toFloat int)))
                        dict

                _ ->
                    dict

        _ ->
            dict


inverseConstraint : Constraint -> Constraint
inverseConstraint constraint =
    case constraint of
        Is bool ->
            Is (not bool)

        Equals (Expression.FunctionOrValue [ "Basics" ] "True") ->
            Is False

        Equals (Expression.FunctionOrValue [ "Basics" ] "False") ->
            Is True

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
                    Expression.OperatorApplication "&&" infix_ (Node _ left) (Node _ right) ->
                        if expression == left then
                            AssocList.insert expr v acc

                        else
                            AssocList.insert expr v acc

                    _ ->
                        AssocList.insert expr v acc
            )
            AssocList.empty
        |> AssocList.insert expression constraint
        |> Inferred


insertIf : Bool -> Expression -> Constraint -> AssocList.Dict Expression Constraint -> AssocList.Dict Expression Constraint
insertIf condition expression constraint inferred =
    if condition then
        AssocList.insert expression constraint inferred

    else
        inferred


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
