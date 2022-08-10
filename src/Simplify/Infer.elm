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
    = Inferred (AssocList.Dict Expression Constraints)


type Constraints
    = Single Constraint


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
        Just (Single (Is bool)) ->
            Just
                (Expression.FunctionOrValue [ "Basics" ]
                    (if bool then
                        "True"

                     else
                        "False"
                    )
                )

        Just (Single (Equals value)) ->
            Just value

        _ ->
            Nothing


getConstraint : Expression -> Inferred -> Maybe Constraint
getConstraint expr (Inferred inferred) =
    AssocList.get expr inferred
        |> Maybe.map (\(Single c) -> c)


inferForIfCondition : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred -> List ( Range, Inferred )
inferForIfCondition condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer [ condition ] (Single (Is True)) inferred )
    , ( falseBranchRange, infer [ condition ] (Single (Is False)) inferred )
    ]


infer : List Expression -> Constraints -> Inferred -> Inferred
infer nodes constraints acc =
    case nodes of
        [] ->
            acc

        node :: rest ->
            let
                dict : Inferred
                dict =
                    injectConstraints node constraints acc
            in
            case node of
                Expression.FunctionOrValue _ _ ->
                    infer rest constraints dict

                Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
                    infer
                        rest
                        constraints
                        (infer [ Node.value expression ] (inverseConstraint constraints) dict)

                Expression.OperatorApplication "&&" _ left right ->
                    if constraints == Single (Is True) then
                        infer (Node.value left :: Node.value right :: rest) constraints dict

                    else
                        -- TODO Add inverse constraint
                        infer rest constraints dict

                Expression.OperatorApplication "||" _ left right ->
                    if constraints == Single (Is False) then
                        infer (Node.value left :: Node.value right :: rest) constraints dict

                    else
                        -- TODO Add inverse constraint
                        infer rest constraints dict

                Expression.OperatorApplication "==" _ left right ->
                    infer rest
                        constraints
                        (dict
                            |> inferOnEquality left right constraints
                            |> inferOnEquality right left constraints
                        )

                Expression.OperatorApplication "/=" _ left right ->
                    let
                        inversedConstraints : Constraints
                        inversedConstraints =
                            inverseConstraint constraints
                    in
                    infer rest
                        constraints
                        (dict
                            |> inferOnEquality left right inversedConstraints
                            |> inferOnEquality right left inversedConstraints
                        )

                _ ->
                    infer rest constraints dict


inferOnEquality : Node Expression -> Node Expression -> Constraints -> Inferred -> Inferred
inferOnEquality (Node _ expr) (Node _ other) constraints dict =
    case expr of
        Expression.Integer int ->
            case constraints of
                Single (Is True) ->
                    injectConstraints
                        other
                        (Single (Equals (Expression.Floatable (Basics.toFloat int))))
                        dict

                Single (Is False) ->
                    injectConstraints
                        other
                        (Single (NotEquals (Expression.Floatable (Basics.toFloat int))))
                        dict

                Single _ ->
                    dict

        _ ->
            dict


inverseConstraint : Constraints -> Constraints
inverseConstraint (Single constraint) =
    Single (inverseSingleConstraint constraint)


inverseSingleConstraint : Constraint -> Constraint
inverseSingleConstraint constraint =
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


injectConstraints : Expression -> Constraints -> Inferred -> Inferred
injectConstraints expression constraints (Inferred inferred) =
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
        |> AssocList.insert expression constraints
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
