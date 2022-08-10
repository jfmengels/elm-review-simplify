module Simplify.Infer exposing
    ( Constraint(..)
    , Inferred
    , Inferred2
    , Resources
    , empty
    , empty2
    , get
    , get2
    , getConstraint
    , getInt
    , inferForIfCondition
    , inferForIfCondition2
    )

import AssocList
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers


type Inferred
    = Inferred (AssocList.Dict Expression Constraints)


type Inferred2
    = Inferred2
        { constraints : List Constraint2
        , deduced : AssocList.Dict Expression Expression
        }


type Constraint2
    = Equals2 Expression Expression
    | NotEquals2 Expression Expression
    | Is2 Bool
    | And2 (List Constraint2)
    | Or2 (List Constraint2)


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
        , inferredConstants2 : ( Inferred2, List Inferred2 )
    }


empty : Inferred
empty =
    Inferred AssocList.empty


empty2 : Inferred2
empty2 =
    Inferred2
        { constraints = []
        , deduced = AssocList.empty
        }


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


get2 : Expression -> Inferred2 -> Maybe Expression
get2 expr (Inferred2 inferred) =
    AssocList.get expr inferred.deduced


getConstraint : Expression -> Inferred -> Maybe Constraint
getConstraint expr (Inferred inferred) =
    AssocList.get expr inferred
        |> Maybe.map (\(Single c) -> c)


inferForIfCondition2 : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred2 -> List ( Range, Inferred2 )
inferForIfCondition2 condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer2 [ condition ] (Is2 True) inferred )
    , ( falseBranchRange, infer2 [ condition ] (Is2 False) inferred )
    ]


infer2 : List Expression -> Constraint2 -> Inferred2 -> Inferred2
infer2 nodes constraint acc =
    case nodes of
        [] ->
            acc

        node :: rest ->
            let
                dict : Inferred2
                dict =
                    injectConstraints2 constraint acc
            in
            case node of
                Expression.FunctionOrValue _ _ ->
                    infer2 rest constraint dict

                Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
                    infer2
                        rest
                        constraint
                        (infer2 [ Node.value expression ] (inverseConstraint2 constraint) dict)

                Expression.OperatorApplication "&&" _ left right ->
                    if constraint == Is2 True then
                        infer2 (Node.value left :: Node.value right :: rest) constraint dict

                    else
                        -- TODO Add inverse constraint
                        infer2 rest constraint dict

                Expression.OperatorApplication "||" _ left right ->
                    if constraint == Is2 False then
                        infer2 (Node.value left :: Node.value right :: rest) constraint dict

                    else
                        -- TODO Add inverse constraint
                        infer2 rest constraint dict

                Expression.OperatorApplication "==" _ left right ->
                    infer2 rest
                        constraint
                        (dict
                            |> inferOnEquality2 left right constraint
                            |> inferOnEquality2 right left constraint
                        )

                Expression.OperatorApplication "/=" _ left right ->
                    let
                        inversedConstraint : Constraint2
                        inversedConstraint =
                            inverseConstraint2 constraint
                    in
                    infer2 rest
                        constraint
                        (dict
                            |> inferOnEquality2 left right inversedConstraint
                            |> inferOnEquality2 right left inversedConstraint
                        )

                _ ->
                    infer2 rest constraint dict


injectConstraints2 : Constraint2 -> Inferred2 -> Inferred2
injectConstraints2 newConstraint (Inferred2 { deduced, constraints }) =
    let
        newDeduced : AssocList.Dict Expression Expression
        newDeduced =
            deduce newConstraint constraints deduced

        newDeduced2 : AssocList.Dict Expression Expression
        newDeduced2 =
            case newConstraint of
                Is2 _ ->
                    newDeduced

                Equals2 a b ->
                    AssocList.insert a b newDeduced

                NotEquals2 a b ->
                    -- TODO Add "a /= b"?
                    newDeduced

                And2 _ ->
                    -- TODO Add "a && b && ..."?
                    newDeduced

                Or2 _ ->
                    -- TODO Add "a || b || ..."?
                    newDeduced
    in
    Inferred2
        { constraints = newConstraint :: constraints
        , deduced = newDeduced2
        }


deduce newConstraint constraints acc =
    --case constraints of
    --    constraint :: restOfConstraints ->
    --        case (constraint, newConstraint) ->
    --            (And2 list, )
    acc


inferOnEquality2 : Node Expression -> Node Expression -> Constraint2 -> Inferred2 -> Inferred2
inferOnEquality2 (Node _ expr) (Node _ other) constraints dict =
    case expr of
        Expression.Integer int ->
            case constraints of
                Is2 True ->
                    injectConstraints2
                        (Equals2 other (Expression.Floatable (Basics.toFloat int)))
                        dict

                Is2 False ->
                    injectConstraints2
                        (NotEquals2 other (Expression.Floatable (Basics.toFloat int)))
                        dict

                _ ->
                    dict

        _ ->
            dict


inverseConstraint2 : Constraint2 -> Constraint2
inverseConstraint2 constraint =
    case constraint of
        Is2 bool ->
            Is2 (not bool)

        Equals2 expr value ->
            NotEquals2 expr value

        NotEquals2 expr value ->
            Equals2 expr value

        And2 constraints ->
            Or2 (List.map inverseConstraint2 constraints)

        Or2 constraints ->
            And2 (List.map inverseConstraint2 constraints)


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
