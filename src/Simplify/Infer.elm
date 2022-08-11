module Simplify.Infer exposing
    ( Constraint(..)
    , Constraint2(..)
    , DeducedValue(..)
    , Inferred
    , Inferred2(..)
    , Resources
    , deduce
    , empty
    , empty2
    , falseExpr
    , get2
    , getConstraint
    , getInt
    , infer2
    , inferForIfCondition
    , inferForIfCondition2
    , mergeConstraints
    , trueExpr
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
        , deduced : AssocList.Dict Expression DeducedValue
        }


type DeducedValue
    = DTrue
    | DFalse
    | DFloat Float


type Constraint2
    = Equals2 Expression Expression
    | NotEquals2 Expression Expression
    | And2 Constraint2 Constraint2
    | Or2 Constraint2 Constraint2


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


get2 : Expression -> Inferred2 -> Maybe Expression
get2 expr (Inferred2 inferred) =
    AssocList.get expr inferred.deduced
        |> Maybe.map
            (\value ->
                case value of
                    DTrue ->
                        trueExpr

                    DFalse ->
                        falseExpr

                    DFloat float ->
                        Expression.Floatable float
            )


getConstraint : Expression -> Inferred -> Maybe Constraint
getConstraint expr (Inferred inferred) =
    AssocList.get expr inferred
        |> Maybe.map (\(Single c) -> c)


inferForIfCondition2 : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred2 -> List ( Range, Inferred2 )
inferForIfCondition2 condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer2 [ condition ] True inferred )
    , ( falseBranchRange, infer2 [ condition ] False inferred )
    ]


trueExpr : Expression
trueExpr =
    Expression.FunctionOrValue [ "Basics" ] "True"


falseExpr : Expression
falseExpr =
    Expression.FunctionOrValue [ "Basics" ] "False"


convertToConstraint : Expression -> Bool -> Constraint2
convertToConstraint expr shouldBe =
    if shouldBe then
        Equals2 expr trueExpr

    else
        Equals2 expr falseExpr


infer2 : List Expression -> Bool -> Inferred2 -> Inferred2
infer2 nodes shouldBe acc =
    case nodes of
        [] ->
            acc

        node :: rest ->
            let
                dict : Inferred2
                dict =
                    injectConstraints2 [ convertToConstraint node shouldBe ] acc
            in
            case node of
                Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
                    infer2
                        rest
                        shouldBe
                        (infer2 [ Node.value expression ] (not shouldBe) dict)

                Expression.OperatorApplication "&&" infix_ (Node _ left) (Node _ right) ->
                    if shouldBe then
                        dict
                            |> injectConstraints2
                                [ And2
                                    (convertToConstraint left True)
                                    (convertToConstraint right True)
                                ]
                            |> infer2 (left :: right :: rest) shouldBe

                    else
                        dict
                            |> injectConstraints2
                                [ Or2
                                    (convertToConstraint left False)
                                    (convertToConstraint right False)
                                ]
                            |> infer2 rest shouldBe

                Expression.OperatorApplication "||" infix_ (Node _ left) (Node _ right) ->
                    if shouldBe then
                        dict
                            |> injectConstraints2
                                [ Or2
                                    (convertToConstraint left True)
                                    (convertToConstraint right True)
                                ]
                            |> infer2 rest shouldBe

                    else
                        dict
                            |> injectConstraints2
                                [ And2
                                    (convertToConstraint left False)
                                    (convertToConstraint right False)
                                ]
                            |> infer2 [ left, right ] shouldBe
                            |> infer2 rest shouldBe

                Expression.OperatorApplication "==" _ left right ->
                    infer2 rest
                        shouldBe
                        (dict
                            |> inferOnEquality2 left right shouldBe
                            |> inferOnEquality2 right left shouldBe
                        )

                Expression.OperatorApplication "/=" _ left right ->
                    infer2 rest
                        shouldBe
                        (dict
                            |> inferOnEquality2 left right (not shouldBe)
                            |> inferOnEquality2 right left (not shouldBe)
                        )

                _ ->
                    infer2 rest shouldBe dict


injectConstraints2 : List Constraint2 -> Inferred2 -> Inferred2
injectConstraints2 newConstraints (Inferred2 inferred) =
    case newConstraints of
        [] ->
            Inferred2 inferred

        newConstraint :: restOfConstraints ->
            if List.member newConstraint inferred.constraints then
                injectConstraints2
                    restOfConstraints
                    (Inferred2 inferred)

            else
                let
                    { deduced, constraints } =
                        deduce
                            { newConstraint = newConstraint
                            , constraints = inferred.constraints
                            }
                            inferred

                    deducedFromNewConstraint : Maybe ( Expression, DeducedValue )
                    deducedFromNewConstraint =
                        case newConstraint of
                            Equals2 a b ->
                                equalsConstraint a b

                            NotEquals2 a b ->
                                equalsConstraint a b
                                    |> Maybe.andThen notDeduced

                            And2 _ _ ->
                                -- TODO Add "a && b && ..."?
                                Nothing

                            Or2 _ _ ->
                                -- TODO Add "a || b || ..."?
                                Nothing
                in
                injectConstraints2
                    (constraints ++ restOfConstraints)
                    (Inferred2
                        { constraints = newConstraint :: inferred.constraints
                        , deduced =
                            case deducedFromNewConstraint of
                                Just ( a, b ) ->
                                    AssocList.insert a b deduced

                                Nothing ->
                                    deduced
                        }
                    )


deduce :
    { newConstraint : Constraint2
    , constraints : List Constraint2
    }
    ->
        { deduced : AssocList.Dict Expression DeducedValue
        , constraints : List Constraint2
        }
    ->
        { deduced : AssocList.Dict Expression DeducedValue
        , constraints : List Constraint2
        }
deduce { newConstraint, constraints } acc =
    -- TODO Remove the constraints which we were able to deduce
    case constraints of
        [] ->
            acc

        constraint :: restOfConstraints ->
            let
                deducedFromThisConstraint : { deduced : List ( Expression, DeducedValue ), constraints : List Constraint2 }
                deducedFromThisConstraint =
                    mergeConstraints newConstraint constraint
            in
            deduce
                { newConstraint = newConstraint
                , constraints = restOfConstraints
                }
                { deduced = List.foldl (\( expr, value ) dict -> AssocList.insert expr value dict) acc.deduced deducedFromThisConstraint.deduced
                , constraints = deducedFromThisConstraint.constraints ++ acc.constraints
                }


mergeConstraints : Constraint2 -> Constraint2 -> { deduced : List ( Expression, DeducedValue ), constraints : List Constraint2 }
mergeConstraints newConstraint constraint =
    case newConstraint of
        Equals2 constraintTarget constraintValue ->
            case expressionToDeduced constraintValue of
                Just value ->
                    mergeEqualConstraints ( constraintTarget, value ) constraint

                Nothing ->
                    { deduced = [], constraints = [] }

        _ ->
            { deduced = [], constraints = [] }


equalsConstraint : Expression -> Expression -> Maybe ( Expression, DeducedValue )
equalsConstraint a b =
    case expressionToDeduced a of
        Just deducedValue ->
            Just ( b, deducedValue )

        Nothing ->
            case expressionToDeduced b of
                Just deducedValue ->
                    Just ( a, deducedValue )

                Nothing ->
                    Nothing


expressionToDeduced : Expression -> Maybe DeducedValue
expressionToDeduced expression =
    case expression of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            Just DTrue

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            Just DFalse

        Expression.Floatable float ->
            Just (DFloat float)

        _ ->
            Nothing


notDeduced : ( a, DeducedValue ) -> Maybe ( a, DeducedValue )
notDeduced ( a, deducedValue ) =
    case deducedValue of
        DTrue ->
            Just ( a, DFalse )

        DFalse ->
            Just ( a, DTrue )

        _ ->
            Nothing


mergeEqualConstraints : ( Expression, DeducedValue ) -> Constraint2 -> { deduced : List ( Expression, DeducedValue ), constraints : List Constraint2 }
mergeEqualConstraints ( target, value ) constraint =
    case constraint of
        Or2 left right ->
            case left of
                Equals2 constraintTarget constraintValue ->
                    if constraintTarget == target && areIncompatible value constraintValue then
                        { deduced = [], constraints = [ right ] }

                    else
                        { deduced = [], constraints = [] }

                _ ->
                    { deduced = [], constraints = [] }

        _ ->
            { deduced = [], constraints = [] }


areIncompatible : DeducedValue -> Expression -> Bool
areIncompatible value constraintValue =
    case ( value, constraintValue ) of
        ( DTrue, Expression.FunctionOrValue [ "Basics" ] "False" ) ->
            True

        ( DFalse, Expression.FunctionOrValue [ "Basics" ] "True" ) ->
            True

        ( DFloat valueFloat, Expression.Floatable constraintFloat ) ->
            if valueFloat == constraintFloat then
                True

            else
                False

        _ ->
            False


inferOnEquality2 : Node Expression -> Node Expression -> Bool -> Inferred2 -> Inferred2
inferOnEquality2 (Node _ expr) (Node _ other) shouldBe dict =
    case expr of
        Expression.Integer int ->
            if shouldBe then
                injectConstraints2
                    [ Equals2 other (Expression.Floatable (Basics.toFloat int)) ]
                    dict

            else
                injectConstraints2
                    [ NotEquals2 other (Expression.Floatable (Basics.toFloat int)) ]
                    dict

        Expression.Floatable float ->
            if shouldBe then
                injectConstraints2
                    [ Equals2 other (Expression.Floatable float) ]
                    dict

            else
                injectConstraints2
                    [ NotEquals2 other (Expression.Floatable float) ]
                    dict

        Expression.FunctionOrValue [ "Basics" ] "True" ->
            injectConstraints2
                [ Equals2 other
                    (if shouldBe then
                        trueExpr

                     else
                        falseExpr
                    )
                ]
                dict

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            injectConstraints2
                [ Equals2 other
                    (if shouldBe then
                        falseExpr

                     else
                        trueExpr
                    )
                ]
                dict

        _ ->
            dict


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
                    |> Maybe.andThen (\moduleName -> get2 (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants2))
            of
                Just (Expression.Integer int) ->
                    Just int

                _ ->
                    Nothing

        _ ->
            Nothing
