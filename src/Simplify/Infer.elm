module Simplify.Infer exposing
    ( Constraint(..)
    , DeducedValue(..)
    , Inferred(..)
    , Resources
    , empty
    , falseExpr
    , get
    , getInt
    , infer
    , inferForIfCondition
    , isBoolean
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
    = Inferred
        { constraints : List Constraint
        , deduced : AssocList.Dict Expression DeducedValue
        }


type DeducedValue
    = DTrue
    | DFalse
    | DFloat Float


type Constraint
    = Equals Expression Expression
    | NotEquals Expression Expression
    | Or Constraint Constraint


type alias Resources a =
    { a
        | lookupTable : ModuleNameLookupTable
        , inferredConstants : ( Inferred, List Inferred )
    }


empty : Inferred
empty =
    Inferred
        { constraints = []
        , deduced = AssocList.empty
        }


get : Expression -> Inferred -> Maybe Expression
get expr (Inferred inferred) =
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


isBoolean : Expression -> Inferred -> Maybe Bool
isBoolean expr (Inferred inferred) =
    AssocList.get expr inferred.deduced
        |> Maybe.andThen
            (\value ->
                case value of
                    DTrue ->
                        Just True

                    DFalse ->
                        Just False

                    DFloat _ ->
                        Nothing
            )


inferForIfCondition : Expression -> { trueBranchRange : Range, falseBranchRange : Range } -> Inferred -> List ( Range, Inferred )
inferForIfCondition condition { trueBranchRange, falseBranchRange } inferred =
    [ ( trueBranchRange, infer [ condition ] True inferred )
    , ( falseBranchRange, infer [ condition ] False inferred )
    ]


trueExpr : Expression
trueExpr =
    Expression.FunctionOrValue [ "Basics" ] "True"


falseExpr : Expression
falseExpr =
    Expression.FunctionOrValue [ "Basics" ] "False"


convertToConstraint : Expression -> Bool -> Constraint
convertToConstraint expr shouldBe =
    if shouldBe then
        Equals expr trueExpr

    else
        Equals expr falseExpr


infer : List Expression -> Bool -> Inferred -> Inferred
infer nodes shouldBe acc =
    case nodes of
        [] ->
            acc

        node :: rest ->
            let
                dict : Inferred
                dict =
                    injectConstraints [ convertToConstraint node shouldBe ] acc
            in
            case node of
                Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
                    infer
                        rest
                        shouldBe
                        (infer [ Node.value expression ] (not shouldBe) dict)

                Expression.OperatorApplication "&&" _ (Node _ left) (Node _ right) ->
                    if shouldBe then
                        dict
                            |> infer (left :: right :: rest) shouldBe

                    else
                        dict
                            |> injectConstraints
                                [ Or
                                    (convertToConstraint left False)
                                    (convertToConstraint right False)
                                ]
                            |> infer rest shouldBe

                Expression.OperatorApplication "||" _ (Node _ left) (Node _ right) ->
                    if shouldBe then
                        dict
                            |> injectConstraints
                                [ Or
                                    (convertToConstraint left True)
                                    (convertToConstraint right True)
                                ]
                            |> infer rest shouldBe

                    else
                        dict
                            |> infer [ left, right ] shouldBe
                            |> infer rest shouldBe

                Expression.OperatorApplication "==" _ left right ->
                    infer rest
                        shouldBe
                        (dict
                            |> inferOnEquality left right shouldBe
                            |> inferOnEquality right left shouldBe
                        )

                Expression.OperatorApplication "/=" _ left right ->
                    infer rest
                        shouldBe
                        (dict
                            |> inferOnEquality left right (not shouldBe)
                            |> inferOnEquality right left (not shouldBe)
                        )

                _ ->
                    infer rest shouldBe dict


injectConstraints : List Constraint -> Inferred -> Inferred
injectConstraints newConstraints (Inferred inferred) =
    case newConstraints of
        [] ->
            Inferred inferred

        newConstraint :: restOfConstraints ->
            if List.member newConstraint inferred.constraints then
                injectConstraints
                    restOfConstraints
                    (Inferred inferred)

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
                            Equals a b ->
                                equalsConstraint a b

                            NotEquals a b ->
                                equalsConstraint a b
                                    |> Maybe.andThen notDeduced

                            Or _ _ ->
                                -- TODO Add "a || b || ..."?
                                Nothing
                in
                injectConstraints
                    (constraints ++ restOfConstraints)
                    (Inferred
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
    { newConstraint : Constraint
    , constraints : List Constraint
    }
    ->
        { deduced : AssocList.Dict Expression DeducedValue
        , constraints : List Constraint
        }
    ->
        { deduced : AssocList.Dict Expression DeducedValue
        , constraints : List Constraint
        }
deduce { newConstraint, constraints } acc =
    -- TODO Remove the constraints which we were able to deduce
    case constraints of
        [] ->
            acc

        constraint :: restOfConstraints ->
            deduce
                { newConstraint = newConstraint
                , constraints = restOfConstraints
                }
                { deduced = acc.deduced
                , constraints = mergeConstraints newConstraint constraint ++ acc.constraints
                }


mergeConstraints : Constraint -> Constraint -> List Constraint
mergeConstraints newConstraint constraint =
    case newConstraint of
        Equals constraintTarget constraintValue ->
            case expressionToDeduced constraintValue of
                Just value ->
                    mergeEqualConstraints ( constraintTarget, value ) constraint

                Nothing ->
                    [ Equals constraintValue constraintTarget ]

        _ ->
            []


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


mergeEqualConstraints : ( Expression, DeducedValue ) -> Constraint -> List Constraint
mergeEqualConstraints ( target, value ) constraint =
    case constraint of
        Or left right ->
            case left of
                Equals constraintTarget constraintValue ->
                    if constraintTarget == target && areIncompatible value constraintValue then
                        [ right ]

                    else
                        []

                _ ->
                    []

        _ ->
            []


areIncompatible : DeducedValue -> Expression -> Bool
areIncompatible value constraintValue =
    case ( value, constraintValue ) of
        ( DTrue, Expression.FunctionOrValue [ "Basics" ] "False" ) ->
            True

        ( DFalse, Expression.FunctionOrValue [ "Basics" ] "True" ) ->
            True

        ( DFloat valueFloat, Expression.Floatable constraintFloat ) ->
            valueFloat /= constraintFloat

        _ ->
            False


inferOnEquality : Node Expression -> Node Expression -> Bool -> Inferred -> Inferred
inferOnEquality (Node _ expr) (Node _ other) shouldBe dict =
    case expr of
        Expression.Integer int ->
            if shouldBe then
                injectConstraints
                    [ Equals other (Expression.Floatable (Basics.toFloat int)) ]
                    dict

            else
                injectConstraints
                    [ NotEquals other (Expression.Floatable (Basics.toFloat int)) ]
                    dict

        Expression.Floatable float ->
            if shouldBe then
                injectConstraints
                    [ Equals other (Expression.Floatable float) ]
                    dict

            else
                injectConstraints
                    [ NotEquals other (Expression.Floatable float) ]
                    dict

        Expression.FunctionOrValue [ "Basics" ] "True" ->
            injectConstraints
                [ Equals other
                    (if shouldBe then
                        trueExpr

                     else
                        falseExpr
                    )
                ]
                dict

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            injectConstraints
                [ Equals other
                    (if shouldBe then
                        falseExpr

                     else
                        trueExpr
                    )
                ]
                dict

        _ ->
            dict


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
