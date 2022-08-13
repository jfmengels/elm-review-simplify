module Simplify.Infer exposing
    ( Constraint(..)
    , DeducedValue(..)
    , Inferred(..)
    , Resources
    , deduceNewConstraints
    , empty
    , falseExpr
    , get
    , getInt
    , infer
    , inferForIfCondition
    , isBoolean
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
                        infer (left :: right :: rest) shouldBe dict

                    else
                        infer rest
                            shouldBe
                            (injectConstraints
                                [ Or
                                    (convertToConstraint left False)
                                    (convertToConstraint right False)
                                ]
                                dict
                            )

                Expression.OperatorApplication "||" _ (Node _ left) (Node _ right) ->
                    if shouldBe then
                        infer rest
                            shouldBe
                            (injectConstraints
                                [ Or
                                    (convertToConstraint left True)
                                    (convertToConstraint right True)
                                ]
                                dict
                            )

                    else
                        infer rest shouldBe (infer [ left, right ] shouldBe dict)

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
                    newConstraintsToVisit : List Constraint
                    newConstraintsToVisit =
                        deduceNewConstraints newConstraint inferred.constraints

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
                    (newConstraintsToVisit ++ restOfConstraints)
                    (Inferred
                        { constraints = newConstraint :: inferred.constraints
                        , deduced =
                            case deducedFromNewConstraint of
                                Just ( a, b ) ->
                                    AssocList.insert a b inferred.deduced

                                Nothing ->
                                    inferred.deduced
                        }
                    )


deduceNewConstraints : Constraint -> List Constraint -> List Constraint
deduceNewConstraints newConstraint constraints =
    case newConstraint of
        Equals constraintTarget constraintValue ->
            case expressionToDeduced constraintValue of
                Just value ->
                    List.concatMap (mergeEqualConstraints ( constraintTarget, value )) constraints

                Nothing ->
                    [ Equals constraintValue constraintTarget ]

        NotEquals _ _ ->
            []

        Or _ _ ->
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
mergeEqualConstraints equalConstraint constraint =
    case constraint of
        Or left right ->
            List.filterMap (ifSatisfy equalConstraint)
                [ ( left, right )
                , ( right, left )
                ]

        _ ->
            []


ifSatisfy : ( Expression, DeducedValue ) -> ( Constraint, a ) -> Maybe a
ifSatisfy ( target, value ) ( targetConstraint, otherConstraint ) =
    case targetConstraint of
        Equals constraintTarget constraintValue ->
            if constraintTarget == target && areIncompatible value constraintValue then
                Just otherConstraint

            else
                Nothing

        _ ->
            Nothing


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
