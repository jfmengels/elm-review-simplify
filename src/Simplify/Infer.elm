module Simplify.Infer exposing
    ( Constraint(..)
    , Constraint2(..)
    , Inferred
    , Inferred2(..)
    , Resources
    , deduce
    , empty
    , empty2
    , falseExpr
    , get
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
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
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
                    injectConstraints2 (convertToConstraint node shouldBe) acc
            in
            case node of
                Expression.FunctionOrValue _ _ ->
                    infer2 rest shouldBe dict

                Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
                    infer2
                        rest
                        shouldBe
                        (infer2 [ Node.value expression ] (not shouldBe) dict)

                Expression.OperatorApplication "&&" infix_ (Node _ left) (Node _ right) ->
                    if shouldBe then
                        dict
                            |> injectConstraints2
                                (And2
                                    (convertToConstraint left True)
                                    (convertToConstraint right True)
                                )
                            |> infer2 (left :: right :: rest) shouldBe

                    else
                        dict
                            |> injectConstraints2
                                (Or2
                                    (convertToConstraint left False)
                                    (convertToConstraint right False)
                                )
                            |> infer2 rest shouldBe

                Expression.OperatorApplication "||" infix_ (Node _ left) (Node _ right) ->
                    if shouldBe then
                        dict
                            |> injectConstraints2
                                (Or2
                                    (convertToConstraint left True)
                                    (convertToConstraint right True)
                                )
                            |> infer2 rest shouldBe

                    else
                        dict
                            |> injectConstraints2
                                (And2
                                    (convertToConstraint left False)
                                    (convertToConstraint right False)
                                )
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


notE : Expression -> Node Expression
notE node =
    case node of
        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
            expression

        _ ->
            Node Range.emptyRange
                (Expression.Application
                    [ Node Range.emptyRange (Expression.FunctionOrValue [ "Basics" ] "not")
                    , Node Range.emptyRange node
                    ]
                )


injectConstraints2 : Constraint2 -> Inferred2 -> Inferred2
injectConstraints2 newConstraint (Inferred2 inferred) =
    let
        { deduced, updatedConstraints } =
            deduce
                { newConstraint = newConstraint
                , constraints = inferred.constraints
                }
                { alreadySeen = []
                , deduced = inferred.deduced
                , updatedConstraints = inferred.constraints
                }

        deducedFromNewConstraint : Maybe ( Expression, Expression )
        deducedFromNewConstraint =
            case newConstraint of
                Equals2 a b ->
                    Just (equalsConstraint a b)

                NotEquals2 a b ->
                    notEqualsConstraint a b

                And2 _ _ ->
                    -- TODO Add "a && b && ..."?
                    Nothing

                Or2 _ _ ->
                    -- TODO Add "a || b || ..."?
                    Nothing
    in
    Inferred2
        { constraints = newConstraint :: updatedConstraints
        , deduced =
            case deducedFromNewConstraint of
                Just ( a, b ) ->
                    AssocList.insert a b deduced

                Nothing ->
                    deduced
        }


injectEqualsInDeduced : Expression -> Expression -> AssocList.Dict Expression Expression -> AssocList.Dict Expression Expression
injectEqualsInDeduced a b deduced =
    if a == trueExpr || a == falseExpr then
        if b == trueExpr || b == falseExpr then
            deduced

        else
            injectEqualsInDeduced b a deduced

    else if b == trueExpr || b == falseExpr then
        AssocList.insert a b deduced

    else
        deduced
            |> AssocList.insert a b
            |> AssocList.insert b a
            |> AssocList.insert (equals a b) trueExpr
            |> AssocList.insert (equals b a) trueExpr
            |> AssocList.insert (notEquals a b) falseExpr
            |> AssocList.insert (notEquals b a) falseExpr


equalsConstraint : Expression -> Expression -> ( Expression, Expression )
equalsConstraint a b =
    if a == trueExpr || a == falseExpr then
        ( b, a )

    else
        ( a, b )


injectNotEqualsInDeduced : Expression -> Expression -> AssocList.Dict Expression Expression -> AssocList.Dict Expression Expression
injectNotEqualsInDeduced a b deduced =
    if a == trueExpr || a == falseExpr then
        if b == trueExpr || b == falseExpr then
            deduced

        else
            injectNotEqualsInDeduced b a deduced

    else if b == falseExpr then
        AssocList.insert a trueExpr deduced

    else if b == trueExpr then
        deduced
            |> AssocList.insert (notEquals a b) trueExpr
            |> AssocList.insert (equals a b) falseExpr

    else
        deduced
            |> AssocList.insert (notEquals a b) trueExpr
            |> AssocList.insert (notEquals b a) trueExpr
            |> AssocList.insert (equals a b) falseExpr
            |> AssocList.insert (equals b a) falseExpr


notEqualsConstraint : Expression -> Expression -> Maybe ( Expression, Expression )
notEqualsConstraint a b =
    if b == falseExpr then
        Just ( a, trueExpr )

    else if b == trueExpr then
        Just ( a, falseExpr )

    else if a == falseExpr then
        Just ( b, trueExpr )

    else if a == trueExpr then
        Just ( b, falseExpr )

    else
        Nothing


equals : Expression -> Expression -> Expression
equals a b =
    Expression.OperatorApplication "==" Infix.Non (Node Range.emptyRange a) (Node Range.emptyRange b)


notEquals : Expression -> Expression -> Expression
notEquals a b =
    Expression.OperatorApplication "/=" Infix.Non (Node Range.emptyRange a) (Node Range.emptyRange b)


deduce :
    { newConstraint : Constraint2
    , constraints : List Constraint2
    }
    ->
        { alreadySeen : List Constraint2
        , deduced : AssocList.Dict Expression Expression
        , updatedConstraints : List Constraint2
        }
    ->
        { alreadySeen : List Constraint2
        , deduced : AssocList.Dict Expression Expression
        , updatedConstraints : List Constraint2
        }
deduce { newConstraint, constraints } acc =
    case constraints of
        [] ->
            acc

        constraint :: restOfConstraints ->
            if List.member constraint acc.alreadySeen then
                deduce
                    { newConstraint = newConstraint
                    , constraints = restOfConstraints
                    }
                    acc

            else
                let
                    newParams : { newConstraint : Constraint2, constraints : List Constraint2 }
                    newParams =
                        { newConstraint = newConstraint
                        , constraints = restOfConstraints
                        }

                    newAcc : { alreadySeen : List Constraint2, deduced : AssocList.Dict Expression Expression, updatedConstraints : List Constraint2 }
                    newAcc =
                        { acc | alreadySeen = constraint :: acc.alreadySeen }
                in
                case constraint of
                    Or2 left right ->
                        if left == newConstraint then
                            let
                                res : { alreadySeen : List Constraint2, deduced : AssocList.Dict Expression Expression, updatedConstraints : List Constraint2 }
                                res =
                                    deduce newParams newAcc
                            in
                            case addDeducedOrConstraint right of
                                Just ( a, b ) ->
                                    { res | deduced = AssocList.insert a b res.deduced }

                                Nothing ->
                                    deduce { newParams | newConstraint = right } res

                        else if right == newConstraint then
                            let
                                res : { alreadySeen : List Constraint2, deduced : AssocList.Dict Expression Expression, updatedConstraints : List Constraint2 }
                                res =
                                    deduce newParams newAcc
                            in
                            case addDeducedOrConstraint left of
                                Just ( a, b ) ->
                                    { res | deduced = AssocList.insert a b res.deduced }

                                Nothing ->
                                    deduce { newParams | newConstraint = left } res

                        else
                            deduce newParams newAcc

                    _ ->
                        deduce newParams newAcc


mergeConstraints : ( Expression, Expression ) -> Constraint2 -> { deduced : List ( Expression, Expression ), constraints : List Constraint2 }
mergeConstraints ( target, value ) constraint =
    case constraint of
        Or2 left right ->
            case left of
                Equals2 constraintTarget constraintValue ->
                    if constraintTarget == target then
                        { deduced = []
                        , constraints = [ right ]
                        }

                    else
                        { deduced = [], constraints = [] }

                _ ->
                    { deduced = [], constraints = [] }

        _ ->
            { deduced = [], constraints = [] }


addDeducedOrConstraint : Constraint2 -> Maybe ( Expression, Expression )
addDeducedOrConstraint constraint =
    case constraint of
        Equals2 a b ->
            Just ( a, b )

        NotEquals2 _ _ ->
            Nothing

        And2 _ _ ->
            Nothing

        Or2 _ _ ->
            Nothing


inferOnEquality2 : Node Expression -> Node Expression -> Bool -> Inferred2 -> Inferred2
inferOnEquality2 (Node _ expr) (Node _ other) shouldBe dict =
    case expr of
        Expression.Integer int ->
            if shouldBe then
                injectConstraints2
                    (Equals2 other (Expression.Floatable (Basics.toFloat int)))
                    dict

            else
                injectConstraints2
                    (NotEquals2 other (Expression.Floatable (Basics.toFloat int)))
                    dict

        Expression.Floatable float ->
            if shouldBe then
                injectConstraints2
                    (Equals2 other (Expression.Floatable float))
                    dict

            else
                injectConstraints2
                    (NotEquals2 other (Expression.Floatable float))
                    dict

        Expression.FunctionOrValue [ "Basics" ] "True" ->
            injectConstraints2
                (Equals2 other
                    (if shouldBe then
                        trueExpr

                     else
                        falseExpr
                    )
                )
                dict

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            injectConstraints2
                (Equals2 other
                    (if shouldBe then
                        falseExpr

                     else
                        trueExpr
                    )
                )
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
