module Simplify.Infer exposing
    ( DeducedValue(..)
    , Fact(..)
    , Inferred(..)
    , NumberRange
    , Resources
    , belongsTo
    , deduceNewFacts
    , empty
    , falseExpr
    , get
    , infer
    , inferForIfCondition
    , intersect
    , isBoolean
    , trueExpr
    , unite
    )

{-| Infers values from `if` conditions.

This is meant to simplify expressions like the following:

```diff
if a then
   -- we know that `a` is True
-  if a && b then
+  if b then
```


### Mechanism

The way that this is done is by collecting "facts" about the conditions we've found. Given the following expression:

    if a && b == 1 then
        1

    else
        2

we can infer that in the `then` branch, the following facts are true:

  - `a && b == 1` is True
  - `a` is True
  - `b == 1` is True
  - `b` equals `1`

and for the `else` branch, that:

  - `a && b == 1` is False
  - `a` is False OR `b == 1` is False
  - `a` is False OR `b /= 1` is True

For a condition like `a || b`, we know that in the `then` branch:

  - `a` is True OR `b` is True

and that in the `else` branch:

  - `a || b` is `False`
  - `a` is `False`
  - `b` is `False`

Whenever we get a new fact from a new `if` condition, we then go through all the previously known facts and see if the
new one can simplify some of the old ones to generate new facts.

For instance, if we knew that `a` is True OR `b` is True, and we encounter `if a then`, then we can infer that for the `else` branch `a` is False.
When comparing that to `a` is True OR `b` is True, we can infer that `b` is True.

Every new fact that we uncover from this comparison will also repeat the process of going through the previous list of facts.

Another thing that we do whenever we encounter a new fact is to try and "deduce" a value from it, which we add to a list
of "deduced" values. A few examples:

  - `a` -> `a` is True
  - `a == 1` -> `a` is equal to `1`
  - `a /= 1` -> `a` is either less than `1` or more than `1`
  - `a` OR `b` -> Can't infer individual values when this is True

(with the exception that we can infer that the whole expression is `True` or `False`)

Before we do all of this analysis, we normalize the AST, so we have a more predictable AST and don't have to do as many checks.


### Application

This data is then used in `Normalize` to change the AST, so that a reference to `a` whose value we have "deduced" is
replaced by that value. Finally, that data is also used in functions like `Evaluate.getBoolean`.
(Note: This might be a bit redundant but that's a simplification for later on)

Whenever we see a boolean expression, we will look at whether we can simplify it, and report an error when that happens.


### Limits

The system does not currently handle `case` expressions. While handling pattern matching against literals should not be
too hard with the current system, storing "shapes" of the value (the value is a `Just` of something) probably requires
some work.

-}

import AssocList
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


type Inferred
    = Inferred
        { facts : List Fact
        , deduced : AssocList.Dict Expression DeducedValue
        }


type DeducedValue
    = DBool Bool (List Bool) -- We could just have a tri-state, but this is more similar to the others and more comfortable
    | DNumber NumberRange (List NumberRange)
    | DStringOneOf String (List String)
    | DStringNeitherOf String (List String)
    | DCharOneOf Char (List Char)
    | DCharNeitherOf Char (List Char)


type alias NumberRange =
    { from : Float
    , fromIncluded : Bool
    , to : Float
    , toIncluded : Bool
    }


infinity : Float
infinity =
    1 / 0


negInfinity : Float
negInfinity =
    -1 / 0


type Fact
    = Equals Expression Expression
    | NotEquals Expression Expression
      -- LessThan x y means x < y
    | LessThan Expression Expression
      -- LessThanOrEquals x y means x <= y
    | LessThanOrEquals Expression Expression
    | Or Fact Fact


type alias Resources a =
    { a
        | lookupTable : ModuleNameLookupTable

        -- This is a nonempty stack. Every time we enter an if we push one frame
        , inferredConstants : ( Inferred, List Inferred )
    }


empty : Inferred
empty =
    Inferred
        { facts = []
        , deduced = AssocList.empty
        }


get : Expression -> Inferred -> Maybe Expression
get expr (Inferred inferred) =
    AssocList.get expr inferred.deduced
        |> Maybe.andThen
            (\value ->
                case value of
                    DBool True [] ->
                        Just <| trueExpr

                    DBool False [] ->
                        Just <| falseExpr

                    DBool _ _ ->
                        Nothing

                    DNumber { from, fromIncluded, to, toIncluded } [] ->
                        if fromIncluded && toIncluded && from == to then
                            Just <| Expression.Floatable from

                        else
                            Nothing

                    DNumber _ _ ->
                        Nothing

                    DStringOneOf s [] ->
                        Just <| Expression.Literal s

                    DStringOneOf _ _ ->
                        Nothing

                    DStringNeitherOf _ _ ->
                        Nothing

                    DCharOneOf c [] ->
                        Just <| Expression.CharLiteral c

                    DCharOneOf _ _ ->
                        Nothing

                    DCharNeitherOf _ _ ->
                        Nothing
            )


isBoolean : Expression -> Inferred -> Maybe Bool
isBoolean expr (Inferred inferred) =
    AssocList.get expr inferred.deduced
        |> Maybe.andThen
            (\value ->
                case value of
                    DBool b [] ->
                        Just b

                    DBool _ _ ->
                        Nothing

                    DNumber _ _ ->
                        Nothing

                    DStringOneOf _ _ ->
                        Nothing

                    DCharOneOf _ _ ->
                        Nothing

                    DStringNeitherOf _ _ ->
                        Nothing

                    DCharNeitherOf _ _ ->
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


convertToFact : Expression -> Bool -> Fact
convertToFact expr shouldBe =
    if shouldBe then
        Equals expr trueExpr

    else
        Equals expr falseExpr


infer : List Expression -> Bool -> Inferred -> Inferred
infer nodes shouldBe acc =
    List.foldl (inferHelp shouldBe) acc nodes


inferHelp : Bool -> Expression -> Inferred -> Inferred
inferHelp shouldBe node acc =
    let
        dict : Inferred
        dict =
            injectFacts [ convertToFact node shouldBe ] acc
    in
    case node of
        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
            inferHelp (not shouldBe) (Node.value expression) dict

        Expression.OperatorApplication "&&" _ (Node _ left) (Node _ right) ->
            if shouldBe then
                infer [ left, right ] shouldBe dict

            else
                injectFacts
                    [ Or
                        (convertToFact left False)
                        (convertToFact right False)
                    ]
                    dict

        Expression.OperatorApplication "||" _ (Node _ left) (Node _ right) ->
            if shouldBe then
                injectFacts
                    [ Or
                        (convertToFact left True)
                        (convertToFact right True)
                    ]
                    dict

            else
                infer [ left, right ] shouldBe dict

        Expression.OperatorApplication "==" _ left right ->
            dict
                |> injectFacts (inferOnEquality left right shouldBe)
                |> injectFacts (inferOnEquality right left shouldBe)

        Expression.OperatorApplication "/=" _ left right ->
            dict
                |> injectFacts (inferOnEquality left right (not shouldBe))
                |> injectFacts (inferOnEquality right left (not shouldBe))

        Expression.OperatorApplication "<" _ left right ->
            dict
                |> injectFacts (inferOnLessThan left right shouldBe)
                |> injectFacts (inferOnLessThanOrEqual right left (not shouldBe))

        _ ->
            dict


injectFacts : List Fact -> Inferred -> Inferred
injectFacts newFacts (Inferred inferred) =
    case newFacts of
        [] ->
            Inferred inferred

        newFact :: restOfFacts ->
            if List.member newFact inferred.facts then
                injectFacts
                    restOfFacts
                    (Inferred inferred)

            else
                let
                    newFactsToVisit : List Fact
                    newFactsToVisit =
                        deduceNewFacts newFact inferred.facts

                    deducedFromNewFact : Maybe ( Expression, DeducedValue )
                    deducedFromNewFact =
                        case newFact of
                            Equals a b ->
                                equalsFact a b

                            NotEquals a b ->
                                equalsFact a b
                                    |> Maybe.andThen notDeduced

                            LessThan _ _ ->
                                Nothing

                            LessThanOrEquals _ _ ->
                                Nothing

                            Or _ _ ->
                                -- TODO Add "a || b || ..."?
                                Nothing
                in
                injectFacts
                    (newFactsToVisit ++ restOfFacts)
                    (Inferred
                        { facts = newFact :: inferred.facts
                        , deduced =
                            case deducedFromNewFact of
                                Just ( a, b ) ->
                                    AssocList.insert a b inferred.deduced

                                Nothing ->
                                    inferred.deduced
                        }
                    )


deduceNewFacts : Fact -> List Fact -> List Fact
deduceNewFacts newFact facts =
    case newFact of
        Equals factTarget factValue ->
            case expressionToDeduced factValue of
                Just value ->
                    List.concatMap (mergeEqualFacts ( factTarget, value )) facts

                Nothing ->
                    [ Equals factValue factTarget ]

        NotEquals _ _ ->
            []

        Or _ _ ->
            []

        LessThan _ _ ->
            []

        LessThanOrEquals _ _ ->
            []


equalsFact : Expression -> Expression -> Maybe ( Expression, DeducedValue )
equalsFact a b =
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
            Just (DBool True [])

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            Just (DBool False [])

        Expression.Floatable float ->
            Just
                (DNumber
                    { from = float
                    , to = float
                    , fromIncluded = True
                    , toIncluded = True
                    }
                    []
                )

        Expression.Literal string ->
            Just (DStringOneOf string [])

        Expression.CharLiteral char ->
            Just (DCharOneOf char [])

        _ ->
            Nothing


notDeduced : ( a, DeducedValue ) -> Maybe ( a, DeducedValue )
notDeduced ( a, deducedValue ) =
    case deducedValue of
        DBool option options ->
            Just ( a, DBool (not option) (List.map not options) )

        DStringOneOf s o ->
            Just ( a, DStringNeitherOf s o )

        DStringNeitherOf s o ->
            Just ( a, DStringOneOf s o )

        DCharOneOf s o ->
            Just ( a, DCharNeitherOf s o )

        DCharNeitherOf s o ->
            Just ( a, DCharOneOf s o )

        DNumber s o ->
            let
                invertRange { from, to, fromIncluded, toIncluded } =
                    (if from > negInfinity then
                        [ if fromIncluded then
                            lessThan from

                          else
                            lessThanOrEquals from
                        ]

                     else
                        []
                    )
                        ++ (if to < infinity then
                                [ if toIncluded then
                                    moreThan to

                                  else
                                    moreThanOrEquals to
                                ]

                            else
                                []
                           )

                step l r =
                    List.concatMap (\le -> List.filterMap (intersect le) r) l
                        -- We have a list of ranges, if we sort them by `from` we'll be able to merge them (if possible) by just considering adjacent ones
                        |> List.sortBy (\{ from } -> -from)
                        |> List.foldl
                            (\e acc ->
                                case acc of
                                    [] ->
                                        [ e ]

                                    last :: rest ->
                                        case unite e last of
                                            Just u ->
                                                u :: rest

                                            Nothing ->
                                                e :: acc
                            )
                            []
                        |> List.reverse
            in
            case
                o
                    |> List.map invertRange
                    |> List.foldl step (invertRange s)
            of
                [] ->
                    Nothing

                h :: t ->
                    Just ( a, DNumber h t )


unite : NumberRange -> NumberRange -> Maybe NumberRange
unite l r =
    if r.from < l.from then
        unite r l

    else if l.to < r.from || (l.to == r.from && not l.toIncluded && not r.fromIncluded) then
        Nothing

    else
        Just
            { from = l.from
            , fromIncluded = l.fromIncluded || (l.from == r.from && r.fromIncluded)
            , to = max l.to r.to
            , toIncluded =
                if l.to < r.to then
                    r.toIncluded

                else if l.to > r.to then
                    l.toIncluded

                else
                    l.toIncluded || r.toIncluded
            }


lessThan : Float -> NumberRange
lessThan float =
    { from = negInfinity, fromIncluded = False, to = float, toIncluded = False }


lessThanOrEquals : Float -> NumberRange
lessThanOrEquals float =
    { from = negInfinity, fromIncluded = False, to = float, toIncluded = True }


moreThan : Float -> NumberRange
moreThan float =
    { from = float, fromIncluded = False, to = infinity, toIncluded = False }


moreThanOrEquals : Float -> NumberRange
moreThanOrEquals float =
    { from = float, fromIncluded = True, to = infinity, toIncluded = False }


intersect : NumberRange -> NumberRange -> Maybe NumberRange
intersect lr rr =
    let
        ( from, fromIncluded ) =
            if lr.from > rr.from then
                ( lr.from, lr.fromIncluded )

            else if lr.from < rr.from then
                ( rr.from, rr.fromIncluded )

            else
                ( lr.from, lr.fromIncluded && rr.fromIncluded )

        ( to, toIncluded ) =
            if lr.to < rr.to then
                ( lr.to, lr.toIncluded )

            else if lr.to > rr.to then
                ( rr.to, rr.toIncluded )

            else
                ( lr.to, lr.toIncluded && rr.toIncluded )
    in
    if from < to || (from == to && fromIncluded && toIncluded) then
        Just { from = from, fromIncluded = fromIncluded, to = to, toIncluded = toIncluded }

    else
        Nothing


belongsTo : NumberRange -> Float -> Bool
belongsTo { from, to, fromIncluded, toIncluded } float =
    (float > from || (float == from && fromIncluded))
        && (float < to || (float == to && toIncluded))


mergeEqualFacts : ( Expression, DeducedValue ) -> Fact -> List Fact
mergeEqualFacts equalFact fact =
    case fact of
        Or left right ->
            List.filterMap (ifSatisfy equalFact)
                [ ( left, right )
                , ( right, left )
                ]

        _ ->
            []


ifSatisfy : ( Expression, DeducedValue ) -> ( Fact, a ) -> Maybe a
ifSatisfy ( target, value ) ( targetFact, otherFact ) =
    case targetFact of
        Equals factTarget factValue ->
            if factTarget == target && areIncompatible value factValue then
                Just otherFact

            else
                Nothing

        _ ->
            Nothing


areIncompatible : DeducedValue -> Expression -> Bool
areIncompatible value factValue =
    case ( value, factValue ) of
        ( DBool True [], Expression.FunctionOrValue [ "Basics" ] "False" ) ->
            True

        ( DBool False [], Expression.FunctionOrValue [ "Basics" ] "True" ) ->
            True

        ( DNumber range ranges, Expression.Floatable factFloat ) ->
            List.all (isIncompatibleWithRange factFloat) (range :: ranges)

        ( DStringOneOf oneOf oneOfs, Expression.Literal constraintString ) ->
            not (List.member constraintString (oneOf :: oneOfs))

        ( DStringNeitherOf neitherOf neitherOfs, Expression.Literal constraintString ) ->
            List.member constraintString (neitherOf :: neitherOfs)

        ( DCharOneOf oneOf oneOfs, Expression.CharLiteral constraintChar ) ->
            not (List.member constraintChar (oneOf :: oneOfs))

        ( DCharNeitherOf neitherOf neitherOfs, Expression.CharLiteral constraintChar ) ->
            List.member constraintChar (neitherOf :: neitherOfs)

        _ ->
            False


isIncompatibleWithRange : Float -> NumberRange -> Bool
isIncompatibleWithRange float range =
    not (belongsTo range float)


inferOnEquality : Node Expression -> Node Expression -> Bool -> List Fact
inferOnEquality (Node _ expr) (Node _ other) shouldBe =
    case expr of
        Expression.Integer int ->
            if shouldBe then
                [ Equals other (Expression.Floatable (Basics.toFloat int)) ]

            else
                [ NotEquals other (Expression.Floatable (Basics.toFloat int)) ]

        Expression.Floatable float ->
            if shouldBe then
                [ Equals other (Expression.Floatable float) ]

            else
                [ NotEquals other (Expression.Floatable float) ]

        Expression.Literal str ->
            if shouldBe then
                [ Equals other (Expression.Literal str) ]

            else
                [ NotEquals other (Expression.Literal str) ]

        Expression.FunctionOrValue [ "Basics" ] "True" ->
            [ Equals other
                (if shouldBe then
                    trueExpr

                 else
                    falseExpr
                )
            ]

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            [ Equals other
                (if shouldBe then
                    falseExpr

                 else
                    trueExpr
                )
            ]

        _ ->
            []


inferOnLessThan : Node Expression -> Node Expression -> Bool -> List Fact
inferOnLessThan (Node _ expr) (Node _ other) shouldBe =
    case expr of
        Expression.Integer int ->
            let
                floatExpression =
                    Expression.Floatable (Basics.toFloat int)
            in
            if shouldBe then
                [ LessThan floatExpression other ]

            else
                [ LessThanOrEquals other floatExpression ]

        Expression.Floatable float ->
            let
                floatExpression =
                    Expression.Floatable float
            in
            if shouldBe then
                [ LessThan floatExpression other ]

            else
                [ LessThanOrEquals other floatExpression ]

        _ ->
            []


inferOnLessThanOrEqual : Node Expression -> Node Expression -> Bool -> List Fact
inferOnLessThanOrEqual (Node _ expr) (Node _ other) shouldBe =
    case expr of
        Expression.Integer int ->
            let
                floatExpression =
                    Expression.Floatable (Basics.toFloat int)
            in
            if shouldBe then
                [ LessThanOrEquals floatExpression other ]

            else
                [ LessThan other floatExpression ]

        Expression.Floatable float ->
            let
                floatExpression =
                    Expression.Floatable float
            in
            if shouldBe then
                [ LessThanOrEquals floatExpression other ]

            else
                [ LessThan other floatExpression ]

        _ ->
            []
