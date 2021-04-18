module Simplify exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Simplify.Normalize as Normalize


{-| Reports when an operation on lists could be simplified to a single literal list.

    config =
        [ Simplify.rule
        ]


## Simplifications


### Booleans

    x || True
    --> True

    x || False
    --> x

    x && True
    --> x

    x && False
    --> False

    not True
    --> False

    anything == anything
    --> True

    anything /= anything
    --> False


### If expressions

    if True then x else y
    --> x

    if False then x else y
    --> y

    if condition then x else x
    --> x

    if condition then True else False
    --> condition

    if condition then False else True
    --> not condition


### Basics functions

    identity x
    --> x


    always x y
    --> x


### Operators

    (++) a b
    --> a ++ b


### Lists

    a :: []
    --> [ a ]

    a :: [ b ]
    --> [ a, b ]

    [a] ++ list
    --> a :: list

    [] ++ list
    --> list

    [ a, b ] ++ [ c ]
    --> [ a, b, c ]

    [ a, b ] ++ [ c ]
    --> [ a, b, c ]

    List.concat []
    --> []

    List.concat [ [ a, b ], [ c ] ]
    --> [ a, b, c ]

    List.concat [ a, [ 1 ], [ 2 ] ]
    --> List.concat [ a, [ 1, 2 ] ]

    List.concatMap identity x
    --> List.concat list

    List.concatMap identity
    --> List.concat

    List.concatMap (\a -> a) list
    --> List.concat list

    List.concatMap fn [ x ]
    --> fn x

    List.concatMap (always []) list
    --> []

    List.map fn [] -- same for List.filter, List.filterMap, List.concatMap
    --> []

    List.map identity list
    --> list

    List.map identity
    --> identity

    List.filter (always True) list
    --> list

    List.filter (\a -> True) list
    --> list

    List.filter (always False) list
    --> []

    List.filter (always True)
    --> identity

    List.filter (always False)
    --> always []

    List.filterMap Just list
    --> list

    List.filterMap (\a -> Just a) list
    --> list

    List.filterMap Just
    --> identity

    List.filterMap (always Nothing) list
    --> []

    List.filterMap (always Nothing)
    --> (always [])

    List.isEmpty []
    --> True

    List.isEmpty [ a ]
    --> False

    List.isEmpty (x :: xs)
    --> False

    List.all fn []
    --> True

    List.all (always True) list
    --> True

    List.any fn []
    --> True

    List.any (always False) list
    --> True

    List.range 6 3
    --> []

    List.length [ a ]
    --> 1


## Success

    _ =
        [ 1, 2, 3, 4, mysteryNumber, 6 ]

    _ =
        [ 1, 2, 3 ] ++ list ++ [ 4, mysteryNumber, 6 ]

    _ =
        List.concat
            [ [ 1, 2, 3 ]
            , list
            , [ 4, mysteryNumber, 6 ]
            ]


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example --rules Simplify
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "Simplify" initialContext
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , rangesToIgnore : List Range
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , rangesToIgnore = []
            }
        )
        |> Rule.withModuleNameLookupTable


errorForAddingEmptyLists : Range -> Range -> Error {}
errorForAddingEmptyLists range rangeToRemove =
    Rule.errorWithFix
        { message = "Concatenating with a single list doesn't have any effect"
        , details = [ "You should remove the concatenation with the empty list." ]
        }
        range
        [ Fix.removeRange rangeToRemove ]



-- DECLARATION VISITOR


declarationVisitor : Node a -> Context -> ( List nothing, Context )
declarationVisitor _ context =
    ( [], { context | rangesToIgnore = [] } )



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    if List.member (Node.range node) context.rangesToIgnore then
        ( [], context )

    else
        let
            ( errors, rangesToIgnore ) =
                expressionVisitorHelp node context
        in
        ( errors, { context | rangesToIgnore = rangesToIgnore ++ context.rangesToIgnore } )


expressionVisitorHelp : Node Expression -> Context -> ( List (Error {}), List Range )
expressionVisitorHelp node { lookupTable } =
    case Node.value node of
        -------------------
        -- BOOLEAN LOGIC --
        -------------------
        Expression.OperatorApplication "||" _ left right ->
            ( List.concat
                [ or_isLeftSimplifiableError node left right
                , or_isRightSimplifiableError node left right
                ]
            , []
            )

        Expression.OperatorApplication "&&" _ left right ->
            ( List.concat
                [ and_isLeftSimplifiableError node left right
                , and_isRightSimplifiableError node left right
                ]
            , []
            )

        Expression.OperatorApplication "==" _ left right ->
            if Normalize.areTheSame lookupTable left right then
                ( [ Rule.errorWithFix
                        { message = "Condition is always True"
                        , details = sameThingOnBothSidesDetails True
                        }
                        (Node.range node)
                        [ Fix.replaceRangeBy (Node.range node) "True"
                        ]
                  ]
                , []
                )

            else
                ( [], [] )

        Expression.OperatorApplication "/=" _ left right ->
            if Normalize.areTheSame lookupTable left right then
                ( [ Rule.errorWithFix
                        { message = "Condition is always False"
                        , details = sameThingOnBothSidesDetails False
                        }
                        (Node.range node)
                        [ Fix.replaceRangeBy (Node.range node) "False"
                        ]
                  ]
                , []
                )

            else
                ( [], [] )

        -------------------
        -- IF EXPRESSION --
        -------------------
        Expression.IfBlock cond trueBranch falseBranch ->
            case getBoolean lookupTable cond of
                Just True ->
                    ( [ Rule.errorWithFix
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            }
                            (targetIf node)
                            [ Fix.removeRange
                                { start = (Node.range node).start
                                , end = (Node.range trueBranch).start
                                }
                            , Fix.removeRange
                                { start = (Node.range trueBranch).end
                                , end = (Node.range node).end
                                }
                            ]
                      ]
                    , []
                    )

                Just False ->
                    ( [ Rule.errorWithFix
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            }
                            (targetIf node)
                            [ Fix.removeRange
                                { start = (Node.range node).start
                                , end = (Node.range falseBranch).start
                                }
                            ]
                      ]
                    , []
                    )

                Nothing ->
                    case ( getBoolean lookupTable trueBranch, getBoolean lookupTable falseBranch ) of
                        ( Just True, Just False ) ->
                            ( [ Rule.errorWithFix
                                    { message = "The if expression's value is the same as the condition"
                                    , details = [ "The expression can be replaced by the condition." ]
                                    }
                                    (targetIf node)
                                    [ Fix.removeRange
                                        { start = (Node.range node).start
                                        , end = (Node.range cond).start
                                        }
                                    , Fix.removeRange
                                        { start = (Node.range cond).end
                                        , end = (Node.range node).end
                                        }
                                    ]
                              ]
                            , []
                            )

                        ( Just False, Just True ) ->
                            ( [ Rule.errorWithFix
                                    { message = "The if expression's value is the inverse of the condition"
                                    , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                                    }
                                    (targetIf node)
                                    [ Fix.replaceRangeBy
                                        { start = (Node.range node).start
                                        , end = (Node.range cond).start
                                        }
                                        "not ("
                                    , Fix.replaceRangeBy
                                        { start = (Node.range cond).end
                                        , end = (Node.range node).end
                                        }
                                        ")"
                                    ]
                              ]
                            , []
                            )

                        _ ->
                            if Normalize.areTheSame lookupTable trueBranch falseBranch then
                                ( [ Rule.errorWithFix
                                        { message = "The values in both branches is the same."
                                        , details = [ "The expression can be replaced by the contents of either branch." ]
                                        }
                                        (targetIf node)
                                        [ Fix.removeRange
                                            { start = (Node.range node).start
                                            , end = (Node.range trueBranch).start
                                            }
                                        , Fix.removeRange
                                            { start = (Node.range trueBranch).end
                                            , end = (Node.range node).end
                                            }
                                        ]
                                  ]
                                , []
                                )

                            else
                                ( [], [] )

        -------------------------------------
        --  FULLY APPLIED PREFIX OPERATOR  --
        -------------------------------------
        Expression.Application [ Node.Node operatorRange (Expression.PrefixOperator operator), left, right ] ->
            ( [ Rule.errorWithFix
                    { message = "Use the infix form (a + b) over the prefix form ((+) a b)"
                    , details = [ "The prefix form is generally more unfamiliar to Elm developers, and therefore it is nicer when the infix form is used." ]
                    }
                    operatorRange
                    [ Fix.removeRange { start = operatorRange.start, end = (Node.range left).start }
                    , Fix.insertAt (Node.range right).start (operator ++ " ")
                    ]
              ]
            , []
            )

        -------------
        --  LISTS  --
        -------------
        Expression.OperatorApplication "++" _ (Node range (Expression.ListExpr [])) other ->
            ( [ errorForAddingEmptyLists range
                    { start = range.start
                    , end = (Node.range other).start
                    }
              ]
            , []
            )

        Expression.OperatorApplication "++" _ other (Node range (Expression.ListExpr [])) ->
            ( [ errorForAddingEmptyLists range
                    { start = (Node.range other).end
                    , end = range.end
                    }
              ]
            , []
            )

        Expression.OperatorApplication "++" _ (Node rangeLeft (Expression.ListExpr _)) (Node rangeRight (Expression.ListExpr _)) ->
            ( [ Rule.errorWithFix
                    { message = "Expression could be simplified to be a single List"
                    , details = [ "Try moving all the elements into a single list." ]
                    }
                    (Node.range node)
                    [ Fix.replaceRangeBy
                        { start = { row = rangeLeft.end.row, column = rangeLeft.end.column - 1 }
                        , end = { row = rangeRight.start.row, column = rangeRight.start.column + 1 }
                        }
                        ","
                    ]
              ]
            , []
            )

        Expression.OperatorApplication "++" _ (Node rangeLeft (Expression.ListExpr [ _ ])) list ->
            ( [ Rule.errorWithFix
                    { message = "Should use (::) instead of (++)"
                    , details = [ "Concatenating a list with a single value is the same as using (::) on the list with the value." ]
                    }
                    (Node.range node)
                    [ Fix.replaceRangeBy
                        { start = rangeLeft.start
                        , end = { row = rangeLeft.start.row, column = rangeLeft.start.column + 1 }
                        }
                        "("
                    , Fix.replaceRangeBy
                        { start = { row = rangeLeft.end.row, column = rangeLeft.end.column - 1 }
                        , end = (Node.range list).start
                        }
                        ") :: "
                    ]
              ]
            , []
            )

        Expression.OperatorApplication "::" _ (Node rangeLeft _) (Node rangeRight (Expression.ListExpr [])) ->
            ( [ Rule.errorWithFix
                    { message = "Element added to the beginning of the list could be included in the list"
                    , details = [ "Try moving the element inside the list it is being added to." ]
                    }
                    rangeLeft
                    [ Fix.insertAt rangeLeft.start "[ "
                    , Fix.replaceRangeBy
                        { start = rangeLeft.end
                        , end = rangeRight.end
                        }
                        " ]"
                    ]
              ]
            , []
            )

        Expression.OperatorApplication "::" _ (Node rangeLeft _) (Node rangeRight (Expression.ListExpr _)) ->
            ( [ Rule.errorWithFix
                    { message = "Element added to the beginning of the list could be included in the list"
                    , details = [ "Try moving the element inside the list it is being added to." ]
                    }
                    rangeLeft
                    [ Fix.insertAt rangeLeft.start "[ "
                    , Fix.replaceRangeBy
                        { start = rangeLeft.end
                        , end = { row = rangeRight.start.row, column = rangeRight.start.column + 1 }
                        }
                        ","
                    ]
              ]
            , []
            )

        Expression.OperatorApplication "<|" _ (Node fnRange (Expression.FunctionOrValue _ fnName)) firstArg ->
            case
                ModuleNameLookupTable.moduleNameAt lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) checkList)
            of
                Just checkFn ->
                    ( checkFn
                        { lookupTable = lookupTable
                        , parentRange = Node.range node
                        , fnRange = fnRange
                        , firstArg = firstArg
                        , secondArg = Nothing
                        , usingRightPizza = False
                        }
                    , []
                    )

                _ ->
                    ( [], [] )

        Expression.OperatorApplication "<|" _ (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: []))) secondArgument ->
            case
                ModuleNameLookupTable.moduleNameAt lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) checkList)
            of
                Just checkFn ->
                    ( checkFn
                        { lookupTable = lookupTable
                        , parentRange = Node.range node
                        , fnRange = fnRange
                        , firstArg = firstArg
                        , secondArg = Just secondArgument
                        , usingRightPizza = False
                        }
                    , [ applicationRange ]
                    )

                _ ->
                    ( [], [] )

        Expression.OperatorApplication "|>" _ firstArg (Node fnRange (Expression.FunctionOrValue _ fnName)) ->
            case
                ModuleNameLookupTable.moduleNameAt lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) checkList)
            of
                Just checkFn ->
                    ( checkFn
                        { lookupTable = lookupTable
                        , parentRange = Node.range node
                        , fnRange = fnRange
                        , firstArg = firstArg
                        , secondArg = Nothing
                        , usingRightPizza = True
                        }
                    , []
                    )

                _ ->
                    ( [], [] )

        Expression.OperatorApplication "|>" _ secondArgument (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: []))) ->
            case
                ModuleNameLookupTable.moduleNameAt lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) checkList)
            of
                Just checkFn ->
                    ( checkFn
                        { lookupTable = lookupTable
                        , parentRange = Node.range node
                        , fnRange = fnRange
                        , firstArg = firstArg
                        , secondArg = Just secondArgument
                        , usingRightPizza = True
                        }
                    , [ applicationRange ]
                    )

                _ ->
                    ( [], [] )

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: restOfArguments) ->
            case
                ModuleNameLookupTable.moduleNameAt lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) checkList)
            of
                Just checkFn ->
                    ( checkFn
                        { lookupTable = lookupTable
                        , parentRange = Node.range node
                        , fnRange = fnRange
                        , firstArg = firstArg
                        , secondArg = List.head restOfArguments
                        , usingRightPizza = False
                        }
                    , []
                    )

                _ ->
                    ( [], [] )

        _ ->
            ( [], [] )


type alias CheckInfo =
    { lookupTable : ModuleNameLookupTable
    , parentRange : Range
    , fnRange : Range
    , firstArg : Node Expression
    , secondArg : Maybe (Node Expression)
    , usingRightPizza : Bool
    }



-- BOOLEAN


notChecks : CheckInfo -> List (Error {})
notChecks { lookupTable, parentRange, firstArg } =
    case getBoolean lookupTable firstArg of
        Just bool ->
            [ Rule.errorWithFix
                { message = "Expression is equal to " ++ boolToString (not bool)
                , details = [ "You can replace the call to `not` by the boolean value directly." ]
                }
                parentRange
                [ Fix.replaceRangeBy parentRange (boolToString (not bool)) ]
            ]

        Nothing ->
            []


or_isLeftSimplifiableError : Node a -> Node Expression -> Node b -> List (Rule.Error {})
or_isLeftSimplifiableError node left right =
    if isTrue left then
        [ Rule.errorWithFix
            { message = "Condition is always True"
            , details = alwaysSameDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).end
                , end = (Node.range right).end
                }
            ]
        ]

    else if isFalse left then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).start
                , end = (Node.range right).start
                }
            ]
        ]

    else
        []


or_isRightSimplifiableError : Node a -> Node b -> Node Expression -> List (Rule.Error {})
or_isRightSimplifiableError node left right =
    if isTrue right then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).start
                , end = (Node.range right).start
                }
            ]
        ]

    else if isFalse right then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).end
                , end = (Node.range right).end
                }
            ]
        ]

    else
        []


and_isLeftSimplifiableError : Node a -> Node Expression -> Node b -> List (Rule.Error {})
and_isLeftSimplifiableError node left right =
    if isTrue left then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).start
                , end = (Node.range right).start
                }
            ]
        ]

    else if isFalse left then
        [ Rule.errorWithFix
            { message = "Condition is always False"
            , details = alwaysSameDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).end
                , end = (Node.range right).end
                }
            ]
        ]

    else
        []


and_isRightSimplifiableError : Node a -> Node b -> Node Expression -> List (Rule.Error {})
and_isRightSimplifiableError node left right =
    if isTrue right then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).end
                , end = (Node.range right).end
                }
            ]
        ]

    else if isFalse right then
        [ Rule.errorWithFix
            { message = "Condition is always False"
            , details = alwaysSameDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).start
                , end = (Node.range right).start
                }
            ]
        ]

    else
        []


isTrue : Node Expression -> Bool
isTrue node =
    case Node.value node of
        Expression.FunctionOrValue [] "True" ->
            True

        Expression.ParenthesizedExpression expr ->
            isTrue expr

        _ ->
            False


isFalse : Node Expression -> Bool
isFalse node =
    case Node.value node of
        Expression.FunctionOrValue [] "False" ->
            True

        Expression.ParenthesizedExpression expr ->
            isFalse expr

        _ ->
            False


alwaysSameDetails : List String
alwaysSameDetails =
    [ "This condition will always result in the same value. You may have hardcoded a value or mistyped a condition."
    ]


unnecessaryMessage : String
unnecessaryMessage =
    "Part of the expression is unnecessary"


unnecessaryDetails : List String
unnecessaryDetails =
    [ "A part of this condition is unnecessary. You can remove it and it would not impact the behavior of the program."
    ]


sameThingOnBothSidesDetails : Bool -> List String
sameThingOnBothSidesDetails computedResult =
    let
        computedResultString : String
        computedResultString =
            if computedResult then
                "True"

            else
                "False"
    in
    [ "The value on the left and on the right are the same. Therefore we can determine that the expression will always be " ++ computedResultString ++ "."
    ]



-- IF EXPRESSIONS


targetIf : Node a -> Range
targetIf node =
    let
        { start } =
            Node.range node
    in
    { start = start
    , end = { start | column = start.column + 2 }
    }



-- BASICS


identityChecks : CheckInfo -> List (Error {})
identityChecks { parentRange, fnRange, firstArg, usingRightPizza } =
    [ Rule.errorWithFix
        { message = "`identity` should be removed"
        , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
        }
        fnRange
        [ if usingRightPizza then
            Fix.removeRange { start = (Node.range firstArg).end, end = parentRange.end }

          else
            Fix.removeRange { start = fnRange.start, end = (Node.range firstArg).start }
        ]
    ]


alwaysChecks : CheckInfo -> List (Error {})
alwaysChecks { fnRange, firstArg, secondArg, usingRightPizza } =
    case secondArg of
        Just (Node secondArgRange _) ->
            [ Rule.errorWithFix
                { message = "Expression can be replaced by the first argument to `always`"
                , details = [ "REPLACEME" ]
                }
                fnRange
                (if usingRightPizza then
                    [ Fix.removeRange { start = secondArgRange.start, end = (Node.range firstArg).start }
                    ]

                 else
                    [ Fix.removeRange { start = fnRange.start, end = (Node.range firstArg).start }
                    , Fix.removeRange { start = (Node.range firstArg).end, end = secondArgRange.end }
                    ]
                )
            ]

        Nothing ->
            []



-- LIST


checkList : Dict ( ModuleName, String ) (CheckInfo -> List (Error {}))
checkList =
    Dict.fromList
        [ reportEmptyListSecondArgument ( ( [ "Basics" ], "identity" ), identityChecks )
        , reportEmptyListSecondArgument ( ( [ "Basics" ], "always" ), alwaysChecks )
        , reportEmptyListSecondArgument ( ( [ "Basics" ], "not" ), notChecks )
        , reportEmptyListSecondArgument ( ( [ "List" ], "map" ), mapChecks )
        , reportEmptyListSecondArgument ( ( [ "List" ], "filter" ), filterChecks )
        , reportEmptyListSecondArgument ( ( [ "List" ], "filterMap" ), filterMapChecks )
        , reportEmptyListFirstArgument ( ( [ "List" ], "concat" ), concatChecks )
        , reportEmptyListSecondArgument ( ( [ "List" ], "concatMap" ), concatMapChecks )
        , ( ( [ "List" ], "isEmpty" ), isEmptyChecks )
        , ( ( [ "List" ], "all" ), allChecks )
        , ( ( [ "List" ], "any" ), anyChecks )
        , ( ( [ "List" ], "range" ), rangeChecks )
        , ( ( [ "List" ], "length" ), lengthChecks )
        ]


reportEmptyListSecondArgument : ( ( ModuleName, String ), CheckInfo -> List (Error {}) ) -> ( ( ModuleName, String ), CheckInfo -> List (Error {}) )
reportEmptyListSecondArgument ( ( moduleName, name ), function ) =
    ( ( moduleName, name )
    , \checkInfo ->
        case checkInfo.secondArg of
            Just (Node _ (Expression.ListExpr [])) ->
                [ Rule.errorWithFix
                    { message = "Using " ++ String.join "." moduleName ++ "." ++ name ++ " on an empty list will result in a empty list"
                    , details = [ "You can replace this call by an empty list" ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                function checkInfo
    )


reportEmptyListFirstArgument : ( ( ModuleName, String ), CheckInfo -> List (Error {}) ) -> ( ( ModuleName, String ), CheckInfo -> List (Error {}) )
reportEmptyListFirstArgument ( ( moduleName, name ), function ) =
    ( ( moduleName, name )
    , \checkInfo ->
        case checkInfo.firstArg of
            Node _ (Expression.ListExpr []) ->
                [ Rule.errorWithFix
                    { message = "Using " ++ String.join "." moduleName ++ "." ++ name ++ " on an empty list will result in a empty list"
                    , details = [ "You can replace this call by an empty list" ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                function checkInfo
    )



-- LIST FUNCTIONS


concatChecks : CheckInfo -> List (Error {})
concatChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr list ->
            case list of
                [ Node elementRange _ ] ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary use of List.concat on a list with 1 element"
                        , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                        }
                        parentRange
                        [ Fix.removeRange { start = parentRange.start, end = elementRange.start }
                        , Fix.removeRange { start = elementRange.end, end = parentRange.end }
                        ]
                    ]

                (firstListElement :: restOfListElements) as args ->
                    if List.all isListLiteral list then
                        [ Rule.errorWithFix
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            }
                            parentRange
                            (Fix.removeRange fnRange
                                :: List.concatMap removeBoundariesFix args
                            )
                        ]

                    else
                        case findConsecutiveListLiterals firstListElement restOfListElements of
                            [] ->
                                []

                            fixes ->
                                [ Rule.errorWithFix
                                    { message = "Consecutive literal lists should be merged"
                                    , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                                    }
                                    fnRange
                                    fixes
                                ]

                _ ->
                    []

        _ ->
            []


findConsecutiveListLiterals : Node Expression -> List (Node Expression) -> List Fix
findConsecutiveListLiterals firstListElement restOfListElements =
    case ( firstListElement, restOfListElements ) of
        ( Node firstRange (Expression.ListExpr _), ((Node secondRange (Expression.ListExpr _)) as second) :: rest ) ->
            Fix.replaceRangeBy
                { start = { row = firstRange.end.row, column = firstRange.end.column - 1 }
                , end = { row = secondRange.start.row, column = secondRange.start.column + 1 }
                }
                ", "
                :: findConsecutiveListLiterals second rest

        ( _, x :: xs ) ->
            findConsecutiveListLiterals x xs

        _ ->
            []


concatMapChecks : CheckInfo -> List (Error {})
concatMapChecks { lookupTable, parentRange, fnRange, firstArg, secondArg, usingRightPizza } =
    if isIdentity lookupTable firstArg then
        [ Rule.errorWithFix
            { message = "Using List.concatMap with an identity function is the same as using List.concat"
            , details = [ "You can replace this call by List.concat" ]
            }
            fnRange
            [ Fix.replaceRangeBy { start = fnRange.start, end = (Node.range firstArg).end } "List.concat" ]
        ]

    else if isAlwaysEmptyList lookupTable firstArg then
        [ Rule.errorWithFix
            { message = "List.concatMap will result in on an empty list"
            , details = [ "You can replace this call by an empty list" ]
            }
            fnRange
            (replaceByEmptyListFix parentRange secondArg)
        ]

    else
        case secondArg of
            Just (Node listRange (Expression.ListExpr [ Node singleElementRange _ ])) ->
                [ Rule.errorWithFix
                    { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                    , details = [ "You can replace this call by a call to the function directly" ]
                    }
                    fnRange
                    (if usingRightPizza then
                        [ Fix.replaceRangeBy { start = listRange.start, end = singleElementRange.start } "("
                        , Fix.replaceRangeBy { start = singleElementRange.end, end = listRange.end } ")"
                        , Fix.removeRange fnRange
                        ]

                     else
                        [ Fix.removeRange fnRange
                        , Fix.replaceRangeBy { start = listRange.start, end = singleElementRange.start } "("
                        , Fix.replaceRangeBy { start = singleElementRange.end, end = listRange.end } ")"
                        ]
                    )
                ]

            _ ->
                []


mapChecks : CheckInfo -> List (Error {})
mapChecks ({ lookupTable, fnRange, firstArg } as checkInfo) =
    if isIdentity lookupTable firstArg then
        [ Rule.errorWithFix
            { message = "Using List.map with an identity function is the same as not using List.map"
            , details = [ "You can remove this call and replace it by the list itself" ]
            }
            fnRange
            (noopFix checkInfo)
        ]

    else
        []


isEmptyChecks : CheckInfo -> List (Error {})
isEmptyChecks { parentRange, fnRange, firstArg } =
    case Node.value (removeParens firstArg) of
        Expression.ListExpr list ->
            if List.isEmpty list then
                [ Rule.errorWithFix
                    { message = "The call to List.isEmpty will result in True"
                    , details = [ "You can replace this call by True." ]
                    }
                    fnRange
                    [ Fix.replaceRangeBy parentRange "True" ]
                ]

            else
                [ Rule.errorWithFix
                    { message = "The call to List.isEmpty will result in False"
                    , details = [ "You can replace this call by False." ]
                    }
                    fnRange
                    [ Fix.replaceRangeBy parentRange "False" ]
                ]

        Expression.OperatorApplication "::" _ _ _ ->
            [ Rule.errorWithFix
                { message = "The call to List.isEmpty will result in False"
                , details = [ "You can replace this call by False." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "False" ]
            ]

        _ ->
            []


allChecks : CheckInfo -> List (Error {})
allChecks { lookupTable, parentRange, fnRange, firstArg, secondArg } =
    case Maybe.map (removeParens >> Node.value) secondArg of
        Just (Expression.ListExpr []) ->
            [ Rule.errorWithFix
                { message = "The call to List.all will result in True"
                , details = [ "You can replace this call by True." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "True" ]
            ]

        _ ->
            case isAlwaysBoolean lookupTable firstArg of
                Just True ->
                    [ Rule.errorWithFix
                        { message = "The call to List.all will result in True"
                        , details = [ "You can replace this call by True." ]
                        }
                        fnRange
                        (replaceByBoolFix parentRange secondArg True)
                    ]

                _ ->
                    []


anyChecks : CheckInfo -> List (Error {})
anyChecks { lookupTable, parentRange, fnRange, firstArg, secondArg } =
    case Maybe.map (removeParens >> Node.value) secondArg of
        Just (Expression.ListExpr []) ->
            [ Rule.errorWithFix
                { message = "The call to List.any will result in False"
                , details = [ "You can replace this call by False." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "False" ]
            ]

        _ ->
            case isAlwaysBoolean lookupTable firstArg of
                Just False ->
                    [ Rule.errorWithFix
                        { message = "The call to List.any will result in False"
                        , details = [ "You can replace this call by False." ]
                        }
                        fnRange
                        (replaceByBoolFix parentRange secondArg False)
                    ]

                _ ->
                    []


filterChecks : CheckInfo -> List (Error {})
filterChecks ({ lookupTable, parentRange, fnRange, firstArg, secondArg } as checkInfo) =
    case isAlwaysBoolean lookupTable firstArg of
        Just True ->
            [ Rule.errorWithFix
                { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                , details = [ "You can remove this call and replace it by the list itself" ]
                }
                fnRange
                (noopFix checkInfo)
            ]

        Just False ->
            [ Rule.errorWithFix
                { message = "Using List.filter with a function that will always return False will result in an empty list"
                , details = [ "You can remove this call and replace it by an empty list" ]
                }
                fnRange
                (replaceByEmptyListFix parentRange secondArg)
            ]

        Nothing ->
            []


filterMapChecks : CheckInfo -> List (Error {})
filterMapChecks ({ lookupTable, parentRange, fnRange, firstArg, secondArg } as checkInfo) =
    case isAlwaysMaybe lookupTable firstArg of
        Just (Just ()) ->
            [ Rule.errorWithFix
                { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                , details = [ "You can remove this call and replace it by the list itself" ]
                }
                fnRange
                (noopFix checkInfo)
            ]

        Just Nothing ->
            [ Rule.errorWithFix
                { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                , details = [ "You can remove this call and replace it by an empty list" ]
                }
                fnRange
                (replaceByEmptyListFix parentRange secondArg)
            ]

        Nothing ->
            []


rangeChecks : CheckInfo -> List (Error {})
rangeChecks { parentRange, fnRange, firstArg, secondArg } =
    case Maybe.map2 Tuple.pair (getIntValue firstArg) (Maybe.andThen getIntValue secondArg) of
        Just ( first, second ) ->
            if first > second then
                [ Rule.errorWithFix
                    { message = "The call to List.range will result in []"
                    , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty slist." ]
                    }
                    fnRange
                    (replaceByEmptyListFix parentRange secondArg)
                ]

            else
                []

        Nothing ->
            []


lengthChecks : CheckInfo -> List (Error {})
lengthChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr list ->
            [ Rule.errorWithFix
                { message = "The length of the list is 0"
                , details = [ "The length of the list can be determined by looking at the code." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange (String.fromInt (List.length list)) ]
            ]

        _ ->
            []


getIntValue : Node Expression -> Maybe Int
getIntValue node =
    case Node.value (removeParens node) of
        Expression.Integer n ->
            Just n

        Expression.Hex n ->
            Just n

        _ ->
            Nothing



-- FIX HELPERS


removeBoundariesFix : Node a -> List Fix
removeBoundariesFix node =
    let
        { start, end } =
            Node.range node
    in
    [ Fix.removeRange
        { start = { row = start.row, column = start.column }
        , end = { row = start.row, column = start.column + 1 }
        }
    , Fix.removeRange
        { start = { row = end.row, column = end.column - 1 }
        , end = { row = end.row, column = end.column }
        }
    ]


noopFix : CheckInfo -> List Fix
noopFix { fnRange, parentRange, secondArg, usingRightPizza } =
    [ case secondArg of
        Just listArg ->
            if usingRightPizza then
                Fix.removeRange { start = (Node.range listArg).end, end = parentRange.end }

            else
                Fix.removeRange { start = fnRange.start, end = (Node.range listArg).start }

        Nothing ->
            Fix.replaceRangeBy parentRange "identity"
    ]


replaceByEmptyListFix : Range -> Maybe a -> List Fix
replaceByEmptyListFix parentRange secondArg =
    [ case secondArg of
        Just _ ->
            Fix.replaceRangeBy parentRange "[]"

        Nothing ->
            Fix.replaceRangeBy parentRange "(always [])"
    ]


replaceByBoolFix : Range -> Maybe a -> Bool -> List Fix
replaceByBoolFix parentRange secondArg replacementValue =
    [ case secondArg of
        Just _ ->
            Fix.replaceRangeBy parentRange (boolToString replacementValue)

        Nothing ->
            Fix.replaceRangeBy parentRange ("(always " ++ boolToString replacementValue ++ ")")
    ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"



-- MATCHERS


isIdentity : ModuleNameLookupTable -> Node Expression -> Bool
isIdentity lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "identity" ->
            ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ]

        Expression.LambdaExpression { args, expression } ->
            case args of
                arg :: [] ->
                    case getVarPattern arg of
                        Just patternName ->
                            case getExpressionName expression of
                                Just expressionName ->
                                    patternName == expressionName

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False

        _ ->
            False


getVarPattern : Node Pattern -> Maybe String
getVarPattern node =
    case Node.value node of
        Pattern.VarPattern name ->
            Just name

        Pattern.ParenthesizedPattern pattern ->
            getVarPattern pattern

        _ ->
            Nothing


getExpressionName : Node Expression -> Maybe String
getExpressionName node =
    case Node.value (removeParens node) of
        Expression.FunctionOrValue [] name ->
            Just name

        _ ->
            Nothing


isListLiteral : Node Expression -> Bool
isListLiteral node =
    case Node.value node of
        Expression.ListExpr _ ->
            True

        _ ->
            False


removeParens : Node Expression -> Node Expression
removeParens node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParens expr

        _ ->
            node


isAlwaysBoolean : ModuleNameLookupTable -> Node Expression -> Maybe Bool
isAlwaysBoolean lookupTable node =
    case Node.value (removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: boolean :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getBoolean lookupTable boolean

                _ ->
                    Nothing

        Expression.LambdaExpression { expression } ->
            getBoolean lookupTable expression

        _ ->
            Nothing


getBoolean : ModuleNameLookupTable -> Node Expression -> Maybe Bool
getBoolean lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "True" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Basics" ] ->
                    Just True

                _ ->
                    Nothing

        Expression.FunctionOrValue _ "False" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Basics" ] ->
                    Just False

                _ ->
                    Nothing

        _ ->
            Nothing


isAlwaysMaybe : ModuleNameLookupTable -> Node Expression -> Maybe (Maybe ())
isAlwaysMaybe lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "Just" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Maybe" ] ->
                    Just (Just ())

                _ ->
                    Nothing

        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: value :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getMaybeValue lookupTable value

                _ ->
                    Nothing

        Expression.LambdaExpression { args, expression } ->
            case Node.value expression of
                Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Just")) :: (Node _ (Expression.FunctionOrValue [] justArgName)) :: []) ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                        Just [ "Maybe" ] ->
                            case args of
                                (Node _ (Pattern.VarPattern lambdaArgName)) :: [] ->
                                    if lambdaArgName == justArgName then
                                        Just (Just ())

                                    else
                                        Nothing

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                Expression.FunctionOrValue _ "Nothing" ->
                    case ModuleNameLookupTable.moduleNameFor lookupTable expression of
                        Just [ "Maybe" ] ->
                            Just Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


getMaybeValue : ModuleNameLookupTable -> Node Expression -> Maybe (Maybe ())
getMaybeValue lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "Just" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Maybe" ] ->
                    Just (Just ())

                _ ->
                    Nothing

        Expression.FunctionOrValue _ "Nothing" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Maybe" ] ->
                    Just Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


isAlwaysEmptyList : ModuleNameLookupTable -> Node Expression -> Bool
isAlwaysEmptyList lookupTable node =
    case Node.value (removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: alwaysValue :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    isEmptyList alwaysValue

                _ ->
                    False

        Expression.LambdaExpression { expression } ->
            isEmptyList expression

        _ ->
            False


isEmptyList : Node Expression -> Bool
isEmptyList node =
    case Node.value (removeParens node) of
        Expression.ListExpr [] ->
            True

        _ ->
            False
