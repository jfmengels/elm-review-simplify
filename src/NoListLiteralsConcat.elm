module NoListLiteralsConcat exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when an operation on lists could be simplified to a single literal list.

    config =
        [ NoListLiteralsConcat.rule
        ]


## Fail

    a :: []
    --> [ a ]

    a :: [ b ]
    --> [ a, b ]

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

    List.concatMap identity x
    --> List.concat list

    List.concatMap identity
    --> List.concat

    List.concatMap (\a -> a) list
    --> List.concat list

    List.concatMap fn []
    --> []

    List.concatMap fn [ x ]
    --> fn x

    List.map fn [] -- same for List.filter, List.filterMap
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
elm-review --template jfmengels/elm-review-simplification/example --rules NoListLiteralsConcat
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoListLiteralsConcat" initialContext
        |> Rule.withExpressionEnterVisitor (\node lookupTable -> ( expressionVisitor node lookupTable, lookupTable ))
        |> Rule.fromModuleRuleSchema


initialContext : Rule.ContextCreator () ModuleNameLookupTable
initialContext =
    Rule.initContextCreator (\lookupTable () -> lookupTable)
        |> Rule.withModuleNameLookupTable


errorForAddingEmptyLists : Range -> Range -> Error {}
errorForAddingEmptyLists range rangeToRemove =
    Rule.errorWithFix
        { message = "Concatenating with a single list doesn't have any effect"
        , details = [ "You should remove the concatenation with the empty list." ]
        }
        range
        [ Review.Fix.removeRange rangeToRemove ]


expressionVisitor : Node Expression -> ModuleNameLookupTable -> List (Error {})
expressionVisitor node lookupTable =
    case Node.value node of
        Expression.OperatorApplication "++" _ (Node range (Expression.ListExpr [])) other ->
            [ errorForAddingEmptyLists range
                { start = range.start
                , end = (Node.range other).start
                }
            ]

        Expression.OperatorApplication "++" _ other (Node range (Expression.ListExpr [])) ->
            [ errorForAddingEmptyLists range
                { start = (Node.range other).end
                , end = range.end
                }
            ]

        Expression.OperatorApplication "++" _ (Node rangeLeft (Expression.ListExpr _)) (Node rangeRight (Expression.ListExpr _)) ->
            [ Rule.errorWithFix
                { message = "Expression could be simplified to be a single List"
                , details = [ "Try moving all the elements into a single list." ]
                }
                (Node.range node)
                [ Review.Fix.replaceRangeBy
                    { start = { row = rangeLeft.end.row, column = rangeLeft.end.column - 1 }
                    , end = { row = rangeRight.start.row, column = rangeRight.start.column + 1 }
                    }
                    ","
                ]
            ]

        Expression.OperatorApplication "::" _ (Node rangeLeft _) (Node rangeRight (Expression.ListExpr [])) ->
            [ Rule.errorWithFix
                { message = "Element added to the beginning of the list could be included in the list"
                , details = [ "Try moving the element inside the list it is being added to." ]
                }
                rangeLeft
                [ Review.Fix.insertAt rangeLeft.start "[ "
                , Review.Fix.replaceRangeBy
                    { start = rangeLeft.end
                    , end = rangeRight.end
                    }
                    " ]"
                ]
            ]

        Expression.OperatorApplication "::" _ (Node rangeLeft _) (Node rangeRight (Expression.ListExpr _)) ->
            [ Rule.errorWithFix
                { message = "Element added to the beginning of the list could be included in the list"
                , details = [ "Try moving the element inside the list it is being added to." ]
                }
                rangeLeft
                [ Review.Fix.insertAt rangeLeft.start "[ "
                , Review.Fix.replaceRangeBy
                    { start = rangeLeft.end
                    , end = { row = rangeRight.start.row, column = rangeRight.start.column + 1 }
                    }
                    ","
                ]
            ]

        Expression.OperatorApplication "<|" _ (Node fnRange (Expression.FunctionOrValue _ fnName)) firstArg ->
            case Dict.get fnName checkList of
                Just checkFn ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable fnRange of
                        Just [ "List" ] ->
                            checkFn
                                { lookupTable = lookupTable
                                , parentRange = Node.range node
                                , listFnRange = fnRange
                                , firstArg = firstArg
                                , secondArg = Nothing
                                }

                        _ ->
                            []

                _ ->
                    []

        Expression.OperatorApplication "|>" _ firstArg (Node fnRange (Expression.FunctionOrValue _ fnName)) ->
            case Dict.get fnName checkList of
                Just checkFn ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable fnRange of
                        Just [ "List" ] ->
                            checkFn
                                { lookupTable = lookupTable
                                , parentRange = Node.range node
                                , listFnRange = fnRange
                                , firstArg = firstArg
                                , secondArg = Nothing
                                }

                        _ ->
                            []

                _ ->
                    []

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: restOfArguments) ->
            case Dict.get fnName checkList of
                Just checkFn ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable fnRange of
                        Just [ "List" ] ->
                            checkFn
                                { lookupTable = lookupTable
                                , parentRange = Node.range node
                                , listFnRange = fnRange
                                , firstArg = firstArg
                                , secondArg = List.head restOfArguments
                                }

                        _ ->
                            []

                _ ->
                    []

        _ ->
            []


type alias CheckInfo =
    { lookupTable : ModuleNameLookupTable
    , parentRange : Range
    , listFnRange : Range
    , firstArg : Node Expression
    , secondArg : Maybe (Node Expression)
    }


checkList : Dict String (CheckInfo -> List (Error {}))
checkList =
    Dict.fromList
        [ reportEmptyListSecondArgument ( "map", mapChecks )
        , reportEmptyListSecondArgument ( "filter", filterChecks )
        , reportEmptyListSecondArgument ( "filterMap", filterMapChecks )
        , reportEmptyListFirstArgument ( "concat", concatChecks )
        , reportEmptyListSecondArgument ( "concatMap", concatMapChecks )
        ]


reportEmptyListSecondArgument : ( String, CheckInfo -> List (Error {}) ) -> ( String, CheckInfo -> List (Error {}) )
reportEmptyListSecondArgument ( name, function ) =
    ( name
    , \checkInfo ->
        case checkInfo.secondArg of
            Just (Node _ (Expression.ListExpr [])) ->
                [ Rule.errorWithFix
                    { message = "Using List." ++ name ++ " on an empty list will result in a empty list"
                    , details = [ "You can replace this call by an empty list" ]
                    }
                    checkInfo.listFnRange
                    [ Review.Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                function checkInfo
    )


reportEmptyListFirstArgument : ( String, CheckInfo -> List (Error {}) ) -> ( String, CheckInfo -> List (Error {}) )
reportEmptyListFirstArgument ( name, function ) =
    ( name
    , \checkInfo ->
        case checkInfo.firstArg of
            Node _ (Expression.ListExpr []) ->
                [ Rule.errorWithFix
                    { message = "Using List." ++ name ++ " on an empty list will result in a empty list"
                    , details = [ "You can replace this call by an empty list" ]
                    }
                    checkInfo.listFnRange
                    [ Review.Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                function checkInfo
    )



-- LIST FUNCTIONS


concatChecks : CheckInfo -> List (Error {})
concatChecks { parentRange, listFnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr list ->
            case list of
                [ Node elementRange _ ] ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary use of List.concat on a list with 1 element"
                        , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                        }
                        parentRange
                        [ Review.Fix.removeRange { start = parentRange.start, end = elementRange.start }
                        , Review.Fix.removeRange { start = elementRange.end, end = parentRange.end }
                        ]
                    ]

                args ->
                    if List.all isListLiteral list then
                        [ Rule.errorWithFix
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            }
                            parentRange
                            (Review.Fix.removeRange listFnRange
                                :: List.concatMap removeBoundariesFix args
                            )
                        ]

                    else
                        []

        _ ->
            []


concatMapChecks : CheckInfo -> List (Error {})
concatMapChecks { lookupTable, listFnRange, firstArg, secondArg } =
    if isIdentity lookupTable firstArg then
        [ Rule.errorWithFix
            { message = "Using List.concatMap with an identity function is the same as using List.concat"
            , details = [ "You can replace this call by List.concat" ]
            }
            listFnRange
            [ Review.Fix.replaceRangeBy { start = listFnRange.start, end = (Node.range firstArg).end } "List.concat" ]
        ]

    else
        case secondArg of
            Just (Node listRange (Expression.ListExpr [ Node singleElementRange _ ])) ->
                [ Rule.errorWithFix
                    { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                    , details = [ "You can replace this call by a call to the function directly" ]
                    }
                    listFnRange
                    [ Review.Fix.replaceRangeBy listFnRange "("
                    , Review.Fix.replaceRangeBy { start = listRange.start, end = singleElementRange.start } "("
                    , Review.Fix.replaceRangeBy { start = singleElementRange.end, end = listRange.end } "))"
                    ]
                ]

            _ ->
                []


mapChecks : CheckInfo -> List (Error {})
mapChecks { lookupTable, listFnRange, firstArg, secondArg } =
    if isIdentity lookupTable firstArg then
        [ Rule.errorWithFix
            { message = "Using List.map with an identity function is the same as not using List.map"
            , details = [ "You can remove this call and replace it by the list itself" ]
            }
            listFnRange
            [ case secondArg of
                Just listArg ->
                    Review.Fix.removeRange { start = listFnRange.start, end = (Node.range listArg).start }

                Nothing ->
                    Review.Fix.removeRange { start = listFnRange.start, end = (Node.range firstArg).start }
            ]
        ]

    else
        []


filterChecks : CheckInfo -> List (Error {})
filterChecks ({ lookupTable, parentRange, listFnRange, firstArg, secondArg } as checkInfo) =
    case isAlwaysBoolean lookupTable firstArg of
        Just True ->
            [ Rule.errorWithFix
                { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                , details = [ "You can remove this call and replace it by the list itself" ]
                }
                listFnRange
                (noopFix checkInfo)
            ]

        Just False ->
            [ Rule.errorWithFix
                { message = "Using List.filter with a function that will always return False will result in an empty list"
                , details = [ "You can remove this call and replace it by an empty list" ]
                }
                listFnRange
                (replaceByEmptyListFix parentRange secondArg)
            ]

        Nothing ->
            []


filterMapChecks : CheckInfo -> List (Error {})
filterMapChecks ({ lookupTable, parentRange, listFnRange, firstArg, secondArg } as checkInfo) =
    case isAlwaysMaybe lookupTable firstArg of
        Just (Just ()) ->
            [ Rule.errorWithFix
                { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                , details = [ "You can remove this call and replace it by the list itself" ]
                }
                listFnRange
                (noopFix checkInfo)
            ]

        Just Nothing ->
            [ Rule.errorWithFix
                { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                , details = [ "You can remove this call and replace it by an empty list" ]
                }
                listFnRange
                (replaceByEmptyListFix parentRange secondArg)
            ]

        Nothing ->
            []



-- FIX HELPERS


removeBoundariesFix : Node a -> List Fix
removeBoundariesFix node =
    let
        { start, end } =
            Node.range node
    in
    [ Review.Fix.removeRange
        { start = { row = start.row, column = start.column }
        , end = { row = start.row, column = start.column + 1 }
        }
    , Review.Fix.removeRange
        { start = { row = end.row, column = end.column - 1 }
        , end = { row = end.row, column = end.column }
        }
    ]


noopFix : CheckInfo -> List Fix
noopFix { listFnRange, parentRange, secondArg } =
    [ case secondArg of
        Just listArg ->
            Review.Fix.removeRange { start = listFnRange.start, end = (Node.range listArg).start }

        Nothing ->
            Review.Fix.replaceRangeBy parentRange "identity"
    ]


replaceByEmptyListFix : Range -> Maybe a -> List Fix
replaceByEmptyListFix parentRange secondArg =
    [ case secondArg of
        Just _ ->
            Review.Fix.replaceRangeBy parentRange "[]"

        Nothing ->
            Review.Fix.replaceRangeBy parentRange "(always [])"
    ]



-- MATCHERS


isIdentity : ModuleNameLookupTable -> Node Expression -> Bool
isIdentity lookupTable node =
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

        Expression.ParenthesizedExpression expr ->
            isIdentity lookupTable expr

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
    case Node.value node of
        Expression.FunctionOrValue [] name ->
            Just name

        Expression.ParenthesizedExpression pattern ->
            getExpressionName pattern

        _ ->
            Nothing


isListLiteral : Node Expression -> Bool
isListLiteral node =
    case Node.value node of
        Expression.ListExpr _ ->
            True

        _ ->
            False


isAlwaysBoolean : ModuleNameLookupTable -> Node Expression -> Maybe Bool
isAlwaysBoolean lookupTable node =
    case Node.value node of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: boolean :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getBoolean lookupTable boolean

                _ ->
                    Nothing

        Expression.LambdaExpression _ ->
            Nothing

        Expression.ParenthesizedExpression expr ->
            isAlwaysBoolean lookupTable expr

        _ ->
            Nothing


getBoolean : ModuleNameLookupTable -> Node Expression -> Maybe Bool
getBoolean lookupTable node =
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

        Expression.ParenthesizedExpression expr ->
            getBoolean lookupTable expr

        _ ->
            Nothing


isAlwaysMaybe : ModuleNameLookupTable -> Node Expression -> Maybe (Maybe ())
isAlwaysMaybe lookupTable node =
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
                    getMaybeValue value

                _ ->
                    Nothing

        Expression.LambdaExpression { args, expression } ->
            case Node.value expression of
                Expression.Application ((Node _ (Expression.FunctionOrValue [] "Just")) :: (Node _ (Expression.FunctionOrValue [] justArgName)) :: []) ->
                    case args of
                        (Node _ (Pattern.VarPattern lambdaArgName)) :: [] ->
                            if lambdaArgName == justArgName then
                                Just (Just ())

                            else
                                Nothing

                        _ ->
                            Nothing

                Expression.Application ((Node _ (Expression.FunctionOrValue [ "Maybe" ] "Just")) :: _) ->
                    Just (Just ())

                Expression.FunctionOrValue _ "Nothing" ->
                    case ModuleNameLookupTable.moduleNameFor lookupTable expression of
                        Just [ "Maybe" ] ->
                            Just Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        Expression.ParenthesizedExpression expr ->
            isAlwaysMaybe lookupTable expr

        _ ->
            Nothing


getMaybeValue : Node Expression -> Maybe (Maybe ())
getMaybeValue node =
    case Node.value node of
        Expression.FunctionOrValue [] "Just" ->
            Just (Just ())

        Expression.FunctionOrValue [ "Maybe" ] "Just" ->
            Just (Just ())

        Expression.FunctionOrValue [] "Nothing" ->
            Just Nothing

        Expression.FunctionOrValue [ "Maybe" ] "Nothing" ->
            Just Nothing

        Expression.ParenthesizedExpression expr ->
            getMaybeValue expr

        _ ->
            Nothing
