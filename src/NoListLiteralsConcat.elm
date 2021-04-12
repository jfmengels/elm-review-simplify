module NoListLiteralsConcat exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when an operation on lists could be simplified to a single literal list.

    config =
        [ NoListLiteralsConcat.rule
        ]


## Fail

    _ =
        [ 1, 2, 3 ] ++ [ 4, mysteryNumber, 6 ]

    _ =
        List.concat
            [ [ 1, 2, 3 ]
            , [ 4, mysteryNumber, 6 ]
            ]

    _ =
        List.concat
            [ [ 1, 2, 3 ]
            ]

    _ =
        1 :: [ 2, 3 ]

    _ =
        [] ++ list

    _ =
        list ++ []


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
    Rule.newModuleRuleSchema "NoListLiteralsConcat" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


errorForAddingEmptyLists : Range -> Range -> Error {}
errorForAddingEmptyLists range rangeToRemove =
    Rule.errorWithFix
        { message = "Concatenating with a single list doesn't have any effect"
        , details = [ "You should remove the concatenation with the empty list." ]
        }
        range
        [ Review.Fix.removeRange rangeToRemove ]


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
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

        Expression.Application [ Node listConcatRange (Expression.FunctionOrValue [ "List" ] "concat"), Node _ (Expression.ListExpr list) ] ->
            case list of
                [] ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary use of List.concat on an empty list"
                        , details = [ "The value of the operation will be []. You should replace this expression by that." ]
                        }
                        (Node.range node)
                        [ Review.Fix.replaceRangeBy
                            (Node.range node)
                            "[]"
                        ]
                    ]

                [ Node elementRange _ ] ->
                    let
                        parentRange : Range
                        parentRange =
                            Node.range node
                    in
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
                        let
                            parentRange : Range
                            parentRange =
                                Node.range node
                        in
                        [ Rule.errorWithFix
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            }
                            parentRange
                            (Review.Fix.removeRange listConcatRange
                                :: List.concatMap removeBoundaries args
                            )
                        ]

                    else
                        []

        Expression.Application ((Node listConcatRange (Expression.FunctionOrValue [ "List" ] "concatMap")) :: firstArg :: _) ->
            if isIdentity firstArg then
                [ Rule.errorWithFix
                    { message = "Using List.concatMap with an identity function is the same as using List.concat"
                    , details = [ "You can replace this call by List.concat" ]
                    }
                    listConcatRange
                    [ Review.Fix.replaceRangeBy { start = listConcatRange.start, end = (Node.range firstArg).end } "List.concat" ]
                ]

            else
                []

        Expression.Application ((Node listMapRange (Expression.FunctionOrValue [ "List" ] "map")) :: _ :: (Node _ (Expression.ListExpr [])) :: []) ->
            [ Rule.errorWithFix
                { message = "Using List.map on an empty list will result in a empty list"
                , details = [ "You can replace this call by en empty list" ]
                }
                listMapRange
                [ Review.Fix.replaceRangeBy (Node.range node) "[]" ]
            ]

        Expression.Application ((Node listMapRange (Expression.FunctionOrValue [ "List" ] "map")) :: firstArg :: restOfArgs) ->
            if isIdentity firstArg then
                [ Rule.errorWithFix
                    { message = "Using List.map with an identity function is the same as not using List.map"
                    , details = [ "You can remove this call and replace it by the list itself" ]
                    }
                    listMapRange
                    [ case restOfArgs of
                        [] ->
                            Review.Fix.removeRange { start = listMapRange.start, end = (Node.range firstArg).start }

                        listArg :: _ ->
                            Review.Fix.removeRange { start = listMapRange.start, end = (Node.range listArg).start }
                    ]
                ]

            else
                []

        Expression.Application ((Node listFn (Expression.FunctionOrValue [ "List" ] "filter")) :: _ :: (Node _ (Expression.ListExpr [])) :: []) ->
            [ Rule.errorWithFix
                { message = "Using List.filter on an empty list will result in a empty list"
                , details = [ "You can replace this call by en empty list" ]
                }
                listFn
                [ Review.Fix.replaceRangeBy (Node.range node) "[]" ]
            ]

        Expression.Application ((Node listFn (Expression.FunctionOrValue [ "List" ] "filter")) :: firstArg :: restOfArgs) ->
            case isAlways firstArg of
                Just True ->
                    [ Rule.errorWithFix
                        { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                        , details = [ "You can remove this call and replace it by the list itself" ]
                        }
                        listFn
                        [ case restOfArgs of
                            [] ->
                                Review.Fix.replaceRangeBy (Node.range node) "identity"

                            listArg :: _ ->
                                Review.Fix.removeRange { start = listFn.start, end = (Node.range listArg).start }
                        ]
                    ]

                Just False ->
                    [ Rule.errorWithFix
                        { message = "Using List.filter with a function that will always return False will result in an empty list"
                        , details = [ "You can remove this call and replace it by an empty list" ]
                        }
                        listFn
                        [ case restOfArgs of
                            [] ->
                                Review.Fix.replaceRangeBy (Node.range node) "(always [])"

                            _ ->
                                Review.Fix.replaceRangeBy (Node.range node) "[]"
                        ]
                    ]

                Nothing ->
                    []

        _ ->
            []


isIdentity : Node Expression -> Bool
isIdentity node =
    case Node.value node of
        Expression.FunctionOrValue [] "identity" ->
            True

        Expression.FunctionOrValue [ "Basics" ] "identity" ->
            True

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
            isIdentity expr

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


removeBoundaries : Node a -> List Fix
removeBoundaries node =
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


isListLiteral : Node Expression -> Bool
isListLiteral node =
    case Node.value node of
        Expression.ListExpr _ ->
            True

        _ ->
            False


isAlways : Node Expression -> Maybe Bool
isAlways node =
    case Node.value node of
        Expression.Application ((Node _ (Expression.FunctionOrValue [] "always")) :: boolean :: []) ->
            getBoolean boolean

        Expression.Application ((Node _ (Expression.FunctionOrValue [ "Basics" ] "always")) :: boolean :: []) ->
            getBoolean boolean

        Expression.LambdaExpression { expression } ->
            Nothing

        Expression.ParenthesizedExpression expr ->
            isAlways expr

        _ ->
            Nothing


getBoolean : Node Expression -> Maybe Bool
getBoolean node =
    case Node.value node of
        Expression.FunctionOrValue [] "True" ->
            Just True

        Expression.FunctionOrValue [ "Basics" ] "True" ->
            Just True

        Expression.FunctionOrValue [] "False" ->
            Just False

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            Just False

        Expression.ParenthesizedExpression expr ->
            getBoolean expr

        _ ->
            Nothing
