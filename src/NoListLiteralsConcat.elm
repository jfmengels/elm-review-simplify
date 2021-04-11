module NoListLiteralsConcat exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
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
                , details = [ "Try moving the element inside single list." ]
                }
                (Node.range node)
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
                        [ Review.Fix.removeRange
                            { start = parentRange.start
                            , end = elementRange.start
                            }
                        , Review.Fix.removeRange
                            { start = elementRange.end
                            , end = parentRange.end
                            }
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

        _ ->
            []


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
