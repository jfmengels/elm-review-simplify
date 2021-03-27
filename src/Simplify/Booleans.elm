module Simplify.Booleans exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)


{-| Reports and fixes conditionals that can be made simpler.

    config =
        [ Simplify.Booleans.rule
        ]


## Fail

    a =
        -- Simplifiable as: True
        True || x

    b =
        -- Simplifiable as: x
        x || False


## Success

    a =
        x || y

    b =
        w && z


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplification/example --rules Simplify.Booleans
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Simplify.Booleans" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Rule.Error {})
expressionVisitor node =
    case Node.value node of
        Expression.OperatorApplication "||" _ left right ->
            List.concat
                [ or_isLeftSimplifiableError node left right
                , or_isRightSimplifiableError node left right
                ]

        Expression.OperatorApplication "&&" _ left right ->
            List.concat
                [ and_isLeftSimplifiableError node left right
                , and_isRightSimplifiableError node left right
                ]

        _ ->
            []


or_isLeftSimplifiableError : Node a -> Node Expression -> Node b -> List (Rule.Error {})
or_isLeftSimplifiableError node left right =
    if isTrue left then
        [ Rule.errorWithFix
            { message = "Condition is always True"
            , details = details
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
            { message = "REPLACEME"
            , details = details
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
            { message = "REPLACEME"
            , details = details
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
            { message = "REPLACEME"
            , details = details
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
            { message = "REPLACEME"
            , details = details
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
            { message = "REPLACEME"
            , details = details
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
            { message = "REPLACEME"
            , details = details
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
            { message = "REPLACEME"
            , details = details
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


details : List String
details =
    [ "This condition will always result in the same value. You may have hardcoded a value or mistyped a condition."
    ]
