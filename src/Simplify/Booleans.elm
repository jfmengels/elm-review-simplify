module Simplify.Booleans exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule)


{-| Reports... REPLACEME

    config =
        [ Simplify.Booleans.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


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
            [ Rule.error
                { message = "REPLACEME"
                , details = [ "REPLACEME" ]
                }
                (Node.range node)
            ]

        _ ->
            []
