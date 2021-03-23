module Simplify.Ifs exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Location, Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports... REPLACEME

    config =
        [ Simplify.Ifs.rule
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
elm-review --template jfmengels/elm-review-simplification/example --rules Simplify.Ifs
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "Simplify.Ifs" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.IfBlock cond trueBranch falseBranch ->
            case Node.value cond of
                Expression.FunctionOrValue [] "True" ->
                    [ Rule.errorWithFix
                        { message = "REPLACEME"
                        , details = [ "REPLACEME" ]
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

                Expression.FunctionOrValue [] "False" ->
                    [ Rule.errorWithFix
                        { message = "REPLACEME"
                        , details = [ "REPLACEME" ]
                        }
                        (targetIf node)
                        [ Fix.removeRange
                            { start = (Node.range node).start
                            , end = (Node.range falseBranch).start
                            }
                        ]
                    ]

                _ ->
                    []

        _ ->
            []


targetIf : Node a -> Range
targetIf node =
    let
        { start } =
            Node.range node
    in
    { start = start
    , end = { start | column = start.column + 2 }
    }
