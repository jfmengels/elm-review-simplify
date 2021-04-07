module Simplify.Ifs exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports and fixes unnecessary `if` conditions, because the branch that will be taken is always the same and can be determined at compile-time.

    config =
        [ Simplify.Ifs.rule
        ]


## Fail

    a =
        if True then
            1

        else
            -- Unnecessary branch
            2

    b =
        if False then
            -- Unnecessary branch
            1

        else
            2


## Success

    a =
        if condition then
            1

        else
            2


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

                Expression.FunctionOrValue [] "False" ->
                    [ Rule.errorWithFix
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

                _ ->
                    case ( Node.value trueBranch, Node.value falseBranch ) of
                        ( Expression.FunctionOrValue [] "True", Expression.FunctionOrValue [] "False" ) ->
                            [ Rule.errorWithFix
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
