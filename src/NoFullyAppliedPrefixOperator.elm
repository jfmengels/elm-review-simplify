module NoFullyAppliedPrefixOperator exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when an operator is used as a prefix operator and all the operands are already given.

    config =
        [ NoFullyAppliedPrefixOperator.rule
        ]


## Fail

    _ =
        (+) 1 2


## Success

    _ =
        1 + 2

    _ =
        (+) 1

    _ =
        (+)


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example --rules NoFullyAppliedPrefixOperator
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoFullyAppliedPrefixOperator" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


error : String -> Range -> Range -> Range -> Error {}
error operator operatorRange left right =
    Rule.errorWithFix
        { message = "Prefer using the infix form (`a + b`) over the prefix form (`(+) a b`) when possible"
        , details = [ "The prefix form is generally harder to read over the infix form." ]
        }
        operatorRange
        [ Fix.removeRange { start = operatorRange.start, end = left.start }
        , Fix.insertAt right.start (operator ++ " ")
        ]


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.Application [ Node.Node range (Expression.PrefixOperator operator), left, right ] ->
            [ error operator range (Node.range left) (Node.range right) ]

        _ ->
            []
