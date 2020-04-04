module NoFullyAppliedPrefixOperator exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
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

-}
rule : Rule
rule =
    Rule.newSchema "NoFullyAppliedPrefixOperator"
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromSchema


error : Range -> Error
error range =
    Rule.error
        { message = "TODO"
        , details =
            [ "TODO"
            ]
        }
        range


expressionVisitor : Node Expression -> List Error
expressionVisitor node =
    case Node.value node of
        Expression.Application [ Node.Node range (Expression.PrefixOperator _), _, _ ] ->
            [ error range ]

        _ ->
            []
