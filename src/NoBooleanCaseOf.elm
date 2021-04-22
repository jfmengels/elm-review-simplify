module NoBooleanCaseOf exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when pattern matching is used for a boolean value.

The idiomatic way to check for a condition is to use an `if` expression.
Read more about it at: <https://guide.elm-lang.org/core_language.html#if-expressions>

    config =
        [ NoBooleanCaseOf.rule
        ]

This won't report pattern matching when a boolean is part of the evaluated value.


## Fail

    _ =
        case bool of
            True ->
                expression

            False ->
                otherExpression


## Success

    _ =
        if bool then
            expression

        else
            otherExpression

    _ =
        case ( bool, somethingElse ) of
            ( True, SomeThingElse ) ->
                expression

            _ ->
                otherExpression


# When (not) to use this rule

You should not use this rule if you do not care about how your boolean values are
evaluated.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example --rules NoBooleanCaseOf
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoBooleanCaseOf" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.CaseExpression { expression, cases } ->
            case cases of
                [ ( firstPattern, Node firstRange _ ), ( Node secondPatternRange _, Node secondExprRange _ ) ] ->
                    case getBooleanPattern firstPattern of
                        Just isTrueFirst ->
                            [ Rule.errorWithFix
                                { message = "Replace `case..of` by an `if` condition"
                                , details =
                                    [ "The idiomatic way to check for a condition is to use an `if` expression."
                                    , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
                                    ]
                                }
                                (Node.range expression)
                                (if isTrueFirst then
                                    [ Fix.replaceRangeBy { start = (Node.range node).start, end = (Node.range expression).start } "if "
                                    , Fix.replaceRangeBy { start = (Node.range expression).end, end = firstRange.start } " then "
                                    , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                                    ]

                                 else
                                    [ Fix.replaceRangeBy { start = (Node.range node).start, end = (Node.range expression).start } "if not ("
                                    , Fix.replaceRangeBy { start = (Node.range expression).end, end = firstRange.start } ") then "
                                    , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                                    ]
                                )
                            ]

                        _ ->
                            []

                _ ->
                    []

        _ ->
            []


getBooleanPattern : Node Pattern -> Maybe Bool
getBooleanPattern node =
    case Node.value node of
        Pattern.NamedPattern { moduleName, name } _ ->
            if moduleName == [] || moduleName == [ "Basics" ] then
                case name of
                    "True" ->
                        Just True

                    "False" ->
                        Just False

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing
