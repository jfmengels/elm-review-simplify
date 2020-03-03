module NoAddingListLiterals exposing (rule)

{-| TODO Rename this to NoUnnecessaryListConcat?

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Rule as Rule exposing (Error, Rule)


{-| Forbids concatenating list literals.

This can be combined into one list.

    config =
        [ NoAddingListLiterals.rule
        ]


## Fail

    _ =
        [ 1, 2, 3 ] ++ [ 4, mysteryNumber, 6 ]

    -- TODO
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


# When (not) to use this rule

You should not use this rule if you do not care about your boolean values are
evaluated.

-}
rule : Rule
rule =
    Rule.newSchema "NoAddingListLiterals"
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
        Expression.OperatorApplication "++" _ (Node.Node _ (Expression.ListExpr _)) (Node.Node _ (Expression.ListExpr _)) ->
            [ error (Node.range node) ]

        Expression.OperatorApplication "++" _ (Node.Node range (Expression.ListExpr [])) _ ->
            [ error range ]

        Expression.OperatorApplication "++" _ _ (Node.Node range (Expression.ListExpr [])) ->
            [ error range ]

        Expression.OperatorApplication "::" _ _ (Node.Node _ (Expression.ListExpr _)) ->
            [ error (Node.range node) ]

        Expression.Application [ Node.Node _ (Expression.FunctionOrValue [ "List" ] "concat"), Node.Node _ (Expression.ListExpr list) ] ->
            if List.length list < 2 then
                [ error (Node.range node) ]

            else if List.all isListLiteral list then
                [ error (Node.range node) ]

            else
                []

        _ ->
            []


isListLiteral : Node Expression -> Bool
isListLiteral node =
    case Node.value node of
        Expression.ListExpr _ ->
            True

        _ ->
            False
