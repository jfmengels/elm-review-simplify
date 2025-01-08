module Simplify.AstHelpersTest exposing (all)

import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect exposing (Expectation)
import Simplify.AstHelpers as AstHelpers
import Test exposing (Test, describe, test)


all : Test
all =
    describe "AstHelpers.isPotentialNaNKey"
        [ test "String literals don't contain NaN" <|
            \() ->
                "\"a\""
                    |> isPotentialNaNKey False
        , test "Char literals don't contain NaN" <|
            \() ->
                "'a'"
                    |> isPotentialNaNKey False
        , test "Integer literals don't contain NaN" <|
            \() ->
                "0"
                    |> isPotentialNaNKey False
        , test "Float literals don't contain NaN" <|
            \() ->
                "0.0"
                    |> isPotentialNaNKey False
        , test "Number addition don't contain literals if all sub-expression can't contain NaN" <|
            \() ->
                "0.0 + 0"
                    |> isPotentialNaNKey False
        , test "Float division can contain NaN" <|
            \() ->
                "0 / 0"
                    |> isPotentialNaNKey True
        , test "Float division can't contain NaN" <|
            \() ->
                "0 // 0"
                    |> isPotentialNaNKey False
        , test "Applied || can't contain NaN" <|
            \() ->
                "a || b"
                    |> isPotentialNaNKey False
        , test "Applied && can't contain NaN" <|
            \() ->
                "a && b"
                    |> isPotentialNaNKey False
        , test "Applied ++ with string literals can't contain NaN" <|
            \() ->
                "\"a\" ++ \"b\""
                    |> isPotentialNaNKey False
        , test "Applied ++ with unknown values can contain NaN" <|
            \() ->
                "a ++ b"
                    |> isPotentialNaNKey True
        , test "Applied <?> can't contain NaN" <|
            \() ->
                "a <?> b"
                    |> isPotentialNaNKey False
        , test "References can contain NaN" <|
            \() ->
                "reference"
                    |> isPotentialNaNKey True
        , test "Lists with only non-NaN can't contain NaN" <|
            \() ->
                "[ 0, 1, 2 ]"
                    |> isPotentialNaNKey False
        , test "Lists with only potential NaN can contain NaN" <|
            \() ->
                "[ 0, reference, 2 ]"
                    |> isPotentialNaNKey True
        , test "Function calls can return NaN" <|
            \() ->
                "fn 0"
                    |> isPotentialNaNKey True
        , test "Function calls with <| can return NaN" <|
            \() ->
                "fn <| 0"
                    |> isPotentialNaNKey True
        , test "Function calls with |> can return NaN" <|
            \() ->
                "0 |> fn"
                    |> isPotentialNaNKey True
        , test "Record access functions can't return NaN" <|
            \() ->
                ".field"
                    |> isPotentialNaNKey False
        , test "Record access can return NaN" <|
            \() ->
                "record.field"
                    |> isPotentialNaNKey True
        , test "let expression checks for the `in` value (literal)" <|
            \() ->
                "let _ = 1 in 0"
                    |> isPotentialNaNKey False
        , test "let expression checks for the `in` value (reference)" <|
            \() ->
                "let _ = 1 in reference"
                    |> isPotentialNaNKey True
        ]


isPotentialNaNKey : Bool -> String -> Expectation
isPotentialNaNKey expected source =
    case Elm.Parser.parseToFile (wrapSource source) of
        Ok ast ->
            case List.head ast.declarations of
                Just (Node _ (Elm.Syntax.Declaration.FunctionDeclaration { declaration })) ->
                    (Node.value declaration).expression
                        |> AstHelpers.isPotentialNaNKey
                        |> Expect.equal expected

                _ ->
                    Debug.todo "Test setup failed"

        Err errors ->
            Debug.todo ("Could not parse source code: " ++ Debug.toString errors)


wrapSource : String -> String
wrapSource source =
    """module A exposing (a)
a = """ ++ source
