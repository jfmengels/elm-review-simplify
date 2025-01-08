module Simplify.AstHelpersTest exposing (all)

import Elm.Parser
import Elm.Syntax.Declaration
import Elm.Syntax.Node as Node exposing (Node(..))
import Expect exposing (Expectation)
import Simplify.AstHelpers as AstHelpers
import Test exposing (Test, describe, test)


all : Test
all =
    describe "AstHelpers.canEqualOrContainNaN"
        [ test "String literals don't contain NaN" <|
            \() ->
                "\"a\""
                    |> canEqualOrContainNaN False
        , test "Char literals don't contain NaN" <|
            \() ->
                "'a'"
                    |> canEqualOrContainNaN False
        , test "Integer literals don't contain NaN" <|
            \() ->
                "0"
                    |> canEqualOrContainNaN False
        , test "Float literals don't contain NaN" <|
            \() ->
                "0.0"
                    |> canEqualOrContainNaN False
        , test "Number addition don't contain literals if all sub-expression can't contain NaN" <|
            \() ->
                "0.0 + 0"
                    |> canEqualOrContainNaN False
        , test "Float division can contain NaN" <|
            \() ->
                "0 / 0"
                    |> canEqualOrContainNaN True
        , test "Float division can't contain NaN" <|
            \() ->
                "0 // 0"
                    |> canEqualOrContainNaN False
        , test "References can contain NaN" <|
            \() ->
                "reference"
                    |> canEqualOrContainNaN True
        , test "Lists with only non-NaN can't contain NaN" <|
            \() ->
                "[ 0, 1, 2 ]"
                    |> canEqualOrContainNaN False
        , test "Lists with only potential NaN can contain NaN" <|
            \() ->
                "[ 0, reference, 2 ]"
                    |> canEqualOrContainNaN True
        , test "Function calls can return NaN" <|
            \() ->
                "fn 0"
                    |> canEqualOrContainNaN True
        , test "Function calls with <| can return NaN" <|
            \() ->
                "fn <| 0"
                    |> canEqualOrContainNaN True
        , test "Function calls with |> can return NaN" <|
            \() ->
                "0 |> fn"
                    |> canEqualOrContainNaN True
        , test "Record access functions can't return NaN" <|
            \() ->
                ".field"
                    |> canEqualOrContainNaN False
        , test "Record access can return NaN" <|
            \() ->
                "record.field"
                    |> canEqualOrContainNaN True
        ]


canEqualOrContainNaN : Bool -> String -> Expectation
canEqualOrContainNaN expected source =
    case Elm.Parser.parseToFile (wrapSource source) of
        Ok ast ->
            case List.head ast.declarations of
                Just (Node _ (Elm.Syntax.Declaration.FunctionDeclaration { declaration })) ->
                    (Node.value declaration).expression
                        |> AstHelpers.canEqualOrContainNaN
                        |> Expect.equal expected

                _ ->
                    Debug.todo "Test setup failed"

        Err errors ->
            Debug.todo ("Could not parse source code: " ++ Debug.toString errors)


wrapSource : String -> String
wrapSource source =
    """module A exposing (a)
a = """ ++ source
