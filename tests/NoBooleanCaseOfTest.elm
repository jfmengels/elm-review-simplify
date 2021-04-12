module NoBooleanCaseOfTest exposing (all)

import NoBooleanCaseOf exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


message : String
message =
    "Replace `case..of` by an `if` condition"


details : List String
details =
    [ "The idiomatic way to check for a condition is to use an `if` expression."
    , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
    ]


tests : List Test
tests =
    [ test "should not report pattern matches for non-boolean values" <|
        \() ->
            """module A exposing (..)
a = case thing of
      Thing -> 1"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should not report pattern matches when the evaluated expression is a tuple of with a boolean" <|
        \() ->
            """module A exposing (..)
a = case ( bool1, bool2 ) of
      ( True, True ) -> 1
      _ -> 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    , test "should report pattern matches when one of the patterns is a bool constructor" <|
        \() ->
            """module A exposing (..)
a = case boolTrue of
      True -> 1
      _ -> 2

b = case boolFalse of
      False -> 1
      _ -> 2

c = case boolAll of
      False -> 1
      True -> 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "boolTrue"
                        }
                    , Review.Test.error
                        { message = message
                        , details = details
                        , under = "boolFalse"
                        }
                    , Review.Test.error
                        { message = message
                        , details = details
                        , under = "boolAll"
                        }
                    ]
    , test "should report pattern matches for booleans even when one of the patterns starts with `Basics.`" <|
        \() ->
            """module A exposing (..)
a = case boolTrue of
      Basics.True -> 1
      _ -> 2

b = case boolFalse of
      Basics.False -> 1
      _ -> 2"""
                |> Review.Test.run rule
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "boolTrue"
                        }
                    , Review.Test.error
                        { message = message
                        , details = details
                        , under = "boolFalse"
                        }
                    ]
    , test "should report pattern matches for booleans even when the constructor seems to be for booleans but comes from an unknown module" <|
        \() ->
            """module A exposing (..)
a = case boolTrue of
      OtherModule.True -> 1
      _ -> 2

b = case boolFalse of
      OtherModule.False -> 1
      _ -> 2"""
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
    ]


all : Test
all =
    describe "NoBooleanCaseOf" tests
