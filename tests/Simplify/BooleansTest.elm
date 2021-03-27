module Simplify.BooleansTest exposing (all)

import Review.Test
import Simplify.Booleans exposing (rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Simplify.Booleans"
        [ test "should not report unsimplifiable condition" <|
            \() ->
                """module A exposing (..)
a = x || y
b = y && z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify 'True || x' to True" <|
            \() ->
                """module A exposing (..)
a = True || x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "True || x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = True
"""
                        ]
        , Test.skip <|
            test "should simplify 'True && x' to x" <|
                \() ->
                    """module A exposing (..)
a = True && x
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "REPLACEME"
                                , details = [ "REPLACEME" ]
                                , under = "True && x"
                                }
                            ]
        , Test.skip <|
            test "should simplify 'False && x' to False" <|
                \() ->
                    """module A exposing (..)
a = False && x
"""
                        |> Review.Test.run rule
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = "REPLACEME"
                                , details = [ "REPLACEME" ]
                                , under = "False && x"
                                }
                            ]
        , test "should simplify 'False || x' to x" <|
            \() ->
                """module A exposing (..)
a = False || x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "REPLACEME"
                            , details = [ "REPLACEME" ]
                            , under = "False || x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        ]
