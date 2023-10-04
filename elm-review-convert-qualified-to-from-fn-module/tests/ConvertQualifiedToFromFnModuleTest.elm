module ConvertQualifiedToFromFnModuleTest exposing (all)

import ConvertQualifiedToFromFnModule exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ConvertQualifiedToFromFnModule"
        [ test "should not report an error when using Fn.Module.name" <|
            \() ->
                """module A exposing (..)
import Set
import Fn.Module

a =
    Fn.Module.name
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report an error when using manual tuple of a normal function" <|
            \() ->
                """module A exposing (..)
import Set

a =
    ( [ "Html", "Attributes" ], "class" )
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Manual qualified fn tuple can be replaced by Fn.Html.Attributes.class"
                            , details = [ "You can replace this tuple by Fn.Html.Attributes.class." ]
                            , under = """( [ "Html", "Attributes" ], "class" )"""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Fn.Html.Attributes
import Set

a =
    Fn.Html.Attributes.class
"""
                        ]
        , test "should report an error when using manual tuple of a variant" <|
            \() ->
                """module A exposing (..)
import Set

a =
    ( [ "Result" ], "Ok" )
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Manual qualified fn tuple can be replaced by Fn.Result.ok"
                            , details = [ "You can replace this tuple by Fn.Result.ok." ]
                            , under = """( [ "Result" ], "Ok" )"""
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
import Fn.Result
import Set

a =
    Fn.Result.ok
"""
                        ]
        ]
