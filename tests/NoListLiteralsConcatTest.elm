module NoListLiteralsConcatTest exposing (all)

import NoListLiteralsConcat exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoListLiteralsConcat"
        [ usingPlusPlusTests
        ]


message : String
message =
    "Expression could be simplified to be a single List"


details : List String
details =
    [ "Try moving all the elements into a single list." ]


usingPlusPlusTests : Test
usingPlusPlusTests =
    describe "Using (++)"
        [ test "should not report a single list literal" <|
            \() ->
                """module A exposing (..)
a = []
b = [1]
c = [ "string", "foo", "bar" ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report concatenating two list literals" <|
            \() ->
                """module A exposing (..)
a = [ 1 ] ++ [ 2, 3 ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "[ 1 ] ++ [ 2, 3 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1 , 2, 3 ]
"""
                        ]
        , test "should report concatenating two list literals, even they contain variables" <|
            \() ->
                """module A exposing (..)
a = [ a, 1 ] ++ [ b, 2 ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "[ a, 1 ] ++ [ b, 2 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a, 1 , b, 2 ]
"""
                        ]
        , test "should report concatenating an empty list and something" <|
            \() ->
                """module A exposing (..)
a = [] ++ something
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Concatenating with a single list doesn't have any effect"
                            , details = [ "You should remove the concatenation with the empty list." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something
"""
                        ]
        , test "should report concatenating something and an empty list" <|
            \() ->
                """module A exposing (..)
a = something ++ []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Concatenating with a single list doesn't have any effect"
                            , details = [ "You should remove the concatenation with the empty list." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something
"""
                        ]
        , test "should not report using :: to a variable or expression" <|
            \() ->
                """module A exposing (..)
a = 1 :: list
b = 1 :: foo bar
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report using :: to a list literal" <|
            \() ->
                """module A exposing (..)
a = 1 :: [ 2, 3]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside single list." ]
                            , under = "1 :: [ 2, 3]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1, 2, 3]
"""
                        ]
        , test "should not report List.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.concat [ foo, bar ]
b = List.concat [ [ 1 ], foo ]
c = List.concat [ foo, [ 1 ] ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report List.concat with no items" <|
            \() ->
                """module A exposing (..)
a = List.concat []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.concat"
                            , details = [ "The value of the operation will be []. You should replace this expression by that." ]
                            , under = "List.concat []"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should report List.concat with a single item" <|
            \() ->
                """module A exposing (..)
a = List.concat [ b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "List.concat [ b ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat that only contains list literals" <|
            \() ->
                """module A exposing (..)
a = List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = message
                            , details = details
                            , under = "List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]"
                            }
                        ]
        ]
