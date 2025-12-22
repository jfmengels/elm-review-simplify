module Simplify.JsonEncodeTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    Test.describe "Json.Encode"
        [ jsonEncodeArrayTests
        , jsonEncodeListTests
        ]


jsonEncodeArrayTests : Test
jsonEncodeArrayTests =
    describe "Json.Encode.array"
        [ test "should not report Json.Encode.array used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a0 = Json.Encode.array
a1 = Json.Encode.array f
a2 = Json.Encode.array f array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Encode.array f (Array.fromList list) by Json.Encode.list f list" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.array f (Array.fromList list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a list, you don't need to convert to an array"
                            , details = [ "Using Json.Encode.list directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.list f list
"""
                        ]
        , test "should replace Json.Encode.array f << Array.fromList by Json.Encode.list f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.array f << Array.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a list, you don't need to convert to an array"
                            , details = [ "Using Json.Encode.list directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.list f
"""
                        ]
        , test "should replace Array.fromList >> Json.Encode.array f by Json.Encode.list f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Array.fromList >> Json.Encode.array f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a list, you don't need to convert to an array"
                            , details = [ "Using Json.Encode.list directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.array"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.list f
"""
                        ]
        , test "should replace Json.Encode.list f (Set.toList set) by Json.Encode.set f set" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.list f (Set.toList set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a set, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.set directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.set f set
"""
                        ]
        , test "should replace Json.Encode.list f << Set.toList by Json.Encode.set f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.list f << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a set, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.set directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.set f
"""
                        ]
        , test "should replace Set.toList >> Json.Encode.list f by Json.Encode.set f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Set.toList >> Json.Encode.list f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode a set, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.set directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.set f
"""
                        ]
        ]


jsonEncodeListTests : Test
jsonEncodeListTests =
    describe "Json.Encode.list"
        [ test "should not report Json.Encode.list used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a0 = Json.Encode.list
a1 = Json.Encode.list f
a2 = Json.Encode.list f array
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Json.Encode.list f (Array.toList array) by Json.Encode.array f list" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.list f (Array.toList array)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode an array, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.array directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.array f array
"""
                        ]
        , test "should replace Json.Encode.list f << Array.toList by Json.Encode.array f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Json.Encode.list f << Array.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode an array, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.array directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.array f
"""
                        ]
        , test "should replace Array.toList >> Json.Encode.list f by Json.Encode.array f" <|
            \() ->
                """module A exposing (..)
import Json.Encode
a = Array.toList >> Json.Encode.list f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To encode an array, you don't need to convert to a list"
                            , details = [ "Using Json.Encode.array directly is meant for this exact purpose and will also be faster." ]
                            , under = "Json.Encode.list"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Json.Encode
a = Json.Encode.array f
"""
                        ]
        ]
