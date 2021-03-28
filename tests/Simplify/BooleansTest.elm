module Simplify.BooleansTest exposing (all)

import Review.Test
import Simplify.Booleans exposing (rule)
import Test exposing (Test, describe, test)


alwaysSameDetails : List String
alwaysSameDetails =
    [ "This condition will always result in the same value. You may have hardcoded a value or mistyped a condition."
    ]


unnecessaryMessage : String
unnecessaryMessage =
    "Part of the expression is unnecessary"


unnecessaryDetails : List String
unnecessaryDetails =
    [ "A part of this condition is unnecessary. You can remove it and it would not impact the behavior of the program."
    ]


sameThingOnBothSidesDetails : String -> List String
sameThingOnBothSidesDetails value =
    [ "The value on the left and on the right are the same. Therefore we can determine that the expression will always be " ++ value ++ "."
    ]


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
        , orTests
        , andTests
        , sameThingOnBothSidesTests
        ]


orTests : Test
orTests =
    describe "||"
        [ test "should simplify 'True || x' to True" <|
            \() ->
                """module A exposing (..)
a = True || x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is always True"
                            , details = alwaysSameDetails
                            , under = "True || x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'x || True' to x" <|
            \() ->
                """module A exposing (..)
a = x || True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "x || True"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'False || x' to x" <|
            \() ->
                """module A exposing (..)
a = False || x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "False || x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x || False' to x" <|
            \() ->
                """module A exposing (..)
a = x || False
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "x || False"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should ignore parens around False" <|
            \() ->
                """module A exposing (..)
a = x || (False)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "x || (False)"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should ignore parens around True" <|
            \() ->
                """module A exposing (..)
a = (True) || x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is always True"
                            , details = alwaysSameDetails
                            , under = "(True) || x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = (True)
"""
                        ]
        ]


andTests : Test
andTests =
    describe "&&"
        [ test "should simplify 'True && x' to x" <|
            \() ->
                """module A exposing (..)
a = True && x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "True && x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x && True' to x" <|
            \() ->
                """module A exposing (..)
a = x && True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = unnecessaryMessage
                            , details = unnecessaryDetails
                            , under = "x && True"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'False && x' to False" <|
            \() ->
                """module A exposing (..)
a = False && x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is always False"
                            , details = alwaysSameDetails
                            , under = "False && x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'x && False' to False" <|
            \() ->
                """module A exposing (..)
a = x && False
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is always False"
                            , details = alwaysSameDetails
                            , under = "x && False"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = False
"""
                        ]
        ]


sameThingOnBothSidesTests : Test
sameThingOnBothSidesTests =
    describe "Same thing on both sides"
        [ test "should not simplify values that can't be determined" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify x == x to True" <|
            \() ->
                """module A exposing (..)
a = x == x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify x == (x) to True" <|
            \() ->
                """module A exposing (..)
a = x == (x)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == (x)"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify x /= x to False" <|
            \() ->
                """module A exposing (..)
a = x /= x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is always False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x /= x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = False
"""
                        ]
        ]
