module Simplify.EqualTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)


sameThingOnBothSidesDetails : String -> List String
sameThingOnBothSidesDetails value =
    [ "Based on the values and/or the context, we can determine the result. You can replace this operation by " ++ value ++ "."
    ]


all : Test
all =
    describe "(==)"
        [ test "should not simplify values that can't be determined" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify x == True to x" <|
            \() ->
                """module A exposing (..)
a = x == True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "== True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify x == False" <|
            \() ->
                """module A exposing (..)
a = x == False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify True == x to x" <|
            \() ->
                """module A exposing (..)
a = True == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "True =="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify False == x" <|
            \() ->
                """module A exposing (..)
a = False == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify x /= True" <|
            \() ->
                """module A exposing (..)
a = x /= True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify x /= False to x" <|
            \() ->
                """module A exposing (..)
a = x /= False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "/= False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify True /= x" <|
            \() ->
                """module A exposing (..)
a = True /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify False /= x to x" <|
            \() ->
                """module A exposing (..)
a = False /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "False /="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify not x == not y to x == y" <|
            \() ->
                """module A exposing (..)
a = not x == not y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (==)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "=="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x == y
"""
                        ]
        , test "should simplify (x |> f |> not) == (y |> g |> not) to (x |> f) == (y |> g)" <|
            \() ->
                """module A exposing (..)
a = (x |> f |> not) == (y |> g |> not)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (==)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "=="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x |> f) == (y |> g)
"""
                        ]
        , test "should simplify not x /= not y to x /= y" <|
            \() ->
                """module A exposing (..)
a = not x /= not y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (/=)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "/="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x /= y
"""
                        ]
        , test "should simplify x == x to True" <|
            \() ->
                """module A exposing (..)
a = x == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify x == x when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = x == x
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should simplify x == (x) to True" <|
            \() ->
                """module A exposing (..)
a = x == (x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == (x)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify x /= x to False" <|
            \() ->
                """module A exposing (..)
a = x /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(/=) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x /= x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify more complex calls (function call and lambda)" <|
            \() ->
                """module A exposing (..)
a = List.map (\\a -> a.value) things == List.map (\\a -> a.value) things
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "List.map (\\a -> a.value) things == List.map (\\a -> a.value) things"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with a single arg that use `<|`" <|
            \() ->
                """module A exposing (..)
a = (f b) == (f <| b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b) == (f <| b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with multiple args that use `<|`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (f b <| c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (f b <| c)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with a single arg that use `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b) == (b |> f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b) == (b |> f)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with multiple args that use `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (c |> f b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (c |> f b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (c |> (b |> f))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (c |> (b |> f))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in let expression" <|
            \() ->
                """module A exposing (..)
a = (let x = 1 in f b c) == (c |> (let x = 1 in f b))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(let x = 1 in f b c) == (c |> (let x = 1 in f b))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in if expression" <|
            \() ->
                """module A exposing (..)
a = (if cond then f b c else g d c) == (c |> (if cond then f b else g d))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(if cond then f b c else g d c) == (c |> (if cond then f b else g d))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in case expression" <|
            \() ->
                """module A exposing (..)
a = (case x of
        X -> f b c
        Y -> g d c
    )
    ==
    ((case x of
        X -> f b
        Y -> g d
    ) <| c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = """(case x of
        X -> f b c
        Y -> g d c
    )
    ==
    ((case x of
        X -> f b
        Y -> g d
    ) <| c)"""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record access comparison" <|
            \() ->
                """module A exposing (..)
a = (b.c) == (.c b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(b.c) == (.c b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record access comparison using pipeline" <|
            \() ->
                """module A exposing (..)
a = (b.c) == (.c <| b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(b.c) == (.c <| b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different literals to False" <|
            \() ->
                """module A exposing (..)
a = "a" == "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "\"a\" == \"b\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different char literals to False" <|
            \() ->
                """module A exposing (..)
a = 'a' == 'b'
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "'a' == 'b'"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify inequality of different literal comparisons to True" <|
            \() ->
                """module A exposing (..)
a = "a" /= "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(/=) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "\"a\" /= \"b\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different number literal comparisons to False" <|
            \() ->
                """module A exposing (..)
a = 1 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (integer left)" <|
            \() ->
                """module A exposing (..)
a = 1 == 2.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 == 2.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (float left)" <|
            \() ->
                """module A exposing (..)
a = 1.0 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1.0 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (hex left)" <|
            \() ->
                """module A exposing (..)
a = 0x10 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "0x10 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (addition left)" <|
            \() ->
                """module A exposing (..)
a = 1 + 3 == 2 + 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 + 3 == 2 + 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (subtraction left)" <|
            \() ->
                """module A exposing (..)
a = 1 - 3 == 2 - 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 - 3 == 2 - 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (multiplication left)" <|
            \() ->
                """module A exposing (..)
a = 2 * 3 == 2 * 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "2 * 3 == 2 * 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (division left)" <|
            \() ->
                """module A exposing (..)
a = 1 / 3 == 2 / 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 / 3 == 2 / 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of same value (() left)" <|
            \() ->
                """module A exposing (..)
a = () == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "() == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of same value (() right)" <|
            \() ->
                """module A exposing (..)
a = x == ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == ()"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of lists (different lengths)" <|
            \() ->
                """module A exposing (..)
a = [ 1 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "[ 1 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of lists (same lengths but different values)" <|
            \() ->
                """module A exposing (..)
a = [ 1, 2 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "[ 1, 2 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of lists (same values)" <|
            \() ->
                """module A exposing (..)
a = [ 1, 2 - 1 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "[ 1, 2 - 1 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different integers comparisons to False (wrapped in parens)" <|
            \() ->
                """module A exposing (..)
a = (1) == (2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "(1) == (2)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of tuples" <|
            \() ->
                """module A exposing (..)
a = ( 1, 2 ) == ( 1, 1 )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "( 1, 2 ) == ( 1, 1 )"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of records" <|
            \() ->
                """module A exposing (..)
a = { a = 1, b = 2 } == { b = 1, a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ a = 1, b = 2 } == { b = 1, a = 1 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of record updates with same base values and different field values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | a = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ x | a = 1 } == { x | a = 2 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of record updates with same base values and field values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "{ x | a = 1 } == { x | a = 1 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify equality of record updates with same base values and different fields" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | b = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify equality of record updates (different base values)" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ x | a = 1 } == { y | a = 2 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should not simplify equality of record updates with same field values but different base values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify equality of record updates with non-corresponding fields but otherwise similar field values and different base values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 1, b = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify comparison of values for which we don't know if they're equal" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should normalize module names" <|
            \() ->
                [ """module A exposing (..)
import B exposing (b)
a = B.b == b
""", """module Other exposing (..)
b = 1
""" ]
                    |> Review.Test.runOnModules ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "(==) comparison will result in True"
                                , details = sameThingOnBothSidesDetails "True"
                                , under = "B.b == b"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B exposing (b)
a = True
"""
                            ]
                          )
                        ]
        , test "should simplify function calls with the same function and similar arguments" <|
            \() ->
                """module A exposing (..)
import List exposing (map)
a = List.map fn 1 == map fn (2 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "List.map fn 1 == map fn (2 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import List exposing (map)
a = True
"""
                        ]
        , test "should not simplify function calls of the same function but with different arguments" <|
            \() ->
                """module A exposing (..)
import List exposing (map)
a = List.map fn 1 == List.map fn 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify if expressions that look like each other" <|
            \() ->
                """module A exposing (..)
a = (if 1 then 2 else 3) == (if 2 - 1 then 3 - 1 else 4 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(if 1 then 2 else 3) == (if 2 - 1 then 3 - 1 else 4 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify if expressions that don't look like each other" <|
            \() ->
                """module A exposing (..)
a = (if a then 2 else 3) == (if a then 1 else 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify negations that look like each other" <|
            \() ->
                """module A exposing (..)
a = -1 == -(2 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "-1 == -(2 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record accesses that look like each other" <|
            \() ->
                """module A exposing (..)
a = ({ a = 1 }).a == ({ a = 2 - 1 }).a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "({ a = 1 }).a == ({ a = 2 - 1 }).a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        , Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 37 }, end = { row = 2, column = 39 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ({ a = 1 }).a == (2 - 1)
"""
                        , Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 16 }, end = { row = 2, column = 18 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1 == ({ a = 2 - 1 }).a
"""
                        ]
        , test "should simplify operator expressions" <|
            \() ->
                """module A exposing (..)
a = (1 |> fn) == (2 - 1 |> fn)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(1 |> fn) == (2 - 1 |> fn)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify with different fields" <|
            \() ->
                """module A exposing (..)
a = ({ a = 1 }).a == ({ a = 1 }).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1 == ({ a = 1 }).b
"""
                        ]
        ]
