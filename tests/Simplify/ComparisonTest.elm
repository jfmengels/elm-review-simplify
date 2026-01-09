module Simplify.ComparisonTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults, whenNotExpectingNaN)


all : Test
all =
    describe "comparison tests"
        [ lessThanTests
        , lessThanOrEqualToTests
        , greaterThanOrEqualToTests
        , greaterThanTests
        ]


lessThanTests : Test
lessThanTests =
    describe "<"
        [ test "should simplify n < n to False, expect NaN enabled" <|
            \() ->
                """module A exposing (..)
a = n < n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with two equal operands results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify n > n to False" <|
            \() ->
                """module A exposing (..)
a = n > n
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "(>) with two equal operands results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify n >= n to True" <|
            \() ->
                """module A exposing (..)
a = n >= n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) with two equal operands results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify n <= n to True" <|
            \() ->
                """module A exposing (..)
a = n <= n
"""
                    |> whenNotExpectingNaN Review.Test.run
                        [ Review.Test.error
                            { message = "(<=) with two equal operands results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report > with okay operands" <|
            \() ->
                """module A exposing (..)
a = x > y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report >= with okay operands" <|
            \() ->
                """module A exposing (..)
a = x >= y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report < with okay operands" <|
            \() ->
                """module A exposing (..)
a = x < y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report <= with okay operands" <|
            \() ->
                """module A exposing (..)
a = x <= y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 1 < 2 to True" <|
            \() ->
                """module A exposing (..)
a = 1 < 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 1.1 < 2.2 to True" <|
            \() ->
                """module A exposing (..)
a = 1.1 < 2.2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'a' < 'b' to True" <|
            \() ->
                """module A exposing (..)
a = 'a' < 'b'
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify \"a\" < \"b\" to True" <|
            \() ->
                """module A exposing (..)
a = "a" < "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify (1.1,2) < (2.2,1) to True" <|
            \() ->
                """module A exposing (..)
a = (1.1,2) < (2.2,1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify (0,1.1) < (0,2.2) to True" <|
            \() ->
                """module A exposing (..)
a = (0,1.1) < (0,2.2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify [1.1,2] < [2.2,1] to True" <|
            \() ->
                """module A exposing (..)
a = [1.1,2] < [2.2,1]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify [1.1,2] < [2.2] to True" <|
            \() ->
                """module A exposing (..)
a = [1.1,2] < [2.2]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify [0,1.1] < ([0,(2.2)]) to True" <|
            \() ->
                """module A exposing (..)
a = [0,1.1] < ([0,(2.2)])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify [0] < [0,2.2] to True" <|
            \() ->
                """module A exposing (..)
a = [0] < [0,2.2]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 1 < 2 + 3 to False" <|
            \() ->
                """module A exposing (..)
a = 1 < 2 + 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 2 < 1 to False" <|
            \() ->
                """module A exposing (..)
a = 2 < 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) with a left value greater than the right results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 > 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 > 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>) with a left value less than the right results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 >= 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) with a left value less than the right results in False"
                            , details = [ "You can replace this call by False." ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 <= 2 to True" <|
            \() ->
                """module A exposing (..)
a = 1 <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) with a left value less than the right results in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length l < 1 with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = List.length l < 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.length < 1 can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace 0 < List.length l with not (List.isEmpty l)" <|
            \() ->
                """module A exposing (..)
a = 0 < List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 < List.length can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by not on List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (<) 0 << List.length with List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = (<) 0 << List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 < List.length can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this composition by not on List.isEmpty."
                                ]
                            , under = "(<)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not << List.isEmpty
"""
                        ]
        , test "should not report List.length l < min 0 n when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.length l < min 0 n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should not report x < List.length list" <|
            \() ->
                """module A exposing (..)
a = x < List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length < x" <|
            \() ->
                """module A exposing (..)
a = List.length length < x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length < String.length str" <|
            \() ->
                """module A exposing (..)
a = List.length length < String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.length l < 0 by False" <|
            \() ->
                """module A exposing (..)
a = List.length l < 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly 0."
                                ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace min -1 n < 0 by True" <|
            \() ->
                """module A exposing (..)
a = min -1 n < 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at most -1 and the right number was determined to be exactly 0."
                                ]
                            , under = "<"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report min 0 n < 0 because the two intervals overlap at 0" <|
            \() ->
                """module A exposing (..)
a = min 0 n < 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


greaterThanOrEqualToTests : Test
greaterThanOrEqualToTests =
    describe ">="
        [ test "should replace 0 >= List.length l with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = 0 >= List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 >= List.length can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace List.length l >= 1 with not (List.isEmpty l)" <|
            \() ->
                """module A exposing (..)
a = List.length l >= 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.length >= 1 can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by not on List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (>=) 0 << List.length with List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = (>=) 0 << List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 >= List.length can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this composition by List.isEmpty."
                                ]
                            , under = "(>=)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty
"""
                        ]
        , test "should not replace List.length l >= min -1 n when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.length l >= min -1 n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should not replace min 0 n >= 0 because the two intervals overlap at 0" <|
            \() ->
                """module A exposing (..)
a = min 0 n >= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace min -1 n >= 0 by False" <|
            \() ->
                """module A exposing (..)
a = min -1 n >= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at most -1 and the right number was determined to be exactly 0."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.length l >= 0 by True" <|
            \() ->
                """module A exposing (..)
a = List.length l >= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly 0."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.length l >= -1 by True" <|
            \() ->
                """module A exposing (..)
a = List.length l >= -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly -1."
                                ]
                            , under = ">="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report x >= List.length list" <|
            \() ->
                """module A exposing (..)
a = x >= List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length >= x" <|
            \() ->
                """module A exposing (..)
a = List.length length >= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length >= String.length str" <|
            \() ->
                """module A exposing (..)
a = List.length length >= String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report x >= y" <|
            \() ->
                """module A exposing (..)
a = x >= y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


lessThanOrEqualToTests : Test
lessThanOrEqualToTests =
    describe "<="
        [ test "should replace List.length l <= 0 with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = List.length l <= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.length <= 0 can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace 1 <= List.length l with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = 1 <= List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "1 <= List.length can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by not on List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (<=) 1 << List.length with List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = (<=) 1 << List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "1 <= List.length can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this composition by not on List.isEmpty."
                                ]
                            , under = "(<=)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not << List.isEmpty
"""
                        ]
        , test "should not replace List.length l <= min -1 n when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.length l <= min -1 n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace List.length l <= -1 by False" <|
            \() ->
                """module A exposing (..)
a = List.length l <= -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly -1."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace min 0 n <= 0 by True" <|
            \() ->
                """module A exposing (..)
a = min 0 n <= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at most 0 and the right number was determined to be exactly 0."
                                ]
                            , under = "<="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report max 0 n <= 0 because the two intervals overlap at 0" <|
            \() ->
                """module A exposing (..)
a = max 0 n <= 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report x <= List.length list" <|
            \() ->
                """module A exposing (..)
a = x <= List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length <= x" <|
            \() ->
                """module A exposing (..)
a = List.length length <= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length <= String.length str" <|
            \() ->
                """module A exposing (..)
a = List.length length <= String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


greaterThanTests : Test
greaterThanTests =
    describe ">"
        [ test "should replace 1 > List.length l with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = 1 > List.length l
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "1 > List.length can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty l
"""
                        ]
        , test "should replace List.length l > 0 with List.isEmpty l" <|
            \() ->
                """module A exposing (..)
a = List.length l > 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.length > 0 can be replaced by not on List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this operation by not on List.isEmpty on the list given to the List.length call."
                                ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (List.isEmpty l)
"""
                        ]
        , test "should replace (>) 1 << List.length with List.isEmpty" <|
            \() ->
                """module A exposing (..)
a = (>) 1 << List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "1 > List.length can be replaced by List.isEmpty"
                            , details =
                                [ "Whereas List.length takes as long to run as the number of elements in the list, List.isEmpty runs in constant time. You can replace this composition by List.isEmpty."
                                ]
                            , under = "(>)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.isEmpty
"""
                        ]
        , test "should not replace List.length l > max -1 n when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.length l > max -1 n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should replace List.length l > -1 by True" <|
            \() ->
                """module A exposing (..)
a = List.length l > -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>) comparison will result in True"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always greater than the interval of the right number. As a result, this operation can be replaced by True."
                                , "The left number was determined to be at least 0 and the right number was determined to be exactly -1."
                                ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace min 0 n > 0 by False" <|
            \() ->
                """module A exposing (..)
a = min 0 n > 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>) comparison will result in False"
                            , details =
                                [ "Based on the values and/or the context, we can determine that the interval of the left number is always less than or equal to the interval of the right number. As a result, this operation can be replaced by False."
                                , "The left number was determined to be at most 0 and the right number was determined to be exactly 0."
                                ]
                            , under = ">"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should not report max 0 n > 0 because the two intervals overlap at 0" <|
            \() ->
                """module A exposing (..)
a = max 0 n > 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report x > List.length list" <|
            \() ->
                """module A exposing (..)
a = x > List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length > x" <|
            \() ->
                """module A exposing (..)
a = List.length length > x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.length length > String.length str" <|
            \() ->
                """module A exposing (..)
a = List.length length > String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report x > y" <|
            \() ->
                """module A exposing (..)
a = x > y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]
