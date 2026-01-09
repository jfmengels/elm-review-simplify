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
        ]
