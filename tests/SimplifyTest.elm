module SimplifyTest exposing (all)

import Review.Test
import Simplify exposing (rule)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Simplify"
        [ identityTests
        , alwaysTests
        , booleanTests
        , ifTests
        , fullyAppliedPrefixOperatorTests
        , listSimplificationTests
        ]



-- BASICS


identityTests : Test
identityTests =
    describe "Basics.identity"
        [ test "should not report identity function on its own" <|
            \() ->
                """module A exposing (..)
a = identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace identity x by x" <|
            \() ->
                """module A exposing (..)
a = identity x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = identity <| x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]


alwaysTests : Test
alwaysTests =
    describe "Basics.always"
        [ test "should not report always function on its own" <|
            \() ->
                """module A exposing (..)
a = always
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report always with 1 argument" <|
            \() ->
                """module A exposing (..)
a = always x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace always x y by x" <|
            \() ->
                """module A exposing (..)
a = always x y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument to `always`"
                            , details = [ "REPLACEME" ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace always x <| y by x" <|
            \() ->
                """module A exposing (..)
a = always x <| y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument to `always`"
                            , details = [ "REPLACEME" ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace y |> always x by x" <|
            \() ->
                """module A exposing (..)
a = y |> always x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument to `always`"
                            , details = [ "REPLACEME" ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]



-- BOOLEANS


booleanTests : Test
booleanTests =
    describe "Booleans"
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
        , notTests
        , sameThingOnBothSidesTests
        ]


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


notTests : Test
notTests =
    describe "not calls"
        [ test "should simplify 'not True' to False" <|
            \() ->
                """module A exposing (..)
a = not True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'not False' to True" <|
            \() ->
                """module A exposing (..)
a = not False
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to True"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'not (True)' to False" <|
            \() ->
                """module A exposing (..)
a = not (True)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not (True)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'not <| True' to False" <|
            \() ->
                """module A exposing (..)
a = not <| True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "not <| True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'True |> not' to False" <|
            \() ->
                """module A exposing (..)
a = True |> not
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression is equal to False"
                            , details = [ "You can replace the call to `not` by the boolean value directly." ]
                            , under = "True |> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
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
        , test "should simplify more complex calls (function call and lambda)" <|
            \() ->
                """module A exposing (..)
a = List.map (\\a -> a.value) things == List.map (\\a -> a.value) things
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is always True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "List.map (\\a -> a.value) things == List.map (\\a -> a.value) things"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = True
"""
                        ]
        , test "should normalize module names" <|
            \() ->
                [ """module A exposing (..)
import B exposing (b)
a = B.b == b
""", """module Other exposing (..)
b = 1
""" ]
                    |> Review.Test.runOnModules rule
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Condition is always True"
                                , details = sameThingOnBothSidesDetails "True"
                                , under = "B.b == b"
                                }
                                |> Review.Test.whenFixed
                                    """module A exposing (..)
import B exposing (b)
a = True
"""
                            ]
                          )
                        ]
        ]



-- IF


ifTests : Test
ifTests =
    describe "if expressions"
        [ test "should remove the else branch when a condition is True" <|
            \() ->
                """module A exposing (..)
a = if True then 1 else 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should remove the if branch when a condition is False" <|
            \() ->
                """module A exposing (..)
a = if False then 1 else 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 2
"""
                        ]
        , test "should not remove anything if the condition is not statically knowable" <|
            \() ->
                """module A exposing (..)
a = if condition then 1 else 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should only keep the condition if then is True and else is False" <|
            \() ->
                """module A exposing (..)
a = if condition then True else False
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The if expression's value is the same as the condition"
                            , details = [ "The expression can be replaced by the condition." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = condition
"""
                        ]
        , test "should only keep the negated condition if then is False and else is True" <|
            \() ->
                """module A exposing (..)
a = if condition then False else True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The if expression's value is the inverse of the condition"
                            , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not (condition)
"""
                        ]
        , test "should replace the expression by the branch if both branches have the same value" <|
            \() ->
                """module A exposing (..)
a = if condition then x else x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The values in both branches is the same."
                            , details = [ "The expression can be replaced by the contents of either branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]



-- FULLY APPLIED PREFIX OPERATOR


fullyAppliedPrefixOperatorMessage : String
fullyAppliedPrefixOperatorMessage =
    "Use the infix form (a + b) over the prefix form ((+) a b)"


fullyAppliedPrefixOperatorDetails : List String
fullyAppliedPrefixOperatorDetails =
    [ "The prefix form is generally more unfamiliar to Elm developers, and therefore it is nicer when the infix form is used."
    ]


fullyAppliedPrefixOperatorTests : Test
fullyAppliedPrefixOperatorTests =
    describe "Fully applied prefix operators"
        [ test "should not report a lonely operator" <|
            \() ->
                """
module A exposing (..)
a = (++)
b = (::)
c = (//)
d = (+)
e = (/)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an operator used in infix position" <|
            \() ->
                """
module A exposing (..)
a = y ++ z
b = y :: z
c = y // z
d = y + z
e = y / z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report an operator used in prefix position with one argument" <|
            \() ->
                """
module A exposing (..)
a = (++) z
b = (::) z
c = (//) z
d = (+) z
e = (/) z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace (++) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (++) y z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y ++ z
"""
                        ]
        , test "should replace (::) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (::) y z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(::)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y :: z
"""
                        ]
        , test "should replace (//) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (//) y z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(//)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y // z
"""
                        ]
        , test "should replace (+) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (+) y z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(+)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y + z
"""
                        ]
        , test "should replace (/) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (/) y z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(/)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y / z
"""
                        ]
        , test "should replace infix operator with 2 arguments, used on several lines" <|
            \() ->
                """module A exposing (..)
a =
    (++)
        y
        z
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    y
        ++ z
"""
                        ]
        , test "should replace infix operator with 2 arguments wrapped in parens and braces" <|
            \() ->
                """module A exposing (..)
a =
    (++) (y + 1)
        [z]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    (y + 1)
        ++ [z]
"""
                        ]
        ]



-- LIST


listSimplificationTests : Test
listSimplificationTests =
    describe "List"
        [ usingPlusPlusTests
        , usingConsTests
        , usingListConcatTests
        , listConcatMapTests
        , listMapTests
        , listFilterTests
        , listFilterMapTests
        , listIsEmptyTests
        , listAllTests
        , listAnyTests
        , listRangeTests
        , listLengthTests
        ]


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
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
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
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
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
        , test "should replace [b] ++ c by b :: c" <|
            \() ->
                """module A exposing (..)
a = [ b ] ++ c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Should use (::) instead of (++)"
                            , details = [ "Concatenating a list with a single value is the same as using (::) on the list with the value." ]
                            , under = "[ b ] ++ c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( b ) :: c
"""
                        ]
        ]


usingConsTests : Test
usingConsTests =
    describe "Using (::)"
        [ test "should not report using :: to a variable or expression" <|
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
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1, 2, 3]
"""
                        ]
        , test "should report using :: to an empty list literal" <|
            \() ->
                """module A exposing (..)
a = 1 :: []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1 ]
"""
                        ]
        ]


usingListConcatTests : Test
usingListConcatTests =
    describe "Using List.concat"
        [ test "should not report List.concat that contains a variable or expression" <|
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
                            { message = "Using List.concat on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.concat"
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
                            { message = "Unnecessary use of List.concat on a list with 1 element"
                            , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                            , under = "List.concat [ b ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat with a single item, using (<|)" <|
            \() ->
                """module A exposing (..)
a = List.concat <| [ b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.concat on a list with 1 element"
                            , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                            , under = "List.concat <| [ b ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat with a single item, using (|>)" <|
            \() ->
                """module A exposing (..)
a = [ b ] |> List.concat
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.concat on a list with 1 element"
                            , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                            , under = "[ b ] |> List.concat"
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
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  [  1, 2, 3 ,  4, 5, 6 ]
"""
                        ]
        , test "should concatenate consecutive list literals in passed to List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concat [ a, [ 0 ], b, [ 1, 2, 3 ], [ 4, 5, 6], [7], c, [8], [9 ] ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Consecutive literal lists should be merged"
                            , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ a, [ 0 ], b, [ 1, 2, 3 ,  4, 5, 6, 7], c, [8, 9 ] ]
"""
                        ]
        ]


listConcatMapTests : Test
listConcatMapTests =
    describe "Using List.concatMap"
        [ test "should replace List.concatMap identity x by List.concat x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap identity x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap with an identity function is the same as using List.concat"
                            , details = [ "You can replace this call by List.concat" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat x
"""
                        ]
        , test "should replace List.concatMap identity by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap with an identity function is the same as using List.concat"
                            , details = [ "You can replace this call by List.concat" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat
"""
                        ]
        , test "should replace List.concatMap (\\x->x) by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\x->x) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap with an identity function is the same as using List.concat"
                            , details = [ "You can replace this call by List.concat" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat x
"""
                        ]
        , test "should not report List.concatMap with a non-identity lambda" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\x->y) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report List.concatMap without an identity function by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report List.concatMap with no items" <|
            \() ->
                """module A exposing (..)
a = List.concatMap fn []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.concatMap (always []) x by []" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (always []) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap will result in on an empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.concatMap (always []) by always []" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (always [])
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap will result in on an empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always [])
"""
                        ]
        , test "should replace List.concatMap fn [ a ] by fn a" <|
            \() ->
                """module A exposing (..)
a = List.concatMap fn [ a ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  fn (a)
"""
                        ]
        , test "should replace List.concatMap fn <| [ b c ] by fn <| (b c)" <|
            \() ->
                """module A exposing (..)
a = List.concatMap fn <| [ b c ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  fn <| (b c)
"""
                        ]
        , test "should replace List.concatMap fn <| [ b c ] by (b c) |> fn" <|
            \() ->
                """module A exposing (..)
a = [ b c ] |> List.concatMap fn
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b c) |>  fn
"""
                        ]
        ]


listMapTests : Test
listMapTests =
    describe "Using List.map"
        [ test "should not report List.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.map fn x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace List.map f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map fn []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map fn <| []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.map f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.map f
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = List.map identity x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.map identity <| x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.map identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = List.map identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = List.map <| identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> List.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> List.map
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.map with an identity function is the same as not using List.map"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


listFilterTests : Test
listFilterTests =
    describe "Using List.filter"
        [ test "should not report List.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.filter fn x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace List.filter f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filter fn []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filter fn <| []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.filter fn by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.filter fn
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always True) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filter (always True) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filter (\\x -> True) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filter (\\x -> True) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filter (always True)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filter <| (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filter <| (always True)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace always True |> List.filter by identity" <|
            \() ->
                """module A exposing (..)
a = always True |> List.filter
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return True is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filter (always False) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (\\x -> False) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (\\x -> False) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always False) <| x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False) <| x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace x |> List.filter (always False) by []" <|
            \() ->
                """module A exposing (..)
a = x |> List.filter (always False)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always False) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always [])
"""
                        ]
        , test "should replace List.filter <| (always False) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filter <| (always False)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always [])
"""
                        ]
        , test "should replace always False |> List.filter by always []" <|
            \() ->
                """module A exposing (..)
a = always False |> List.filter
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filter with a function that will always return False will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always [])
"""
                        ]
        ]


listFilterMapTests : Test
listFilterMapTests =
    describe "Using List.filterMap"
        [ test "should not report List.filterMap used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.filterMap fn x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace List.filterMap f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap fn []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap fn <| []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.filterMap fn by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.filterMap fn
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap on an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) <| x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing) <| x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace x |> List.filterMap (always Nothing) by []" <|
            \() ->
                """module A exposing (..)
a = x |> List.filterMap (always Nothing)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always [])
"""
                        ]
        , test "should replace List.filterMap <| always Nothing by always []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap <| always Nothing
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always [])
"""
                        ]
        , test "should replace always Nothing |> List.filterMap by always []" <|
            \() ->
                """module A exposing (..)
a = always Nothing |> List.filterMap
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always [])
"""
                        ]
        , test "should replace List.filterMap Just x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap Just <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just <| x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.filterMap Just by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.filterMap Just
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap Just by identity" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filterMap (\\a -> Nothing) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Nothing) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (\\a -> Just a) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just a) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap (\\a -> Just a) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just a)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filter"
                            , details = [ "You can remove this call and replace it by the list itself" ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should not report List.filterMap (\\a -> Just b) x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just b) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report List.filterMap (\\a b -> Just a) x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a b -> Just a) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


listIsEmptyTests : Test
listIsEmptyTests =
    describe "Using List.isEmpty"
        [ test "should not report List.isEmpty with a non-literal argument" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace List.isEmpty [] by True" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.isEmpty [x] by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty [x]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (x :: xs) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (x :: xs)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace x :: xs |> List.isEmpty by False" <|
            \() ->
                """module A exposing (..)
a = x :: xs |> List.isEmpty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


listAllTests : Test
listAllTests =
    describe "Using List.all"
        [ test "should not report List.all used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.all fn list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace List.all fn [] by True" <|
            \() ->
                """module A exposing (..)
a = List.all fn []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.all will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.all (always True) x by True" <|
            \() ->
                """module A exposing (..)
a = List.all (always True) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.all will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.all (always True) by always True" <|
            \() ->
                """module A exposing (..)
a = List.all (always True)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.all will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always True)
"""
                        ]
        ]


listAnyTests : Test
listAnyTests =
    describe "Using List.any"
        [ test "should not report List.any used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.any fn list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace List.any fn [] by False" <|
            \() ->
                """module A exposing (..)
a = List.any fn []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.any will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.any (always False) x by False" <|
            \() ->
                """module A exposing (..)
a = List.any (always False) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.any will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.any (always False) by always False" <|
            \() ->
                """module A exposing (..)
a = List.any (always False)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.any will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always False)
"""
                        ]
        ]


listRangeTests : Test
listRangeTests =
    describe "Using List.range"
        [ test "should not report List.range used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.range
a = List.range 5
a = List.range 5 10
a = List.range 5 0xF
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace List.range 10 5 by []" <|
            \() ->
                """module A exposing (..)
a = List.range 10 5
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.range will result in []"
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty slist." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.range 0xF 5 by []" <|
            \() ->
                """module A exposing (..)
a = List.range 0xF 5
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.range will result in []"
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty slist." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace 5 |> List.range 10 by []" <|
            \() ->
                """module A exposing (..)
a = 5 |> List.range 10
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to List.range will result in []"
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty slist." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listLengthTests : Test
listLengthTests =
    describe "Using List.length"
        [ test "should not report List.length used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.length
a = List.length b
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace List.length [] by 0" <|
            \() ->
                """module A exposing (..)
a = List.length []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 0"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace [] |> List.length by 0" <|
            \() ->
                """module A exposing (..)
a = [] |> List.length
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 0"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        ]
