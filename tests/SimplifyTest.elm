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
        , caseOfTests
        , booleanCaseOfTests
        , ifTests
        , numberTests
        , fullyAppliedPrefixOperatorTests
        , appliedLambdaTests
        , usingPlusPlusTests
        , stringSimplificationTests
        , listSimplificationTests
        , setSimplificationTests
        , cmdTests
        , subTests
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
        , test "should replace f >> identity by f" <|
            \() ->
                """module A exposing (..)
a = f >> identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace identity >> f by f" <|
            \() ->
                """module A exposing (..)
a = identity >> f
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace f << identity by f" <|
            \() ->
                """module A exposing (..)
a = f << identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace identity << f by f" <|
            \() ->
                """module A exposing (..)
a = identity << f
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
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
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
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
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
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
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace f >> always g by always g" <|
            \() ->
                """module A exposing (..)
a = f >> always g
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            , under = "always g"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always g
"""
                        ]
        , test "should replace always g << f by always g" <|
            \() ->
                """module A exposing (..)
a = always g << f
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            , under = "always g"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always g
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
        , equalTests
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
        , test "should simplify not >> not to identity" <|
            \() ->
                """module A exposing (..)
a = not >> not
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not >> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify a >> not >> not to a >> identity" <|
            \() ->
                """module A exposing (..)
a = a >> not >> not
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not >> not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a >> identity
"""
                        ]
        , test "should simplify not >> not >> a to identity >> a" <|
            \() ->
                """module A exposing (..)
a = not >> not >> a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not >> not >> "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify not << not to identity" <|
            \() ->
                """module A exposing (..)
a = not << not
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not << not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify not << not << a to identity << a" <|
            \() ->
                """module A exposing (..)
a = not << not << a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not << not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity << a
"""
                        ]
        , test "should simplify a << not << not to a" <|
            \() ->
                """module A exposing (..)
a = a << not << not
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = " << not << not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify (not >> a) << not to a" <|
            \() ->
                """module A exposing (..)
a = (not >> a) << not
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `not` with `not` cancel each other out." ]
                            , under = "not >> a) << not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (a)
"""
                        ]
        , test "should not simplify (not << a) << not" <|
            \() ->
                """module A exposing (..)
a = (not << a) << not
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]



-- CASE OF


caseOfTests : Test
caseOfTests =
    describe "Case of"
        [ test "should not report case of when the body of the branches are different" <|
            \() ->
                """module A exposing (..)
a = case value of
      A -> 1
      B -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace case of with a single wildcard case by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      _ -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace case of with a single case by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      A -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace case of with a single case with ignored arguments by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B C) -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not replace case of where a pattern introduces a variable" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B c) -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace case of with multiple cases that have the same body" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B C) -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace boolean case of with the same body by that body" <|
            \() ->
                """module A exposing (..)
a = case value of
      True -> x
      False -> x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]


booleanCaseOfMessage : String
booleanCaseOfMessage =
    "Replace `case..of` by an `if` condition"


booleanCaseOfDetails : List String
booleanCaseOfDetails =
    [ "The idiomatic way to check for a condition is to use an `if` expression."
    , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
    ]


booleanCaseOfTests : Test
booleanCaseOfTests =
    describe "Boolean case of"
        [ test "should not report pattern matches for non-boolean values" <|
            \() ->
                """module A exposing (..)
a = case thing of
      Thing -> 1
      Bar -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not report pattern matches when the evaluated expression is a tuple of with a boolean" <|
            \() ->
                """module A exposing (..)
a = case ( bool1, bool2 ) of
      ( True, True ) -> 1
      _ -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should report pattern matches when one of the patterns is a bool constructor (True and False)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      True -> 1
      False -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (on multiple lines)" <|
            \() ->
                """module A exposing (..)
a =
    case bool of
        True ->
            1
        False ->
            2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    if bool then 1
        else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (False and True)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      False -> 1
      True -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if not (bool) then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (True and wildcard)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      True -> 1
      _ -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (False and wildcard)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      False -> 1
      _ -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if not (bool) then 1
      else 2
"""
                        ]
        , test "should report pattern matches for booleans even when one of the patterns starts with `Basics.`" <|
            \() ->
                """module A exposing (..)
a = case bool of
      Basics.True -> 1
      _ -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "Basics.True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches for booleans even when the constructor seems to be for booleans but comes from an unknown module" <|
            \() ->
                """module A exposing (..)
a = case bool of
      OtherModule.True -> 1
      _ -> 2

b = case bool of
      OtherModule.False -> 1
      _ -> 2
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]



-- NUMBER


numberTests : Test
numberTests =
    describe "Number tests"
        [ plusTests
        , minusTests
        , multiplyTests
        , divisionTests
        , negationTest
        , basicsNegateTests
        ]


plusTests : Test
plusTests =
    describe "(+)"
        [ test "should not simplify (+) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b + 1
b = 2 + 3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify n + 0 to n" <|
            \() ->
                """module A exposing (..)
a = n + 0
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary addition with 0"
                            , details = [ "Adding 0 does not change the value of the number." ]
                            , under = " + 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n + 0.0 to n" <|
            \() ->
                """module A exposing (..)
a = n + 0.0
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary addition with 0"
                            , details = [ "Adding 0 does not change the value of the number." ]
                            , under = " + 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 + n to n" <|
            \() ->
                """module A exposing (..)
a = 0 + n
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary addition with 0"
                            , details = [ "Adding 0 does not change the value of the number." ]
                            , under = "0 + "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        ]


minusTests : Test
minusTests =
    describe "(-)"
        [ test "should not simplify (-) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b - 1
b = 2 - 3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify n - 0 to n" <|
            \() ->
                """module A exposing (..)
a = n - 0
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtraction with 0"
                            , details = [ "Subtracting 0 does not change the value of the number." ]
                            , under = " - 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n - 0.0 to n" <|
            \() ->
                """module A exposing (..)
a = n - 0.0
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtraction with 0"
                            , details = [ "Subtracting 0 does not change the value of the number." ]
                            , under = " - 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 - n to -n" <|
            \() ->
                """module A exposing (..)
a = 0 - n
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtracting from 0"
                            , details = [ "You can negate the expression on the right like `-n`." ]
                            , under = "0 - "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -n
"""
                        ]
        ]


multiplyTests : Test
multiplyTests =
    describe "(*)"
        [ test "should not simplify (*) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b * 2
b = 2 * 3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify n * 1 to n" <|
            \() ->
                """module A exposing (..)
a = n * 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary multiplication by 1"
                            , details = [ "Multiplying by 1 does not change the value of the number." ]
                            , under = " * 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n * 1.0 to n" <|
            \() ->
                """module A exposing (..)
a = n * 1.0
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary multiplication by 1"
                            , details = [ "Multiplying by 1 does not change the value of the number." ]
                            , under = " * 1.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 1 * n to n" <|
            \() ->
                """module A exposing (..)
a = 1 * n
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary multiplication by 1"
                            , details = [ "Multiplying by 1 does not change the value of the number." ]
                            , under = "1 * "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n * 0 to 0" <|
            \() ->
                """module A exposing (..)
a = n * 0
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplying by 0 equals 0"
                            , details = [ "You can replace this value by 0." ]
                            , under = " * 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify n * 0.0 to 0" <|
            \() ->
                """module A exposing (..)
a = n * 0.0
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplying by 0 equals 0"
                            , details = [ "You can replace this value by 0." ]
                            , under = " * 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify 0 * n to 0" <|
            \() ->
                """module A exposing (..)
a = 0 * n
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplying by 0 equals 0"
                            , details = [ "You can replace this value by 0." ]
                            , under = "0 * "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        ]


divisionTests : Test
divisionTests =
    describe "(/)"
        [ test "should not simplify (/) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = 1 / 2
b = 2 / 3
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify n / 1 to n" <|
            \() ->
                """module A exposing (..)
a = n / 1
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary division by 1"
                            , details = [ "Dividing by 1 does not change the value of the number." ]
                            , under = " / 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n / 1.0 to n" <|
            \() ->
                """module A exposing (..)
a = n / 1.0
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary division by 1"
                            , details = [ "Dividing by 1 does not change the value of the number." ]
                            , under = " / 1.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        ]


negationTest : Test
negationTest =
    describe "Unary negation"
        [ test "should not report negation used in okay situations" <|
            \() ->
                """module A exposing (..)
a = -1
a = -(-1 + 2)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify -(-n) to n" <|
            \() ->
                """module A exposing (..)
a = -(-n)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            , under = "-(-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (n)
"""
                        ]
        ]


basicsNegateTests : Test
basicsNegateTests =
    describe "Basics.negate"
        [ test "should simplify negate >> negate to identity" <|
            \() ->
                """module A exposing (..)
a = negate >> negate
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate >> negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify a >> negate >> negate to a >> identity" <|
            \() ->
                """module A exposing (..)
a = a >> negate >> negate
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate >> negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a >> identity
"""
                        ]
        , test "should simplify negate >> negate >> a to identity >> a" <|
            \() ->
                """module A exposing (..)
a = negate >> negate >> a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate >> negate >> "
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify negate << negate to identity" <|
            \() ->
                """module A exposing (..)
a = negate << negate
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate << negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify negate << negate << a to identity << a" <|
            \() ->
                """module A exposing (..)
a = negate << negate << a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate << negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity << a
"""
                        ]
        , test "should simplify a << negate << negate to a" <|
            \() ->
                """module A exposing (..)
a = a << negate << negate
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = " << negate << negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify (negate >> a) << negate to a" <|
            \() ->
                """module A exposing (..)
a = (negate >> a) << negate
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double negation"
                            , details = [ "Composing `negate` with `negate` cancel each other out." ]
                            , under = "negate >> a) << negate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (a)
"""
                        ]
        , test "should negate simplify (negate << a) << negate" <|
            \() ->
                """module A exposing (..)
a = (negate << a) << negate
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


equalTests : Test
equalTests =
    describe "(==)"
        [ test "should not simplify values that can't be determined" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify x == True to x" <|
            \() ->
                """module A exposing (..)
a = x == True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "x == True"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify x == False" <|
            \() ->
                """module A exposing (..)
a = x == False
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify True == x to x" <|
            \() ->
                """module A exposing (..)
a = True == x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "True == x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify False == x" <|
            \() ->
                """module A exposing (..)
a = False == x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should not simplify x /= True" <|
            \() ->
                """module A exposing (..)
a = x /= True
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify x /= False to x" <|
            \() ->
                """module A exposing (..)
a = x /= False
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "x /= False"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify True /= x" <|
            \() ->
                """module A exposing (..)
a = True /= x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should simplify False /= x to x" <|
            \() ->
                """module A exposing (..)
a = False /= x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "False /= x"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify not x == not y to x == y" <|
            \() ->
                """module A exposing (..)
a = not x == not y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary negation on both sides"
                            , details = [ "Since both sides are negated using `not`, they are redundant and can be removed." ]
                            , under = "not x == not y"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =  x ==  y
"""
                        ]
        , test "should simplify not x /= not y to x /= y" <|
            \() ->
                """module A exposing (..)
a = not x /= not y
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary negation on both sides"
                            , details = [ "Since both sides are negated using `not`, they are redundant and can be removed." ]
                            , under = "not x /= not y"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a =  x /=  y
"""
                        ]
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


appliedLambdaTests : Test
appliedLambdaTests =
    describe "Applied lambda functions"
        [ test "should not report a okay function/lambda calls" <|
            \() ->
                """
module A exposing (..)
a = fn ()
b = (\\x y -> x + y) n
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace (\\() -> x) () by x" <|
            \() ->
                """module A exposing (..)
a = (\\() -> x) ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a = (x)$
""" |> String.replace "$" " ")
                        ]
        , test "should replace (\\_ -> x) a by x" <|
            \() ->
                """module A exposing (..)
a = (\\_ -> x) a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary wildcard argument argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a = (x)$
""" |> String.replace "$" " ")
                        ]
        , test "should replace (\\() y -> x) () by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\() y -> x) ()
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a = (\\y -> x)$
""" |> String.replace "$" " ")
                        ]
        , test "should replace (\\_ y -> x) a by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\_ y -> x) a
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary wildcard argument argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed ("""module A exposing (..)
a = (\\y -> x)$
""" |> String.replace "$" " ")
                        ]
        ]



-- (++)


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
        , test "should not report simple strings" <|
            \() ->
                """module A exposing (..)
a = "abc" ++ value
b = \"\"\"123\"\"\"
c = \"\"\"multi
line
string
\"\"\"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test """should replace "a" ++ "" by "a\"""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary concatenation with an empty string"
                            , details = [ "You should remove the concatenation with the empty string." ]
                            , under = "\"\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a"
"""
                        ]
        , test """should replace "" ++ "a" by "a\"""" <|
            \() ->
                """module A exposing (..)
a = "" ++ "a"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary concatenation with an empty string"
                            , details = [ "You should remove the concatenation with the empty string." ]
                            , under = "\"\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a"
"""
                        ]
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



-- STRING


stringSimplificationTests : Test
stringSimplificationTests =
    describe "String"
        [ stringIsEmptyTests
        , concatTests
        , joinTests
        , stringRepeatTests
        , stringWordsTests
        , stringLinesTests
        ]


stringIsEmptyTests : Test
stringIsEmptyTests =
    describe "String.isEmpty"
        [ test "should not report String.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty
b = String.isEmpty value
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace String.isEmpty \"\" by True" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to String.isEmpty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.isEmpty \"a\" by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty "a"
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The call to String.isEmpty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


concatTests : Test
concatTests =
    describe "String.concat"
        [ test "should not report String.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.concat list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test """should replace String.concat [] by \"\"""" <|
            \() ->
                """module A exposing (..)
a = String.concat []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.concat on an empty list will result in a empty string"
                            , details = [ "You can replace this call by an empty string" ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        ]


joinTests : Test
joinTests =
    describe "String.join"
        [ test "should not report String.join that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.join b c
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test """should replace String.join b [] by \"\"""" <|
            \() ->
                """module A exposing (..)
a = String.join b []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.join on an empty list will result in a empty string"
                            , details = [ "You can replace this call by an empty string" ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.join "" list by String.concat list""" <|
            \() ->
                """module A exposing (..)
a = String.join "" list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use String.concat instead"
                            , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat list
"""
                        ]
        , test """should replace String.join "" by String.concat""" <|
            \() ->
                """module A exposing (..)
a = String.join ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use String.concat instead"
                            , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat
"""
                        ]
        , test """should replace list |> String.join "" by list |> String.concat""" <|
            \() ->
                """module A exposing (..)
a = list |> String.join ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use String.concat instead"
                            , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> String.concat
"""
                        ]
        ]


stringRepeatTests : Test
stringRepeatTests =
    describe "String.repeat"
        [ test "should not report String.repeat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.repeat n str
b = String.repeat 5 str
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test """should replace String.repeat n "" by \"\"""" <|
            \() ->
                """module A exposing (..)
a = String.repeat n ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.repeat with an empty string will result in a empty string"
                            , details = [ "You can replace this call by an empty string" ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.repeat 0 str by \"\"""" <|
            \() ->
                """module A exposing (..)
a = String.repeat 0 str
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat will result in an empty string"
                            , details = [ "Using String.repeat with a number less than 1 will result in an empty string. You can replace this call by an empty string." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.repeat -5 str by \"\"""" <|
            \() ->
                """module A exposing (..)
a = String.repeat -5 str
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat will result in an empty string"
                            , details = [ "Using String.repeat with a number less than 1 will result in an empty string. You can replace this call by an empty string." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.repeat 1 str by str" <|
            \() ->
                """module A exposing (..)
a = String.repeat 1 str
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat 1 won't do anything"
                            , details = [ "Using String.repeat with 1 will result in the second argument." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =  str
"""
                        ]
        ]


stringWordsTests : Test
stringWordsTests =
    describe "String.words"
        [ test "should not report String.words that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.words
b = String.words str
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test """should replace String.words "" by []""" <|
            \() ->
                """module A exposing (..)
a = String.words ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.words on an empty string will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "String.words"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


stringLinesTests : Test
stringLinesTests =
    describe "String.lines"
        [ test "should not report String.lines that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.lines
b = String.lines str
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test """should replace String.lines "" by []""" <|
            \() ->
                """module A exposing (..)
a = String.lines ""
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using String.lines on an empty string will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "String.lines"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]



-- LIST


listSimplificationTests : Test
listSimplificationTests =
    describe "List"
        [ usingConsTests
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
        , listRepeatTests
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
                            , details = [ "You can remove this call and replace it by the list itself." ]
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
                            , details = [ "You can remove this call and replace it by the list itself." ]
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
                            , details = [ "You can remove this call and replace it by the list itself." ]
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
                            , details = [ "You can remove this call and replace it by the list itself." ]
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
                            , details = [ "You can remove this call and replace it by the list itself." ]
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
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
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
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
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
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
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
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
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
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
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
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
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
                            { message = "Using List.filter with a function that will always return False will result in []"
                            , details = [ "You can remove this call and replace it by []." ]
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
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty list." ]
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
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty list." ]
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
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty list." ]
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
        , test "should replace List.length [b, c, d] by 3" <|
            \() ->
                """module A exposing (..)
a = List.length [b, c, d]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 3"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
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


listRepeatTests : Test
listRepeatTests =
    describe "List.repeat"
        [ test "should not report List.repeat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.repeat n list
b = List.repeat 5 list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test """should replace List.repeat n [] by []""" <|
            \() ->
                """module A exposing (..)
a = List.repeat n []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.repeat with an empty list will result in a empty list"
                            , details = [ "You can replace this call by an empty list" ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test """should replace List.repeat 0 list by []""" <|
            \() ->
                """module A exposing (..)
a = List.repeat 0 list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat will result in an empty list"
                            , details = [ "Using List.repeat with a number less than 1 will result in an empty list. You can replace this call by an empty list." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test """should replace List.repeat -5 list by []""" <|
            \() ->
                """module A exposing (..)
a = List.repeat -5 list
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat will result in an empty list"
                            , details = [ "Using List.repeat with a number less than 1 will result in an empty list. You can replace this call by an empty list." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should not replace List.repeat 1" <|
            \() ->
                """module A exposing (..)
a = List.repeat 1 x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]



-- Set


setSimplificationTests : Test
setSimplificationTests =
    describe "Set"
        [ setMapTests
        , setFilterTests

        --, setIsEmptyTests
        --, setSizeTests
        ]


setMapTests : Test
setMapTests =
    describe "Set.map"
        [ test "should not report Set.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.map fn x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace Set.map f Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.map fn Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.map f <| Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.map fn <| Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.empty |> Set.map f by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.map f
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = Set.map identity x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Set.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = Set.map identity <| x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> Set.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> Set.map identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Set.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = Set.map identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Set.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = Set.map <| identity
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> Set.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> Set.map
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.map with an identity function is the same as not using Set.map"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


setFilterTests : Test
setFilterTests =
    describe "Using Set.filter"
        [ test "should not report Set.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Set.filter fn x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace Set.filter f Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter fn Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter f <| Set.empty by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter fn <| Set.empty
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.empty |> Set.filter fn by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.empty |> Set.filter fn
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter on Set.empty will result in Set.empty"
                            , details = [ "You can replace this call by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (always True) x by x" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always True) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Set.filter (\\x -> True) x by x" <|
            \() ->
                """module A exposing (..)
a = Set.filter (\\x -> True) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace Set.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always True)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Set.filter <| (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = Set.filter <| (always True)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace always True |> Set.filter by identity" <|
            \() ->
                """module A exposing (..)
a = always True |> Set.filter
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return True is the same as not using Set.filter"
                            , details = [ "You can remove this call and replace it by the set itself." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Set.filter (always False) x by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always False) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (\\x -> False) x by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter (\\x -> False) x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (always False) <| x by Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always False) <| x
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace x |> Set.filter (always False) by Set.empty" <|
            \() ->
                """module A exposing (..)
a = x |> Set.filter (always False)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.empty
"""
                        ]
        , test "should replace Set.filter (always False) by always Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter (always False)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always Set.empty)
"""
                        ]
        , test "should replace Set.filter <| (always False) by always Set.empty" <|
            \() ->
                """module A exposing (..)
a = Set.filter <| (always False)
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always Set.empty)
"""
                        ]
        , test "should replace always False |> Set.filter by always Set.empty" <|
            \() ->
                """module A exposing (..)
a = always False |> Set.filter
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Set.filter with a function that will always return False will result in Set.empty"
                            , details = [ "You can remove this call and replace it by Set.empty." ]
                            , under = "Set.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (always Set.empty)
"""
                        ]
        ]



-- Cmd


cmdTests : Test
cmdTests =
    describe "Cmd.batch"
        [ test "should not report Cmd.batch used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch
a = Cmd.batch b
a = Cmd.batch [ b, x ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace Cmd.batch [] by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Replace by Cmd.batch"
                            , details = [ "Cmd.batch [] and Cmd.none are equivalent but the latter is more idiomatic in Elm code" ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.none
"""
                        ]
        , test "should replace Cmd.batch [ a, Cmd.none, b ] by Cmd.batch [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ a, Cmd.none, b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.none"
                            , details = [ "Cmd.none will be ignored by Cmd.batch." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ a, b ]
"""
                        ]
        , test "should replace Cmd.batch [ Cmd.none ] by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ Cmd.none ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.batch"
                            , details = [ "Cmd.batch with a single element is equal to that element." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Cmd.none)
"""
                        ]
        , test "should replace Cmd.batch [ b ] by b" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.batch"
                            , details = [ "Cmd.batch with a single element is equal to that element." ]
                            , under = "Cmd.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b)
"""
                        ]
        , test "should replace Cmd.batch [ b, Cmd.none ] by Cmd.batch []" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ b, Cmd.none ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.none"
                            , details = [ "Cmd.none will be ignored by Cmd.batch." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ b ]
"""
                        ]
        , test "should replace Cmd.batch [ Cmd.none, b ] by Cmd.batch [ b ]" <|
            \() ->
                """module A exposing (..)
a = Cmd.batch [ Cmd.none, b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Cmd.none"
                            , details = [ "Cmd.none will be ignored by Cmd.batch." ]
                            , under = "Cmd.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.batch [ b ]
"""
                        ]
        , test "should replace Cmd.map identity cmd by cmd" <|
            \() ->
                """module A exposing (..)
a = Cmd.map identity cmd
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Cmd.map with an identity function is the same as not using Cmd.map"
                            , details = [ "You can remove this call and replace it by the command itself." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = cmd
"""
                        ]
        , test "should replace Cmd.map fn Cmd.none by Cmd.none" <|
            \() ->
                """module A exposing (..)
a = Cmd.map fn Cmd.none
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Cmd.map on Cmd.none will result in Cmd.none"
                            , details = [ "You can replace this call by Cmd.none." ]
                            , under = "Cmd.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Cmd.none
"""
                        ]
        ]



-- Sub


subTests : Test
subTests =
    describe "Sub.batch"
        [ test "should not report Sub.batch used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Sub.batch
a = Sub.batch b
a = Sub.batch [ b, x ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        , test "should replace Sub.batch [] by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.batch []
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Replace by Sub.batch"
                            , details = [ "Sub.batch [] and Sub.none are equivalent but the latter is more idiomatic in Elm code" ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.none
"""
                        ]
        , test "should replace Sub.batch [ a, Sub.none, b ] by Sub.batch [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ a, Sub.none, b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.none"
                            , details = [ "Sub.none will be ignored by Sub.batch." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ a, b ]
"""
                        ]
        , test "should replace Sub.batch [ Sub.none ] by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ Sub.none ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.batch"
                            , details = [ "Sub.batch with a single element is equal to that element." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Sub.none)
"""
                        ]
        , test "should replace Sub.batch [ b ] by b" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.batch"
                            , details = [ "Sub.batch with a single element is equal to that element." ]
                            , under = "Sub.batch"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b)
"""
                        ]
        , test "should replace Sub.batch [ b, Sub.none ] by Sub.batch []" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ b, Sub.none ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.none"
                            , details = [ "Sub.none will be ignored by Sub.batch." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ b ]
"""
                        ]
        , test "should replace Sub.batch [ Sub.none, b ] by Sub.batch [ b ]" <|
            \() ->
                """module A exposing (..)
a = Sub.batch [ Sub.none, b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Sub.none"
                            , details = [ "Sub.none will be ignored by Sub.batch." ]
                            , under = "Sub.none"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.batch [ b ]
"""
                        ]
        , test "should replace Sub.map identity sub by sub" <|
            \() ->
                """module A exposing (..)
a = Sub.map identity sub
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Sub.map with an identity function is the same as not using Sub.map"
                            , details = [ "You can remove this call and replace it by the subscription itself." ]
                            , under = "Sub.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = sub
"""
                        ]
        , test "should replace Sub.map fn Sub.none by Sub.none" <|
            \() ->
                """module A exposing (..)
a = Sub.map fn Sub.none
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using Sub.map on Sub.none will result in Sub.none"
                            , details = [ "You can replace this call by Sub.none." ]
                            , under = "Sub.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Sub.none
"""
                        ]
        ]
