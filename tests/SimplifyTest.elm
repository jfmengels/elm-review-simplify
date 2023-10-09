module SimplifyTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Simplify"
        [ identityTests
        , alwaysTests
        , toFloatTests
        , roundTests
        , ceilingTests
        , floorTests
        , truncateTests
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
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace identity x by x" <|
            \() ->
                """module A exposing (..)
a = identity x
"""
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report always with 1 argument" <|
            \() ->
                """module A exposing (..)
a = always x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace always x y by x" <|
            \() ->
                """module A exposing (..)
a = always x y
"""
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
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
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            , under = "always"
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
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always g
"""
                        ]
        ]


toFloatTests : Test
toFloatTests =
    describe "Basics.toFloat"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = toFloat
b = toFloat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify toFloat 1 to 1" <|
            \() ->
                """module A exposing (..)
a = toFloat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify toFloat -1 to -1" <|
            \() ->
                """module A exposing (..)
a = toFloat -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify toFloat 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = toFloat 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should not report toFloat 1.2" <|
            \() ->
                """module A exposing (..)
a = toFloat 1.2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


roundTests : Test
roundTests =
    describe "Basics.round"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = round
b = round n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify round 1 to 1" <|
            \() ->
                """module A exposing (..)
a = round 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify round -1 to -1" <|
            \() ->
                """module A exposing (..)
a = round -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify round 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = round 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify round <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = round <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.round cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify round << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = round << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.round cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


ceilingTests : Test
ceilingTests =
    describe "Basics.ceiling"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = ceiling
b = ceiling n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify ceiling 1 to 1" <|
            \() ->
                """module A exposing (..)
a = ceiling 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify ceiling -1 to -1" <|
            \() ->
                """module A exposing (..)
a = ceiling -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify ceiling 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = ceiling 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify ceiling <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = ceiling <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.ceiling cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify ceiling << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = ceiling << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.ceiling cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


floorTests : Test
floorTests =
    describe "Basics.floor"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = floor
b = floor n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify floor 1 to 1" <|
            \() ->
                """module A exposing (..)
a = floor 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify floor -1 to -1" <|
            \() ->
                """module A exposing (..)
a = floor -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify floor 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = floor 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify floor <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = floor <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.floor cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify floor << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = floor << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.floor cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


truncateTests : Test
truncateTests =
    describe "Basics.truncate"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = truncate
b = truncate n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify truncate 1 to 1" <|
            \() ->
                """module A exposing (..)
a = truncate 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify truncate -1 to -1" <|
            \() ->
                """module A exposing (..)
a = truncate -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify truncate 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = truncate 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify truncate <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = truncate <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.truncate cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify truncate << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = truncate << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.truncate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]
