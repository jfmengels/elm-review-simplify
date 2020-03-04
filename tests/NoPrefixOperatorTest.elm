module NoPrefixOperatorTest exposing (all)

import NoPrefixOperator exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


testRule : String -> ReviewResult
testRule string =
    "module A exposing (..)\n\n"
        ++ string
        |> Review.Test.run rule


message : String
message =
    "TODO"


details : List String
details =
    [ "TODO"
    ]


tests : List Test
tests =
    [ test "should not report a lonely operator" <|
        \() ->
            testRule """
a = (++)
b = (::)
c = (//)
d = (+)
e = (/)
"""
                |> Review.Test.expectNoErrors
    , test "should not report an operator used in infix position" <|
        \() ->
            testRule """
a = y ++ z
b = y :: z
c = y // z
d = y + z
e = y / z
"""
                |> Review.Test.expectNoErrors
    , test "should not report an operator used in prefix position with one argument" <|
        \() ->
            testRule """
a = (++) z
b = (::) z
c = (//) z
d = (+) z
e = (/) z
"""
                |> Review.Test.expectNoErrors
    , test "should report an operator used in prefix position with both arguments" <|
        \() ->
            testRule """
a = (++) y z
b = (::) y z
c = (//) y z
d = (+) y z
e = (/) y z
"""
                |> Review.Test.expectErrors
                    [ Review.Test.error
                        { message = message
                        , details = details
                        , under = "(++)"
                        }
                    , Review.Test.error
                        { message = message
                        , details = details
                        , under = "(::)"
                        }
                    , Review.Test.error
                        { message = message
                        , details = details
                        , under = "(//)"
                        }
                    , Review.Test.error
                        { message = message
                        , details = details
                        , under = "(+)"
                        }
                    , Review.Test.error
                        { message = message
                        , details = details
                        , under = "(/)"
                        }
                    ]
    ]


all : Test
all =
    describe "NoPrefixOperator" tests
