module NoFullyAppliedPrefixOperatorTest exposing (all)

import NoFullyAppliedPrefixOperator exposing (rule)
import Review.Test exposing (ReviewResult)
import Test exposing (Test, describe, test)


message : String
message =
    "TODO"


details : List String
details =
    [ "TODO"
    ]


all : Test
all =
    describe "NoFullyAppliedPrefixOperator"
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
        , test "should report an operator used in prefix position with both arguments" <|
            \() ->
                """
module A exposing (..)
a = (++) y z
b = (::) y z
c = (//) y z
d = (+) y z
e = (/) y z
"""
                    |> Review.Test.run rule
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
