module Simplify.DictTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Dict"
        [ dictIsEmptyTests
        , dictFromListTests
        , dictToListTests
        , dictSizeTests
        , dictMemberTests
        , dictRemoveTests
        , dictFilterTests
        , dictPartitionTests
        , dictMapTests
        , dictUnionTests
        , dictIntersectTests
        , dictDiffTests
        , dictFoldlTests
        , dictFoldrTests
        ]


dictIsEmptyTests : Test
dictIsEmptyTests =
    describe "Dict.isEmpty"
        [ test "should not report Dict.isEmpty with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty
b = Dict.isEmpty dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.isEmpty Dict.empty by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on Dict.empty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.fromList [x]) by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty (Dict.fromList [x])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on this dict will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.fromList []) by True" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty (Dict.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on Dict.empty will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = True
"""
                        , Review.Test.error
                            { message = "Dict.fromList on [] will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.isEmpty (Dict.empty)
"""
                        ]
        , test "should replace Dict.isEmpty (Dict.singleton x) by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.isEmpty (Dict.singleton x y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on this dict will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        , test "should replace x :: xs |> Dict.isEmpty by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.singleton x y |> Dict.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.isEmpty on this dict will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        ]


dictFromListTests : Test
dictFromListTests =
    describe "Dict.fromList"
        [ test "should not report Dict.fromList with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList
b = Dict.fromList list
c = Dict.fromList [x]
d = Dict.fromList [x, y]
e = Dict.fromList
f = Dict.fromList list
g = Dict.fromList << fun << Dict.toList
h = (Dict.fromList << fun) << Dict.toList
i = Dict.fromList << (fun << Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.fromList [] by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on [] will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace x |> f |> Dict.toList |> Dict.fromList by x |> f" <|
            \() ->
                """module A exposing (..)
import Dict
a = x |> f |> Dict.toList |> Dict.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can replace this call by the argument given to Dict.toList." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = x |> f
"""
                        ]
        , test "should replace Dict.fromList << Dict.toList by identity" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = identity
"""
                        ]
        , test "should replace Dict.fromList << (Dict.toList << f) by (f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList << (Dict.toList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f)
"""
                        ]
        , test "should replace Dict.fromList << (Dict.toList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList << (Dict.toList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (g << f)
"""
                        ]
        , test "should replace Dict.fromList << (f >> Dict.toList) by (f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList << (f >> Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f)
"""
                        ]
        , test "should replace (f << Dict.fromList) << Dict.toList by (f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = (f << Dict.fromList) << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f)
"""
                        ]
        , test "should replace (Dict.fromList >> f) << Dict.toList by (f)" <|
            \() ->
                """module A exposing (..)
import Dict
a = (Dict.fromList >> f) << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f)
"""
                        ]
        , test "should replace (Dict.fromList >> f >> g) << Dict.toList by (f >> g)" <|
            \() ->
                """module A exposing (..)
import Dict
a = (Dict.fromList >> f >> g) << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then Dict.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (f >> g)
"""
                        ]
        ]


dictToListTests : Test
dictToListTests =
    describe "Dict.toList"
        [ test "should not report Dict.toList with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList
b = Dict.toList dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.toList Dict.empty by []" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList on Dict.empty will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "Dict.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = []
"""
                        ]
        ]


dictSizeTests : Test
dictSizeTests =
    describe "Dict.size"
        [ test "should not report Dict.size used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size
a = Dict.size b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace Dict.size Dict.fromList with unknown keys because they could contain duplicate keys which would make the final dict size smaller" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [b, c, d])
a = Dict.size (Dict.fromList [(b, 'b'), (c,'c'), (d,'d')])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.size Dict.empty by 0" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 0"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 0
"""
                        ]
        , test "should replace Dict.empty |> Dict.size by 0" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 0"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 0
"""
                        ]
        , test "should replace Dict.singleton x y |> Dict.size by 1" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.singleton x y |> Dict.size
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 1"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 1
"""
                        ]
        , test "should replace Dict.size (Dict.fromList []) by 0" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.fromList on [] will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.size (Dict.empty)
"""
                        , Review.Test.error
                            { message = "The size of the dict is 0"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 0
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [a]) by 1" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [a])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 1"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 1
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [(1,1), (2,1), (3,1)]) by 3" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [(1,1), (2,1), (3,1)])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 3"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 3
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [(1,1), (2,1), (3,1), (3,2), (0x3,2)]) by 3" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [(1,1), (2,1), (3,1), (3,2), (0x3,2)])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 3"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 3
"""
                        ]
        , test "should replace Dict.size (Dict.fromList [(1.3,()), (-1.3,()), (2.1,()), (2.1,())]) by 3" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.size (Dict.fromList [(1.3,()), (-1.3,()), (2.1,()), (2.1,())])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The size of the dict is 3"
                            , details = [ "The size of the dict can be determined by looking at the code." ]
                            , under = "Dict.size"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = 3
"""
                        ]
        ]


dictMemberTests : Test
dictMemberTests =
    describe "Dict.member"
        [ test "should not report Dict.member used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.member x Dict.empty by False" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.member x Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.member on Dict.empty will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "Dict.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = False
"""
                        ]
        ]


dictRemoveTests : Test
dictRemoveTests =
    describe "Dict.remove"
        [ test "should not report Dict.remove used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.remove
a1 = Dict.remove k
a2 = Dict.remove k dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.remove k Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.remove k Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.remove on Dict.empty will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.remove"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        ]


dictFilterTests : Test
dictFilterTests =
    describe "Dict.filter"
        [ test "should not report Dict.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.filter
a1 = Dict.filter f
a2 = Dict.filter f dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.filter f Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter f Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter on Dict.empty will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.filter (always (always True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (always True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (always (\\_ -> True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (\\_ -> True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (\\_ -> (\\_ -> True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (\\_ -> (\\_ -> True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (\\_ -> (always True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (\\_ -> (always True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (\\_ _ -> True)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (\\_ _ -> True) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.filter (always (always True)) by identity" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (always True))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return True will always return the same given dict"
                            , details = [ "You can replace this call by identity." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = identity
"""
                        ]
        , test "should replace Dict.filter (always (always False)) dict by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (always False)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return False will always result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.filter (always (always False)) dict by always Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.filter (always (always False))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.filter with a function that will always return False will always result in Dict.empty"
                            , details = [ "You can replace this call by always Dict.empty." ]
                            , under = "Dict.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always Dict.empty
"""
                        ]
        ]


dictPartitionTests : Test
dictPartitionTests =
    describe "Dict.partition"
        [ test "should not report Dict.partition used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.partition
a1 = Dict.partition f
a2 = Dict.partition f dict
a3 = Dict.partition (\\_ _ -> bool)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.partition f Dict.empty by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition f Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition f <| Dict.empty by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition f <| Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.empty |> Dict.partition f by ( Dict.empty, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition on Dict.empty will result in ( Dict.empty, Dict.empty )"
                            , details = [ "You can replace this call by ( Dict.empty, Dict.empty )." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( Dict.empty, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (always (always True)) x by ( x, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (always True)) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( x, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (always (\\_ -> True)) dict by ( dict, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (\\_ -> True)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( dict, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (\\_ _ -> True) dict by ( dict, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (\\_ _ -> True) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( dict, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (\\_ -> \\_ -> True) dict by ( dict, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (\\_ -> \\_ -> True) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( dict, Dict.empty )
"""
                        ]
        , test "should replace Dict.partition (\\_ -> always True) dict by ( dict, Dict.empty )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (\\_ -> always True) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first dict"
                            , details = [ "Since the predicate function always returns True, the second dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( dict, Dict.empty )
"""
                        ]
        , test "should not replace Dict.partition (always True)" <|
            -- We'd likely need an anonymous function which could introduce naming conflicts
            -- Could be improved if we knew what names are available at this point in scope (or are used anywhere)
            -- so that we can generate a unique variable.
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (always True))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.partition (always (always False)) dict by ( Dict.empty, dict )" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (always False)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second dict"
                            , details = [ "Since the predicate function always returns False, the first dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = ( Dict.empty, dict )
"""
                        ]
        , test "should replace Dict.partition (always (always False)) by (Tuple.pair Dict.empty)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.partition (always (always False))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second dict"
                            , details = [ "Since the predicate function always returns False, the first dict will always be Dict.empty." ]
                            , under = "Dict.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Tuple.pair Dict.empty)
"""
                        ]
        ]


dictIntersectTests : Test
dictIntersectTests =
    describe "Dict.intersect"
        [ test "should not report Dict.intersect used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect
b = Dict.intersect x
c = Dict.intersect x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.intersect Dict.empty dict by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect Dict.empty dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect on Dict.empty will always result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.intersect Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect on Dict.empty will always result in Dict.empty"
                            , details = [ "You can replace this call by always Dict.empty." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always Dict.empty
"""
                        ]
        , test "should replace Dict.intersect dict Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect dict Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect on Dict.empty will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.intersect dict dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.intersect dict dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect where the first and second argument are equal will always return the same given last argument"
                            , details = [ "You can replace this call by the last argument itself." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace value.field |> Dict.intersect (.field value) by value.field" <|
            \() ->
                """module A exposing (..)
import Dict
a = value.field |> Dict.intersect (.field value)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.intersect where the first and second argument are equal will always return the same given last argument"
                            , details = [ "You can replace this call by the last argument itself." ]
                            , under = "Dict.intersect"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = value.field
"""
                        ]
        ]


dictDiffTests : Test
dictDiffTests =
    describe "Dict.diff"
        [ test "should not report Dict.diff used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.diff x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.diff Dict.empty dict by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.diff Dict.empty dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.diff Dict.empty will always result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.diff dict Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.diff dict Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.diff with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.empty |> Dict.diff dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.diff dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.diff with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.diff"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        ]


dictMapTests : Test
dictMapTests =
    describe "Dict.map"
        [ test "should not report Dict.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.map
a1 = Dict.map f
a2 = Dict.map f dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.map f Dict.empty by Dict.empty" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map f Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map on Dict.empty will result in Dict.empty"
                            , details = [ "You can replace this call by Dict.empty." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.empty
"""
                        ]
        , test "should replace Dict.map (always identity) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map (always identity) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map with a function that maps to the unchanged value will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.map (always (\\v -> v)) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map (always (\\v -> v)) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map with a function that maps to the unchanged value will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.map (\\_ -> identity) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map (\\_ -> identity) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map with a function that maps to the unchanged value will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.map (\\_ v -> v) dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.map (\\_ v -> v) dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.map with a function that maps to the unchanged value will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        ]


dictUnionTests : Test
dictUnionTests =
    describe "Dict.union"
        [ test "should not report Dict.union used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.union Dict.empty dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union Dict.empty dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union Dict.empty will always return the same given dict"
                            , details = [ "You can replace this call by the dict itself." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.union dict Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union dict Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.union with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace Dict.empty |> Dict.union dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.union dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.union with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace dict |> Dict.union Dict.empty by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.empty |> Dict.union dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Dict.union with Dict.empty"
                            , details = [ "You can replace this call by the given first dict." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should report Dict.union applied on two dict literals" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.fromList [b,c]) (Dict.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [d,e,b,c])
"""
                        ]
        , test "should report Dict.union applied on two dict literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.fromList [ b, c ]) (Dict.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [d,e, b, c ])
"""
                        ]
        , test "should report Dict.union <| on two dict literals" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union (Dict.fromList [b,c]) <| Dict.fromList [d,e]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [d,e,b,c])
"""
                        ]
        , test "should replace Dict.fromList [d,e] |> Dict.union (Dict.fromList [b, c]) by (Dict.fromList [d,e,b, c])" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [d,e] |> Dict.union (Dict.fromList [b, c])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [d,e,b, c])
"""
                        ]
        , test "should replace Dict.fromList [b,c] |> Dict.union (Dict.fromList [d,e]) by (Dict.fromList [b,c,d,e])" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.fromList [b,c] |> Dict.union (Dict.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union on Dict.fromList calls can be turned into a single Dict.fromList call"
                            , details = [ "Try moving all the elements into a single Dict.fromList call." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.fromList [b,c,d,e])
"""
                        ]
        , test "should replace Dict.union dict dict by dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.union dict dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union where the first and second argument are equal will always return the same given last argument"
                            , details = [ "You can replace this call by the last argument itself." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict
"""
                        ]
        , test "should replace value.field |> Dict.union (.field value) by value.field" <|
            \() ->
                """module A exposing (..)
import Dict
a = value.field |> Dict.union (.field value)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.union where the first and second argument are equal will always return the same given last argument"
                            , details = [ "You can replace this call by the last argument itself." ]
                            , under = "Dict.union"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = value.field
"""
                        ]
        ]


dictFoldlTests : Test
dictFoldlTests =
    describe "Dict.foldl"
        [ test "should not report Dict.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.foldl
a1 = Dict.foldl (\\_ el soFar -> soFar - el)
a2 = Dict.foldl (\\_ el soFar -> soFar - el) 20
a3 = Dict.foldl (\\_ el soFar -> soFar - el) 20 dict
a4 = Dict.foldl (always identity) initial dict
a5 = Dict.foldl (\\_ -> identity) initial dict
a6 = Dict.foldl (\\_ v -> v) initial dict
a6 = Dict.foldl (always (\\v -> v)) initial dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.foldl f initial Dict.empty by initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl f initial Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl on Dict.empty will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = initial
"""
                        ]
        , test "should replace Dict.foldl (always (always identity)) initial dict by initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (always (always identity)) initial dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = initial
"""
                        ]
        , test "should replace Dict.foldl (always (always identity)) initial by always initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (always (always identity)) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always initial
"""
                        ]
        , test "should replace Dict.foldl (always (always identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (always (always identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl (always (\\_ -> identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (always (\\_ -> identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl (\\_ -> (always identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (\\_ -> (always identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl (\\_ _ -> identity) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (\\_ _ -> identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldl (\\_ _ soFar -> soFar) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldl (\\_ _ soFar -> soFar)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        ]


dictFoldrTests : Test
dictFoldrTests =
    describe "Dict.foldr"
        [ test "should not report Dict.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Dict
a0 = Dict.foldr
a1 = Dict.foldr (\\_ el soFar -> soFar - el)
a2 = Dict.foldr (\\_ el soFar -> soFar - el) 20
a3 = Dict.foldr (\\_ el soFar -> soFar - el) 20 dict
a4 = Dict.foldr (always identity) initial dict
a5 = Dict.foldr (\\_ -> identity) initial dict
a6 = Dict.foldr (\\_ v -> v) initial dict
a6 = Dict.foldr (always (\\v -> v)) initial dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Dict.foldr f initial Dict.empty by initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr f initial Dict.empty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr on Dict.empty will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = initial
"""
                        ]
        , test "should replace Dict.foldr (always (always identity)) initial dict by initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (always identity)) initial dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = initial
"""
                        ]
        , test "should replace Dict.foldr (always (always identity)) initial by always initial" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (always identity)) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always initial
"""
                        ]
        , test "should replace Dict.foldr (always (always identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (always identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr (always (\\_ -> identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (always (\\_ -> identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr (\\_ -> (always identity)) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (\\_ -> (always identity))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr (\\_ _ -> identity) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (\\_ _ -> identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        , test "should replace Dict.foldr (\\_ _ soFar -> soFar) by always" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.foldr (\\_ _ soFar -> soFar)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which dict is supplied next." ]
                            , under = "Dict.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = always
"""
                        ]
        ]
