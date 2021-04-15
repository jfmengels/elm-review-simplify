module NoListLiteralsConcatTest exposing (all)

import NoListLiteralsConcat exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoListLiteralsConcat"
        [ usingPlusPlusTests
        , usingConsTests
        , usingListConcatTests
        , listConcatMapTests
        , listMapTests
        , listFilterTests
        , listFilterMapTests
        ]


message : String
message =
    "Expression could be simplified to be a single List"


details : List String
details =
    [ "Try moving all the elements into a single list." ]


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
                            { message = message
                            , details = details
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
                            { message = message
                            , details = details
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
a = ( fn (a))
"""
                        ]
        , test "should replace List.concatMap fn <| [ a b ] by (fn <| (a b))" <|
            \() ->
                """module A exposing (..)
a = List.concatMap fn <| [ a b ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly" ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( fn <| (a b))
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
