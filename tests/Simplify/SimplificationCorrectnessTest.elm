module Simplify.SimplificationCorrectnessTest exposing (all)

import Dict
import Expect
import Fuzz
import Set
import Test exposing (Test)


all : Test
all =
    Test.describe "simplification correctness"
        [ Test.fuzz
            Fuzz.float
            "abs after abs has no effect"
            (\n ->
                abs (abs n)
                    |> compareFloatNaNIsEqual (abs n)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            Fuzz.float
            "negate before abs has no effect"
            (\n ->
                abs (negate n)
                    |> compareFloatNaNIsEqual (abs n)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            Fuzz.float
            "max n -n is the same as abs n"
            (\n ->
                max n -n
                    |> compareFloatNaNIsEqual (abs n)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            Fuzz.float
            "max -n n is the same as abs -n"
            (\n ->
                max -n n
                    |> compareFloatNaNIsEqual (abs -n)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            Fuzz.float
            "min n -n is the same as -(abs n)"
            (\n ->
                min n -n
                    |> compareFloatNaNIsEqual -(abs n)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            Fuzz.float
            "min -n n is the same as -(abs -n)"
            (\n ->
                min -n n
                    |> compareFloatNaNIsEqual -(abs -n)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            (Fuzz.map Dict.fromList
                (Fuzz.list
                    (Fuzz.pair
                        (Fuzz.pair Fuzz.float Fuzz.float)
                        (Fuzz.pair Fuzz.float Fuzz.float)
                    )
                )
            )
            "List.sort after Dict.toList has no effect"
            (\dict ->
                compareListOf
                    (compareTuple
                        (compareTuple compareFloatNaNIsEqual compareFloatNaNIsEqual)
                        (compareTuple compareFloatNaNIsEqual compareFloatNaNIsEqual)
                    )
                    (dict |> Dict.toList)
                    (dict |> Dict.toList |> List.sort)
                    |> Expect.equal EQ
            )
        , Test.fuzz (Fuzz.map Set.fromList (Fuzz.list (Fuzz.pair Fuzz.float Fuzz.float)))
            "List.sort after Set.toList has no effect"
            (\dict ->
                compareListOf (compareTuple compareFloatNaNIsEqual compareFloatNaNIsEqual)
                    (dict |> Set.toList)
                    (dict |> Set.toList |> List.sort)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            (Fuzz.map Dict.fromList
                (Fuzz.list (Fuzz.pair Fuzz.int Fuzz.int))
            )
            "Dict.keys >> List.foldl reduce is the same as Dict.foldl (((<<) always))"
            (\dict ->
                dict
                    |> Dict.keys
                    |> List.foldl (::) []
                    |> Expect.equalLists
                        (dict
                            |> Dict.foldl
                                (always << (::))
                                []
                        )
            )
        , Test.fuzz
            (Fuzz.list Fuzz.string)
            "List.reverse >> List.foldl is the same as List.foldr"
            (\list ->
                list
                    |> List.reverse
                    |> List.foldl (::) []
                    |> Expect.equalLists
                        (list
                            |> List.foldr (::) []
                        )
            )
        , Test.fuzz
            (Fuzz.list Fuzz.string)
            "List.reverse >> List.foldr is the same as List.foldl"
            (\list ->
                list
                    |> List.reverse
                    |> List.foldr (::) []
                    |> Expect.equalLists
                        (list
                            |> List.foldl (::) []
                        )
            )
        , Test.fuzz
            (Fuzz.list Fuzz.string)
            "List.foldr String.append \"\" is the same as String.concat"
            (\strings ->
                strings
                    |> List.foldr String.append ""
                    |> Expect.equal
                        (strings
                            |> String.concat
                        )
            )
        , Test.fuzz
            (Fuzz.list Fuzz.string)
            "List.foldr (++) \"\" is the same as String.concat"
            (\strings ->
                strings
                    |> List.foldr (++) ""
                    |> Expect.equal
                        (strings
                            |> String.concat
                        )
            )
        , Test.fuzz
            (Fuzz.list (Fuzz.list Fuzz.string))
            "List.foldr List.append [] is the same as List.concat"
            (\lists ->
                lists
                    |> List.foldr List.append []
                    |> Expect.equal
                        (lists
                            |> List.concat
                        )
            )
        , Test.fuzz
            (Fuzz.list Fuzz.string)
            "List.foldr (::) [] is the same as identity"
            (\list ->
                list
                    |> List.foldr (::) []
                    |> Expect.equalLists
                        list
            )
        , Test.fuzz
            (Fuzz.list Fuzz.string)
            "List.foldl (::) [] is the same as List.reverse"
            (\list ->
                list
                    |> List.foldl (::) []
                    |> Expect.equalLists
                        (list |> List.reverse)
            )
        , Test.fuzz
            Fuzz.int
            "List.range n n is the same as [ n ]"
            (\n ->
                List.range n n
                    |> Expect.equal
                        [ n ]
            )
        , Test.fuzz
            (Fuzz.pair Fuzz.int (Fuzz.intRange 0 10))
            "List.minimum (List.range x y); x <= y results in Just x"
            (\( start, len ) ->
                List.minimum (List.range start (start + len))
                    |> Expect.equal (Just start)
            )
        , Test.fuzz
            (Fuzz.pair Fuzz.int (Fuzz.intRange 0 10))
            "List.maximum (List.range x y); x <= y results in Just y"
            (\( start, len ) ->
                let
                    end : Int
                    end =
                        start + len
                in
                List.maximum (List.range start end)
                    |> Expect.equal (Just end)
            )
        ]


compareFloatNaNIsEqual : Float -> Float -> Order
compareFloatNaNIsEqual a b =
    if Basics.isNaN a && Basics.isNaN b then
        EQ

    else
        compare a b


compareTuple : (a -> b -> Order) -> (c -> d -> Order) -> ( a, c ) -> ( b, d ) -> Order
compareTuple compareFirst compareSecond ( aHeadFirst, aHeadSecond ) ( bHeadFirst, bHeadSecond ) =
    case compareFirst aHeadFirst bHeadFirst of
        EQ ->
            compareSecond aHeadSecond bHeadSecond

        notEQ ->
            notEQ


compareListOf : (a -> b -> Order) -> List a -> List b -> Order
compareListOf compareElement aList bList =
    case ( aList, bList ) of
        ( [], [] ) ->
            EQ

        ( _ :: _, [] ) ->
            GT

        ( [], _ :: _ ) ->
            LT

        ( aHead :: aTail, bHead :: bTail ) ->
            case compareElement aHead bHead of
                EQ ->
                    compareListOf compareElement aTail bTail

                notEQ ->
                    notEQ
