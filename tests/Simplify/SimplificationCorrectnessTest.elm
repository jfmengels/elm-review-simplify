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
