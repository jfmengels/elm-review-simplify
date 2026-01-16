module Simplify.SimplificationCorrectnessTest exposing (all)

import Dict
import Expect
import Fuzz
import Set
import Test exposing (Test, test)


all : Test
all =
    Test.describe "simplification correctness"
        [ Test.fuzz Fuzz.char
            "char as string length is at most 2"
            (\char ->
                String.length (String.fromChar char)
                    |> Expect.lessThan 3
            )
        , Test.fuzz
            (Fuzz.pair
                (Fuzz.pair fuzzFloatWithoutNaN fuzzFloatWithoutNaN
                    |> Fuzz.map
                        (\( min, max ) ->
                            if min > max then
                                ( max, min )

                            else
                                ( min, max )
                        )
                )
                Fuzz.float
            )
            "negating a number with bounds min,max has bounds -max,-min"
            (\( ( min, max ), n ) ->
                let
                    negatedInBounds : Float
                    negatedInBounds =
                        -(Basics.clamp min max n)
                in
                (negatedInBounds >= -max && negatedInBounds <= -min)
                    |> Expect.equal True
                    |> Expect.onFail
                        (Debug.toString
                            { min = min
                            , max = max
                            , inBounds = Basics.clamp min max n
                            , expectedNegatedBounds = ( -max, -min )
                            , actualNegated = negatedInBounds
                            }
                        )
            )
        , Test.fuzz
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
        , Test.fuzz (Fuzz.list Fuzz.unit)
            "(>) 1 << List.length is the same as List.isEmpty"
            (\list ->
                ((>) 1 << List.length) list
                    |> Expect.equal
                        (List.isEmpty list)
            )
        , Test.fuzz (Fuzz.list Fuzz.unit)
            "(>=) 0 << List.length is the same as List.isEmpty"
            (\list ->
                ((>=) 0 << List.length) list
                    |> Expect.equal
                        (List.isEmpty list)
            )
        , Test.fuzz (Fuzz.list Fuzz.unit)
            "(<) 0 << List.length is the same as not << List.isEmpty"
            (\list ->
                ((<) 0 << List.length) list
                    |> Expect.equal
                        ((not << List.isEmpty) list)
            )
        , Test.fuzz (Fuzz.list Fuzz.unit)
            "(<=) 1 << List.length is the same as not << List.isEmpty"
            (\list ->
                ((<=) 1 << List.length) list
                    |> Expect.equal
                        ((not << List.isEmpty) list)
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
            (Fuzz.map Set.fromList (Fuzz.list (Fuzz.pair Fuzz.float Fuzz.float)))
            "Set.foldr (::) [] is the same as Set.toList"
            (\set ->
                set
                    |> Set.foldr (::) []
                    |> compareListOf (compareTuple compareFloatNaNIsEqual compareFloatNaNIsEqual)
                        (set |> Set.toList)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            -- fails when generating NaN and checking with compareListOf
            (Fuzz.map Set.fromList (Fuzz.list Fuzz.niceFloat))
            "Set.foldr Set.insert Set.empty is the same as identity"
            (\set ->
                set
                    |> Set.foldr Set.insert Set.empty
                    |> Expect.equalSets
                        set
            )
        , Test.fuzz
            (Fuzz.map Set.fromList (Fuzz.list Fuzz.niceFloat))
            "Set.foldl Set.insert Set.empty is the same as identity"
            (\set ->
                set
                    |> Set.foldl Set.insert Set.empty
                    |> Expect.equalSets
                        set
            )
        , Test.fuzz
            (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair Fuzz.niceFloat Fuzz.string)))
            "Dict.foldr Dict.insert Dict.empty is the same as identity"
            (\dict ->
                dict
                    |> Dict.foldr Dict.insert Dict.empty
                    |> Expect.equalDicts
                        dict
            )
        , Test.fuzz
            (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair Fuzz.niceFloat Fuzz.string)))
            "Dict.foldl Dict.insert Dict.empty is the same as identity"
            (\dict ->
                dict
                    |> Dict.foldl Dict.insert Dict.empty
                    |> Expect.equalDicts
                        dict
            )
        , Test.fuzz
            (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair (Fuzz.pair Fuzz.float Fuzz.float) (Fuzz.pair Fuzz.float Fuzz.float))))
            "Dict.foldr (always (::)) [] is the same as Dict.values"
            (\dict ->
                dict
                    |> Dict.foldr (always (::)) []
                    |> compareListOf (compareTuple compareFloatNaNIsEqual compareFloatNaNIsEqual)
                        (Dict.values dict)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair (Fuzz.pair Fuzz.float Fuzz.float) Fuzz.string)))
            "Dict.foldr (always << (::)) [] is the same as Dict.keys"
            (\dict ->
                dict
                    |> Dict.foldr (always << (::)) []
                    |> compareListOf (compareTuple compareFloatNaNIsEqual compareFloatNaNIsEqual)
                        (Dict.keys dict)
                    |> Expect.equal EQ
            )
        , Test.fuzz
            (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair (Fuzz.pair Fuzz.float Fuzz.float) Fuzz.string)))
            "Dict.foldr (\\k -> (::) << Tuple.pair k) [] is the same as Dict.keys"
            (\dict ->
                dict
                    |> Dict.foldr (\k -> (::) << Tuple.pair k) []
                    |> compareListOf (compareTuple (compareTuple compareFloatNaNIsEqual compareFloatNaNIsEqual) compare)
                        (Dict.toList dict)
                    |> Expect.equal EQ
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
        , Test.fuzz
            -- fails when generating NaN and checking with compareListOf
            (Fuzz.list Fuzz.string)
            "Set.fromList << List.reverse is the same as Set.fromList"
            (\list ->
                Set.fromList (List.reverse list)
                    |> Expect.equal (Set.fromList list)
            )
        , Test.fuzz
            (Fuzz.pair
                -- too large numbers lead to RangeError: Invalid string length
                (Fuzz.intRange -100 100)
                Fuzz.char
            )
            "String.map f (String.repeat n (String.fromChar c)) is the same as String.repeat n (String.fromChar (f c))"
            (\( n, c ) ->
                String.map Char.toUpper (String.repeat n (String.fromChar c))
                    |> Expect.equal
                        (String.repeat n (String.fromChar (Char.toUpper c)))
            )
        , Test.fuzz
            -- fails when generating NaN and checking with compareListOf
            (Fuzz.pair
                (Fuzz.intRange -100 100)
                Fuzz.string
            )
            "Set.fromList << List.repeat n is the same as if n >= 1 then Set.singleton else always Set.empty"
            (\( n, elementToRepeat ) ->
                Set.fromList (List.repeat n elementToRepeat)
                    |> Expect.equalSets
                        (if n >= 1 then
                            Set.singleton elementToRepeat

                         else
                            Set.empty
                        )
            )
        , Test.fuzz (Fuzz.list Fuzz.int)
            "List.isEmpty << List.filter f is the same as Basics.not << List.any f"
            (\list ->
                List.isEmpty (List.filter isEven list)
                    |> Expect.equal
                        (Basics.not (List.any isEven list))
            )
        , Test.fuzz (Fuzz.list Fuzz.int)
            "List.isEmpty << List.filter (Basics.not << f) is the same as List.all f"
            (\list ->
                List.isEmpty (List.filter (Basics.not << isEven) list)
                    |> Expect.equal
                        (List.all isEven list)
            )
        , Test.fuzz (Fuzz.list Fuzz.bool)
            "List.isEmpty << List.filter Basics.not is the same as List.all identity"
            (\list ->
                List.isEmpty (List.filter Basics.not list)
                    |> Expect.equal
                        (List.all identity list)
            )
        , Test.fuzz
            (Fuzz.list Fuzz.int)
            "Basics.not << List.any (Basics.not << f) is the same as List.all f"
            (\list ->
                (Basics.not <| List.any (Basics.not << isEven) list)
                    |> Expect.equal
                        (List.all isEven list)
            )
        , Test.fuzz
            (Fuzz.list Fuzz.bool)
            "Basics.not << List.any Basics.not is the same as List.all identity"
            (\list ->
                (Basics.not <| List.any Basics.not list)
                    |> Expect.equal
                        (List.all identity list)
            )
        , Test.fuzz
            (Fuzz.list Fuzz.int)
            "Basics.not << List.all (Basics.not << f) is the same as List.any f"
            (\list ->
                (Basics.not <| List.all (Basics.not << isEven) list)
                    |> Expect.equal
                        (List.any isEven list)
            )
        , Test.fuzz
            (Fuzz.list Fuzz.bool)
            "Basics.not << List.all Basics.not is the same as List.any identity"
            (\list ->
                (Basics.not <| List.all Basics.not list)
                    |> Expect.equal
                        (List.any identity list)
            )
        , Test.fuzz Fuzz.string
            "String.toUpper << String.toUpper is the same as String.toUpper"
            (\string ->
                String.toUpper (String.toUpper string)
                    |> Expect.equal
                        (String.toUpper string)
            )
        , test "String.toUpper << String.toLower is NOT the same as String.toUpper"
            (\() ->
                -- https://github.com/jfmengels/elm-review-simplify/pull/429#issuecomment-3746681750
                String.toUpper (String.toLower "Ω")
                    |> Expect.notEqual
                        (String.toUpper "Ω")
            )
        , Test.fuzz Fuzz.string
            "String.toLower << String.toLower is the same as String.toLower"
            (\string ->
                String.toLower (String.toLower string)
                    |> Expect.equal
                        (String.toLower string)
            )
        , Test.fuzz Fuzz.string
            "String.trimLeft << String.trimRight is the same as String.trim"
            (\string ->
                String.trimLeft (String.trimRight string)
                    |> Expect.equal
                        (String.trim string)
            )
        , Test.fuzz Fuzz.string
            "String.trimRight << String.trimLeft is the same as String.trim"
            (\string ->
                String.trimRight (String.trimLeft string)
                    |> Expect.equal
                        (String.trim string)
            )
        , Test.fuzz Fuzz.string
            "String.trimLeft << String.trim is the same as String.trim"
            (\string ->
                String.trimLeft (String.trim string)
                    |> Expect.equal
                        (String.trim string)
            )
        , Test.fuzz Fuzz.string
            "String.trimRight << String.trim is the same as String.trim"
            (\string ->
                String.trimRight (String.trim string)
                    |> Expect.equal
                        (String.trim string)
            )
        , Test.fuzz Fuzz.string
            "String.trim << String.trim is the same as String.trim"
            (\string ->
                String.trim (String.trim string)
                    |> Expect.equal
                        (String.trim string)
            )
        , Test.fuzz Fuzz.string
            "String.trimRight << String.trimRight is the same as String.trimRight"
            (\string ->
                String.trimRight (String.trimRight string)
                    |> Expect.equal
                        (String.trimRight string)
            )
        , Test.fuzz Fuzz.string
            "String.trimLeft << String.trimLeft is the same as String.trimLeft"
            (\string ->
                String.trimLeft (String.trimLeft string)
                    |> Expect.equal
                        (String.trimLeft string)
            )
        , Test.fuzz Fuzz.string
            "String.trim << String.trimLeft is the same as String.trim"
            (\string ->
                String.trim (String.trimLeft string)
                    |> Expect.equal
                        (String.trim string)
            )
        , Test.fuzz Fuzz.string
            "String.trim << String.trimRight is the same as String.trim"
            (\string ->
                String.trim (String.trimRight string)
                    |> Expect.equal
                        (String.trim string)
            )
        , Test.fuzz Fuzz.string
            "String.all (always False) is the same as String.isEmpty"
            (\string ->
                String.all (always False) string
                    |> Expect.equal
                        (String.isEmpty string)
            )
        , Test.fuzz Fuzz.string
            "String.any (always True) is the same as not << String.isEmpty"
            (\string ->
                String.any (always True) string
                    |> Expect.equal
                        (Basics.not (String.isEmpty string))
            )
        , Test.fuzz Fuzz.string
            "String.filter f << String.reverse is the same as String.reverse << String.filter f"
            (\string ->
                String.filter charCodeIsEven (String.reverse string)
                    |> Expect.equal
                        (String.reverse (String.filter charCodeIsEven string))
            )
        ]


isEven : Int -> Bool
isEven n =
    remainderBy 2 n == 0


charCodeIsEven : Char -> Bool
charCodeIsEven c =
    isEven (Char.toCode c)


fuzzFloatWithoutNaN : Fuzz.Fuzzer Float
fuzzFloatWithoutNaN =
    Fuzz.float |> Fuzz.filter (\float -> Basics.not (Basics.isNaN float))


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
