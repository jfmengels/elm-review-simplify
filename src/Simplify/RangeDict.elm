module Simplify.RangeDict exposing (RangeDict, any, diff, empty, fromList, get, insert, map, mapFromList, member, singleton, union)

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)


type alias RangeDict v =
    -- TODO: make opaque to disallow type confusion with other String dicts
    Dict String v


empty : RangeDict v
empty =
    Dict.empty


singleton : Range -> v -> RangeDict v
singleton range value =
    Dict.singleton (rangeAsString range) value


{-| Indirect [`fromList`](#fromList) to avoid successive List.map calls.
-}
mapFromList : (a -> ( Range, v )) -> List a -> RangeDict v
mapFromList toAssociation list =
    Dict.fromList
        (List.map
            (\element ->
                let
                    ( range, v ) =
                        toAssociation element
                in
                ( rangeAsString range, v )
            )
            list
        )


{-| Use [`mapFromList`](#mapFromList) if you use List.map before.
-}
fromList : List ( Range, v ) -> RangeDict v
fromList associations =
    Dict.fromList
        (List.map
            (\( range, v ) ->
                ( rangeAsString range, v )
            )
            associations
        )


insert : Range -> v -> RangeDict v -> RangeDict v
insert range rangeDict =
    Dict.insert (rangeAsString range) rangeDict


get : Range -> RangeDict v -> Maybe v
get range rangeDict =
    Dict.get (rangeAsString range) rangeDict


member : Range -> RangeDict v -> Bool
member range rangeDict =
    Dict.member (rangeAsString range) rangeDict


map : (a -> b) -> RangeDict a -> RangeDict b
map valueChange rangeDict =
    Dict.map (\_ -> valueChange) rangeDict


foldl : (v -> folded -> folded) -> folded -> RangeDict v -> folded
foldl reduce initialFolded rangeDict =
    Dict.foldl (\_ -> reduce) initialFolded rangeDict


any : (v -> Bool) -> RangeDict v -> Bool
any isFound rangeDict =
    foldl (\value soFar -> soFar || isFound value)
        False
        rangeDict


union : RangeDict v -> RangeDict v -> RangeDict v
union aRangeDict bRangeDict =
    Dict.union aRangeDict bRangeDict


diff : RangeDict a -> RangeDict b -> RangeDict a
diff baseRangeDict exceptionsRangeDict =
    Dict.diff baseRangeDict exceptionsRangeDict


rangeAsString : Range -> String
rangeAsString range =
    [ range.start.row
    , range.start.column
    , range.end.row
    , range.end.column
    ]
        |> List.map String.fromInt
        |> String.join "_"
