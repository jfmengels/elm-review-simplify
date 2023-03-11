module Simplify.RangeDict exposing (RangeDict, any, diff, empty, get, insert, map, mapFromList, member, remove, singleton, union)

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


{-| Indirect conversion from a list to key-value pairs to avoid successive List.map calls.
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


insert : Range -> v -> RangeDict v -> RangeDict v
insert range value rangeDict =
    Dict.insert (rangeAsString range) value rangeDict


remove : Range -> RangeDict v -> RangeDict v
remove range rangeDict =
    Dict.remove (rangeAsString range) rangeDict


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
