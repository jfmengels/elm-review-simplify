module Simplify.CollectionHelpers exposing
    ( concatMapInto
    , filterMapInto
    , mapIntoDict
    )

import Dict exposing (Dict)


filterMapInto : (a -> Maybe b) -> (b -> c -> c) -> c -> List a -> c
filterMapInto mapper insert initialValue list =
    List.foldl
        (\value acc ->
            case mapper value of
                Just name ->
                    insert name acc

                Nothing ->
                    acc
        )
        initialValue
        list


concatMapInto : (a -> List b) -> (b -> c) -> (c -> d -> d) -> d -> List a -> d
concatMapInto mapper subMap insert initialValue list =
    List.foldl
        (\value acc ->
            List.foldl
                (\subValue subAcc ->
                    insert (subMap subValue) subAcc
                )
                acc
                (mapper value)
        )
        initialValue
        list


mapIntoDict : (a -> comparable) -> (a -> b) -> Dict comparable b -> List a -> Dict comparable b
mapIntoDict toKey toValue initialValue list =
    List.foldl
        (\value acc ->
            Dict.insert
                (toKey value)
                (toValue value)
                acc
        )
        initialValue
        list
