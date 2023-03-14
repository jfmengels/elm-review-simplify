module Simplify.CollectionHelpers exposing (filterMapInto)


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
