module Simplify.Match exposing
    ( Match(..)
    , map
    )

{-|

@docs Match
@docs map

-}


type Match a
    = Determined a
    | Undetermined


map : (a -> b) -> Match a -> Match b
map mapper match =
    case match of
        Determined a ->
            Determined (mapper a)

        Undetermined ->
            Undetermined
