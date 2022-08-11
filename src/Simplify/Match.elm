module Simplify.Match exposing
    ( Match(..)
    , map
    , maybeAndThen
    , and
    )

{-|

@docs Match
@docs map
@docs maybeAndThen

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


and : Match Bool -> (() -> Match Bool) -> Match Bool
and initial additional =
    case initial of
        Determined True ->
            additional ()

        _ ->
            Undetermined


maybeAndThen : (a -> Match b) -> Maybe a -> Match b
maybeAndThen fn maybe =
    case maybe of
        Just a ->
            fn a

        Nothing ->
            Undetermined
