module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoBooleanCaseOf
import NoListLiteralsConcat
import NoPrefixOperator
import NoUnused.CustomTypeConstructors
import NoUnused.Variables
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoBooleanCaseOf.rule
    , NoListLiteralsConcat.rule
    , NoPrefixOperator.rule
    , NoUnused.Variables.rule
    , NoUnused.CustomTypeConstructors.rule
    ]
