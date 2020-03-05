module ReviewConfigForElmAnalyse exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

-- import NoDebug
-- import NoUnused.CustomTypeConstructors
-- import NoUnused.Variables

import NoAddingListLiterals
import NoBooleanCaseOf
import NoMissingTypeAnnotation
import NoPrefixOperator
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoAddingListLiterals.rule
    , NoBooleanCaseOf.rule

    -- , NoDebug.rule
    , NoMissingTypeAnnotation.rule
    , NoPrefixOperator.rule

    -- , NoUnused.CustomTypeConstructors.rule
    -- , NoUnused.Variables.rule
    ]
