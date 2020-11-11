module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Review.Rule exposing (Rule)
import NoInvalidRGBValues
import NoRedundantConcat
import NoRedundantCons
import NoUnused.Dependencies
import NoUnused.Exports


config : List Rule
config =
    [ NoInvalidRGBValues.rule
    , NoRedundantConcat.rule
    , NoRedundantCons.rule
    , NoUnused.Dependencies.rule
    -- , NoUnused.Exports.rule
    ]
