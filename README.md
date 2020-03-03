# review-simplification

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect ways to simplify Elm code.


## Provided rules

- [`NoBooleanCaseOf`](./NoBooleanCaseOf) - Reports when pattern matching is used for a boolean value.


## Configuration

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import NoBooleanCaseOf

config : List Rule
config =
    [ NoBooleanCaseOf.rule
    ]
```
