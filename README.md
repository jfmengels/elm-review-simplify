# review-simplification

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect ways to simplify Elm code.


## Provided rules

- [`NoBooleanCaseOf`](./NoBooleanCaseOf) - Reports when pattern matching is used for a boolean value.
- [`NoListLiteralsConcat`](./NoListLiteralsConcat) - Reports when an operation on lists could be simplified to a single literal list.


## Configuration

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import NoBooleanCaseOf
import NoListLiteralsConcat

config : List Rule
config =
    [ NoBooleanCaseOf.rule
    , NoListLiteralsConcat.rule
    ]
```
