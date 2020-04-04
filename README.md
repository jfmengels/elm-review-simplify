# review-simplification

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect ways to simplify Elm code.


## Provided rules

- [`NoBooleanCaseOf`](https://package.elm-lang.org/packages/jfmengels/elm-review/1.0.0/NoBooleanCaseOf) - Reports when pattern matching is used for a boolean value.
- [`NoFullyAppliedPrefixOperator`](https://package.elm-lang.org/packages/jfmengels/elm-review/1.0.0/NoFullyAppliedPrefixOperator) - Reports when an operator is used as a prefix operator and all the operands are already given.
- [`NoListLiteralsConcat`](https://package.elm-lang.org/packages/jfmengels/elm-review/1.0.0/NoListLiteralsConcat) - Reports when an operation on lists could be simplified to a single literal list.


## Configuration

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import NoBooleanCaseOf
import NoFullyAppliedPrefixOperator
import NoListLiteralsConcat

config : List Rule
config =
    [ NoBooleanCaseOf.rule
    , NoFullyAppliedPrefixOperator.rule
    , NoListLiteralsConcat.rule
    ]
```
