# elm-review-simplify

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to simplify Elm code.

## Provided rules

- [`Simplify`](https://elm-doc-preview.netlify.app/Simplify?repo=jfmengels%2Felm-review-simplify&version=master) - Reports when an operation can be simplified.
- [`NoBooleanCaseOf`](https://elm-doc-preview.netlify.app/NoBooleanCaseOf?repo=jfmengels%2Felm-review-simplify&version=master) - Reports when pattern matching is used for a boolean value.
- [`NoFullyAppliedPrefixOperator`](https://elm-doc-preview.netlify.app/NoFullyAppliedPrefixOperator?repo=jfmengels%2Felm-review-simplify&version=master) - Reports when an operator is used as a prefix operator and all the operands are already given.

## Configuration

```elm
module ReviewConfig exposing (config)

import NoBooleanCaseOf
import NoFullyAppliedPrefixOperator
import Review.Rule exposing (Rule)
import Simplify

config : List Rule
config =
    [ Simplify.rule
    , NoBooleanCaseOf.rule
    , NoFullyAppliedPrefixOperator.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example
```
