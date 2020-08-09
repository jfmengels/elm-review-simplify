# elm-review-simplification

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to simplify Elm code.

**UNMAINTAINED** I do not feel like maintaining this set of rules and have therefore not published these as an Elm package. You can copy them into your project, find another package that has published these, or publish them yourself. You can check out https://github.com/jfmengels/elm-review-rule-ideas/ for similar ideas. Check out the [Try it out](#try-it-out) section below to run the rules.

## Provided rules

- [`NoBooleanCaseOf`](https://elm-doc-preview.netlify.app/NoBooleanCaseOf?repo=jfmengels%2Freview-simplification&version=master) - Reports when pattern matching is used for a boolean value.
- [`NoFullyAppliedPrefixOperator`](https://elm-doc-preview.netlify.app/NoFullyAppliedPrefixOperator?repo=jfmengels%2Freview-simplification&version=master) - Reports when an operator is used as a prefix operator and all the operands are already given.
- [`NoListLiteralsConcat`](https://elm-doc-preview.netlify.app/NoListLiteralsConcat?repo=jfmengels%2Freview-simplification&version=master) - Reports when an operation on lists could be simplified to a single literal list.


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


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplification/example
```
