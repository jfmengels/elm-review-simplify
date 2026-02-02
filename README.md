# elm-review-simplify

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to simplify Elm code.

## Provided rules

- [🔧 `Simplify`](https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/2.1.15/Simplify/ "Provides automatic fixes") - Reports when an expression can be simplified.

## Configuration

```elm
module ReviewConfig exposing (config)

import Review.Rule exposing (Rule)
import Simplify

config : List Rule
config =
    [ Simplify.rule Simplify.defaults
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example
```
