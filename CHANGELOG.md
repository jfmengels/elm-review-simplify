# Changelog

## [2.0.17] - 2022-08-14

#### Removal of unreachable `if` branches [#31]

`Simplify` now has the ability to infer values from if conditions, which it will use to simplify boolean expressions and even to remove some `if` branches.
```elm
if a && b then
  if a then -- we know this must be true
    1
  else -- so we can remove this else
    2
else
  3
```

It should also be able to catch things like

```elm
if x == 1 then
  if x == 2 then -- we know this must be false
    ...
```

Can some people try it out, and see whether the tool is wrong in some cases, or what it doesn't catch? (and we'll see if I can add support for those)


Note that this will (purposefully) only simplify boolean expressions by what has been inferred from conditions. Therefore,
the following will not be simplified. The reasoning behind the decision is that you would not be able to write code like
below, which can be useful if you want to rely on top-level constants that you may wish to change at a later point in time.

```elm
enableDevMode = False

value =
    if enableDevMode then
```


#### Simplifications

The rule now simplifies:
- `(a < b) == (b > a)` to `True`
- `(a <= b) == (b >= a)` to `True`
- `(a && b) == (b && a)` to `True`
- `(a || b) == (b || a)` to `True`
- `Dict.member x Dict.empty` to `False`

## [2.0.16] - 2022-07-16

The rule now simplifies:
- `.field a == a.b` to `True`
- `a |> fn == fn a` to `True`
- `fn <| a == fn <| a` to `True`


## [2.0.15] - 2022-05-05

The rule now simplifies:
- `List.member x []` to `False`
- `Set.member x Set.empty` to `False`
- `List.intersperse x []` to `[]`


## [2.0.14] - 2022-04-22

The rule now simplifies:
- `List.indexedMap f []` to `[]`
- `List.indexedMap (\_ value -> f value) list` to `List.map (\value -> f value) list`
- `List.indexedMap (always f) list` to `List.map f list`


## [2.0.13] - 2022-04-21

The rule now simplifies:
- `Maybe.andThen (\b -> let y = 1 in Just y) maybe` to `Maybe.map (\b -> let y = 1 in y) maybe`
- `Result.andThen (\b -> let y = 1 in Ok y) result` to `Result.map (\b -> let y = 1 in y) result`


## [2.0.12] - 2022-04-09

The rule now simplifies:
- `List.concatMap (\a -> [ b ]) list` to `List.map (\a -> b) list` 
- `Maybe.andThen (\a -> if condition a then Just b else Just c) maybe` to `Maybe.map (\a -> if condition a then b else c) maybe`
- `List.filterMap (\a -> if condition a then Just b else Just c) maybe` to `List.map (\a -> if condition a then b else c) maybe`


## Missing changelog

Help would be appreciated to fill the blanks!

[2.0.17]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.17
[2.0.16]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.16
[2.0.15]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.15
[2.0.14]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.14
[2.0.13]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.13
[2.0.12]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.12

[#31]: https://github.com/jfmengels/elm-review-simplify/pull/31