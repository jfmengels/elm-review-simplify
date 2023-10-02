# Changelog

## [Unreleased]

- composition checks now also detect function pairs across nested compositions like `(here << ...) >> (... << there)`
- `List.sort (List.sort list)` to `List.sort list`
- `List.sortBy f (List.sortBy f list)` to `List.sortBy f list`
- `String.concat (List.repeat n str)` to `String.repeat n str`
- `String.concat (List.intersperse str strings)` to `String.join str strings`
- `Set.foldl/r f initial Set.empty` to `initial`
- `Set.foldl/r (\_ soFar -> soFar) initial set` to `initial`
- `String.foldl/r f initial ""` to `initial`
- `String.foldl/r (\_ soFar -> soFar) initial string` to `initial`
- `Result.fromMaybe x (Just a)` to `Ok a`
- `Result.fromMaybe x Nothing` to `Err x`
- the same operations for `Json.Decode.map` as for e.g. `Task.map` and `Result.map`
- the same operations for `Json.Decode.map2-8` as for e.g. `Task.mapN` and `Result.mapN`
- the same operations for `Json.Decode.andThen` as for e.g. `Task.andThen` and `Result.andThen`
- `Tuple.first (Tuple.mapSecond changeFirst tuple)` to `Tuple.first tuple`
- `Tuple.first (Tuple.mapBoth changeFirst changeSecond tuple)` to `Tuple.first (Tuple.mapFirst changeFirst tuple)`
- `Tuple.second (Tuple.mapFirst changeSecond tuple)` to `Tuple.second tuple`
- `Tuple.second (Tuple.mapBoth changeFirst changeSecond tuple)` to `Tuple.second (Tuple.mapSecond changeSecond tuple)`
- `Maybe.withDefault a << Just` to `identity`
- `Result.withDefault a << Ok` to `identity`
- `List.sum << List.singleton` to `identity`
- `List.product << List.singleton` to `identity`
- `List.concat << List.singleton` to `identity`
- `Platform.Cmd.batch << List.singleton` to `identity`
- `Platform.Sub.batch << List.singleton` to `identity`
- `List.minimum << List.singleton` to `Just`
- `List.maximum << List.singleton` to `Just`
- `Maybe.map2 f firstMaybe Nothing` to `Nothing` (same for all Maybe.mapN)
- `Maybe.map2 f (Just a) (Just b)` to `Just (f a b)` (same for all Maybe.mapN)
- `Array.foldl f initial Array.empty` to `initial` (same for `Array.foldr`)
- `Array.foldl (\_ soFar -> soFar) initial array` to `initial` (same for `Array.foldr`)
- `Array.toList Array.empty` to `[]`
- `Array.toList (Array.repeat n a)` to `List.repeat n a`
- `Array.toIndexedList Array.empty` to `[]`
- `List.map Tuple.second (Array.toIndexedList array)` to `Array.toList array`

Bug fixes:
- Fixed an issue where `Dict.intersect Dict.empty` would be fixed to `Dict.empty`
- Fixed an issue where `Set.intersect Set.empty` would be fixed to `Set.empty`

## [2.1.2] - 2023-09-28

Lots of new simplifications, especially for `Array` and `Task`.

The rule now simplifies:
- `Array.fromList []` to `Array.empty`
- `Array.fromList (Array.toList array)` to `array`
- `Array.toList (Array.fromList list)` to `list`
- `Array.map f Array.empty` to `Array.empty`
- `Array.map identity array` to `array`
- `Array.indexedMap (\_ value -> f value) array` to `Array.map (\value -> f value) array`
- the same operations for `Array.filter` as for `List.filter` and `Set.filter`
- `Array.isEmpty Array.empty` to `True`
- `Array.isEmpty (Array.fromList [ x ])` to `False`
- `Array.repeat 0 n` to `Array.empty`
- `Array.initialize 0 f` to `Array.empty`
- `Array.length Array.empty` to `0`
- `Array.length (Array.fromList [ a, b, c ])` to `3`
- `Array.length (Array.repeat 3 x)` to `3`
- `Array.length (Array.initialize 3 f)` to `3`
- `Array.length (Array.repeat n x)` to `max 0 n`
- `Array.length (Array.initialize n f)` to `max 0 n`
- `Array.append Array.empty array` to `array`
- `Array.append (Array.fromList [ a, b ]) (Array.fromList [ c, d ])` to `Array.fromList [ a, b, c, d ]`
- `Array.get n Array.empty` to `Nothing`
- `Array.get 1 (Array.fromList [ a, b, c ])` to `Just b`
- `Array.get 100 (Array.fromList [ a, b, c ])` to `Nothing`
- `Array.get -1 array` to `Nothing`
- `Array.set n x Array.empty` to `Array.empty`
- `Array.set -1 x array` to `array`
- `Array.set 1 x (Array.fromList [ a, b, c ])` to `Array.fromList [ a, x, c ]`
- `Array.set 100 x (Array.fromList [ a, b, c ])` to `Array.fromList [ a, b, c ]`


- `Task.andThen f (Task.fail x)` to `Task.fail x`
- `Task.andThen f (Task.succeed a)` to `f a`
- `Task.andThen Task.succeed task` to `task`
- `Task.andThen (\a -> Task.succeed b) task` to `Task.map (\a -> b) x`
- `Task.onError f (Task.succeed a)` to `Task.succeed a`
- `Task.onError f (Task.fail x)` to `f x`
- `Task.onError Task.fail task` to `task`
- `Task.onError (\x -> Task.fail y) task` to `Task.mapError (\x -> y) x`
- `Task.sequence [ Task.succeed a, Task.succeed b ]` to `Task.succeed [ a, b ]`
- `Task.sequence [ Task.succeed a, Task.fail x ]` to `Task.fail x`
- `Task.sequence [ a, Task.fail x, b ]` to `Task.sequence [ a, Task.fail x ]`
- `Task.sequence [ task ]` to `Task.map List.singleton task`
- `Task.map identity task` to `task`
- `Task.map f (Task.fail x)` to `Task.fail x`
- `Task.map f (Task.succeed a)` to `Task.succeed (f a)`
- `Task.map3 f (Task.succeed a) (Task.succeed b) (Task.succeed c)` to `Task.succeed (f a b c)` (same for all `Task.mapN` functions)
- `Task.map3 f (Task.succeed a) (Task.fail x) thirdTask` to `Task.fail x`
- `Task.map3 f firstTask (Task.fail x) thirdTask` to `Task.map2 f firstTask (Task.fail x)`
- `Task.mapError identity task` to `task`
- `Task.mapError f (Task.succeed a)` to `Task.succeed a`
- `Task.mapError f (Task.fail x)` to `Task.fail (f x)`


- `List.map f [ a ]` to `[ f a ]`
- `List.filterMap identity [ a, Nothing, b ]` to `List.filterMap identity [ a, b ]`
- `List.singleton >> String.fromList` to `String.fromChar`
- `Result.map3 f (Ok a) (Ok b) (Ok c)` to `Ok (f a b c)` (same for all `Result.mapN` functions)
- `Result.map3 f (Ok a) (Err x) thirdResult` to `Err x`
- `Result.map3 f firstResult (Err x) thirdResult` to `Result.map2 f firstResult (Err x)`
- `String.append String.empty str` to `str`
- `String.append (String.fromList [ a, b ]) (String.fromList [ c, d ])` to `String.fromList [ a, b, c, d ]`
- `String.fromList [ a, b ] ++ String.fromList [ c, d ]` to `String.fromList [ a, b, c, d ]`
- `String.fromList (String.toList str)` to `str`
- `String.toList (String.fromList list)` to `list`
- `String.reverse >> String.reverse` to `identity`
- `List.reverse >> List.reverse` to `identity`
- `Set.union (Set.fromList [ a, b ]) (Set.fromList [ c, d ])` to `Set.fromList [ a, b, c, d ]`
- `Set.fromList (Set.toList set)` to `set`
- `Dict.fromList (Dict.toList dict)` to `dict`
- `Dict.union (Dict.fromList [ a, b ]) (Dict.fromList [ c, d ])` to `Dict.fromList [ c, d, a, b ]`


## [2.1.1] - 2023-09-18

- A very large number of error messages were reworded to be more consistent, precise and descriptive.
- Checks that applied on `[ a ]` now also report for `List.singleton a` (ex: `List.concatMap f (List.singleton 1)` gets simplified to `f 1`)

The simplification `(\x y -> x + y) n m` introduced in [2.1.0] was removed ([#147](https://github.com/jfmengels/elm-review-simplify/pull/147)).

The rule now simplifies:
- `0 // n` to `0`
- `n // 0` to `0`
- `n // 1` to `n`
- `Tuple.first ( a, b )` to `a`
- `Tuple.second ( a, b )` to `b`
- `Tuple.pair a b` to `( a, b )`
- `List.repeat 1 x` to `List.singleton x`
- `List.reverse [ x ]` to `[ x ]`
- `List.intersperse s [ x ]` to `[ x ]`
- `List.concatMap List.singleton x` to `x`
- `String.reverse (String.fromChar a)` to `String.fromChar a`
- `Dict.intersect Dict.empty dict` to `Dict.empty`
- `Dict.diff Dict.empty dict` to `Dict.empty`
- `Dict.diff dict Dict.empty` to `dict`
- `Dict.union dict Dict.empty` to `dict`
- `Random.andThen f (Random.constant x)` to `f x`
- `Random.andThen Random.constant generator` to `generator`
- `Random.andThen (\a -> Random.constant b) generator` to `Random.map (\a -> b) generator`
- `Random.andThen (always thenGenerator) generator` to `thenGenerator`
- `Result.mapError f (if x then Err a else Err b)` to `f (if x then a else b)`
- `Random.map identity generator` to `generator`
- `Random.map (always a) generator` to `Random.constant a`
- `Random.map f (Random.constant x)` to `Random.constant (f x)`
- `Random.list 0 generator` to `Random.constant []`
- `Random.list -1/-2/-3/... generator` to `Random.constant []`
- `Random.list 1 generator` to `Random.map List.singleton generator`
- `Random.list n (Random.constant el)` to `Random.constant (List.repeat n el)`
- `Random.uniform a []` to `Random.constant a`
- `Random.weighted ( weight, a ) []` to `Random.constant a`
- `Random.weighted tuple []` to `Random.constant (Tuple.first tuple)`
- `List.member (List.singleton b) b` to `b == b` when [`expectNaN`] is enabled (and to `True` otherwise)

Bug fixes:
- Fixed an issue where `Dict.size (Dict.fromList [...])` would be fixed to an incorrect value
- Fixed an issue where `Result.toMaybe (if c then Err a else Ok b)` would be fixed to `Nothing`
- Fixed an issue where `Maybe.andThen (always (Just a)) maybe` would be fixed to `maybe`

## [2.1.0] - 2023-08-15

New opt-in configuration option [`expectNaN`] which will disable some simplifications when the user indicates their
project is likely to use `NaN` values. This disables the following simplifications:
- `x == x` to `True`
- `List.member x [ x ]` to `True`
- `n * 0` to `0`
- `not (a < b)` to `a >= b` (similarly for `>`, `<=`, `>=`, `==` and `/=`)

The rule now simplifies:
- `List.any ((==) x) list` to `List.member x list`
- `List.any (\y -> x == y) list` to `List.member x list`
- `n - n` to `0`
- `-n + n` to `0`
- `0 / n` to `0`
- `n * 0` to `0` is now autofixed

The rule now reports:
- Immediately invoked anonymous functions `(\x y -> x + y) 1 2`. This is very simplifiable but there is no autofix because there are varied ways to simplify it.
  - EDIT: This was removed in [2.1.1].

Bug fixes:
- Fixed an issue where `[ [ 1 ], [ 2 ] ] |> List.concat` would be incorrectly fixed and cause a compiler error

Misc:
- Improved error positioning and fixes for errors related to the usage of operators

## [2.0.33] - 2023-08-13

The rule now simplifies:
- `a |> f >> g` to `a |> f |> g`

## [2.0.32] - 2023-07-11

The rule now simplifies:
- `List.concat [ a, [], b ]` to `List.concat [ a, b ]`

## [2.0.31] - 2023-06-25

Now avoids simplifying `String.replace` when the pattern to find contains `\r`.

## [2.0.30] - 2023-06-25

Fixed an issue where `String.replace` would be fixed incorrectly ([01d3ff](https://github.com/jfmengels/elm-review-simplify/commit/01d3ff0e1ce920b5fcece74fed8d12537b7813c9))

## [2.0.29] - 2023-04-17

The rule now simplifies:
- `Html.Attributes.classList [ x, y, ( z, False ) ]` to `Html.Attributes.classList [ x, y ]`
- `Html.Attributes.classList [ ( onlyOneThing, True ) ]` to `Html.Attributes.class onlyOneThing`
- `Set.fromList [ a ]` to `Set.singleton a`
- `Dict.partition f Dict.empty` to `( Dict.empty, Dict.empty )`
- `Dict.partition (always True) dict` to `( dict, Dict.empty )`
- `Dict.partition (always False) dict` to `( Dict.empty, dict )`
- `Result.toMaybe (Ok x)` to `Just x`
- `Result.toMaybe (Err e)` to `Nothing`
- `Result.mapError identity x` to `x`
- `Result.mapError f (Ok x)` to `Ok x`
- `Result.mapError f (Err x)` to `Err (f x)`
- `List.map Tuple.first (Dict.toList dict)` to `Dict.keys dict`
- `List.map Tuple.second (Dict.toList dict)` to `Dict.values dict`

We now also do a better job at figuring what code is the same:
- `(f >> g) a == (g << f) a` will now be replaced by `True`.
- `(\f -> List.map f)` is considered equivalent in all simplifications to `List.map` (and similarly for a number of other functions).

Bug fixes:
- Fixed an issue where `List.append` would be fixed incorrectly ([#105](https://github.com/jfmengels/elm-review-simplify/issues/105))

## [2.0.28] - 2023-02-25

- Fixed an issue where errors for `List.foldl` operations would be incorrectly fixed ([#86](https://github.com/jfmengels/elm-review-simplify/issues/86))

## [2.0.27] - 2023-02-21

- Fixed an issue where `String.fromList [ f x ]` would incorrectly be changed to `String.fromChar f x` ([#85](https://github.com/jfmengels/elm-review-simplify/issues/85))

## [2.0.26] - 2023-02-06

The simplification `String.slice 0 n str` -> `String.left n str` has been removed because they were not necessarily equivalent. In the case where `n` is negative, then the behavior of the 2 functions differ.

The rule now simplifies:
- `List.member a []` to `False`
- `List.member a [ a, b, c ]` to `True`


## [2.0.25] - 2023-02-02

The rule now simplifies:
- `String.fromList []` to `""`
- `String.fromList [ a ]` to `String.fromChar a`
- `List.append [] list` to `list`
- `List.head []` to `Nothing`
- `List.head (a :: bToZ)` to `a`
- `List.tail []` to `Nothing`
- `List.tail (a :: bToZ)` to `bToZ`
- `List.sum []` to `0`
- `List.sum [ a ]` to `a`
- `List.product []` to `1`
- `List.product [ a ]` to `a`
- `List.minimum []` to `Nothing`
- `List.minimum [ a ]` to `Just a`
- `List.maximum []` to `Nothing`
- `List.maximum [ a ]` to `Just a`
- `List.map2 fn xs []` to `[]` (same for up to `List.map5`)
- `List.map2 fn [] ys` to `[]` (same for up to `List.map5`)
- `List.unzip []` to `( [], [] )`
- `List.foldl f x (Set.toList set)` to `Set.foldl f x set`

All the changes in this release were contributed by [@lue-bird](https://github.com/lue-bird).

## [2.0.24] - 2023-01-20

All the changes in this release were contributed by [@lue-bird](https://github.com/lue-bird).

The rule now simplifies:
- `String.slice n n str` to `""`
- `String.slice 0 n str` to `String.left n str`
- `String.slice n 0 str` to `""`
- `String.slice a z ""` to `""`
- `String.left 0 str` to `""`
- `String.left -1 str` to `""`
- `String.left n ""` to `""`
- `String.right 0 str` to `""`
- `String.right -1 str` to `""`
- `String.right n ""` to `""`
- `String.slice 2 1 str` to `""`
- `String.slice -1 -2 str` to `""`
- `List.sortBy (\_ -> a) list ` to `list`
- `List.sortBy identity list ` to `List.sort list`
- `List.sortWith (\_ _ -> LT) list ` to `List.reverse list`
- `List.sortWith (\_ _ -> EQ) list ` to `list`
- `List.sortWith (\_ _ -> GT) list ` to `list`

The following simplifications for `List.sort` also work for `List.sortBy fn` and `List.sortWith fn`:
- `List.sort []` to `[]`
- `List.sort [ a ]` to `[ a ]`

The following simplifications for List.foldl also work for `List.foldr`:
- `List.foldl fn x []` to `x`
- `List.foldl (\_ soFar -> soFar) x list ` to `x`
- `List.foldl (+) 0 list ` to `List.sum list`
- `List.foldl (+) initial list ` to `initial + List.sum list`
- `List.foldl (*) 1 list ` to `List.product list`
- `List.foldl (*) 0 list ` to `0`
- `List.foldl (*) initial list ` to `initial * List.product list`
- `List.foldl (&&) True list ` to `List.all identity list`
- `List.foldl (&&) False list ` to `False`
- `List.foldl (||) False list ` to `List.any identity list`
- `List.foldl (||) True list ` to `True`


## [2.0.23] - 2022-11-08

Add better support for `jfmengels/elm-review` v2.10.0.

## [2.0.22] - 2022-11-03

Fixed an issue where `0 - f x` would be simplified to `-f x` instead of `-(f x)` [#52]

## [2.0.21] - 2022-09-06

Fixed an issue in where let declarations would not be fused when there was only a single element in the first let declaration.

## [2.0.20] - 2022-09-06

The rule now simplifies:
1. Applying record access simplification over if and case expressions.
```elm
(if condition then
   { record | a = 1 }
 else
   { record | field = 2 }
).field
-->
 if condition then
   { record | a = 1 }.field
 else
   { record | field = 2 }.field
```
when all patterns can later be simplified. In this example the final result will be
```elm
if condition then
  record.field
else
  2
```

This also applies to case expressions when all branches can be simplified. Thanks [@miniBill]! [#40]

2. Simplify case expressions that can be simplified to let variables [#48]

```elm
a =
  case value of
    { x, y } ->
      1
-->
a =
  let
    { x, y } =
      value
  in
  1
```

3. Merging multiple let declarations

```elm
let
    a = 1
in
let
    b = 1
in
a + b
-->
let
    a = 1
    b = 1
in
a + b
```

## [2.0.19] - 2022-08-29

The rule now DOESN'T (it did before) simplify case of expressions where all the branches have the same code when one of
the patterns references a custom type from your project. For example
```elm
case x of
  A -> 1
  B -> 1
  C -> 1
```
does not get simplified to `1` like before. But the simplification still happens if the patterns only reference custom
types that come from dependencies or `elm/core`, like
```elm
case x of
  Just _ -> 1
  Nothing -> 1
--> 1
```

The reasoning is that often you want the compiler to give you a reminder when you introduce a new custom type, which this
simplification made very hard. It also sometimes created some worse code when you pattern matched on a custom type with
only a single constructor.

The configuration setting `Simplify.ignoreCaseOfForTypes` now only takes custom types from dependencies. Any type
provided to this function that is not found in the dependencies will now trigger a global error.
It is likely that you won't need this function anymore. If you do, please open an issue because I'd love to know. 

A number of `elm-review` users didn't use `Simplify` because of the simplification above, so I'm hoping that this change will make
you able to use the rule again.

The rule now simplifies:
- `{ a = 1, b = 2 }.a` to `1`. Thanks [@miniBill]! [#35]
- `{ foo | b = 1 }.a` to `foo.a`. Thanks [@miniBill]! [#37]
- `if a == "a" then if a == "b" then 1 else 2 else 3` to `if a == "a" then 2 else 3`

## [2.0.18] - 2022-08-14

Improves the error message for some simplifications.

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

[Unreleased]: https://github.com/jfmengels/elm-review-simplify/compare/v2.1.2...HEAD
[2.1.2]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.2
[2.1.1]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.1
[2.1.0]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.0
[2.0.33]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.33
[2.0.32]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.32
[2.0.31]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.31
[2.0.30]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.30
[2.0.29]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.29
[2.0.28]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.28
[2.0.27]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.27
[2.0.26]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.26
[2.0.25]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.25
[2.0.24]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.24
[2.0.23]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.23
[2.0.22]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.22
[2.0.21]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.21
[2.0.20]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.20
[2.0.19]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.19
[2.0.18]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.18
[2.0.17]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.17
[2.0.16]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.16
[2.0.15]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.15
[2.0.14]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.14
[2.0.13]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.13
[2.0.12]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.0.12

[#31]: https://github.com/jfmengels/elm-review-simplify/pull/31
[#35]: https://github.com/jfmengels/elm-review-simplify/pull/35
[#37]: https://github.com/jfmengels/elm-review-simplify/pull/37
[#40]: https://github.com/jfmengels/elm-review-simplify/pull/40
[#48]: https://github.com/jfmengels/elm-review-simplify/pull/48
[#52]: https://github.com/jfmengels/elm-review-simplify/pull/52

[@miniBill]: https://github.com/miniBill

[`expectNaN`]: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN