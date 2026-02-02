# Changelog

## [Unreleased]

Bug fixes:
- `(f << g) a` (any applied composition with a single argument) was previously fixed to `f <| g a` without parens which could lead to compile errors with precedence, for example in the fixed code `f <| g a == y`

## [2.1.14] - 2026-01-30

- Disabled `List.sum` and `List.product` simplifications on list reordering operations: `List.reverse`, `List.sort`, `List.sortBy`, `List.sortWith`.
  Applying floating point operations across elements using lots of intermediate temporary numbers can lose some amount of precision, and importantly the order in which the numbers are traversed will influence how much precision is lost. An illustrative example by [Sébastien Besnier](https://github.com/sebsheep):
  ```elm
  List.sum (1 :: List.repeat 100000 (2^(-53)))
  ```
  will result in `1` because the tiny fraction gets "swallowed" by the much larger accumulated value whereas
  ```elm
  List.sum (List.reverse (1 :: List.repeat 100000 (2^(-53))))
  ```
  will result in a more correct `1.0000000000111022`.
  
  You can read more about this problem in the [documentation of javascript's `Math.sumPrecise()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sumPrecise#description)

The rule now simplifies:
- `(\x -> x) data` to `data`
- `(\_ -> x) <| f <| y` to `x` (previously not always applied in pipelines)
- `(\()) -> x) <| f <| y` to `x` (previously not always applied in pipelines)
- `(\a _ -> x) y z` to `(\a -> x) y` (previously only the first argument would get removed)
- `f >> (\_ -> x)` to `(\_ -> x)`
- `(f >> g) data` to `g <| f data`
- `(f >> g) <| data` to `g <| f <| data`
- `data |> (g << f)` to `data |> f |> g`
- `List.map f (List.repeat n a)` to `List.repeat n (f a)`
- `Array.map f (Array.repeat n a)` to `Array.repeat n (f a)`
- `String.map f (String.repeat n (String.fromChar c))` to `String.repeat n (String.fromChar (f c))`
- `List.head (List.repeat n a)` to `if n >= 1 then Just a else Nothing`
- `Set.fromList (List.repeat n a)` to `if n >= 1 then Set.singleton a else Set.empty` when [`expectNaN`] is not enabled
- `Dict.fromList (List.repeat n a)` to `if n >= 1 then Dict.fromList [ a ] else Dict.empty` when [`expectNaN`] is not enabled
- `List.any f (List.repeat n a)` to `n >= 1 && f a`
- `List.member needle (List.repeat n b)` to `n >= 1 && needle == b`
- `List.all f (List.repeat n a)` to `n <= 0 || f a`
- `List.all (always False) list` to `List.isEmpty list`
- `String.all (always False) str` to `String.isEmpty str`
- `List.any (always True) list` to `not (List.isEmpty list)`
- `String.any (always True) str` to `not (String.isEmpty str)`
- `List.any identity (List.map f list)` to `List.any f list`
- `List.all identity (List.map f list)` to `List.all f list`
- `List.isEmpty (List.filter f list)` to `not (List.any f list)`
- `List.isEmpty (List.filter (not << f) list)` to `List.all f list`
- `not (List.any (not << f) list)` to `List.all f list`
- `not (List.all (not << f) list)` to `List.any f list`
- `String.map f ""` to `""`
- `String.map identity str` to `str`
- `String.dropLeft n ""` to `""` (same for `String.dropRight`)
- `String.dropLeft 0 str` or `String.dropLeft -1 str` to `""` (same for `String.dropRight`)
- `String.dropLeft 10 "Hello"` to `""` (same for `String.dropRight`)
- `String.uncons ""` to `Nothing`
- `String.filter f (String.filter f str)` to `String.filter f str`
- `String.filter (always True) str` to `str`
- `String.filter (always False) str` to `""`
- `String.any f ""` to `False`
- `String.any (always False) str` to `False`
- `String.all f ""` to `True`
- `String.all (always True) str` to `True`
- `String.toLower ""` to `""`
- `String.toLower (String.toLower str)` to `String.toLower str`
- `String.toUpper ""` to `""`
- `String.toUpper (String.toUpper str)` to `String.toUpper str`
- `String.trimLeft ""` to `""`
- `String.trimLeft (String.trimLeft str)` to `String.trimLeft str`
- `String.trimLeft (String.trimRight str)` to `String.trim str`
- `String.trimLeft (String.trim str)` to `String.trim str`
- `String.trimRight ""` to `""`
- `String.trimRight (String.trimLeft str)` to `String.trim str`
- `String.trimRight (String.trimRight str)` to `String.trimRight str`
- `String.trimRight (String.trim str)` to `String.trim str`
- `String.trim ""` to `""`
- `String.trim (String.trimLeft str)` to `String.trim str`
- `String.trim (String.trimRight str)` to `String.trim str`
- `String.trim (String.trim str)` to `String.trim str`
- `List.isEmpty (List.reverse list)` to `List.isEmpty list` (same for `List.sort`, `List.sortBy`, `List.sortWith`, `List.map` and `List.indexedMap`)
- `List.length (List.reverse list)` to `List.length list` (same for `List.sort`, `List.sortBy`, `List.sortWith`, `List.map` and `List.indexedMap`)
- `Array.isEmpty (Array.map f array)` to `Array.isEmpty array` (same for `Array.indexedMap` and `Array.set`)
- `Array.length (Array.map f array)` to `Array.length array` (same for `Array.indexedMap` and `Array.set`)
- `Array.fromList (List.repeat n a)` to `Array.repeat n a`
- `String.length (String.reverse str)` to `String.length str`
- `String.isEmpty (String.map f str)` to `String.isEmpty str` (same for `String.reverse`)
- `Set.isEmpty (Set.map f set)` to `Set.isEmpty set`
- `Dict.isEmpty (Dict.map f dict)` to `Dict.isEmpty dict`
- `Dict.size (Dict.map f dict)` to `Dict.size dict`
- `List.isEmpty (String.toList string)` to `String.isEmpty string`
- `String.isEmpty (String.fromList list)` to `List.isEmpty list`
- `List.filter f (List.reverse list)` to `List.reverse (List.filter f list)` (same for `List.sort`, `List.sortBy` and `List.sortWith`)
- `String.filter f (String.reverse str)` to `String.reverse (String.filter f str)`
- `Set.filter (\k -> k /= specificKey) set` to `Set.remove specificKey set` when [`expectNaN`] is not enabled
- `Dict.filter (\k _ -> k /= specificKey) dict` to `Dict.remove specificKey dict` when [`expectNaN`] is not enabled
- `Array.initialize n (always a)` to `Array.repeat n a`
- more string sizes can be determined, to simplify for example `String.isEmpty (String.fromInt str)` to `False`
- `Array.map f (Array.initialize n identity)` to `Array.initialize n f`
- `Array.fromList (List.range 0 n)` to `Array.initialize (n + 1) identity`
- `Array.fromList (List.map f (List.range 0 n))` to `Array.initialize (n + 1) f`
- `List.map Tuple.first (Array.toIndexedList array)` to `List.range 0 (Array.length array - 1)`
- comparison operations like `List.length l >= min -1 n` to `True` where intervals can be determined to always pass or fail the comparison

Bug fixes:
- `String.length` simplifications incorrectly assumed that a `Char` always has length 1 (2-part UTF-16 characters don't follow that rule, try in `elm repl`: `String.length (String.fromChar '👀')`). As a result, code like `String.length (String.fromList [ a, b ])` now does not get reported and simplified to `2`
- `List.length` was fixed to an off-by-one number on `List.range`. For example, previously `List.length (List.range 3 7)` was incorrectly fixed to `4` and is now correctly fixed to `5`

## [2.1.13] - 2026-01-07

The rule now simplifies:
- `String.length str == 0` to `String.isEmpty str` (including all the variations mentioned in "Other improvements")
- `List.foldl (\( k, v ) -> f k v) init (Dict.toList dict)` to `Dict.foldl (\k v -> f k v) init dict` (same for foldr)
- `List.head (List.intersperse sep list)` to `List.head list`
- `List.sum (List.reverse list)` to `List.sum list` (same for `List.sort`, `List.sortBy`, `List.sortWith`)
- `List.product (List.reverse list)` to `List.product list` (same for `List.sort`, `List.sortBy`, `List.sortWith`)
- `List.minimum (List.reverse list)` to `List.minimum list` (same for `List.sort`, `List.sortBy`, `List.sortWith`) when [`expectNaN`] is not enabled
- `List.maximum (List.reverse list)` to `List.maximum list` (same for `List.sort`, `List.sortBy`, `List.sortWith`) when [`expectNaN`] is not enabled
- `Set.fromList (List.reverse list)` to `Set.fromList list` (same for `List.sort`, `List.sortBy`, `List.sortWith`) when [`expectNaN`] is not enabled
- `Dict.diff dict (Dict.map f remove)` to `Dict.diff dict remove`
- `Result.toMaybe (Result.mapError f result)` to `Result.toMaybe result`

Other improvements:
- The various equality checks for lengths/sizes against 0 are now also supported for composition into partially applied prefix operators and also `case` expressions like
  ```elm
  case Set.size set of
    0 -> x
    _ -> y
  ```
  to
  ```elm
  if Set.isEmpty set then
    x
  
  else
    y
  ```
- In addition, various directional comparisons of lengths/sizes with 0/1 are now also turned into (not on) isEmpty, for example: `0 >= Set.size set` to `Set.isEmpty set`

Bug fixes:
- `Basics.toFloat`, `Basics.round`, `Basics.truncate`, `Basics.floor`, `Basics.ceiling` previously tried to simplify non-literal arguments, leading to issues like `floor (1 / 4 * 100)` being fixed to `1 / 4 * 100`

## [2.1.12] - 2026-01-06

The rule now simplifies:
- `List.head (List.map f list)` to `Maybe.map f (List.head list)`
- `List.take n (List.map f list)` to `List.map f (List.take n list)`
- `List.take n (List.indexedMap f list)` to `List.indexedMap f (List.take n list)`
- `List.drop n (List.map f list)` to `List.map f (List.drop n list)`
- `Array.get i (Array.map f array)` to `Maybe.map f (Array.get i array)`
- `Array.slice start end (Array.map f array)` to `Array.map f (Array.slice start end array)`
- `Array.slice 0 end (Array.indexedMap f array)` to `Array.indexedMap f (Array.slice 0 end array)`
- `Dict.remove k (Dict.map g dict)` to `Dict.map g (Dict.remove k dict)`
- `Dict.filter (\k _ -> f k) (Dict.map g dict)` to `Dict.map g (Dict.filter (\k _ -> f k) dict)`
- `Dict.diff (Dict.map f dict) remove` to `Dict.map f (Dict.diff dict remove)`

Bug fixes:
- `Cmd.map f (Task.attempt notIdentity task)` was incorrectly fixed to `Task.attempt f task` (same for `Task.perform`)

## [2.1.11] - 2025-12-30

- Disabled `List.concat` simplifications that merged `List.concat` without structure.
```elm
grid =
    List.concat
        [ [ O, X, X ]
        , [ O, O, X ]
        , [ X, O, O ]
        ]
--> previously
grid =
    [ O, X, X, O, O, X, X, O, O ]
```
This simplification made it sometimes harder to understand the data, and is therefore getting removed.
It will however still apply when there doesn't seem to be any structure,
i.e. when the list is one line or when all sub-items are on different lines.
- `[ 1, 2 ] ++ [ 3, 4 ]` has also partially been disabled with similar logic.

The rule now simplifies:
- `Maybe.withDefault Nothing (Maybe.map f maybe)` to `Maybe.andThen f maybe`
- `List.reverse (List.repeat n a)` to `List.repeat n a`
- `List.sort (List.repeat n a)` to `List.repeat n a`
- `List.sortBy f (List.repeat n a)` to `List.repeat n a`
- `List.sortWith f (List.repeat n a)` to `List.repeat n a`
- `Dict.update k identity dict` to `dict`
- `Dict.update k (\_ -> Nothing) dict` to `Dict.remove k dict`
- `Dict.update k (\_ -> Just v) dict` to `Dict.insert k v dict`
- `"a" ++ "" ++ x"` to `"a" ++ "x"` in more cases
- `"a" ++ "b"` to `"ab"` when the two strings start and end on the same line
- `List.filter f (List.filter f list)` to `List.filter f list`
- `Array.filter f (Array.filter f array)` to `Array.filter f array`
- `Set.filter f (Set.filter f set)` to `Set.filter f set`
- `Dict.filter f (Dict.filter f dict)` to `Dict.filter f dict`
- `Set.insert k (Set.insert k set)` to `Set.insert k set`
- `Dict.insert k v1 (Dict.insert k v0 dict)` to `Dict.insert k v1 dict`
- `Array.set i v1 (Array.set i v0 array)` to `Array.set i v1 array`
- `Set.remove k (Set.remove k set)` to `Set.remove k set`
- `Dict.remove k (Dict.remove k dict)` to `Dict.remove k dict`
- `List.take n (List.take n list)` to `List.take n list`
- `String.left n (String.left n string)` to `String.left n string`
- `String.right n (String.right n string)` to `String.right n string`
- `Basics.abs (Basics.abs n)` to `Basics.abs n`
- `Basics.abs -n` to `Basics.abs n`
- `Basics.abs 3` to `3`
- `Basics.min n n` to `n`
- `Basics.min n -n` to `-(Basics.abs n)`
- `Basics.min (Basics.min n0 n1) n0` to `Basics.min n0 n1` (any equal inner values across the two arguments, or composition)
- `Basics.min 3 4` to `3`
- `Basics.max n n` to `n`
- `Basics.max n -n` to `Basics.abs n`
- `Basics.max (Basics.max n0 n1) n0` to `Basics.max n0 n1` (any equal inner values across the two arguments, or composition)
- `Basics.max 3 4` to `4`
- `Basics.compare n n` to `EQ` when [`expectNaN`] is not enabled
- `Basics.compare 3 4` to `LT`
- `n < n` to `False`
- `n > n` to `False` when [`expectNaN`] is not enabled
- `n <= n` to `False` when [`expectNaN`] is not enabled
- `n >= n` to `False`
- `Basics.truncate 23.4` to `23` (same for `round`, `floor`, `ceiling`)
- `Set.union (Set.union set0 set1) set0` to `Set.union set0 set1` (any equal inner values across the two arguments, or composition)
- `Set.intersect (Set.intersect set0 set1) set0` to `Set.intersect set0 set1` (any equal inner values across the two arguments, or composition)
- `Dict.union (Dict.union dict0 dict1) dict0` to `Dict.union dict0 dict1` (any equal inner values across the two arguments, or composition)
- `Dict.intersect (Dict.intersect dict0 dict1) dict0` to `Dict.intersect dict0 dict1` (any equal inner values across the two arguments, or composition)
- `List.foldl f x (List.reverse list)` to `List.foldr f x list` (same for `foldr`)
- `List.foldl f x (Array.toList array)` to `Array.foldl f x array` (same for `foldr`)
- `Array.foldl f x (Array.fromList list)` to `List.foldl f x array` (same for `foldr`)
- `List.member x (Set.toList set)` to `Set.member x set` when [`expectNaN`] is not enabled
- `Set.member x (Set.fromList list)` to `List.member x list` when [`expectNaN`] is not enabled
- `Dict.member x (Dict.fromList list)` to `List.any (Tuple.first >> (==) x) list` when [`expectNaN`] is not enabled
- `Json.Encode.list f (Array.toList array)` to `Json.Encode.array f array`
- `Json.Encode.array identity (Array.map f array)` to `Json.Encode.array f array`
- `Json.Encode.array f (Array.fromList list)` to `Json.Encode.list f list`
- `Json.Encode.list identity (List.map f list)` to `Json.Encode.list f list`
- `Json.Encode.list f (Set.toList set)` to `Json.Encode.set f set`
- `Json.Encode.set identity (Set.map f set)` to `Json.Encode.set f set`
- `List.sort (Set.toList set)` to `Set.toList set`
- `List.sort (Dict.toList dict)` to `Dict.toList dict`
- `List.foldl (\v s -> f v s) init (Dict.values dict)` to `Dict.foldl (\_ v s -> f v s) init dict` (same for `foldr`)
- `List.foldl (\k s -> f k s) init (Dict.keys dict)` to `Dict.foldl (\k _ s -> f k s) init dict` (same for `foldr`)
- `Tuple.first (Tuple.mapFirst f tuple)` to `f (Tuple.first tuple)`
- `Tuple.second (Tuple.mapSecond f tuple)` to `f (Tuple.second tuple)`
- `Task.attempt identity (Task.map f task)` to `Task.attempt f task`
- `Task.perform identity (Task.map f task)` to `Task.perform f task`
- `Cmd.map f (Task.perform identity task)` to `Task.perform f task`
- `Cmd.map f (Task.attempt identity task)` to `Task.attempt f task`
- `List.foldr (++) "" list` to `String.concat list`
- `List.foldr (++) [] list` to `List.concat list`
- `List.foldr (::) [] list` to `list`
- `List.foldl (::) [] list` to `List.reverse list`
- `Set.foldr (::) [] set` to `Set.toList set`
- `Set.foldl Set.insert Set.empty set` to `set` (same for `Set.foldr`)
- `Dict.foldl Dict.insert Dict.empty dict` to `dict` (same for `Dict.foldr`)
- `Dict.foldr (\k _ ks -> k :: ks) [] dict` to `Dict.keys dict`
- `Dict.foldr (\_ v vs -> v :: vs) [] dict` to `Dict.values dict`
- `Dict.foldr (\k v kvs -> ( k, v ) :: kvs) [] dict` to `Dict.toList dict`
- `List.range n n` to `[ n ]`
- `List.minimum (List.range 2 3)` to `Just 2`
- `List.maximum (List.range 2 3)` to `Just 3`

Other improvements:
- Now recognizes more lambdas as "equivalent to identity",
  to catch issues like `Maybe.map (\(x, y) -> (x, y))`
- Now recognizes more `if`s as equal or different,
  to for example fix `(if c then 2 else 3) == (if c then 1 else 4)` to `False`
- Now evaluates `<`, `<=`, `>=`, `>` for any two comparable operands to for example fix `"a" < "b"` to `True`
- Now fixes `Tuple.first (Tuple.mapBoth changeFirst changeSecond tuple)` to `changeFirst (Tuple.first tuple)` instead of `Tuple.first (Tuple.mapFirst changeFirst tuple)` (same for second)
- Now recognizes more lambdas as equivalent, to for example detect equal branches like `if c then f else \a -> f a`

Bug fixes:
- Simplifying directly applied lambdas doesn't remove extra arguments. `(\_ a -> ...) b c` is now simplified to `(\a -> ...) c` instead of `(\a -> ...)`. 
- Some lambdas like `\a -> f a a` were incorrectly treated like they could be reduced to `f a`, leading to rare bugs when composing for example `(\n -> List.repeat n n) >> List.sort`

## [2.1.10] - 2025-11-21

The rule now simplifies (thanks to [@miniBill]):
- `String.fromInt 123` to `"123"`
- `String.fromFloat 1.23` to `"1.23"`

## [2.1.9] - 2025-06-29

The rule now runs at least 20% faster, shaving off a few seconds in larger projects (thanks to [@miniBill])

## [2.1.8] - 2025-06-08

The rule now simplifies (thanks to [@miniBill]):
- `x == []` to `List.isEmpty x`
- `x == Array.empty` to `Array.isEmpty x` (same for `Dict` and `Set`)

## [2.1.7] - 2025-06-06

The rule now simplifies:
- `Set.union (Set.singleton a) set` to `Set.insert a set`
- `Set.union set (Set.singleton a)` to `Set.insert set a`
- `Dict.union (Dict.singleton k v) dict` to `Dict.insert k v dict`

## [2.1.6] - 2025-01-12

The rule now simplifies:
- `Set.fromList [ a, a ]` to `Set.fromList [ a ]`
- `Set.member x (Set.singleton y)` to `x == y`
- `Set.member x (Set.fromList [ y, x ])` to `True`
- `Set.member -999 (Set.fromList [ 0, 1 ])` to `False`
- `Dict.fromList [ a, a ]` to `Dict.fromList [ a ]`
- `Dict.fromList [ ( a, v0 ), ( a, v1 ) ]` to `Dict.fromList [ ( a, v1 ) ]`
- `Dict.member x (Dict.fromList [ ( y, v0 ), ( x, v1 ) ])` to `True`
- `Dict.member -999 [ ( 0, v0 ), ( 1, v1 ) ]` to `False`
- `List.member -999 [ 0, 1 ]` to `False`

The rule also simplifies (thanks to [@perkee] and [@mfonism]):
- `List.length l == 0` to `List.isEmpty l`
- `List.length l /= 0` to `not (List.isEmpty l)`
- ...and the same thing for `Array.length`, `Set.size` and `Dict.size`

Other improvements:
- When having `expectNaN` enabled, we now still check expressions we know can't contain NaN like literal numbers. For example `List.member 0 [ 0, 1 ]` would previously not have been reported when expecting NaN, now it is.

## [2.1.5] - 2024-06-28

The rule now simplifies (thanks to [@morteako]):
- `Set.isEmpty (Set.fromList list)` to `List.isEmpty list`
- `Dict.isEmpty (Dict.fromList list)` to `List.isEmpty list`
- `Array.isEmpty (Array.fromList list)` to `List.isEmpty list`

Other improvements:
- Improved the error message for the `data |> (f >> g)` simplification

## [2.1.4] - 2024-04-01

The rule now simplifies unnecessary wrapping of values evaluated by a case expression:
```elm
case Just value of
    Nothing -> a
    Just (Ok b) -> c
    Just (Err d) -> e
```
is simplified to
```elm
case value of
    Ok b -> c
    Err d -> e
```
  (same with any variant, list or tuple containing either)

The rule also simplifies (thanks to [@morteako]):
- `Array.length (Array.fromList list)` to `List.length list`
- `List.length (Array.toList array)` to `Array.length array` (same for `Array.toIndexedList`)
- `List.isEmpty (Array.toList array)` to `Array.isEmpty array` (same for `Array.toIndexedList`)
- `List.length (Set.toList set)` to `Set.size set`
- `List.isEmpty (Set.toList set)` to `Set.isEmpty set`
- `List.length (Dict.toList dict)` to `Dict.size dict` (same for `Dict.values` and `Dict.keys`)
- `List.isEmpty (Dict.toList dict)` to `Dict.isEmpty dict` (same for `Dict.values` and `Dict.keys`)

Bug fixes:
- Fixed an issue where `String.words ""` would incorrectly be fixed to `[]`. Thanks [@w0rm] and [@lue-bird]! [#300]
- Fixed an issue where `String.lines ""` would incorrectly be fixed to `[]`. Thanks [@w0rm] and [@lue-bird]! [#300]

## [2.1.3] - 2023-10-23

The rule now simplifies:
- composition checks now also detect function pairs across nested compositions like `(here << ...) >> (... << there)`
- `List.sort (List.sort list)` to `List.sort list`
- `List.sortBy f (List.sortBy f list)` to `List.sortBy f list`
- `String.concat (List.repeat n str)` to `String.repeat n str`
- `String.concat (List.intersperse str strings)` to `String.join str strings`
- `Set.foldl/r f initial Set.empty` to `initial`
- `Set.foldl/r (\_ soFar -> soFar) initial set` to `initial`
- `Set.union set set` to `set`
- `Set.intersect set set` to `set`
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
- `Array.get 2 (Array.repeat 10 x)` -> `Just x`
- `Array.get 100 (Array.repeat 10 x)` -> `Nothing`
- `Array.get 2 (Array.initialize 10 f)` -> `Just (f 2)`
- `Array.get 100 (Array.initialize 10 f)` -> `Nothing`
- `Array.foldl f initial Array.empty` to `initial` (same for `Array.foldr`)
- `Array.foldl (\_ soFar -> soFar) initial array` to `initial` (same for `Array.foldr`)
- `Array.toList Array.empty` to `[]`
- `Array.toList (Array.repeat n a)` to `List.repeat n a`
- `Array.toIndexedList Array.empty` to `[]`
- `Array.slice n n array` to `Array.empty`
- `Array.slice n 0 array` to `Array.empty`
- `Array.slice a z Array.empty` to `Array.empty`
- `Array.slice 2 1 array` to `Array.empty`
- `Array.slice -1 -2 array` to `Array.empty`
- `List.map Tuple.second (Array.toIndexedList array)` to `Array.toList array`
- `Result.map f << Err` to `Err`
- `Result.andThen f << Err` to `Err`
- `Task.map f << Task.fail` to `Task.fail`
- `Task.andThen f << Task.fail` to `Task.fail`
- `Task.mapError f << Task.succeed` to `Task.succeed`
- `Task.onError f << Task.succeed` to `Task.succeed`
- `Json.Decode.map f << Json.Decode.fail` to `Json.Decode.fail`
- `Json.Decode.andThen f << Json.Decode.fail` to `Json.Decode.fail`
- `Maybe.andThen f << Just` to `f`
- `Result.andThen f << Ok` to `f`
- `Json.Decode.andThen f << Json.Decode.succeed` to `f`
- `Random.andThen f << Random.constant` to `f`
- `List.concatMap f << List.singleton` to `f`
- `Task.andThen f << Task.succeed` to `f`
- `Task.onError f << Task.fail` to `f`
- `Dict.map f Dict.empty` to `Dict.empty`
- `Dict.map (\_ value -> value) dict` to `dict`
- `Dict.filter f Dict.empty` to `Dict.empty`
- `Dict.filter (\_ _ -> True) dict` to `dict`
- `Dict.filter (\_ _ -> False) dict` to `Dict.empty`
- `Dict.remove k Dict.empty` to `Dict.empty`
- `Dict.foldl f initial Dict.empty` to `initial` (same for `Dict.foldr`)
- `Dict.foldl (\_ soFar -> soFar) initial dict` to `initial` (same for `Dict.foldr`)
- `Dict.union dict dict` to `dict`
- `Dict.intersect dict dict` to `dict`
- `Tuple.first (List.partition f list)` to `List.filter f list` (same for `Set.partition` and `Dict.partition`)
- `List.sum [ a, 0, b ]` to `List.sum [ a, b ]`
- `List.product [ a, 1, b ]` to `List.product [ a, b ]`
- `List.product [ a, 0, b ]` to `0` when [`expectNaN`] is not enabled
- `List.product [ a, 0 / 0, b ]` to `0 / 0` when [`expectNaN`] is enabled
- `List.sum [ a, 0 / 0, b ]` to `0 / 0` when [`expectNaN`] is enabled
- `List.all identity [ a, False, b ]` to `False`
- `List.any identity [ a, True, b ]` to `True`
- `List.all not [ a, True, b ]` to `False`
- `List.any not [ a, False, b ]` to `True`
- `List.any identity [ a, False, b ]` to `List.any identity [ a, b ]`
- `List.any not [ a, True, b ]` to `List.any not [ a, b ]`
- `List.all identity [ a, True, b ]` to `List.all identity [ a, b ]`
- `List.all not [ a, False, b ]` to `List.all not [ a, b ]`
- `toFloat 1` to `1`
- `round 1` to `1`
- `ceiling 1` to `1`
- `floor 1` to `1`
- `truncate 1` to `1`
- `round (toFloat n)` to `n` (and same for ceiling, floor, truncate)
- where `type alias Record = { first : Int, second : Int }`:
    - all simplifications for `(Record first second)` that already exist for literal records,
      like `(Record first second).first` to `first`
    - `.second << Record first` to `identity`
    - `.first << Record first` to `always first`
- `List.drop -1 list` to `list`
- `List.drop 3 [ a, b ]` to `[]` (same for lists with determined size like `List.singleton`)
- `List.drop 2 [ a, b, c ]` to `[ c ]`
- `Maybe.andThen (f << Just) maybe` to `Maybe.map f maybe` (same for `Result.andThen`, `List.concatMap`, `Task.andThen`, `Task.onError`, `Json.Decode.andThen`, `Random.andThen`)
- `Test.concat [ test ]` to `test`
- `Test.concat [ test0, Test.concat [ test1, test2 ], test3 ]` to `Test.concat [ test0, test1, test2, test3 ]`
- `List.concat [ a, List.concat [ b, c ], d ]` to `List.concat [ a, b, c, d ]`
- `Platform.Cmd.batch [ a, Platform.Cmd.batch [ b, c ], d ]` to `Platform.Cmd.batch [ a, b, c, d ]`
- `Platform.Sub.batch [ a, Platform.Sub.batch [ b, c ], d ]` to `Platform.Sub.batch [ a, b, c, d ]`
- `String.concat [ a, String.concat [ b, c ], d ]` to `String.concat [ a, b, c, d ]`
- `String.concat [ string ]` to `string`
- `String.concat [ string0, "", string1 ]` to `String.concat [ string0, string1 ]`

Bug fixes:
- Fixed an issue where `Dict.intersect Dict.empty` would be fixed to `Dict.empty`
- Fixed an issue where `Set.intersect Set.empty` would be fixed to `Set.empty`
- Fixed an issue where `(a |> not) == (b |> not)` would be fixed to `(a |> ) == (b |> )`
- Fixed an issue where `List.intersperse << List.singleton` would be fixed to `List.singleton`
- Fixed an issue where e.g. `List.sortBy f << g` would be fixed to `g`
- Fixed an issue where `Dict.partition (always (always True/False)) dict` would not be reported
- Fixed an issue where `List.filterMap f [ a, Nothing, b ]` would be fixed to `List.filterMap f [ a, b ]`
- Fixed an issue where `Random.map << always` would be fixed to `Random.constant`
- Fixed an issue where `List.any (\x -> x == (f x)) list` would be fixed to `List.member (f x) list`

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

All the changes in this release were contributed by [@lue-bird].

## [2.0.24] - 2023-01-20

All the changes in this release were contributed by [@lue-bird].

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

[Unreleased]: https://github.com/jfmengels/elm-review-simplify/compare/2.1.14...HEAD
[2.1.14]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.14
[2.1.13]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.13
[2.1.12]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.12
[2.1.11]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.11
[2.1.10]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.10
[2.1.9]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.9
[2.1.8]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.8
[2.1.7]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.7
[2.1.6]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.6
[2.1.5]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.5
[2.1.4]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.4
[2.1.3]: https://github.com/jfmengels/elm-review-simplify/releases/tag/2.1.3
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
[#300]: https://github.com/jfmengels/elm-review-simplify/issues/300

[@miniBill]: https://github.com/miniBill
[@lue-bird]: https://github.com/lue-bird
[@morteako]: https://github.com/morteako
[@w0rm]: https://github.com/w0rm
[@perkee]: https://github.com/perkee
[@mfonism]: https://github.com/mfonism

[`expectNaN`]: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN
