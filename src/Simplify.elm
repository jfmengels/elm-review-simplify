module Simplify exposing
    ( rule
    , Configuration, defaults, expectNaN, ignoreCaseOfForTypes
    )

{-| Reports when an expression can be simplified.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Simplify.rule Simplify.defaults
        ]

@docs rule
@docs Configuration, defaults, expectNaN, ignoreCaseOfForTypes


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplify/example --rules Simplify
```


## Simplifications

Below is the list of all kinds of simplifications this rule applies.


### Booleans

    x || True
    --> True

    x || False
    --> x

    x && True
    --> x

    x && False
    --> False

    not True
    --> False

    not (not x)
    --> x

    -- for `<`, `>`, `<=`, `>=`, `==` and `/=`
    not (a < b)
    --> a >= b


### Comparisons

    x == True
    --> x

    x /= False
    --> x

    not x == not y
    --> x == y

    anything == anything
    --> True

    anything /= anything
    --> False

    { r | a = 1 } == { r | a = 2 }
    --> False


### If expressions

    if True then x else y
    --> x

    if False then x else y
    --> y

    if condition then x else x
    --> x

    if condition then True else False
    --> condition

    if condition then False else True
    --> not condition

    a =
        if condition then
            if not condition then
                1
            else
                2
        else
            3
    --> if condition then 2 else 3


### Case expressions

    case condition of
        True -> x
        False -> y
    --> if condition then x else y

    case condition of
        False -> y
        True -> x
    --> if not condition then x else y

    -- only when no variables are introduced in the pattern
    -- and no custom types defined in the project are referenced
    case value of
        Just _ -> x
        Nothing -> x
    --> x

    -- same with any variant, list or tuple containing either
    case Just value of
        Nothing -> a
        Just (Ok b) -> c
        Just (Err d) -> e
    --> case value of
    -->     Ok b -> c
    -->     Err d -> e

Destructuring using case expressions

    case value of
        ( x, y ) ->
            x + y

    -->
    let
        ( x, y ) =
            value
    in
    x + y


### Let expressions

    let
        a =
            1
    in
    let
        b =
            1
    in
    a + b

    -->
    let
        a =
            1

        b =
            1
    in
    a + b


### Record updates

    { a | b = a.b }
    --> a

    { a | b = a.b, c = 1 }
    --> { a | c = 1 }


### Field access

    { a = b }.a
    --> b

    { a | b = c }.b
    --> c

    { a | b = c }.d
    --> a.d

    (let a = b in c).d
    --> let a = b in c.d

    (Record first second).first
    --> first


### Basics functions

    identity x
    --> x

    f >> identity
    --> f

    always x y
    --> x

    f >> always x
    --> always x

    toFloat 1
    --> 1

    round 1
    --> 1

    ceiling 1
    --> 1

    floor 1
    --> 1

    truncate 1
    --> 1

    round (toFloat n) -- same for ceiling, floor and truncate
    --> n


### Lambdas

    (\_ -> x) data
    --> x

    (\() y -> x) ()
    --> (\y -> x)

    (\_ y -> x) data
    --> (\y -> x)


### Operators

    (++) a b
    --> a ++ b

    a |> f >> g
    --> a |> f |> g


### Numbers

    n + 0
    --> n

    n - 0
    --> n

    0 - n
    --> -n

    n * 1
    --> n

    0 // n
    --> 0

    n // 0
    --> 0

    n // 1
    --> n

    n / 1
    --> n

    0 / n
    --> 0

    -(-n)
    --> n

    negate (negate n)
    --> n

    n - n
    --> 0


### Tuples

    Tuple.pair a b
    --> ( a, b )

    Tuple.first ( a, b )
    --> a

    Tuple.first (Tuple.mapSecond changeFirst tuple)
    --> Tuple.first tuple

    Tuple.first (Tuple.mapBoth changeFirst changeSecond tuple)
    --> Tuple.first (Tuple.mapFirst changeFirst tuple)

    Tuple.second ( a, b )
    --> b

    Tuple.second (Tuple.mapFirst changeSecond tuple)
    --> Tuple.second tuple

    Tuple.second (Tuple.mapBoth changeFirst changeSecond tuple)
    --> Tuple.second (Tuple.mapSecond changeSecond tuple)


### Strings

    "a" ++ ""
    --> "a"

    String.fromList []
    --> ""

    String.fromList [ a ]
    --> String.fromChar a

    String.fromList (String.toList str)
    --> str

    String.toList (String.fromList list)
    --> list

    String.isEmpty ""
    --> True

    String.isEmpty "a"
    --> False

    String.concat []
    --> ""

    String.concat [ string ]
    --> string

    String.concat [ hello, "", world ]
    --> String.concat [ hello, world ]

    String.concat [ "a", String.concat [ b, c ], d ]
    --> String.concat [ "a", b, c, d ]

    String.concat (List.repeat n str)
    --> String.repeat n str

    String.concat (List.intersperse str strings)
    --> String.join str strings

    String.append "" str
    --> str

    String.append (String.fromList [ a, b ]) (String.fromList [ c, d ])
    --> String.fromList [ a, b, c, d ]

    String.join str []
    --> ""

    String.join "" list
    --> String.concat list

    String.length "abc"
    --> 3

    String.repeat n ""
    --> ""

    String.repeat 0 str
    --> ""

    String.repeat 1 str
    --> str

    String.replace x y ""
    --> ""

    String.replace x x z
    --> z

    String.replace "x" "y" "z"
    --> "z" -- only when resulting string is unchanged

    String.words ""
    --> [ "" ]

    String.lines ""
    --> [ "" ]

    String.reverse ""
    --> ""

    String.reverse (String.fromChar a)
    --> String.fromChar a

    String.reverse (String.reverse str)
    --> str

    String.slice n n str
    --> ""

    String.slice n 0 str
    --> ""

    String.slice a z ""
    --> ""

    String.left 0 str
    --> ""

    String.left -1 str
    --> ""

    String.left n ""
    --> ""

    String.right 0 str
    --> ""

    String.right -1 str
    --> ""

    String.right n ""
    --> ""

    String.slice 2 1 str
    --> ""

    String.slice -1 -2 str
    --> ""

    -- The following simplifications for String.foldl also work for String.foldr
    String.foldl f initial ""
    --> initial

    String.foldl (\_ soFar -> soFar) initial string
    --> initial


### Maybes

    Maybe.map identity x
    --> x

    Maybe.map f Nothing
    --> Nothing

    Maybe.map f (Just x)
    --> Just (f x)

    -- the following simplifications for map2 work for all Maybe.mapN
    Maybe.map2 f firstMaybe Nothing
    --> Nothing

    Maybe.map2 f (Just a) (Just b)
    --> Just (f a b)

    Maybe.andThen f Nothing
    --> Nothing

    Maybe.andThen (always Nothing) x
    --> Nothing

    Maybe.andThen (\a -> Just b) x
    --> Maybe.map (\a -> b) x

    Maybe.andThen (\a -> if condition a then Just b else Just c) x
    --> Maybe.map (\a -> if condition a then b else c) x

    Maybe.andThen f (Just x)
    --> f x

    Maybe.withDefault x Nothing
    --> x

    Maybe.withDefault x (Just y)
    --> y


### Results

    Result.map identity x
    --> x

    Result.map f (Err x)
    --> Err x

    Result.map f (Ok x)
    --> Ok (f x)

    -- the following simplifications for map3 work for all Result.mapN
    Result.map3 f (Ok a) (Ok b) (Ok c)
    --> Ok (f a b c)

    Result.map3 f (Ok a) (Err x) thirdResult
    --> Err x

    Result.map3 f firstResult (Err x) thirdResult
    --> Result.map2 f firstResult (Err x)

    Result.mapError identity x
    --> x

    Result.mapError f (Ok x)
    --> Ok x

    Result.mapError f (Err x)
    --> Err (f x)

    Result.andThen f (Err x)
    --> Err x

    Result.andThen f (Ok x)
    --> f x

    Result.andThen (\a -> Ok b) x
    --> Result.map (\a -> b) x

    Result.withDefault x (Err y)
    --> x

    Result.withDefault x (Ok y)
    --> y

    Result.fromMaybe x (Just a)
    --> Ok a

    Result.fromMaybe x Nothing
    --> Err x

    Result.toMaybe (Ok x)
    --> Just x

    Result.toMaybe (Err e)
    --> Nothing


### Lists

    a :: []
    --> [ a ]

    a :: [ b ]
    --> [ a, b ]

    [ a ] ++ list
    --> a :: list

    [] ++ list
    --> list

    [ a, b ] ++ [ c ]
    --> [ a, b, c ]

    List.append [] ys
    --> ys

    List.append [ a, b ] [ c ]
    --> [ a, b, c ]

    List.head []
    --> Nothing

    List.head (a :: bToZ)
    --> Just a

    List.tail []
    --> Nothing

    List.tail (a :: bToZ)
    --> Just bToZ

    List.member a []
    --> False

    List.member a [ a, b, c ]
    --> True

    List.member a ([a, b] ++ list)
    --> True

    List.member a [ b ]
    --> a == b

    List.map f [] -- same for most List functions like List.filter, List.filterMap, ...
    --> []

    List.map identity list
    --> list

    List.map f [ a ]
    --> [ f a ]

    List.filter (always True) list
    --> list

    List.filter (always False) list
    --> []

    List.filterMap Just list
    --> list

    List.filterMap (\a -> if condition a then Just b else Just c) list
    --> List.map (\a -> if condition a then b else c) list

    List.filterMap (always Nothing) list
    --> []

    List.filterMap identity (List.map f list)
    --> List.filterMap f list

    List.filterMap identity [ Just x, Just y ]
    --> [ x, y ]

    List.filterMap identity [ a, Nothing, b ]
    --> List.filterMap identity [ a, b ]

    List.concat [ [ a, b ], [ c ] ]
    --> [ a, b, c ]

    List.concat [ a, [ 1 ], [ 2 ] ]
    --> List.concat [ a, [ 1, 2 ] ]

    List.concat [ a, [], b ]
    --> List.concat [ a, b ]

    List.concat [ a, List.concat [ b, c ], d ]
    --> List.concat [ a, b, c, d ]

    List.concatMap identity list
    --> List.concat list

    List.concatMap (\a -> [ b ]) list
    --> List.map (\a -> b) list

    List.concatMap f [ x ]
    --> f x

    List.concatMap (always []) list
    --> []

    List.concat (List.map f list)
    --> List.concatMap f list

    List.indexedMap (\_ value -> f value) list
    --> List.map (\value -> f value) list

    List.intersperse a []
    --> []

    List.intersperse s [ a ]
    --> [ a ]

    List.isEmpty []
    --> True

    List.isEmpty [ a ]
    --> False

    List.isEmpty (x :: xs)
    --> False

    List.sum []
    --> 0

    List.sum [ a ]
    --> a

    List.sum [ a, 0, b ]
    --> List.sum [ a, b ]

    -- when `expectNaN` is enabled
    List.sum [ a, 0 / 0, b ]
    --> 0 / 0

    List.product []
    --> 1

    List.product [ a ]
    --> a

    List.product [ a, 1, b ]
    --> List.product [ a, b ]

    --  when `expectNaN` is not enabled
    List.product [ a, 0, b ]
    --> 0

    -- when `expectNaN` is enabled
    List.product [ a, 0 / 0, b ]
    --> 0 / 0

    List.minimum []
    --> Nothing

    List.minimum [ a ]
    --> Just a

    List.maximum []
    --> Nothing

    List.maximum [ a ]
    --> Just a

    -- The following simplifications for List.foldl also work for List.foldr
    List.foldl f x []
    --> x

    List.foldl (\_ soFar -> soFar) x list
    --> x

    List.foldl (+) 0 list
    --> List.sum list

    List.foldl (+) initial list
    --> initial + List.sum list

    List.foldl (*) 1 list
    --> List.product list

    List.foldl (*) 0 list
    --> 0

    List.foldl (*) initial list
    --> initial * List.product list

    List.foldl (&&) True list
    --> List.all identity list

    List.foldl (&&) False list
    --> False

    List.foldl (||) False list
    --> List.any identity list

    List.foldl (||) True list
    --> True

    List.all f []
    --> True

    List.all (always True) list
    --> True

    List.all identity [ a, False, b ]
    --> False

    List.all not [ a, True, b ]
    --> False

    List.all identity [ a, True, b ]
    --> List.all identity [ a, b ]

    List.all not [ a, False, b ]
    --> List.all not [ a, b ]

    List.any f []
    --> True

    List.any (always False) list
    --> False

    List.any identity [ a, True, b ]
    --> True

    List.any not [ a, False, b ]
    --> True

    List.any identity [ a, False, b ]
    --> List.any identity [ a, b ]

    List.any not [ a, True, b ]
    --> List.any not [ a, b ]

    List.any ((==) x) list
    --> List.member x list

    List.range 6 3
    --> []

    List.length [ a, b, c ]
    --> 3

    List.repeat 0 x
    --> []

    List.repeat 1 x
    --> List.singleton x

    List.partition f []
    --> ( [], [] )

    List.partition (always True) list
    --> ( list, [] )

    List.partition (always False) list
    --> ( [], list )

    Tuple.first (List.partition f list)
    --> List.filter f list

    List.take 0 list
    --> []

    List.drop 0 list
    --> list

    List.drop 3 [ a, b ]
    --> []

    List.drop 2 [ a, b, c ]
    --> [ c ]

    List.reverse []
    --> []

    List.reverse [ a ]
    --> [ a ]

    List.reverse (List.reverse list)
    --> list

    List.sort (List.sort list)
    --> List.sort list

    List.sortBy (always a) list
    --> list

    List.sortBy identity list
    --> List.sort list

    List.sortBy f (List.sortBy f list)
    --> List.sortBy f list

    List.sortWith (\_ _ -> LT) list
    --> List.reverse list

    List.sortWith (\_ _ -> EQ) list
    --> list

    List.sortWith (\_ _ -> GT) list
    --> list

    -- The following simplifications for List.sort also work for List.sortBy f and List.sortWith f
    List.sort []
    --> []

    List.sort [ a ]
    --> [ a ]


    -- same for up to List.map5 when any list is empty
    List.map2 f xs []
    --> []

    List.map2 f [] ys
    --> []

    List.unzip []
    --> ( [], [] )


### Arrays

    Array.fromList []
    --> Array.empty

    Array.fromList (Array.toList array)
    --> array

    Array.toList (Array.fromList list)
    --> list

    Array.toList Array.empty
    --> []

    Array.toList (Array.repeat n a)
    --> List.repeat n a

    Array.map f Array.empty -- same for Array.filter
    --> Array.empty

    Array.map identity array
    --> array

    Array.indexedMap (\_ value -> f value) array
    --> Array.map (\value -> f value) array

    Array.isEmpty Array.empty
    --> True

    Array.repeat 0 x
    --> Array.empty

    Array.initialize 0 f
    --> Array.empty

    Array.length Array.empty
    --> 0

    Array.length (Array.fromList [ a, b, c ])
    --> 3

    Array.length (Array.repeat 3 x)
    --> 3

    Array.length (Array.initialize 3 f)
    --> 3

    Array.length (Array.repeat n x)
    --> max 0 n

    Array.length (Array.initialize n f)
    --> max 0 n

    Array.append Array.empty array
    --> array

    Array.append (Array.fromList [ a, b ]) (Array.fromList [ c, d ])
    --> Array.fromList [ a, b, c, d ]

    Array.slice n n array
    --> Array.empty

    Array.slice n 0 array
    --> Array.empty

    Array.slice a z Array.empty
    --> Array.empty

    Array.slice 2 1 array
    --> Array.empty

    Array.slice -1 -2 array
    --> Array.empty

    Array.get n Array.empty
    --> Nothing

    Array.get 1 (Array.fromList [ a, b, c ])
    --> Just b

    Array.get 100 (Array.fromList [ a, b, c ])
    --> Nothing

    Array.get -1 array
    --> Nothing

    Array.get 2 (Array.repeat 10 x)
    --> Just x

    Array.get 100 (Array.repeat 10 x)
    --> Nothing

    Array.get 2 (Array.initialize 10 f)
    --> Just (f 2)

    Array.get 100 (Array.initialize 10 f)
    --> Nothing

    Array.set n x Array.empty
    --> Array.empty

    Array.set -1 x array
    --> array

    Array.set 1 x (Array.fromList [ a, b, c ])
    --> Array.fromList [ a, x, c ]

    Array.set 100 x (Array.fromList [ a, b, c ])
    --> Array.fromList [ a, b, c ]

    -- The following simplifications for Array.foldl also work for Array.foldr
    Array.foldl f initial Array.empty
    --> initial

    Array.foldl (\_ soFar -> soFar) initial array
    --> initial

    Array.toIndexedList Array.empty
    --> []

    List.map Tuple.second (Array.toIndexedList array)
    --> Array.toList array

    Array.length (Array.fromList list)
    --> List.length list

    -- The following simplification also works for Array.toIndexedList
    List.length (Array.toList array)
    --> Array.length array

    -- The following simplification also works for Array.toIndexedList
    List.isEmpty (Array.toList array)
    --> Array.isEmpty array


### Sets

    Set.fromList []
    --> Set.empty

    Set.fromList [ a ]
    --> Set.singleton a

    Set.fromList (Set.toList set)
    --> set

    Set.fromList [ a, a ]
    --> Set.fromList [ a ]

    Set.map f Set.empty -- same for Set.filter, Set.remove...
    --> Set.empty

    Set.map identity set
    --> set

    Set.isEmpty Set.empty
    --> True

    Set.isEmpty (Set.fromList ([a] ++ list)
    --> False

    Set.member x Set.empty
    --> False

    Set.toList Set.empty
    --> []

    Set.length Set.empty
    --> 0

    Set.intersect Set.empty set
    --> Set.empty

    Set.intersect set set
    --> set

    Set.diff Set.empty set
    --> Set.empty

    Set.diff set Set.empty
    --> set

    Set.union set Set.empty
    --> set

    Set.union set set
    --> set

    Set.union (Set.fromList [ a, b ]) (Set.fromList [ c, d ])
    --> Set.fromList [ a, b, c, d]

    Set.insert x Set.empty
    --> Set.singleton x

    -- same for foldr
    List.foldl f x (Set.toList set)
    --> Set.foldl f x set

    Set.filter (\_ -> True) set
    --> set

    Set.filter (\_ -> False) set
    --> Set.empty

    Set.partition f Set.empty
    --> ( Set.empty, Set.empty )

    Set.partition (always True) set
    --> ( set, Set.empty )

    Tuple.first (Set.partition f set)
    --> Set.filter f set

    -- The following simplifications for Set.foldl also work for Set.foldr
    Set.foldl f initial Set.empty
    --> initial

    Set.foldl (\_ soFar -> soFar) initial set
    --> initial

    List.length (Set.toList set)
    --> Set.size set

    List.isEmpty (Set.toList set)
    --> Set.isEmpty set


### Dict

    Dict.fromList []
    --> Dict.empty

    Dict.fromList (Dict.toList dict)
    --> dict

    Dict.fromList [ ( key, a ), ( key, b ) ]
    --> Dict.fromList [ ( key, b ) ]

    Dict.isEmpty Dict.empty
    --> True

    Dict.toList Dict.empty
    --> []

    Dict.size Dict.empty
    --> 0

    Dict.member x Dict.empty
    --> False

    Dict.remove k Dict.empty
    --> Dict.empty

    Dict.filter f Dict.empty
    --> Dict.empty

    Dict.filter (\_ _ -> True) dict
    --> dict

    Dict.filter (\_ _ -> False) dict
    --> Dict.empty

    Dict.map f Dict.empty
    --> Dict.empty

    Dict.map (\_ value -> value) dict
    --> dict

    Dict.intersect Dict.empty dict
    --> Dict.empty

    Dict.intersect dict dict
    --> dict

    Dict.diff Dict.empty dict
    --> Dict.empty

    Dict.diff dict Dict.empty
    --> dict

    Dict.union dict Dict.empty
    --> dict

    Dict.union dict dict
    --> dict

    Dict.union (Dict.fromList [ a, b ]) (Dict.fromList [ c, d ])
    --> Dict.fromList [ c, d, a, b ]

    Dict.partition f Dict.empty
    --> ( Dict.empty, Dict.empty )

    Dict.partition (\_ _ -> True) dict
    --> ( dict, Dict.empty )

    Dict.partition (\_ _ -> False) dict
    --> ( Dict.empty, dict )

    Tuple.first (Dict.partition f dict)
    --> Dict.filter f dict

    List.map Tuple.first (Dict.toList dict)
    --> Dict.keys dict

    List.map Tuple.second (Dict.toList dict)
    --> Dict.values dict

    -- same for foldr
    Dict.foldl f initial Dict.empty
    --> initial

    Dict.foldl (\_ soFar -> soFar) initial dict
    --> initial

    -- The following simplification also works for Dict.keys, Dict.values
    List.length (Dict.toList dict)
    --> Dict.size dict

    -- The following simplification also works for Dict.keys, Dict.values
    List.isEmpty (Dict.toList dict)
    --> Dict.isEmpty dict


### Cmd / Sub

All of these also apply for `Sub`.

    Cmd.batch []
    --> Cmd.none

    Cmd.batch [ a ]
    --> a

    Cmd.batch [ a, Cmd.none, b ]
    --> Cmd.batch [ a, b ]

    Cmd.batch [ a, Cmd.batch [ b, c ], d ]
    --> Cmd.batch [ a, b, c, d ]

    Cmd.map identity cmd
    --> cmd

    Cmd.map f Cmd.none
    --> Cmd.none


### Task

    Task.map identity task
    --> task

    Task.map f (Task.fail x)
    --> Task.fail x

    Task.map f (Task.succeed a)
    --> Task.succeed (f a)

    -- the following simplifications for map3 work for all Task.mapN
    Task.map3 f (Task.succeed a) (Task.succeed b) (Task.succeed c)
    --> Task.succeed (f a b c)

    Task.map3 f (Task.succeed a) (Task.fail x) thirdTask
    --> Task.fail x

    Task.map3 f firstTask (Task.fail x) thirdTask
    --> Task.map2 f firstTask (Task.fail x)

    Task.andThen f (Task.fail x)
    --> Task.fail x

    Task.andThen f (Task.succeed a)
    --> f a

    Task.andThen Task.succeed task
    --> task

    Task.andThen (\a -> Task.succeed b) task
    --> Task.map (\a -> b) task

    Task.mapError identity task
    --> task

    Task.mapError f (Task.succeed a)
    --> Task.succeed a

    Task.mapError f (Task.fail x)
    --> Task.fail (f x)

    Task.onError f (Task.succeed a)
    --> Task.succeed a

    Task.onError f (Task.fail x)
    --> f x

    Task.onError Task.fail task
    --> task

    Task.onError (\x -> Task.fail y) task
    --> Task.mapError (\x -> y) x

    Task.sequence [ Task.succeed a, Task.succeed b ]
    --> Task.succeed [ a, b ]

    Task.sequence [ Task.succeed a, Task.fail x ]
    --> Task.fail x

    Task.sequence [ a, Task.fail x, b ]
    --> Task.sequence [ a, Task.fail x ]

    Task.sequence [ task ]
    --> Task.map List.singleton task


### Html.Attributes

    Html.Attributes.classList [ x, y, ( z, False ) ]
    --> Html.Attributes.classList [ x, y ]

    Html.Attributes.classList [ ( onlyOneThing, True ) ]
    --> Html.Attributes.class onlyOneThing


### Json.Decode

    Json.Decode.map identity decoder
    --> decoder

    Json.Decode.map f (Json.Decode.fail x)
    --> Json.Decode.fail x

    Json.Decode.map f (Json.Decode.succeed a)
    --> Json.Decode.succeed (f a)

    -- the following simplifications for map3 work for all Json.Decode.mapN
    Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.succeed b) (Json.Decode.succeed c)
    --> Json.Decode.succeed (f a b c)

    Json.Decode.map3 f (Json.Decode.succeed a) (Json.Decode.fail x) thirdDecoder
    --> Json.Decode.fail x

    Json.Decode.map3 f firstDecoder (Json.Decode.fail x) thirdDecoder
    --> Json.Decode.map2 f firstDecoder (Json.Decode.fail x)

    Json.Decode.andThen f (Json.Decode.fail x)
    --> Json.Decode.fail x

    Json.Decode.andThen f (Json.Decode.succeed a)
    --> f a

    Json.Decode.andThen Json.Decode.succeed decoder
    --> decoder

    Json.Decode.andThen (\a -> Json.Decode.succeed b) decoder
    --> Json.Decode.map (\a -> b) decoder

    Json.Decode.oneOf [ a ]
    --> a


### Parser

    Parser.oneOf [ a ]
    --> a


### Random

    Random.uniform a []
    --> Random.constant a

    Random.weighted ( weight, a ) []
    --> Random.constant a

    Random.weighted tuple []
    --> Random.constant (Tuple.first tuple)

    Random.list 0 generator
    --> Random.constant []

    Random.list 1 generator
    --> Random.map List.singleton generator

    Random.list n (Random.constant el)
    --> Random.constant (List.repeat n el)

    Random.map identity generator
    --> generator

    Random.map (always a) generator
    --> Random.constant a

    Random.map f (Random.constant x)
    --> Random.constant (f x)

    Random.andThen f (Random.constant x)
    --> f x

    Random.andThen Random.constant generator
    --> generator

    Random.andThen (\a -> Random.constant b) generator
    --> Random.map (\a -> b) generator

    Random.andThen (always thenGenerator) generator
    --> thenGenerator


### Test

    Test.concat [ test ]
    --> test

    Test.concat [ test0, Test.concat [ test1, test2 ], test3 ]
    --> Test.concat [ test0, test1, test2, test3 ]

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Project exposing (Exposed)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Elm.Type
import Fn.Array
import Fn.Basics
import Fn.Dict
import Fn.Html.Attributes
import Fn.Json.Decode
import Fn.List
import Fn.Maybe
import Fn.Parser
import Fn.Parser.Advanced
import Fn.Platform.Cmd
import Fn.Platform.Sub
import Fn.Random
import Fn.Result
import Fn.Set
import Fn.String
import Fn.Task
import Fn.Test
import Fn.Tuple
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Simplify.AstHelpers as AstHelpers exposing (emptyStringAsString, qualifiedToString)
import Simplify.Evaluate as Evaluate
import Simplify.Infer as Infer
import Simplify.Match exposing (Match(..))
import Simplify.Normalize as Normalize
import Simplify.RangeDict as RangeDict exposing (RangeDict)


{-| Rule to simplify Elm code.
-}
rule : Configuration -> Rule
rule (Configuration config) =
    Rule.newProjectRuleSchema "Simplify" initialContext
        |> Rule.withDirectDependenciesProjectVisitor (dependenciesVisitor (Set.fromList config.ignoreConstructors))
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : { config | expectNaN : Bool } -> Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config schema =
    schema
        |> Rule.withCommentsVisitor (\comments context -> ( [], commentsVisitor comments context ))
        |> Rule.withDeclarationListVisitor (\decls context -> ( [], declarationListVisitor decls context ))
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor (\expressionNode context -> expressionVisitor expressionNode config context)
        |> Rule.withExpressionExitVisitor (\node context -> ( [], expressionExitVisitor node context ))



-- CONFIGURATION


{-| Configuration for this rule. Create a new one with [`defaults`](#defaults) and use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) and [`expectNaN`](#expectNaN) to alter it.
-}
type Configuration
    = Configuration
        { ignoreConstructors : List String
        , expectNaN : Bool
        }


{-| Default configuration for this rule.

The rule aims tries to improve the code through simplifications that don't impact the behavior. An exception to this are
when the presence of `NaN` values

Use [`expectNaN`](#expectNaN) if you want to opt out of changes that can impact the behaviour of your code if you expect to work with `NaN` values.

Use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) if you want to prevent simplifying case expressions that work on custom types defined in dependencies.

    config =
        [ Simplify.rule Simplify.defaults
        ]

    -- or
    config =
        [ Simplify.defaults
            |> Simplify.expectNaN
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

-}
defaults : Configuration
defaults =
    Configuration
        { ignoreConstructors = []
        , expectNaN = False
        }


{-| Ignore some reports about types from dependencies used in case expressions.

This rule simplifies the following construct:

    module Module.Name exposing (..)

    case value of
        Just _ -> x
        Nothing -> x
    --> x

(Since `v2.0.19`) it will not try to simplify the case expression when some of the patterns references custom types constructors
defined in the project. It will only do so for custom types that are defined in dependencies (including `elm/core`).

If you do happen to want to disable this simplification for a type `Module.Name.Type`, you can configure the rule like this:

    config =
        [ Simplify.defaults
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

I personally don't recommend to use this function too much, because this could be a sign of premature abstraction, and because
I think that often [You Aren't Gonna Need this code](https://jfmengels.net/safe-dead-code-removal/#yagni-you-arent-gonna-need-it).

Please let me know by opening an issue if you do use this function, I am very curious to know;

-}
ignoreCaseOfForTypes : List String -> Configuration -> Configuration
ignoreCaseOfForTypes ignoreConstructors (Configuration config) =
    Configuration { ignoreConstructors = ignoreConstructors ++ config.ignoreConstructors, expectNaN = config.expectNaN }


{-| Usually, `elm-review-simplify` will only suggest simplifications that are safe to apply without risk of changing the original behavior.
However, when encountering [`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN)
values, some simplifications can actually impact behavior.

For instance, the following expression will evaluate to `True`:

    x == x
    --> True

However, if `x` is `NaN` or a value containing `NaN` then the expression will evaluate to `False`:

    -- given x = NaN
    x == x
    --> False

    -- given x = { a = ( NaN, 0 ) }
    x == x
    --> False

Given the potential presence of `NaN`, some simplifications become unsafe to apply:

  - `x == x` to `True`
  - `List.member x [ x ]` to `True`
  - `n * 0` to `0`

This special value is hard to recreate in Elm code both intentionally and unintentionally,
and it's therefore unlikely to be found in your application,
which is why the rule applies these simplifications by defaults.

If you somehow expect to create and encounter `NaN` values in your codebase, then you can use this function to disable these simplifications altogether.

    config =
        [ Simplify.defaults
            |> Simplify.expectNaN
            |> Simplify.rule
        ]

-}
expectNaN : Configuration -> Configuration
expectNaN (Configuration config) =
    Configuration { ignoreConstructors = config.ignoreConstructors, expectNaN = True }



-- CONTEXT


type alias ProjectContext =
    { customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    , exposedVariants : Dict ModuleName (Set String)
    , exposedRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , exposedCustomTypes : Dict ModuleName (Dict String { variantNames : Set String })
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : ModuleName
    , exposed : ExposingContext
    , commentRanges : List Range
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , importCustomTypes : Dict ModuleName (Dict String { variantNames : Set String })
    , moduleCustomTypes : Dict String { variantNames : Set String }
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , branchLocalBindings : RangeDict (Set String)
    , rangesToIgnore : RangeDict ()
    , rightSidesOfPlusPlus : RangeDict ()
    , customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    , localIgnoredCustomTypes : List Constructor
    , constructorsToIgnore : Set ( ModuleName, String )
    , inferredConstantsDict : RangeDict Infer.Inferred
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , extractSourceCode : Range -> String
    , exposedVariants : Set String
    , importLookup : ImportLookup
    }


type alias ImportLookup =
    Dict
        ModuleName
        { alias : Maybe ModuleName
        , exposed : Exposed -- includes names of found variants
        }


type alias QualifyResources a =
    { a
        | importLookup : ImportLookup
        , moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }


defaultQualifyResources : QualifyResources {}
defaultQualifyResources =
    { importLookup = implicitImports
    , localBindings = RangeDict.empty
    , moduleBindings = Set.empty
    }


type ExposingContext
    = ExposingAllContext
    | ExposingSomeContext { typesExposingVariants : Set String, potentialTypeAliases : Set String }


type Exposed
    = ExposedAll
    | ExposedSome (Set String)


isExposedFrom : Exposed -> String -> Bool
isExposedFrom exposed name =
    case exposed of
        ExposedAll ->
            True

        ExposedSome some ->
            Set.member name some


type alias ConstructorName =
    String


type alias Constructor =
    { moduleName : ModuleName
    , name : String
    , constructors : List String
    }


initialContext : ProjectContext
initialContext =
    { customTypesToReportInCases = Set.empty
    , exposedVariants = Dict.empty
    , exposedRecordTypeAliases = Dict.empty
    , exposedCustomTypes = Dict.empty
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { customTypesToReportInCases = Set.empty
            , exposedVariants =
                Dict.singleton moduleContext.moduleName
                    moduleContext.exposedVariants
            , exposedRecordTypeAliases =
                Dict.singleton moduleContext.moduleName
                    (case moduleContext.exposed of
                        ExposingAllContext ->
                            moduleContext.moduleRecordTypeAliases

                        ExposingSomeContext exposingSomeContext ->
                            Set.foldl
                                (\exposedPotentialTypeAlias soFar ->
                                    case Dict.get exposedPotentialTypeAlias moduleContext.moduleRecordTypeAliases of
                                        Nothing ->
                                            soFar

                                        Just recordTypeAlias ->
                                            Dict.insert exposedPotentialTypeAlias recordTypeAlias soFar
                                )
                                Dict.empty
                                exposingSomeContext.potentialTypeAliases
                    )
            , exposedCustomTypes =
                Dict.singleton moduleContext.moduleName
                    (case moduleContext.exposed of
                        ExposingAllContext ->
                            moduleContext.moduleCustomTypes

                        ExposingSomeContext exposingSomeContext ->
                            Set.foldl
                                (\exposedPotentialTypeAlias soFar ->
                                    case Dict.get exposedPotentialTypeAlias moduleContext.moduleCustomTypes of
                                        Nothing ->
                                            soFar

                                        Just recordTypeAlias ->
                                            Dict.insert exposedPotentialTypeAlias recordTypeAlias soFar
                                )
                                Dict.empty
                                exposingSomeContext.typesExposingVariants
                    )
            }
        )


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable metadata extractSourceCode fullAst projectContext ->
            let
                imports : ImportLookup
                imports =
                    List.foldl
                        (\import_ importLookup ->
                            let
                                importInfo : { moduleName : ModuleName, exposed : Exposed, alias : Maybe ModuleName }
                                importInfo =
                                    importContext import_
                            in
                            insertImport importInfo.moduleName { alias = importInfo.alias, exposed = importInfo.exposed } importLookup
                        )
                        implicitImports
                        fullAst.imports
            in
            { lookupTable = lookupTable
            , moduleName = Rule.moduleNameFromMetadata metadata
            , exposed =
                moduleExposingContext (Elm.Syntax.Module.exposingList (Node.value fullAst.moduleDefinition))
            , importLookup =
                createImportLookup
                    { imports = imports
                    , importExposedVariants = projectContext.exposedVariants
                    }
            , commentRanges = []
            , importRecordTypeAliases = projectContext.exposedRecordTypeAliases
            , moduleRecordTypeAliases = Dict.empty
            , importCustomTypes = projectContext.exposedCustomTypes
            , moduleCustomTypes = Dict.empty
            , moduleBindings = Set.empty
            , localBindings = RangeDict.empty
            , branchLocalBindings = RangeDict.empty
            , rangesToIgnore = RangeDict.empty
            , rightSidesOfPlusPlus = RangeDict.empty
            , localIgnoredCustomTypes = []
            , customTypesToReportInCases = projectContext.customTypesToReportInCases
            , constructorsToIgnore = Set.empty
            , inferredConstantsDict = RangeDict.empty
            , inferredConstants = ( Infer.empty, [] )
            , extractSourceCode = extractSourceCode
            , exposedVariants = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withMetadata
        |> Rule.withSourceCodeExtractor
        |> Rule.withFullAst


importContext : Node Import -> { moduleName : ModuleName, exposed : Exposed, alias : Maybe ModuleName }
importContext importNode =
    let
        import_ : Import
        import_ =
            Node.value importNode
    in
    { moduleName = import_.moduleName |> Node.value
    , alias =
        import_.moduleAlias |> Maybe.map Node.value
    , exposed =
        case import_.exposingList of
            Nothing ->
                ExposedSome Set.empty

            Just (Node _ existingExposing) ->
                case existingExposing of
                    Exposing.All _ ->
                        ExposedAll

                    Exposing.Explicit exposes ->
                        ExposedSome
                            (Set.fromList
                                (List.map
                                    (\(Node _ expose) -> AstHelpers.nameOfExpose expose)
                                    exposes
                                )
                            )
    }


createImportLookup :
    { imports : Dict ModuleName { alias : Maybe ModuleName, exposed : Exposed }
    , importExposedVariants : Dict ModuleName (Set String)
    }
    -> ImportLookup
createImportLookup context =
    context.imports
        |> Dict.map
            (\moduleName import_ ->
                case import_.exposed of
                    ExposedAll ->
                        import_

                    ExposedSome some ->
                        case Dict.get moduleName context.importExposedVariants of
                            Nothing ->
                                import_

                            Just importExposedVariants ->
                                { import_
                                    | exposed =
                                        ExposedSome
                                            (Set.union some importExposedVariants)
                                }
            )


moduleExposingContext : Exposing.Exposing -> ExposingContext
moduleExposingContext exposingSyntax =
    case exposingSyntax of
        Exposing.All _ ->
            ExposingAllContext

        Exposing.Explicit some ->
            ExposingSomeContext
                (List.foldl
                    (\(Node _ expose) soFar ->
                        case expose of
                            Exposing.InfixExpose _ ->
                                soFar

                            Exposing.FunctionExpose _ ->
                                soFar

                            Exposing.TypeOrAliasExpose name ->
                                { soFar | potentialTypeAliases = Set.insert name soFar.potentialTypeAliases }

                            Exposing.TypeExpose variantType ->
                                case variantType.open of
                                    Nothing ->
                                        soFar

                                    Just _ ->
                                        { soFar | typesExposingVariants = Set.insert variantType.name soFar.typesExposingVariants }
                    )
                    { typesExposingVariants = Set.empty
                    , potentialTypeAliases = Set.empty
                    }
                    some
                )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { customTypesToReportInCases = Set.empty
    , exposedVariants = Dict.union newContext.exposedVariants previousContext.exposedVariants
    , exposedRecordTypeAliases = Dict.union newContext.exposedRecordTypeAliases previousContext.exposedRecordTypeAliases
    , exposedCustomTypes = Dict.union newContext.exposedCustomTypes previousContext.exposedCustomTypes
    }



-- DEPENDENCIES VISITOR


dependenciesVisitor : Set String -> Dict String Dependency -> ProjectContext -> ( List (Error scope), ProjectContext )
dependenciesVisitor typeNamesAsStrings dict context =
    let
        modules : List Elm.Docs.Module
        modules =
            dict
                |> Dict.values
                |> List.concatMap Dependency.modules

        unions : Set String
        unions =
            List.concatMap (\module_ -> List.map (\union -> module_.name ++ "." ++ union.name) module_.unions) modules
                |> Set.fromList

        unknownTypesToIgnore : List String
        unknownTypesToIgnore =
            Set.diff typeNamesAsStrings unions
                |> Set.toList

        customTypesToReportInCases : Set ( ModuleName, String )
        customTypesToReportInCases =
            modules
                |> List.concatMap
                    (\mod ->
                        let
                            moduleName : ModuleName
                            moduleName =
                                AstHelpers.moduleNameFromString mod.name
                        in
                        mod.unions
                            |> List.filter (\union -> not (Set.member (mod.name ++ "." ++ union.name) typeNamesAsStrings))
                            |> List.concatMap (\union -> union.tags)
                            |> List.map (\( tagName, _ ) -> ( moduleName, tagName ))
                    )
                |> Set.fromList

        dependencyExposedVariants : Dict ModuleName (Set String)
        dependencyExposedVariants =
            List.foldl
                (\moduleDoc acc ->
                    Dict.insert
                        (AstHelpers.moduleNameFromString moduleDoc.name)
                        (moduleDoc.unions
                            |> List.concatMap
                                (\union ->
                                    union.tags
                                        |> List.map (\( variantName, _ ) -> variantName)
                                )
                            |> Set.fromList
                        )
                        acc
                )
                context.exposedVariants
                modules

        recordTypeAliases : Dict ModuleName (Dict String (List String))
        recordTypeAliases =
            modules
                |> List.foldl
                    (\moduleDocs soFar ->
                        Dict.insert (AstHelpers.moduleNameFromString moduleDocs.name)
                            (moduleDocs.aliases
                                |> List.filterMap
                                    (\typeAliasDocs ->
                                        case typeAliasDocs.tipe of
                                            Elm.Type.Record fields Nothing ->
                                                Just ( typeAliasDocs.name, List.map (\( name, _ ) -> name) fields )

                                            _ ->
                                                Nothing
                                    )
                                |> Dict.fromList
                            )
                            soFar
                    )
                    Dict.empty

        exposedCustomTypes : Dict ModuleName (Dict String { variantNames : Set String })
        exposedCustomTypes =
            Dict.union
                (dict
                    |> Dict.values
                    |> List.concatMap
                        (\dependency ->
                            dependency
                                |> Dependency.modules
                                |> List.map
                                    (\moduleDocs ->
                                        ( moduleDocs.name |> AstHelpers.moduleNameFromString
                                        , moduleDocs.unions
                                            |> List.map
                                                (\choiceTypeDocs ->
                                                    ( choiceTypeDocs.name
                                                    , { variantNames =
                                                            choiceTypeDocs.tags |> List.map (\( name, _ ) -> name) |> Set.fromList
                                                      }
                                                    )
                                                )
                                            |> Dict.fromList
                                        )
                                    )
                        )
                    |> Dict.fromList
                )
                context.exposedCustomTypes
    in
    ( if List.isEmpty unknownTypesToIgnore then
        []

      else
        [ errorForUnknownIgnoredConstructor unknownTypesToIgnore ]
    , { customTypesToReportInCases = customTypesToReportInCases
      , exposedVariants = dependencyExposedVariants
      , exposedRecordTypeAliases = recordTypeAliases
      , exposedCustomTypes = exposedCustomTypes
      }
    )


errorForUnknownIgnoredConstructor : List String -> Error scope
errorForUnknownIgnoredConstructor list =
    Rule.globalError
        { message = "Could not find type names: " ++ (String.join ", " <| List.map wrapInBackticks list)
        , details =
            [ "I expected to find these custom types in the dependencies, but I could not find them."
            , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
            , "If you find that these types have been moved or renamed, please update your configuration."
            , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
            , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
            ]
        }



-- COMMENTS VISITOR


commentsVisitor : List (Node String) -> ModuleContext -> ModuleContext
commentsVisitor comments context =
    { context | commentRanges = List.map Node.range comments }



-- DECLARATION LIST VISITOR


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ModuleContext
declarationListVisitor declarationList context =
    { context
        | moduleBindings = AstHelpers.declarationListBindings declarationList
        , moduleRecordTypeAliases =
            List.foldl
                (\(Node _ declaration) soFar ->
                    case declaration of
                        Declaration.AliasDeclaration typeAliasDeclaration ->
                            case typeAliasDeclaration.typeAnnotation of
                                Node _ (TypeAnnotation.Record fields) ->
                                    Dict.insert (Node.value typeAliasDeclaration.name)
                                        (List.map (\(Node _ ( Node _ field, _ )) -> field) fields)
                                        soFar

                                _ ->
                                    soFar

                        _ ->
                            soFar
                )
                Dict.empty
                declarationList
        , moduleCustomTypes =
            List.foldl
                (\(Node _ declaration) soFar ->
                    case declaration of
                        Declaration.CustomTypeDeclaration variantType ->
                            Dict.insert (Node.value variantType.name)
                                { variantNames =
                                    variantType.constructors
                                        |> List.map (\(Node _ variant) -> Node.value variant.name)
                                        |> Set.fromList
                                }
                                soFar

                        _ ->
                            soFar
                )
                Dict.empty
                declarationList
    }



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor declarationNode context =
    case Node.value declarationNode of
        Declaration.CustomTypeDeclaration variantType ->
            let
                variantsAreExposed : Bool
                variantsAreExposed =
                    case context.exposed of
                        ExposingAllContext ->
                            True

                        ExposingSomeContext exposingSome ->
                            Set.member (Node.value variantType.name) exposingSome.typesExposingVariants
            in
            if variantsAreExposed then
                let
                    exposedVariants : Set String
                    exposedVariants =
                        List.foldl
                            (\(Node _ variant) acc -> Set.insert (Node.value variant.name) acc)
                            context.exposedVariants
                            variantType.constructors
                in
                { context | exposedVariants = exposedVariants }

            else
                context

        Declaration.FunctionDeclaration functionDeclaration ->
            { context
                | rangesToIgnore = RangeDict.empty
                , rightSidesOfPlusPlus = RangeDict.empty
                , inferredConstantsDict = RangeDict.empty
                , localBindings =
                    RangeDict.singleton
                        (Node.range functionDeclaration.declaration)
                        (AstHelpers.patternListBindings (Node.value functionDeclaration.declaration).arguments)
            }

        _ ->
            context



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> { config | expectNaN : Bool } -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node config context =
    let
        expressionRange : Range
        expressionRange =
            Node.range node

        contextWithInferredConstants : ModuleContext
        contextWithInferredConstants =
            case RangeDict.get expressionRange context.inferredConstantsDict of
                Nothing ->
                    context

                Just inferredConstants ->
                    let
                        ( previous, previousStack ) =
                            context.inferredConstants
                    in
                    { context
                        | inferredConstants = ( inferredConstants, previous :: previousStack )
                    }
    in
    if RangeDict.member expressionRange context.rangesToIgnore then
        ( [], contextWithInferredConstants )

    else
        let
            expression : Expression
            expression =
                Node.value node

            withExpressionSurfaceBindings : RangeDict (Set String)
            withExpressionSurfaceBindings =
                RangeDict.insert expressionRange (expressionSurfaceBindings expression) context.localBindings

            withNewBranchLocalBindings : RangeDict (Set String)
            withNewBranchLocalBindings =
                RangeDict.union (expressionBranchLocalBindings expression)
                    context.branchLocalBindings

            contextWithInferredConstantsAndLocalBindings : ModuleContext
            contextWithInferredConstantsAndLocalBindings =
                case RangeDict.get expressionRange context.branchLocalBindings of
                    Nothing ->
                        { contextWithInferredConstants
                            | localBindings = withExpressionSurfaceBindings
                            , branchLocalBindings =
                                withNewBranchLocalBindings
                        }

                    Just currentBranchLocalBindings ->
                        { contextWithInferredConstants
                            | localBindings =
                                RangeDict.insert expressionRange currentBranchLocalBindings withExpressionSurfaceBindings
                            , branchLocalBindings =
                                RangeDict.remove expressionRange withNewBranchLocalBindings
                        }

            expressionChecked : { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
            expressionChecked =
                expressionVisitorHelp node config contextWithInferredConstantsAndLocalBindings
        in
        ( expressionChecked.error |> maybeToList
        , { contextWithInferredConstantsAndLocalBindings
            | rangesToIgnore = RangeDict.union expressionChecked.rangesToIgnore context.rangesToIgnore
            , rightSidesOfPlusPlus = RangeDict.union expressionChecked.rightSidesOfPlusPlus context.rightSidesOfPlusPlus
            , inferredConstantsDict =
                List.foldl (\( range, constants ) acc -> RangeDict.insert range constants acc)
                    contextWithInferredConstants.inferredConstantsDict
                    expressionChecked.inferredConstants
          }
        )


{-| From the `elm/core` readme:

>
> ### Default Imports

> The modules in this package are so common, that some of them are imported by default in all Elm files. So it is as if every Elm file starts with these imports:
>
>     import Basics exposing (..)
>     import List exposing (List, (::))
>     import Maybe exposing (Maybe(..))
>     import Result exposing (Result(..))
>     import String exposing (String)
>     import Char exposing (Char)
>     import Tuple
>     import Debug
>     import Platform exposing (Program)
>     import Platform.Cmd as Cmd exposing (Cmd)
>     import Platform.Sub as Sub exposing (Sub)

-}
implicitImports : ImportLookup
implicitImports =
    [ ( [ "Basics" ], { alias = Nothing, exposed = ExposedAll } )
    , ( [ "List" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "List", "(::)" ]) } )
    , ( [ "Maybe" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "Maybe", "Just", "Nothing" ]) } )
    , ( [ "Result" ], { alias = Nothing, exposed = ExposedSome (Set.fromList [ "Result", "Ok", "Err" ]) } )
    , ( [ "String" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "String") } )
    , ( [ "Char" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "Char") } )
    , ( [ "Tuple" ], { alias = Nothing, exposed = ExposedSome Set.empty } )
    , ( [ "Debug" ], { alias = Nothing, exposed = ExposedSome Set.empty } )
    , ( [ "Platform" ], { alias = Nothing, exposed = ExposedSome (Set.singleton "Program") } )
    , ( [ "Platform", "Cmd" ], { alias = Just [ "Cmd" ], exposed = ExposedSome (Set.singleton "Cmd") } )
    , ( [ "Platform", "Sub" ], { alias = Just [ "Sub" ], exposed = ExposedSome (Set.singleton "Sub") } )
    ]
        |> Dict.fromList


{-| Merge a given new import with an existing import lookup.
This is strongly preferred over Dict.insert since the implicit default imports can be overridden
-}
insertImport : ModuleName -> { alias : Maybe ModuleName, exposed : Exposed } -> ImportLookup -> ImportLookup
insertImport moduleName importInfoToAdd importLookup =
    Dict.update moduleName
        (\existingImport ->
            let
                newImportInfo : { alias : Maybe ModuleName, exposed : Exposed }
                newImportInfo =
                    case existingImport of
                        Nothing ->
                            importInfoToAdd

                        Just import_ ->
                            { alias = findMap .alias [ import_, importInfoToAdd ]
                            , exposed = exposedMerge ( import_.exposed, importInfoToAdd.exposed )
                            }
            in
            Just newImportInfo
        )
        importLookup


exposedMerge : ( Exposed, Exposed ) -> Exposed
exposedMerge exposedTuple =
    case exposedTuple of
        ( ExposedAll, _ ) ->
            ExposedAll

        ( ExposedSome _, ExposedAll ) ->
            ExposedAll

        ( ExposedSome aSet, ExposedSome bSet ) ->
            ExposedSome (Set.union aSet bSet)


qualify : ( ModuleName, String ) -> QualifyResources a -> ( ModuleName, String )
qualify ( moduleName, name ) qualifyResources =
    let
        qualification : ModuleName
        qualification =
            case qualifyResources.importLookup |> Dict.get moduleName of
                Nothing ->
                    moduleName

                Just import_ ->
                    let
                        moduleImportedName : ModuleName
                        moduleImportedName =
                            import_.alias |> Maybe.withDefault moduleName
                    in
                    if not (isExposedFrom import_.exposed name) then
                        moduleImportedName

                    else
                        let
                            isShadowed : Bool
                            isShadowed =
                                isBindingInScope qualifyResources name
                        in
                        if isShadowed then
                            moduleImportedName

                        else
                            []
    in
    ( qualification, name )


isBindingInScope :
    { a
        | moduleBindings : Set String
        , localBindings : RangeDict (Set String)
    }
    -> String
    -> Bool
isBindingInScope resources name =
    Set.member name resources.moduleBindings
        || RangeDict.any (\bindings -> Set.member name bindings) resources.localBindings


{-| Whenever you add ranges on expression enter, the same ranges should be removed on expression exit.
Having one function finding unique ranges and a function for extracting bindings there ensures said consistency.

An alternative approach would be to use some kind of tree structure
with parent and sub ranges and bindings as leaves (maybe a "trie", tho I've not seen one as an elm package).

Removing all bindings for an expression's range on leave would then be trivial

-}
expressionSurfaceBindings : Expression -> Set String
expressionSurfaceBindings expression =
    case expression of
        Expression.LambdaExpression lambda ->
            AstHelpers.patternListBindings lambda.args

        Expression.LetExpression letBlock ->
            AstHelpers.letDeclarationListBindings letBlock.declarations

        _ ->
            Set.empty


expressionBranchLocalBindings : Expression -> RangeDict (Set String)
expressionBranchLocalBindings expression =
    case expression of
        Expression.CaseExpression caseBlock ->
            RangeDict.mapFromList
                (\( Node _ pattern, Node resultRange _ ) ->
                    ( resultRange
                    , AstHelpers.patternBindings pattern
                    )
                )
                caseBlock.cases

        Expression.LetExpression letBlock ->
            List.foldl
                (\(Node _ letDeclaration) acc ->
                    case letDeclaration of
                        Expression.LetFunction letFunctionOrValueDeclaration ->
                            RangeDict.insert
                                (Node.range (Node.value letFunctionOrValueDeclaration.declaration).expression)
                                (AstHelpers.patternListBindings
                                    (Node.value letFunctionOrValueDeclaration.declaration).arguments
                                )
                                acc

                        _ ->
                            acc
                )
                RangeDict.empty
                letBlock.declarations

        _ ->
            RangeDict.empty


expressionExitVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionExitVisitor (Node expressionRange _) context =
    let
        contextWithUpdatedLocalBindings : ModuleContext
        contextWithUpdatedLocalBindings =
            if RangeDict.member expressionRange context.rangesToIgnore then
                context

            else
                { context
                    | localBindings =
                        RangeDict.remove expressionRange context.localBindings
                }
    in
    if RangeDict.member expressionRange context.inferredConstantsDict then
        case Tuple.second context.inferredConstants of
            topOfStack :: restOfStack ->
                { contextWithUpdatedLocalBindings | inferredConstants = ( topOfStack, restOfStack ) }

            [] ->
                -- should never be empty
                contextWithUpdatedLocalBindings

    else
        contextWithUpdatedLocalBindings


maybeErrorAndRangesToIgnore : Maybe (Error {}) -> RangeDict () -> { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
maybeErrorAndRangesToIgnore maybeError rangesToIgnore =
    { error = maybeError
    , rangesToIgnore = rangesToIgnore
    , rightSidesOfPlusPlus = RangeDict.empty
    , inferredConstants = []
    }


onlyMaybeError : Maybe (Error {}) -> { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
onlyMaybeError maybeError =
    { error = maybeError
    , rangesToIgnore = RangeDict.empty
    , rightSidesOfPlusPlus = RangeDict.empty
    , inferredConstants = []
    }


expressionVisitorHelp : Node Expression -> { config | expectNaN : Bool } -> ModuleContext -> { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
expressionVisitorHelp (Node expressionRange expression) config context =
    let
        toCheckInfo :
            { fnRange : Range
            , fn : ( ModuleName, String )
            , argCount : Int
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            , callStyle : FunctionCallStyle
            }
            -> CallCheckInfo
        toCheckInfo checkInfo =
            let
                ( parentRange, callStyle ) =
                    case List.drop (checkInfo.argCount - 1) (checkInfo.firstArg :: checkInfo.argsAfterFirst) of
                        lastExpectedArg :: _ :: _ ->
                            -- Too many arguments!
                            -- We'll update the range to drop the extra ones and force the call style to application
                            ( case checkInfo.callStyle of
                                Application ->
                                    { start = checkInfo.fnRange.start, end = (Node.range lastExpectedArg).end }

                                Pipe LeftToRight ->
                                    { start = checkInfo.fnRange.start, end = (Node.range lastExpectedArg).end }

                                Pipe RightToLeft ->
                                    { start = (Node.range checkInfo.firstArg).start, end = (Node.range checkInfo.firstArg).end }
                            , Application
                            )

                        -- [] | _ :: [] ->
                        _ ->
                            ( expressionRange, checkInfo.callStyle )

                argsAfterFirst : List (Node Expression)
                argsAfterFirst =
                    -- Drop the extra arguments
                    List.take (checkInfo.argCount - 1) checkInfo.argsAfterFirst
            in
            { lookupTable = context.lookupTable
            , expectNaN = config.expectNaN
            , extractSourceCode = context.extractSourceCode
            , importLookup = context.importLookup
            , commentRanges = context.commentRanges
            , moduleBindings = context.moduleBindings
            , localBindings = context.localBindings
            , inferredConstants = context.inferredConstants
            , parentRange = parentRange
            , fnRange = checkInfo.fnRange
            , fn = checkInfo.fn
            , argCount = checkInfo.argCount
            , firstArg = checkInfo.firstArg
            , argsAfterFirst = argsAfterFirst
            , secondArg = List.head argsAfterFirst
            , thirdArg = List.head (List.drop 1 argsAfterFirst)
            , callStyle = callStyle
            }

        toCompositionCheckInfo :
            { earlier : Node Expression
            , later : Node Expression
            }
            -> CompositionCheckInfo
        toCompositionCheckInfo compositionSpecific =
            let
                innerComposition :
                    { earlier :
                        { node : Node Expression, removeRange : Range }
                    , later :
                        { node : Node Expression, removeRange : Range }
                    , isEmbeddedInComposition : Bool
                    }
                innerComposition =
                    getInnerComposition compositionSpecific
            in
            { lookupTable = context.lookupTable
            , importLookup = context.importLookup
            , importRecordTypeAliases = context.importRecordTypeAliases
            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
            , inferredConstants = context.inferredConstants
            , moduleBindings = context.moduleBindings
            , localBindings = context.localBindings
            , extractSourceCode = context.extractSourceCode
            , earlier = innerComposition.earlier
            , later = innerComposition.later
            , isEmbeddedInComposition = innerComposition.isEmbeddedInComposition
            }
    in
    case expression of
        -----------------
        -- APPLICATION --
        -----------------
        Expression.Application (applied :: firstArg :: argsAfterFirst) ->
            onlyMaybeError
                (case applied of
                    Node fnRange (Expression.FunctionOrValue _ fnName) ->
                        case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                            Just moduleName ->
                                case Dict.get ( moduleName, fnName ) functionCallChecks of
                                    Just ( argCount, checkFn ) ->
                                        checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , fn = ( moduleName, fnName )
                                                , argCount = argCount
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsAfterFirst
                                                , callStyle = Application
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing

                    Node _ (Expression.ParenthesizedExpression (Node lambdaRange (Expression.LambdaExpression lambda))) ->
                        appliedLambdaError
                            { nodeRange = expressionRange
                            , lambdaRange = lambdaRange
                            , lambda = lambda
                            }

                    Node operatorRange (Expression.PrefixOperator operator) ->
                        case argsAfterFirst of
                            right :: [] ->
                                Just
                                    (fullyAppliedPrefixOperatorError
                                        { operator = operator
                                        , operatorRange = operatorRange
                                        , left = firstArg
                                        , right = right
                                        }
                                    )

                            _ ->
                                Nothing

                    otherApplied ->
                        case AstHelpers.getRecordAccessFunction otherApplied of
                            Just fieldName ->
                                accessingRecordChecks
                                    { parentRange = Range.combine [ Node.range applied, Node.range firstArg ]
                                    , record = firstArg
                                    , fieldRange = Node.range otherApplied
                                    , fieldName = fieldName
                                    , importRecordTypeAliases = context.importRecordTypeAliases
                                    , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                                    , lookupTable = context.lookupTable
                                    }
                                    |> Maybe.map (\e -> Rule.errorWithFix e.info (Node.range otherApplied) e.fix)

                            Nothing ->
                                Nothing
                )

        ----------
        -- (<|) --
        ----------
        Expression.OperatorApplication "<|" _ pipedInto lastArg ->
            case pipedInto of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    onlyMaybeError
                        (case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                            Just moduleName ->
                                case Dict.get ( moduleName, fnName ) functionCallChecks of
                                    Just ( argCount, checkFn ) ->
                                        checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , fn = ( moduleName, fnName )
                                                , argCount = argCount
                                                , firstArg = lastArg
                                                , argsAfterFirst = []
                                                , callStyle = Pipe RightToLeft
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                        )

                Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsBetweenFirstAndLast)) ->
                    case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                        Just moduleName ->
                            case Dict.get ( moduleName, fnName ) functionCallChecks of
                                Just ( argCount, checkFn ) ->
                                    maybeErrorAndRangesToIgnore
                                        (checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , argCount = argCount
                                                , fn = ( moduleName, fnName )
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                                , callStyle = Pipe RightToLeft
                                                }
                                            )
                                        )
                                        (RangeDict.singleton applicationRange ())

                                Nothing ->
                                    onlyMaybeError Nothing

                        Nothing ->
                            onlyMaybeError Nothing

                pipedIntoOther ->
                    onlyMaybeError
                        (pipelineChecks
                            { commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , direction = RightToLeft
                            , nodeRange = expressionRange
                            , pipedInto = pipedIntoOther
                            , arg = lastArg
                            , importRecordTypeAliases = context.importRecordTypeAliases
                            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                            , lookupTable = context.lookupTable
                            }
                        )

        ----------
        -- (|>) --
        ----------
        Expression.OperatorApplication "|>" _ lastArg pipedInto ->
            case pipedInto of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    onlyMaybeError
                        (case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                            Just moduleName ->
                                case Dict.get ( moduleName, fnName ) functionCallChecks of
                                    Just ( argCount, checks ) ->
                                        checks
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , fn = ( moduleName, fnName )
                                                , argCount = argCount
                                                , firstArg = lastArg
                                                , argsAfterFirst = []
                                                , callStyle = Pipe LeftToRight
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing
                        )

                Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsBetweenFirstAndLast)) ->
                    case ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange of
                        Just moduleName ->
                            case Dict.get ( moduleName, fnName ) functionCallChecks of
                                Just ( argCount, checks ) ->
                                    maybeErrorAndRangesToIgnore
                                        (checks
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , fn = ( moduleName, fnName )
                                                , argCount = argCount
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                                , callStyle = Pipe LeftToRight
                                                }
                                            )
                                        )
                                        (RangeDict.singleton applicationRange ())

                                Nothing ->
                                    onlyMaybeError Nothing

                        Nothing ->
                            onlyMaybeError Nothing

                pipedIntoOther ->
                    onlyMaybeError
                        (pipelineChecks
                            { commentRanges = context.commentRanges
                            , extractSourceCode = context.extractSourceCode
                            , direction = LeftToRight
                            , nodeRange = expressionRange
                            , pipedInto = pipedIntoOther
                            , arg = lastArg
                            , importRecordTypeAliases = context.importRecordTypeAliases
                            , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                            , lookupTable = context.lookupTable
                            }
                        )

        ----------
        -- (>>) --
        ----------
        Expression.OperatorApplication ">>" _ earlier composedLater ->
            onlyMaybeError
                (firstThatConstructsJust compositionChecks
                    (toCompositionCheckInfo { earlier = earlier, later = composedLater })
                )

        ----------
        -- (<<) --
        ----------
        Expression.OperatorApplication "<<" _ composedLater earlier ->
            onlyMaybeError
                (firstThatConstructsJust compositionChecks
                    (toCompositionCheckInfo { earlier = earlier, later = composedLater })
                )

        ---------------------
        -- OTHER OPERATION --
        ---------------------
        Expression.OperatorApplication operator _ left right ->
            case Dict.get operator operatorApplicationChecks of
                Just checkFn ->
                    { error =
                        let
                            leftRange : Range
                            leftRange =
                                Node.range left

                            rightRange : Range
                            rightRange =
                                Node.range right
                        in
                        checkFn
                            { lookupTable = context.lookupTable
                            , extractSourceCode = context.extractSourceCode
                            , expectNaN = config.expectNaN
                            , importLookup = context.importLookup
                            , moduleBindings = context.moduleBindings
                            , localBindings = context.localBindings
                            , inferredConstants = context.inferredConstants
                            , parentRange = expressionRange
                            , operator = operator
                            , operatorRange =
                                findOperatorRange
                                    { operator = operator
                                    , commentRanges = context.commentRanges
                                    , extractSourceCode = context.extractSourceCode
                                    , leftRange = leftRange
                                    , rightRange = rightRange
                                    }
                            , left = left
                            , leftRange = leftRange
                            , right = right
                            , rightRange = rightRange
                            , isOnTheRightSideOfPlusPlus = RangeDict.member expressionRange context.rightSidesOfPlusPlus
                            }
                    , rangesToIgnore = RangeDict.empty
                    , rightSidesOfPlusPlus =
                        case operator of
                            "++" ->
                                RangeDict.singleton (Node.range (AstHelpers.removeParens right)) ()

                            _ ->
                                RangeDict.empty
                    , inferredConstants = []
                    }

                Nothing ->
                    onlyMaybeError Nothing

        --------------
        -- NEGATION --
        --------------
        Expression.Negation negatedExpression ->
            onlyMaybeError
                (negationChecks { parentRange = expressionRange, negatedExpression = negatedExpression })

        -------------------
        -- RECORD ACCESS --
        -------------------
        Expression.RecordAccess record (Node fieldRange fieldName) ->
            onlyMaybeError
                (accessingRecordChecks
                    { parentRange = expressionRange
                    , record = record
                    , fieldRange = fieldRange
                    , fieldName = fieldName
                    , importRecordTypeAliases = context.importRecordTypeAliases
                    , moduleRecordTypeAliases = context.moduleRecordTypeAliases
                    , lookupTable = context.lookupTable
                    }
                    |> Maybe.map (\e -> Rule.errorWithFix e.info { start = (Node.range record).end, end = fieldRange.end } e.fix)
                )

        --------
        -- IF --
        --------
        Expression.IfBlock condition trueBranch falseBranch ->
            let
                ifCheckInfo : IfCheckInfo
                ifCheckInfo =
                    { nodeRange = expressionRange
                    , condition = condition
                    , trueBranch = trueBranch
                    , falseBranch = falseBranch
                    , lookupTable = context.lookupTable
                    , inferredConstants = context.inferredConstants
                    , importLookup = context.importLookup
                    , moduleBindings = context.moduleBindings
                    , localBindings = context.localBindings
                    }
            in
            case ifChecks ifCheckInfo of
                Just ifErrors ->
                    maybeErrorAndRangesToIgnore (Just ifErrors.errors) ifErrors.rangesToIgnore

                Nothing ->
                    { error = Nothing
                    , rangesToIgnore = RangeDict.empty
                    , rightSidesOfPlusPlus = RangeDict.empty
                    , inferredConstants =
                        Infer.inferForIfCondition
                            (Node.value (Normalize.normalize context condition))
                            { trueBranchRange = Node.range trueBranch
                            , falseBranchRange = Node.range falseBranch
                            }
                            (Tuple.first context.inferredConstants)
                    }

        -------------
        -- CASE OF --
        -------------
        Expression.CaseExpression caseBlock ->
            onlyMaybeError
                (firstThatConstructsJust caseOfChecks
                    { lookupTable = context.lookupTable
                    , moduleCustomTypes = context.moduleCustomTypes
                    , importCustomTypes = context.importCustomTypes
                    , extractSourceCode = context.extractSourceCode
                    , customTypesToReportInCases = context.customTypesToReportInCases
                    , inferredConstants = context.inferredConstants
                    , parentRange = expressionRange
                    , caseOf = caseBlock
                    }
                )

        ------------
        -- LET IN --
        ------------
        Expression.LetExpression caseBlock ->
            onlyMaybeError (letInChecks caseBlock)

        -------------------
        -- RECORD UPDATE --
        -------------------
        Expression.RecordUpdateExpression variable fields ->
            onlyMaybeError (recordUpdateChecks expressionRange variable fields)

        --------------------
        -- NOT SIMPLIFIED --
        --------------------
        Expression.UnitExpr ->
            onlyMaybeError Nothing

        Expression.CharLiteral _ ->
            onlyMaybeError Nothing

        Expression.Integer _ ->
            onlyMaybeError Nothing

        Expression.Hex _ ->
            onlyMaybeError Nothing

        Expression.Floatable _ ->
            onlyMaybeError Nothing

        Expression.Literal _ ->
            onlyMaybeError Nothing

        Expression.GLSLExpression _ ->
            onlyMaybeError Nothing

        Expression.PrefixOperator _ ->
            onlyMaybeError Nothing

        Expression.RecordAccessFunction _ ->
            onlyMaybeError Nothing

        Expression.FunctionOrValue _ _ ->
            onlyMaybeError Nothing

        Expression.ParenthesizedExpression _ ->
            onlyMaybeError Nothing

        Expression.TupledExpression _ ->
            onlyMaybeError Nothing

        Expression.ListExpr _ ->
            onlyMaybeError Nothing

        Expression.RecordExpr _ ->
            onlyMaybeError Nothing

        Expression.LambdaExpression _ ->
            onlyMaybeError Nothing

        ----------------------
        -- IMPOSSIBLE CASES --
        ----------------------
        Expression.Operator _ ->
            onlyMaybeError Nothing

        Expression.Application [] ->
            onlyMaybeError Nothing

        Expression.Application (_ :: []) ->
            onlyMaybeError Nothing


type alias OperatorApplicationCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , extractSourceCode : Range -> String
    , expectNaN : Bool
    , importLookup : ImportLookup
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , parentRange : Range
    , operator : String
    , operatorRange : Range
    , left : Node Expression
    , leftRange : Range
    , right : Node Expression
    , rightRange : Range
    , isOnTheRightSideOfPlusPlus : Bool
    }


operatorApplicationChecks : Dict String (OperatorApplicationCheckInfo -> Maybe (Error {}))
operatorApplicationChecks =
    Dict.fromList
        [ ( "+", plusChecks )
        , ( "-", minusChecks )
        , ( "*", multiplyChecks )
        , ( "/", divisionChecks )
        , ( "//", intDivideChecks )
        , ( "++", plusplusChecks )
        , ( "::", consChecks )
        , ( "||", orChecks )
        , ( "&&", andChecks )
        , ( "==", equalityChecks True )
        , ( "/=", equalityChecks False )
        , ( "<", numberComparisonChecks (<) )
        , ( ">", numberComparisonChecks (>) )
        , ( "<=", numberComparisonChecks (<=) )
        , ( ">=", numberComparisonChecks (>=) )
        ]


type alias CallCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , expectNaN : Bool
    , importLookup : ImportLookup
    , extractSourceCode : Range -> String
    , commentRanges : List Range
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , parentRange : Range
    , fnRange : Range
    , fn : ( ModuleName, String )
    , argCount : Int
    , callStyle : FunctionCallStyle
    , firstArg : Node Expression
    , argsAfterFirst : List (Node Expression)

    -- stored for quick access since usage is very common
    -- prefer using secondArg and thirdArg functions
    -- because the optimization could change in the future
    , secondArg : Maybe (Node Expression)
    , thirdArg : Maybe (Node Expression)
    }


{-| How an argument is given as input to a function:

  - `Pipe RightToLeft`: `function <| argument`
  - `Pipe LeftToRight`: `argument |> function`
  - `Application`: `function argument`

-}
type FunctionCallStyle
    = Application
    | Pipe LeftOrRightDirection


type LeftOrRightDirection
    = RightToLeft
    | LeftToRight


secondArg : CallCheckInfo -> Maybe (Node Expression)
secondArg checkInfo =
    checkInfo.secondArg


thirdArg : CallCheckInfo -> Maybe (Node Expression)
thirdArg checkInfo =
    checkInfo.thirdArg


type alias CompositionIntoCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , importLookup : ImportLookup
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , extractSourceCode : Range -> String
    , later :
        { range : Range
        , fn : ( ModuleName, String )
        , fnRange : Range
        , args : List (Node Expression)
        , -- how many arguments a fully applied call would have
          argCount : Int
        , removeRange : Range
        }
    , earlier :
        { range : Range
        , fn : ( ModuleName, String )
        , fnRange : Range
        , args : List (Node Expression)
        , removeRange : Range
        }
    , isEmbeddedInComposition : Bool
    }


type alias ErrorInfoAndFix =
    { info : { message : String, details : List String }
    , fix : List Fix
    }


{-| Checking both the function call of and composition into a specific fn.

Construct the record directly or use `intoFnCheckOnlyCall`/`intoFnCheckOnlyComposition`.
Provide multiple checks using `intoFnChecksFirstThatConstructsError`.

-}
type alias IntoFnCheck =
    { call : CallCheckInfo -> Maybe (Error {})
    , composition : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
    }


{-| There is no equivalent composition check.
-}
intoFnCheckOnlyCall : (CallCheckInfo -> Maybe (Error {})) -> IntoFnCheck
intoFnCheckOnlyCall callFnCheck =
    { call = callFnCheck, composition = \_ -> Nothing }


{-| There is no equivalent call check.
-}
intoFnCheckOnlyComposition : (CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix) -> IntoFnCheck
intoFnCheckOnlyComposition compositionIntoFnCheck =
    { composition = compositionIntoFnCheck, call = \_ -> Nothing }


{-| Try the given `IntoFnCheck`s in order and report the first found error.
-}
intoFnChecksFirstThatConstructsError : List IntoFnCheck -> IntoFnCheck
intoFnChecksFirstThatConstructsError intoFnCheckList =
    { call =
        \checkInfo ->
            findMap (\fnCheck -> fnCheck.call checkInfo) intoFnCheckList
    , composition =
        \checkInfo ->
            findMap (\fnCheck -> fnCheck.composition checkInfo) intoFnCheckList
    }


intoFnChecks : Dict ( ModuleName, String ) ( Int, IntoFnCheck )
intoFnChecks =
    -- The number of arguments is used to determine how many arguments to pass to the check function.
    -- This corresponds to the number of arguments that the function to check is expected to have.
    -- Any additional arguments will be ignored in order to avoid removing too many arguments
    -- when replacing the entire argument, which is quite common.
    Dict.fromList
        [ ( Fn.Basics.identity, ( 1, basicsIdentityChecks ) )
        , ( Fn.Basics.always, ( 2, basicsAlwaysChecks ) )
        , ( Fn.Basics.not, ( 1, basicsNotChecks ) )
        , ( Fn.Basics.negate, ( 1, basicsNegateChecks ) )
        , ( Fn.Basics.toFloat, ( 1, basicsToFloatChecks ) )
        , ( Fn.Basics.round, ( 1, intToIntChecks ) )
        , ( Fn.Basics.ceiling, ( 1, intToIntChecks ) )
        , ( Fn.Basics.floor, ( 1, intToIntChecks ) )
        , ( Fn.Basics.truncate, ( 1, intToIntChecks ) )
        , ( Fn.Tuple.first, ( 1, tupleFirstChecks ) )
        , ( Fn.Tuple.second, ( 1, tupleSecondChecks ) )
        , ( Fn.Tuple.pair, ( 2, tuplePairChecks ) )
        , ( Fn.Maybe.map, ( 2, maybeMapChecks ) )
        , ( Fn.Maybe.map2, ( 3, maybeMapNChecks ) )
        , ( Fn.Maybe.map3, ( 4, maybeMapNChecks ) )
        , ( Fn.Maybe.map4, ( 5, maybeMapNChecks ) )
        , ( Fn.Maybe.map5, ( 6, maybeMapNChecks ) )
        , ( Fn.Maybe.andThen, ( 2, maybeAndThenChecks ) )
        , ( Fn.Maybe.withDefault, ( 2, maybeWithDefaultChecks ) )
        , ( Fn.Result.map, ( 2, resultMapChecks ) )
        , ( Fn.Result.map2, ( 3, resultMapNChecks ) )
        , ( Fn.Result.map3, ( 4, resultMapNChecks ) )
        , ( Fn.Result.map4, ( 5, resultMapNChecks ) )
        , ( Fn.Result.map5, ( 6, resultMapNChecks ) )
        , ( Fn.Result.mapError, ( 2, resultMapErrorChecks ) )
        , ( Fn.Result.andThen, ( 2, resultAndThenChecks ) )
        , ( Fn.Result.withDefault, ( 2, resultWithDefaultChecks ) )
        , ( Fn.Result.toMaybe, ( 1, resultToMaybeChecks ) )
        , ( Fn.Result.fromMaybe, ( 2, resultFromMaybeWithEmptyValueOnNothingCheck ) )
        , ( Fn.List.append, ( 2, listAppendChecks ) )
        , ( Fn.List.head, ( 1, listHeadChecks ) )
        , ( Fn.List.tail, ( 1, listTailChecks ) )
        , ( Fn.List.member, ( 2, listMemberChecks ) )
        , ( Fn.List.map, ( 2, listMapChecks ) )
        , ( Fn.List.filter, ( 2, listFilterChecks ) )
        , ( Fn.List.filterMap, ( 2, listFilterMapChecks ) )
        , ( Fn.List.concat, ( 1, listConcatChecks ) )
        , ( Fn.List.concatMap, ( 2, listConcatMapChecks ) )
        , ( Fn.List.indexedMap, ( 2, listIndexedMapChecks ) )
        , ( Fn.List.intersperse, ( 2, listIntersperseChecks ) )
        , ( Fn.List.sum, ( 1, listSumChecks ) )
        , ( Fn.List.product, ( 1, listProductChecks ) )
        , ( Fn.List.minimum, ( 1, listMinimumChecks ) )
        , ( Fn.List.maximum, ( 1, listMaximumChecks ) )
        , ( Fn.List.foldl, ( 3, listFoldlChecks ) )
        , ( Fn.List.foldr, ( 3, listFoldrChecks ) )
        , ( Fn.List.all, ( 2, listAllChecks ) )
        , ( Fn.List.any, ( 2, listAnyChecks ) )
        , ( Fn.List.range, ( 2, listRangeChecks ) )
        , ( Fn.List.length, ( 1, listLengthChecks ) )
        , ( Fn.List.repeat, ( 2, listRepeatChecks ) )
        , ( Fn.List.isEmpty, ( 1, listIsEmptyChecks ) )
        , ( Fn.List.partition, ( 2, listPartitionChecks ) )
        , ( Fn.List.reverse, ( 1, listReverseChecks ) )
        , ( Fn.List.sort, ( 1, listSortChecks ) )
        , ( Fn.List.sortBy, ( 2, listSortByChecks ) )
        , ( Fn.List.sortWith, ( 2, listSortWithChecks ) )
        , ( Fn.List.take, ( 2, listTakeChecks ) )
        , ( Fn.List.drop, ( 2, listDropChecks ) )
        , ( Fn.List.map2, ( 3, listMapNChecks ) )
        , ( Fn.List.map3, ( 4, listMapNChecks ) )
        , ( Fn.List.map4, ( 5, listMapNChecks ) )
        , ( Fn.List.map5, ( 6, listMapNChecks ) )
        , ( Fn.List.unzip, ( 1, listUnzipChecks ) )
        , ( Fn.Array.toList, ( 1, arrayToListChecks ) )
        , ( Fn.Array.toIndexedList, ( 1, arrayToIndexedListChecks ) )
        , ( Fn.Array.fromList, ( 1, arrayFromListChecks ) )
        , ( Fn.Array.map, ( 2, arrayMapChecks ) )
        , ( Fn.Array.indexedMap, ( 2, arrayIndexedMapChecks ) )
        , ( Fn.Array.filter, ( 2, arrayFilterChecks ) )
        , ( Fn.Array.isEmpty, ( 1, arrayIsEmptyChecks ) )
        , ( Fn.Array.length, ( 1, arrayLengthChecks ) )
        , ( Fn.Array.repeat, ( 2, arrayRepeatChecks ) )
        , ( Fn.Array.initialize, ( 2, arrayInitializeChecks ) )
        , ( Fn.Array.append, ( 2, arrayAppendChecks ) )
        , ( Fn.Array.get, ( 2, arrayGetChecks ) )
        , ( Fn.Array.set, ( 3, arraySetChecks ) )
        , ( Fn.Array.slice, ( 3, arraySliceChecks ) )
        , ( Fn.Array.foldl, ( 3, arrayFoldlChecks ) )
        , ( Fn.Array.foldr, ( 3, arrayFoldrChecks ) )
        , ( Fn.Set.map, ( 2, setMapChecks ) )
        , ( Fn.Set.filter, ( 2, setFilterChecks ) )
        , ( Fn.Set.remove, ( 2, setRemoveChecks ) )
        , ( Fn.Set.isEmpty, ( 1, setIsEmptyChecks ) )
        , ( Fn.Set.size, ( 1, setSizeChecks ) )
        , ( Fn.Set.member, ( 2, setMemberChecks ) )
        , ( Fn.Set.fromList, ( 1, setFromListChecks ) )
        , ( Fn.Set.toList, ( 1, setToListChecks ) )
        , ( Fn.Set.partition, ( 2, setPartitionChecks ) )
        , ( Fn.Set.intersect, ( 2, setIntersectChecks ) )
        , ( Fn.Set.diff, ( 2, setDiffChecks ) )
        , ( Fn.Set.union, ( 2, setUnionChecks ) )
        , ( Fn.Set.insert, ( 2, setInsertChecks ) )
        , ( Fn.Set.foldl, ( 3, setFoldlChecks ) )
        , ( Fn.Set.foldr, ( 3, setFoldrChecks ) )
        , ( Fn.Dict.isEmpty, ( 1, dictIsEmptyChecks ) )
        , ( Fn.Dict.fromList, ( 1, dictFromListChecks ) )
        , ( Fn.Dict.toList, ( 1, dictToListChecks ) )
        , ( Fn.Dict.size, ( 1, dictSizeChecks ) )
        , ( Fn.Dict.member, ( 2, dictMemberChecks ) )
        , ( Fn.Dict.remove, ( 2, dictRemoveChecks ) )
        , ( Fn.Dict.filter, ( 2, dictFilterChecks ) )
        , ( Fn.Dict.partition, ( 2, dictPartitionChecks ) )
        , ( Fn.Dict.map, ( 2, dictMapChecks ) )
        , ( Fn.Dict.intersect, ( 2, dictIntersectChecks ) )
        , ( Fn.Dict.diff, ( 2, dictDiffChecks ) )
        , ( Fn.Dict.union, ( 2, dictUnionChecks ) )
        , ( Fn.Dict.foldl, ( 3, dictFoldlChecks ) )
        , ( Fn.Dict.foldr, ( 3, dictFoldrChecks ) )
        , ( Fn.String.toList, ( 1, stringToListChecks ) )
        , ( Fn.String.fromList, ( 1, stringFromListChecks ) )
        , ( Fn.String.isEmpty, ( 1, stringIsEmptyChecks ) )
        , ( Fn.String.concat, ( 1, stringConcatChecks ) )
        , ( Fn.String.join, ( 2, stringJoinChecks ) )
        , ( Fn.String.length, ( 1, stringLengthChecks ) )
        , ( Fn.String.repeat, ( 2, stringRepeatChecks ) )
        , ( Fn.String.replace, ( 3, stringReplaceChecks ) )
        , ( Fn.String.words, ( 1, stringWordsChecks ) )
        , ( Fn.String.lines, ( 1, stringLinesChecks ) )
        , ( Fn.String.reverse, ( 1, stringReverseChecks ) )
        , ( Fn.String.slice, ( 3, stringSliceChecks ) )
        , ( Fn.String.left, ( 2, stringLeftChecks ) )
        , ( Fn.String.right, ( 2, stringRightChecks ) )
        , ( Fn.String.append, ( 2, stringAppendChecks ) )
        , ( Fn.String.foldl, ( 3, stringFoldlChecks ) )
        , ( Fn.String.foldr, ( 3, stringFoldrChecks ) )
        , ( Fn.Platform.Cmd.batch, ( 1, platformCmdBatchChecks ) )
        , ( Fn.Platform.Cmd.map, ( 2, platformCmdMapChecks ) )
        , ( Fn.Platform.Sub.batch, ( 1, platformSubBatchChecks ) )
        , ( Fn.Platform.Sub.map, ( 2, platformSubChecks ) )
        , ( Fn.Task.map, ( 2, taskMapChecks ) )
        , ( Fn.Task.map2, ( 3, taskMapNChecks ) )
        , ( Fn.Task.map3, ( 4, taskMapNChecks ) )
        , ( Fn.Task.map4, ( 5, taskMapNChecks ) )
        , ( Fn.Task.map5, ( 6, taskMapNChecks ) )
        , ( Fn.Task.andThen, ( 2, taskAndThenChecks ) )
        , ( Fn.Task.mapError, ( 2, taskMapErrorChecks ) )
        , ( Fn.Task.onError, ( 2, taskOnErrorChecks ) )
        , ( Fn.Task.sequence, ( 1, taskSequenceChecks ) )
        , ( Fn.Json.Decode.oneOf, ( 1, jsonDecodeOneOfChecks ) )
        , ( Fn.Json.Decode.map, ( 2, jsonDecodeMapChecks ) )
        , ( Fn.Json.Decode.map2, ( 3, jsonDecodeMapNChecks ) )
        , ( Fn.Json.Decode.map3, ( 4, jsonDecodeMapNChecks ) )
        , ( Fn.Json.Decode.map4, ( 5, jsonDecodeMapNChecks ) )
        , ( Fn.Json.Decode.map5, ( 6, jsonDecodeMapNChecks ) )
        , ( Fn.Json.Decode.map6, ( 7, jsonDecodeMapNChecks ) )
        , ( Fn.Json.Decode.map7, ( 8, jsonDecodeMapNChecks ) )
        , ( Fn.Json.Decode.map8, ( 9, jsonDecodeMapNChecks ) )
        , ( Fn.Json.Decode.andThen, ( 2, jsonDecodeAndThenChecks ) )
        , ( Fn.Html.Attributes.classList, ( 1, htmlAttributesClassListChecks ) )
        , ( Fn.Parser.oneOf, ( 1, parserOneOfChecks ) )
        , ( Fn.Parser.Advanced.oneOf, ( 1, parserAdvancedOneOfChecks ) )
        , ( Fn.Random.uniform, ( 2, randomUniformChecks ) )
        , ( Fn.Random.weighted, ( 2, randomWeightedChecks ) )
        , ( Fn.Random.list, ( 2, randomListChecks ) )
        , ( Fn.Random.map, ( 2, randomMapChecks ) )
        , ( Fn.Random.andThen, ( 2, randomAndThenChecks ) )
        , ( Fn.Test.concat, ( 1, testConcatChecks ) )
        ]


functionCallChecks : Dict ( ModuleName, String ) ( Int, CallCheckInfo -> Maybe (Error {}) )
functionCallChecks =
    Dict.map (\_ ( argCount, checks ) -> ( argCount, checks.call )) intoFnChecks


type alias CompositionCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , importLookup : ImportLookup
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , extractSourceCode : Range -> String
    , earlier :
        { node : Node Expression
        , removeRange : Range
        }
    , later :
        { node : Node Expression
        , removeRange : Range
        }
    , isEmbeddedInComposition : Bool
    }


compositionChecks : List (CompositionCheckInfo -> Maybe (Error {}))
compositionChecks =
    [ accessingRecordCompositionChecks
    , basicsIdentityCompositionChecks
    , \checkInfo ->
        case
            ( AstHelpers.getValueOrFnOrFnCall checkInfo.earlier.node
            , AstHelpers.getValueOrFnOrFnCall checkInfo.later.node
            )
        of
            ( Just earlierFnOrCall, Just laterFnOrCall ) ->
                case
                    ( ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable earlierFnOrCall.fnRange
                    , ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable laterFnOrCall.fnRange
                    )
                of
                    ( Just earlierFnModuleName, Just laterFnModuleName ) ->
                        case Dict.get ( laterFnModuleName, laterFnOrCall.fnName ) compositionIntoChecks of
                            Just ( laterArgCount, compositionIntoChecksForSpecificLater ) ->
                                compositionIntoChecksForSpecificLater
                                    { lookupTable = checkInfo.lookupTable
                                    , importLookup = checkInfo.importLookup
                                    , inferredConstants = checkInfo.inferredConstants
                                    , moduleBindings = checkInfo.moduleBindings
                                    , localBindings = checkInfo.localBindings
                                    , extractSourceCode = checkInfo.extractSourceCode
                                    , later =
                                        { range = laterFnOrCall.nodeRange
                                        , fn = ( laterFnModuleName, laterFnOrCall.fnName )
                                        , fnRange = laterFnOrCall.fnRange
                                        , args = laterFnOrCall.args
                                        , argCount = laterArgCount
                                        , removeRange = checkInfo.later.removeRange
                                        }
                                    , earlier =
                                        { range = earlierFnOrCall.nodeRange
                                        , fn = ( earlierFnModuleName, earlierFnOrCall.fnName )
                                        , fnRange = earlierFnOrCall.fnRange
                                        , args = earlierFnOrCall.args
                                        , removeRange = checkInfo.earlier.removeRange
                                        }
                                    , isEmbeddedInComposition = checkInfo.isEmbeddedInComposition
                                    }
                                    |> Maybe.map (\e -> Rule.errorWithFix e.info laterFnOrCall.fnRange e.fix)

                            Nothing ->
                                Nothing

                    ( Nothing, _ ) ->
                        Nothing

                    ( _, Nothing ) ->
                        Nothing

            ( Nothing, _ ) ->
                Nothing

            ( _, Nothing ) ->
                Nothing
    ]


compositionIntoChecks : Dict ( ModuleName, String ) ( Int, CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix )
compositionIntoChecks =
    Dict.map (\_ ( argCount, checks ) -> ( argCount, checks.composition )) intoFnChecks


findOperatorRange :
    { extractSourceCode : Range -> String
    , commentRanges : List Range
    , operator : String
    , leftRange : Range
    , rightRange : Range
    }
    -> Range
findOperatorRange context =
    let
        betweenOperands : String
        betweenOperands =
            context.extractSourceCode
                { start = context.leftRange.end, end = context.rightRange.start }

        operatorStartLocationFound : Maybe Location
        operatorStartLocationFound =
            String.indexes context.operator betweenOperands
                |> findMap
                    (\operatorOffset ->
                        let
                            operatorStartLocation : Location
                            operatorStartLocation =
                                offsetInStringToLocation
                                    { offset = operatorOffset
                                    , startLocation = context.leftRange.end
                                    , source = betweenOperands
                                    }

                            isPartOfComment : Bool
                            isPartOfComment =
                                List.any
                                    (\commentRange ->
                                        rangeContainsLocation operatorStartLocation commentRange
                                    )
                                    context.commentRanges
                        in
                        if isPartOfComment then
                            Nothing

                        else
                            Just operatorStartLocation
                    )
    in
    case operatorStartLocationFound of
        Just operatorStartLocation ->
            { start = operatorStartLocation
            , end =
                { row = operatorStartLocation.row
                , column = operatorStartLocation.column + String.length context.operator
                }
            }

        -- there's a bug somewhere
        Nothing ->
            Range.emptyRange


offsetInStringToLocation : { offset : Int, source : String, startLocation : Location } -> Location
offsetInStringToLocation config =
    case config.source |> String.left config.offset |> String.lines |> List.reverse of
        [] ->
            config.startLocation

        onlyLine :: [] ->
            { row = config.startLocation.row
            , column = config.startLocation.column + String.length onlyLine
            }

        lineWithOffsetLocation :: _ :: linesBeforeBeforeWithOffsetLocation ->
            { row = config.startLocation.row + 1 + List.length linesBeforeBeforeWithOffsetLocation
            , column = 1 + String.length lineWithOffsetLocation
            }


plusChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
plusChecks =
    firstThatConstructsJust
        [ addingZeroCheck
        , addingOppositesCheck
        ]


addingZeroCheck : OperatorApplicationCheckInfo -> Maybe (Error {})
addingZeroCheck checkInfo =
    findMap
        (\side ->
            if AstHelpers.getUncomputedNumberValue side.node == Just 0 then
                Just
                    (Rule.errorWithFix
                        { message = "Unnecessary adding 0"
                        , details = [ "You can replace this operation by the " ++ side.otherDescription ++ " number you added 0 to." ]
                        }
                        (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
                    )

            else
                Nothing
        )
        (operationSides checkInfo)


addingOppositesCheck : OperatorApplicationCheckInfo -> Maybe (Error {})
addingOppositesCheck checkInfo =
    if checkInfo.expectNaN then
        Nothing

    else
        case Normalize.compare checkInfo checkInfo.left (Node Range.emptyRange (Expression.Negation checkInfo.right)) of
            Normalize.ConfirmedEquality ->
                Just
                    (Rule.errorWithFix
                        { message = "Adding opposite numbers will result in 0"
                        , details = [ "Adding two numbers with an equal absolute value and an opposite sign will cancel each other out. You can replace this operation by 0." ]
                        }
                        checkInfo.parentRange
                        [ Fix.replaceRangeBy checkInfo.parentRange "0" ]
                    )

            Normalize.ConfirmedInequality ->
                Nothing

            Normalize.Unconfirmed ->
                Nothing


minusChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
minusChecks checkInfo =
    if AstHelpers.getUncomputedNumberValue checkInfo.right == Just 0 then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary subtracting 0"
                , details = [ "You can replace this operation by the left number you subtracted 0 from." ]
                }
                checkInfo.operatorRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.left })
            )

    else if AstHelpers.getUncomputedNumberValue checkInfo.left == Just 0 then
        Just
            (Rule.errorWithFix
                { message = "Subtracting from 0 is the same as negating"
                , details = [ "You can replace this operation by the negated right number you subtracted from 0, like `-n`." ]
                }
                checkInfo.operatorRange
                (replaceBySubExpressionFix checkInfo.parentRange checkInfo.right
                    ++ [ Fix.insertAt checkInfo.parentRange.start "-" ]
                )
            )

    else
        checkIfMinusResultsInZero checkInfo


checkIfMinusResultsInZero : OperatorApplicationCheckInfo -> Maybe (Error {})
checkIfMinusResultsInZero checkInfo =
    if checkInfo.expectNaN then
        Nothing

    else
        case Normalize.compare checkInfo checkInfo.left checkInfo.right of
            Normalize.ConfirmedEquality ->
                Just
                    (Rule.errorWithFix
                        { message = "Subtracting equal numbers will result in 0"
                        , details = [ "You can replace this operation by 0." ]
                        }
                        checkInfo.parentRange
                        [ Fix.replaceRangeBy checkInfo.parentRange "0" ]
                    )

            Normalize.ConfirmedInequality ->
                Nothing

            Normalize.Unconfirmed ->
                Nothing


multiplyChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
multiplyChecks =
    firstThatConstructsJust
        [ \checkInfo -> findMap (\side -> unnecessaryOperationWithEmptySideChecks numberForMultiplyProperties side checkInfo) (operationSides checkInfo)
        , \checkInfo ->
            findMap
                (\side ->
                    if numberNotExpectingNaNForMultiplyProperties.absorbing.is (extractInferResources checkInfo) side.node then
                        Just
                            (Rule.errorWithFix
                                { message = "Multiplication by 0 should be replaced"
                                , details =
                                    [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                    , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                    , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                    , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                    ]
                                }
                                (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                                (if checkInfo.expectNaN then
                                    []

                                 else
                                    [ Fix.replaceRangeBy checkInfo.parentRange "0" ]
                                )
                            )

                    else
                        Nothing
                )
                (operationSides checkInfo)
        ]


operationSides : OperatorApplicationCheckInfo -> List { node : Node Expression, otherNode : Node Expression, otherDescription : String }
operationSides checkInfo =
    [ { node = checkInfo.left, otherNode = checkInfo.right, otherDescription = "right" }
    , { node = checkInfo.right, otherNode = checkInfo.left, otherDescription = "left" }
    ]


divisionChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
divisionChecks checkInfo =
    let
        maybeDivisorNumber : Maybe Float
        maybeDivisorNumber =
            AstHelpers.getUncomputedNumberValue checkInfo.right
    in
    if maybeDivisorNumber == Just 1 then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary dividing by 1"
                , details = [ "You can replace this operation by the left number you divided by 1." ]
                }
                checkInfo.operatorRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.left })
            )

    else if not checkInfo.expectNaN && (AstHelpers.getUncomputedNumberValue checkInfo.left == Just 0) then
        if maybeDivisorNumber == Just 0 then
            Just
                (Rule.error
                    { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                    , details =
                        [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                        , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                        , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                        , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                        ]
                    }
                    checkInfo.operatorRange
                )

        else
            Just
                (Rule.errorWithFix
                    { message = "Dividing 0 will result in 0"
                    , details =
                        [ "Dividing 0 by anything, even infinite numbers, gives 0 which means you can replace the whole division operation by 0."
                        , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                        ]
                    }
                    checkInfo.operatorRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.leftRange })
                )

    else
        Nothing


intDivideChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
intDivideChecks =
    firstThatConstructsJust
        [ \checkInfo ->
            case AstHelpers.getUncomputedNumberValue checkInfo.right of
                Just rightNumber ->
                    if rightNumber == 1 then
                        Just
                            (Rule.errorWithFix
                                { message = "Unnecessary dividing by 1"
                                , details = [ "You can replace this operation by the left integer you divided by 1." ]
                                }
                                checkInfo.operatorRange
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.leftRange })
                            )

                    else if rightNumber == 0 then
                        Just
                            (Rule.errorWithFix
                                { message = "Dividing by 0 will result in 0"
                                , details =
                                    [ "Dividing anything by 0 using (//) gives 0 which means you can replace the whole division operation by 0."
                                    , "Most likely, dividing by 0 was unintentional and you had a different number in mind."
                                    ]
                                }
                                checkInfo.operatorRange
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.rightRange })
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
        , \checkInfo ->
            if AstHelpers.getUncomputedNumberValue checkInfo.left == Just 0 then
                Just
                    (Rule.errorWithFix
                        { message = "Dividing 0 will result in 0"
                        , details =
                            [ "Dividing 0 by anything using (//), even 0, gives 0 which means you can replace the whole division operation by 0."
                            , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                            ]
                        }
                        checkInfo.operatorRange
                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.leftRange })
                    )

            else
                Nothing
        ]


plusplusChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
plusplusChecks =
    firstThatConstructsJust
        [ \checkInfo ->
            findMap
                (\side ->
                    case Node.value side.otherNode of
                        Expression.Literal _ ->
                            appendEmptyCheck side stringCollection checkInfo

                        _ ->
                            Nothing
                )
                (operationSides checkInfo)
        , \checkInfo ->
            findMap (\side -> appendEmptyCheck side listCollection checkInfo) (operationSides checkInfo)
        , \checkInfo ->
            collectionUnionWithLiteralsChecks
                { leftElementsStayOnTheLeft = True }
                { first = checkInfo.left
                , second = checkInfo.right
                , operationRange = checkInfo.operatorRange
                , operation = "++"
                }
                listCollection
                checkInfo
        , \checkInfo ->
            collectionUnionWithLiteralsChecks
                { leftElementsStayOnTheLeft = True }
                { first = checkInfo.left
                , second = checkInfo.right
                , operationRange = checkInfo.operatorRange
                , operation = "++"
                }
                stringCollection
                checkInfo
        , \checkInfo ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.left of
                Just leftListSingleton ->
                    if checkInfo.isOnTheRightSideOfPlusPlus then
                        Nothing

                    else
                        Just
                            (Rule.errorWithFix
                                { message = "Appending a singleton list to the beginning is the same as using (::) with the value inside"
                                , details = [ "You can replace this (++) operation by using (::) with the value inside the left singleton list on the right list." ]
                                }
                                checkInfo.operatorRange
                                (Fix.replaceRangeBy checkInfo.operatorRange
                                    "::"
                                    :: replaceBySubExpressionFix checkInfo.leftRange leftListSingleton.element
                                )
                            )

                Nothing ->
                    Nothing
        ]


appendEmptyCheck :
    { side | node : Node Expression, otherNode : Node Expression, otherDescription : String }
    -> TypeProperties (EmptiableProperties empty otherProperties)
    -> OperatorApplicationCheckInfo
    -> Maybe (Error {})
appendEmptyCheck side collection checkInfo =
    if isInTypeSubset collection.empty checkInfo side.node then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary appending " ++ typeSubsetDescriptionIndefinite collection.empty
                , details = [ "You can replace this operation by the " ++ side.otherDescription ++ " " ++ collection.represents ++ "." ]
                }
                checkInfo.operatorRange
                (keepOnlyFix
                    { keep = Node.range side.otherNode
                    , parentRange = checkInfo.parentRange
                    }
                )
            )

    else
        Nothing


consChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
consChecks =
    firstThatConstructsJust
        [ \checkInfo ->
            case Node.value checkInfo.right of
                Expression.ListExpr tailElements ->
                    let
                        fix : List Fix
                        fix =
                            case tailElements of
                                [] ->
                                    [ Fix.insertAt checkInfo.leftRange.start "[ "
                                    , Fix.replaceRangeBy
                                        { start = checkInfo.leftRange.end
                                        , end = checkInfo.rightRange.end
                                        }
                                        " ]"
                                    ]

                                _ :: _ ->
                                    [ Fix.insertAt checkInfo.leftRange.start "[ "
                                    , Fix.replaceRangeBy checkInfo.operatorRange ","
                                    , Fix.removeRange (leftBoundaryRange checkInfo.rightRange)
                                    ]
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            }
                            checkInfo.operatorRange
                            fix
                        )

                _ ->
                    Nothing
        , \checkInfo ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.right of
                Just tailSingleton ->
                    Just
                        (Rule.errorWithFix
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "You can replace this operation by a list that contains both the added element and the value inside the singleton list." ]
                            }
                            checkInfo.operatorRange
                            [ Fix.insertAt checkInfo.leftRange.start "[ "
                            , Fix.replaceRangeBy
                                { start = checkInfo.leftRange.end
                                , end = (Node.range tailSingleton.element).start
                                }
                                ", "
                            , Fix.replaceRangeBy
                                { start = (Node.range tailSingleton.element).end
                                , end = checkInfo.parentRange.end
                                }
                                " ]"
                            ]
                        )

                Nothing ->
                    Nothing
        ]



-- EQUALITY


equalityChecks : Bool -> OperatorApplicationCheckInfo -> Maybe (Error {})
equalityChecks isEqual =
    firstThatConstructsJust
        [ \checkInfo ->
            findMap
                (\side ->
                    if Evaluate.getBoolean checkInfo side.node == Determined isEqual then
                        Just
                            (Rule.errorWithFix
                                { message = "Unnecessary comparison with boolean"
                                , details = [ "The result of the expression will be the same with or without the comparison." ]
                                }
                                (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
                            )

                    else
                        Nothing
                )
                (operationSides checkInfo)
        , \checkInfo ->
            case
                ( AstHelpers.getSpecificFnCall Fn.Basics.not checkInfo.lookupTable checkInfo.left
                , AstHelpers.getSpecificFnCall Fn.Basics.not checkInfo.lookupTable checkInfo.right
                )
            of
                ( Just leftNotCall, Just rightNotCall ) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Unnecessary `not` on both sides of (" ++ checkInfo.operator ++ ")"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            }
                            checkInfo.operatorRange
                            (replaceBySubExpressionFix leftNotCall.nodeRange leftNotCall.firstArg
                                ++ replaceBySubExpressionFix rightNotCall.nodeRange rightNotCall.firstArg
                            )
                        )

                _ ->
                    Nothing
        , \checkInfo ->
            let
                inferred : Infer.Inferred
                inferred =
                    Tuple.first checkInfo.inferredConstants

                normalizeAndInfer : Node Expression -> Node Expression
                normalizeAndInfer expressionNode =
                    let
                        normalizedExpressionNode : Node Expression
                        normalizedExpressionNode =
                            Normalize.normalize checkInfo expressionNode
                    in
                    case Infer.get (Node.value normalizedExpressionNode) inferred of
                        Just expr ->
                            Node Range.emptyRange expr

                        Nothing ->
                            normalizedExpressionNode

                normalizedLeft : Node Expression
                normalizedLeft =
                    normalizeAndInfer checkInfo.left

                normalizedRight : Node Expression
                normalizedRight =
                    normalizeAndInfer checkInfo.right
            in
            case Normalize.compareWithoutNormalization normalizedLeft normalizedRight of
                Normalize.ConfirmedEquality ->
                    if checkInfo.expectNaN then
                        Nothing

                    else
                        Just (comparisonError isEqual checkInfo)

                Normalize.ConfirmedInequality ->
                    Just (comparisonError (not isEqual) checkInfo)

                Normalize.Unconfirmed ->
                    Nothing
        ]



-- COMPARISONS


numberComparisonChecks : (Float -> Float -> Bool) -> OperatorApplicationCheckInfo -> Maybe (Error {})
numberComparisonChecks operatorFunction operatorCheckInfo =
    case
        Maybe.map2 operatorFunction
            (Normalize.getNumberValue operatorCheckInfo.left)
            (Normalize.getNumberValue operatorCheckInfo.right)
    of
        Just bool ->
            Just (comparisonError bool operatorCheckInfo)

        Nothing ->
            Nothing


comparisonError : Bool -> QualifyResources { a | parentRange : Range, operator : String } -> Error {}
comparisonError bool checkInfo =
    let
        boolAsString : String
        boolAsString =
            AstHelpers.boolToString bool
    in
    Rule.errorWithFix
        { message = "(" ++ checkInfo.operator ++ ") comparison will result in " ++ boolAsString
        , details =
            [ "Based on the values and/or the context, we can determine the result. You can replace this operation by " ++ boolAsString ++ "."
            ]
        }
        checkInfo.parentRange
        [ Fix.replaceRangeBy checkInfo.parentRange
            (qualifiedToString (qualify ( [ "Basics" ], boolAsString ) checkInfo))
        ]



-- BASICS FUNCTIONS


basicsIdentityChecks : IntoFnCheck
basicsIdentityChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            Just
                (Rule.errorWithFix
                    { message = "`identity` should be removed"
                    , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                    }
                    checkInfo.fnRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                )
        )


basicsIdentityCompositionErrorMessage : { message : String, details : List String }
basicsIdentityCompositionErrorMessage =
    { message = "`identity` should be removed"
    , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
    }


basicsIdentityCompositionChecks : CompositionCheckInfo -> Maybe (Error {})
basicsIdentityCompositionChecks checkInfo =
    if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.later.node then
        Just
            (Rule.errorWithFix
                basicsIdentityCompositionErrorMessage
                (Node.range checkInfo.later.node)
                [ Fix.removeRange checkInfo.later.removeRange ]
            )

    else if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.earlier.node then
        Just
            (Rule.errorWithFix
                basicsIdentityCompositionErrorMessage
                (Node.range checkInfo.earlier.node)
                [ Fix.removeRange checkInfo.earlier.removeRange ]
            )

    else
        Nothing


basicsAlwaysChecks : IntoFnCheck
basicsAlwaysChecks =
    { call =
        \checkInfo ->
            case secondArg checkInfo of
                Just (Node secondArgRange _) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix
                                (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg, secondArgRange ])
                                checkInfo.firstArg
                            )
                        )

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            case checkInfo.later.args of
                _ :: [] ->
                    Just
                        { info =
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            }
                        , fix =
                            [ Fix.removeRange checkInfo.earlier.removeRange ]
                        }

                _ ->
                    Nothing
    }


basicsNegateChecks : IntoFnCheck
basicsNegateChecks =
    toggleFnChecks


basicsNotChecks : IntoFnCheck
basicsNotChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall notOnKnownBoolCheck
        , toggleFnChecks
        , intoFnCheckOnlyCall isNotOnBooleanOperatorCheck
        ]


notOnKnownBoolCheck : CallCheckInfo -> Maybe (Error {})
notOnKnownBoolCheck checkInfo =
    case Evaluate.getBoolean checkInfo checkInfo.firstArg of
        Determined bool ->
            let
                notBoolAsString : String
                notBoolAsString =
                    AstHelpers.boolToString (not bool)
            in
            Just
                (Rule.errorWithFix
                    { message = wrapInBackticks "not" ++ " on a bool known to be " ++ AstHelpers.boolToString bool ++ " can be replaced by " ++ notBoolAsString
                    , details = [ "You can replace this call by " ++ notBoolAsString ++ "." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Basics" ], notBoolAsString ) checkInfo))
                    ]
                )

        Undetermined ->
            Nothing


isNotOnBooleanOperatorCheck : CallCheckInfo -> Maybe (Error {})
isNotOnBooleanOperatorCheck checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.ParenthesizedExpression (Node _ (Expression.OperatorApplication operator _ (Node leftRange _) (Node rightRange _))) ->
            case isNegatableOperator operator of
                Just replacement ->
                    let
                        operatorRange : Range
                        operatorRange =
                            findOperatorRange
                                { operator = operator
                                , commentRanges = checkInfo.commentRanges
                                , extractSourceCode = checkInfo.extractSourceCode
                                , leftRange = leftRange
                                , rightRange = rightRange
                                }
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `" ++ replacement ++ "` instead." ]
                            }
                            checkInfo.fnRange
                            [ Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                            , Fix.replaceRangeBy operatorRange replacement
                            ]
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


isNegatableOperator : String -> Maybe String
isNegatableOperator op =
    case op of
        "<" ->
            Just ">="

        ">" ->
            Just "<="

        "<=" ->
            Just ">"

        ">=" ->
            Just "<"

        "==" ->
            Just "/="

        "/=" ->
            Just "=="

        _ ->
            Nothing


basicsToFloatChecks : IntoFnCheck
basicsToFloatChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just _ ->
                    Just
                        (Rule.errorWithFix
                            { message = "Unnecessary " ++ qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            }
                            checkInfo.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                        )

                Nothing ->
                    Nothing
        )


intToIntChecks : IntoFnCheck
intToIntChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall unnecessaryConversionToIntOnIntCheck
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Basics.toFloat
        ]


unnecessaryConversionToIntOnIntCheck : CallCheckInfo -> Maybe (Error {})
unnecessaryConversionToIntOnIntCheck checkInfo =
    case Evaluate.getInt checkInfo checkInfo.firstArg of
        Just _ ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary integer conversion on a literal integer"
                    , details =
                        [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                        , "You can replace this function call by the literal integer."
                        ]
                    }
                    checkInfo.fnRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                )

        Nothing ->
            Nothing


orChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
orChecks =
    firstThatConstructsJust
        [ \checkInfo -> findMap (\side -> unnecessaryOperationWithEmptySideChecks boolForOrProperties side checkInfo) (operationSides checkInfo)
        , \checkInfo -> findMap (\side -> operationWithAbsorbingSideChecks boolForOrProperties side checkInfo) (operationSides checkInfo)
        , findSimilarConditionsError
        ]


andChecks : OperatorApplicationCheckInfo -> Maybe (Error {})
andChecks =
    firstThatConstructsJust
        [ \checkInfo -> findMap (\side -> unnecessaryOperationWithEmptySideChecks boolForAndProperties side checkInfo) (operationSides checkInfo)
        , \checkInfo -> findMap (\side -> operationWithAbsorbingSideChecks boolForAndProperties side checkInfo) (operationSides checkInfo)
        , findSimilarConditionsError
        ]


unnecessaryOperationWithEmptySideChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> { side | node : Node Expression, otherNode : Node Expression, otherDescription : String } -> OperatorApplicationCheckInfo -> Maybe (Error {})
unnecessaryOperationWithEmptySideChecks forOperationProperties side checkInfo =
    if isInTypeSubset forOperationProperties.empty checkInfo side.node then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary " ++ checkInfo.operator ++ " " ++ typeSubsetDescriptionIndefinite forOperationProperties.empty
                , details = [ "You can replace this operation by the " ++ side.otherDescription ++ " " ++ forOperationProperties.represents ++ "." ]
                }
                (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
            )

    else
        Nothing


operationWithAbsorbingSideChecks : TypeProperties (AbsorbableProperties otherProperties) -> { side | node : Node Expression, otherNode : Node Expression, otherDescription : String } -> OperatorApplicationCheckInfo -> Maybe (Error {})
operationWithAbsorbingSideChecks forOperationProperties side checkInfo =
    if forOperationProperties.absorbing.is (extractInferResources checkInfo) side.node then
        Just
            (Rule.errorWithFix
                { message = "(" ++ checkInfo.operator ++ ") with any side being " ++ forOperationProperties.absorbing.description ++ " will result in " ++ forOperationProperties.absorbing.description
                , details =
                    [ "You can replace this operation by " ++ forOperationProperties.absorbing.asString defaultQualifyResources ++ "."
                    , "Maybe you have hardcoded a value or mistyped a condition?"
                    ]
                }
                checkInfo.parentRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.node })
            )

    else
        Nothing


type RedundantConditionResolution
    = RemoveFrom Location
    | ReplaceByNoop Bool


findSimilarConditionsError : OperatorApplicationCheckInfo -> Maybe (Error {})
findSimilarConditionsError operatorCheckInfo =
    let
        conditionsOnTheRight : List ( RedundantConditionResolution, Node Expression )
        conditionsOnTheRight =
            listConditions
                operatorCheckInfo.operator
                (RemoveFrom operatorCheckInfo.leftRange.end)
                operatorCheckInfo.right

        errorsForNode : Node Expression -> Maybe (Error {})
        errorsForNode nodeToCompareTo =
            findMap
                (areSimilarConditionsError
                    operatorCheckInfo
                    operatorCheckInfo.operator
                    nodeToCompareTo
                )
                conditionsOnTheRight
    in
    operatorCheckInfo.left
        |> listConditions operatorCheckInfo.operator (RemoveFrom operatorCheckInfo.leftRange.end)
        |> findMap (Tuple.second >> errorsForNode)


areSimilarConditionsError :
    QualifyResources (Infer.Resources a)
    -> String
    -> Node Expression
    -> ( RedundantConditionResolution, Node Expression )
    -> Maybe (Error {})
areSimilarConditionsError resources operator nodeToCompareTo ( redundantConditionResolution, nodeToLookAt ) =
    case Normalize.compare resources nodeToCompareTo nodeToLookAt of
        Normalize.ConfirmedEquality ->
            Just (errorForRedundantCondition operator redundantConditionResolution nodeToLookAt resources)

        Normalize.ConfirmedInequality ->
            Nothing

        Normalize.Unconfirmed ->
            Nothing


errorForRedundantCondition : String -> RedundantConditionResolution -> Node a -> QualifyResources b -> Error {}
errorForRedundantCondition operator redundantConditionResolution node qualifyResources =
    let
        ( range, fix ) =
            rangeAndFixForRedundantCondition redundantConditionResolution node qualifyResources
    in
    Rule.errorWithFix
        { message = "Condition is redundant"
        , details =
            [ "This condition is the same as another one found on the left side of the (" ++ operator ++ ") operator, therefore one of them can be removed."
            ]
        }
        range
        fix


rangeAndFixForRedundantCondition : RedundantConditionResolution -> Node a -> QualifyResources b -> ( Range, List Fix )
rangeAndFixForRedundantCondition redundantConditionResolution (Node nodeRange _) qualifyResources =
    case redundantConditionResolution of
        RemoveFrom locationOfPrevElement ->
            let
                range : Range
                range =
                    { start = locationOfPrevElement
                    , end = nodeRange.end
                    }
            in
            ( range
            , [ Fix.removeRange range ]
            )

        ReplaceByNoop noopValue ->
            ( nodeRange
            , [ Fix.replaceRangeBy nodeRange
                    (qualifiedToString (qualify ( [ "Basics" ], AstHelpers.boolToString noopValue ) qualifyResources))
              ]
            )


listConditions : String -> RedundantConditionResolution -> Node Expression -> List ( RedundantConditionResolution, Node Expression )
listConditions operatorToLookFor redundantConditionResolution expressionNode =
    case Node.value expressionNode of
        Expression.ParenthesizedExpression expr ->
            let
                noopValue : Bool
                noopValue =
                    operatorToLookFor == "&&"
            in
            listConditions operatorToLookFor (ReplaceByNoop noopValue) expr

        Expression.OperatorApplication operator _ left right ->
            if operator == operatorToLookFor then
                listConditions operatorToLookFor redundantConditionResolution left
                    ++ listConditions operatorToLookFor (RemoveFrom (Node.range left).end) right

            else
                [ ( redundantConditionResolution, expressionNode ) ]

        _ ->
            [ ( redundantConditionResolution, expressionNode ) ]



-- TUPLE FUNCTIONS


tuplePairChecks : IntoFnCheck
tuplePairChecks =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case checkInfo.argsAfterFirst of
                tuplePairCallSecondArg :: _ ->
                    let
                        firstRange : Range
                        firstRange =
                            Node.range checkInfo.firstArg

                        secondRange : Range
                        secondRange =
                            Node.range tuplePairCallSecondArg
                    in
                    case Range.compareLocations firstRange.end secondRange.start of
                        LT ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Fully constructed " ++ qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " can be replaced by tuple literal"
                                    , details = [ "You can replace this call by a tuple literal ( _, _ ). Consistently using ( _, _ ) to create a tuple is more idiomatic in elm." ]
                                    }
                                    checkInfo.fnRange
                                    (if checkInfo.parentRange.start.row /= checkInfo.parentRange.end.row then
                                        [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = firstRange.start }
                                            ("(\n" ++ String.repeat (firstRange.start.column - 1) " ")
                                        , Fix.replaceRangeBy { start = firstRange.end, end = secondRange.start }
                                            ("\n"
                                                ++ String.repeat (checkInfo.parentRange.start.column - 1) " "
                                                ++ ",\n"
                                                ++ String.repeat (secondRange.start.column - 1) " "
                                            )
                                        , Fix.replaceRangeBy { start = secondRange.end, end = checkInfo.parentRange.end }
                                            ("\n"
                                                ++ String.repeat (checkInfo.parentRange.start.column - 1) " "
                                                ++ ")"
                                            )
                                        ]

                                     else
                                        [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = firstRange.start } "( "
                                        , Fix.replaceRangeBy { start = firstRange.end, end = secondRange.start } ", "
                                        , Fix.replaceRangeBy { start = secondRange.end, end = checkInfo.parentRange.end } " )"
                                        ]
                                    )
                                )

                        EQ ->
                            Nothing

                        GT ->
                            Nothing

                [] ->
                    Nothing
        )


tupleFirstChecks : IntoFnCheck
tupleFirstChecks =
    intoFnChecksFirstThatConstructsError
        [ tuplePartChecks
            { part = TupleFirst
            , description = "first"
            , mapUnrelatedFn = Fn.Tuple.mapSecond
            , mapFn = Fn.Tuple.mapFirst
            }
        , intoFnCheckOnlyComposition
            (\checkInfo ->
                case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                    ( ( [ "Tuple" ], "pair" ), first :: [] ) ->
                        Just
                            { info =
                                { message = qualifiedToString (qualify checkInfo.earlier.fn defaultQualifyResources) ++ " with a first part, then " ++ qualifiedToString (qualify checkInfo.later.fn defaultQualifyResources) ++ " will always result in that first part"
                                , details = [ "You can replace this call by always with the first argument given to " ++ qualifiedToString (qualify checkInfo.earlier.fn defaultQualifyResources) ++ "." ]
                                }
                            , fix =
                                replaceBySubExpressionFix checkInfo.earlier.range first
                                    ++ [ Fix.insertAt checkInfo.earlier.range.start
                                            (qualifiedToString (qualify Fn.Basics.always checkInfo) ++ " ")
                                       , Fix.removeRange checkInfo.later.removeRange
                                       ]
                            }

                    _ ->
                        Nothing
            )
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.List.partition, combinedFn = Fn.List.filter }
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.Set.partition, combinedFn = Fn.Set.filter }
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.Dict.partition, combinedFn = Fn.Dict.filter }
        ]


tupleSecondChecks : IntoFnCheck
tupleSecondChecks =
    intoFnChecksFirstThatConstructsError
        [ tuplePartChecks
            { part = TupleSecond
            , description = "second"
            , mapFn = Fn.Tuple.mapSecond
            , mapUnrelatedFn = Fn.Tuple.mapFirst
            }
        , intoFnCheckOnlyComposition
            (\checkInfo ->
                case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                    ( ( [ "Tuple" ], "pair" ), _ :: [] ) ->
                        Just
                            (compositionAlwaysReturnsIncomingError
                                (qualifiedToString (qualify checkInfo.earlier.fn defaultQualifyResources) ++ " with a first part, then " ++ qualifiedToString (qualify checkInfo.later.fn defaultQualifyResources) ++ " will always result in the incoming second part")
                                checkInfo
                            )

                    _ ->
                        Nothing
            )
        ]


type TuplePart
    = TupleFirst
    | TupleSecond


tuplePartChecks :
    { part : TuplePart
    , description : String
    , mapFn : ( ModuleName, String )
    , mapUnrelatedFn : ( ModuleName, String )
    }
    -> IntoFnCheck
tuplePartChecks partConfig =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                Maybe.map
                    (\tuple ->
                        Rule.errorWithFix
                            { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on a known tuple will result in the tuple's " ++ partConfig.description ++ " part"
                            , details = [ "You can replace this call by the tuple's " ++ partConfig.description ++ " part." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange
                                (case partConfig.part of
                                    TupleFirst ->
                                        tuple.first

                                    TupleSecond ->
                                        tuple.second
                                )
                            )
                    )
                    (AstHelpers.getTuple2 checkInfo.lookupTable checkInfo.firstArg)
            )
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getSpecificFnCall partConfig.mapUnrelatedFn checkInfo.lookupTable checkInfo.firstArg of
                    Just mapSecondCall ->
                        case mapSecondCall.argsAfterFirst of
                            unmappedTuple :: [] ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Unnecessary " ++ qualifiedToString partConfig.mapUnrelatedFn ++ " before " ++ qualifiedToString checkInfo.fn
                                        , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the " ++ qualifiedToString partConfig.mapUnrelatedFn ++ " call by the unchanged tuple." ]
                                        }
                                        checkInfo.fnRange
                                        (keepOnlyFix { parentRange = mapSecondCall.nodeRange, keep = Node.range unmappedTuple })
                                    )

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getSpecificFnCall Fn.Tuple.mapBoth checkInfo.lookupTable checkInfo.firstArg of
                    Just tupleMapBothCall ->
                        case tupleMapBothCall.argsAfterFirst of
                            secondMapperArg :: _ :: [] ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString Fn.Tuple.mapBoth ++ " before " ++ qualifiedToString checkInfo.fn ++ " is the same as " ++ qualifiedToString partConfig.mapFn
                                        , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the " ++ qualifiedToString Fn.Tuple.mapBoth ++ " call by " ++ qualifiedToString partConfig.mapFn ++ " with the same " ++ partConfig.description ++ " mapping and tuple." ]
                                        }
                                        checkInfo.fnRange
                                        (case partConfig.part of
                                            TupleFirst ->
                                                Fix.replaceRangeBy tupleMapBothCall.fnRange
                                                    (qualifiedToString (qualify partConfig.mapFn checkInfo))
                                                    :: keepOnlyFix
                                                        { parentRange = Range.combine [ tupleMapBothCall.fnRange, Node.range tupleMapBothCall.firstArg, Node.range secondMapperArg ]
                                                        , keep = Range.combine [ tupleMapBothCall.fnRange, Node.range tupleMapBothCall.firstArg ]
                                                        }

                                            TupleSecond ->
                                                [ Fix.replaceRangeBy (Range.combine [ tupleMapBothCall.fnRange, Node.range tupleMapBothCall.firstArg ])
                                                    (qualifiedToString (qualify partConfig.mapFn checkInfo))
                                                ]
                                        )
                                    )

                            _ ->
                                Nothing

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyComposition
            (\checkInfo ->
                if checkInfo.earlier.fn == partConfig.mapUnrelatedFn then
                    Just
                        { info =
                            { message = "Unnecessary " ++ qualifiedToString partConfig.mapUnrelatedFn ++ " before " ++ qualifiedToString checkInfo.later.fn
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can remove the " ++ qualifiedToString partConfig.mapUnrelatedFn ++ " call." ]
                            }
                        , fix = [ Fix.removeRange checkInfo.earlier.removeRange ]
                        }

                else
                    Nothing
            )
        , intoFnCheckOnlyComposition
            (\checkInfo ->
                case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                    ( ( [ "Tuple" ], "mapBoth" ), firstMapperArg :: _ :: [] ) ->
                        Just
                            { info =
                                { message = qualifiedToString Fn.Tuple.mapBoth ++ " before " ++ qualifiedToString checkInfo.later.fn ++ " is the same as " ++ qualifiedToString partConfig.mapFn
                                , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the " ++ qualifiedToString Fn.Tuple.mapBoth ++ " call by " ++ qualifiedToString partConfig.mapFn ++ " with the same " ++ partConfig.description ++ " mapping." ]
                                }
                            , fix =
                                case partConfig.part of
                                    TupleFirst ->
                                        Fix.replaceRangeBy checkInfo.earlier.fnRange
                                            (qualifiedToString (qualify partConfig.mapFn checkInfo))
                                            :: keepOnlyFix
                                                { parentRange = checkInfo.earlier.range
                                                , keep = Range.combine [ checkInfo.earlier.fnRange, Node.range firstMapperArg ]
                                                }

                                    TupleSecond ->
                                        [ Fix.replaceRangeBy (Range.combine [ checkInfo.earlier.fnRange, Node.range firstMapperArg ])
                                            (qualifiedToString (qualify partConfig.mapFn checkInfo))
                                        ]
                            }

                    _ ->
                        Nothing
            )
        ]



-- STRING FUNCTIONS


stringFromListChecks : IntoFnCheck
stringFromListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck { resultAsString = stringCollection.empty.specific.asString } listCollection)
        , wrapperFromListSingletonChecks stringCollection
        , onSpecificFnCallReturnsItsLastArgCheck Fn.String.toList
        ]


stringIsEmptyChecks : IntoFnCheck
stringIsEmptyChecks =
    intoFnCheckOnlyCall (collectionIsEmptyChecks stringCollection)


stringLengthChecks : IntoFnCheck
stringLengthChecks =
    intoFnCheckOnlyCall (collectionSizeChecks stringCollection)


stringSliceChecks : IntoFnCheck
stringSliceChecks =
    collectionSliceChecks stringCollection


stringReverseChecks : IntoFnCheck
stringReverseChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableReverseChecks stringCollection
        , unnecessaryOnWrappedCheck stringCollection
        ]


stringLeftChecks : IntoFnCheck
stringLeftChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case Evaluate.getInt checkInfo checkInfo.firstArg of
                    Just length ->
                        callWithNonPositiveIntCanBeReplacedByCheck
                            { int = length
                            , intDescription = "length"
                            , replacement = stringCollection.empty.specific.asString
                            }
                            checkInfo

                    Nothing ->
                        Nothing
            )
        ]


stringRightChecks : IntoFnCheck
stringRightChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case Evaluate.getInt checkInfo checkInfo.firstArg of
                    Just length ->
                        callWithNonPositiveIntCanBeReplacedByCheck
                            { int = length
                            , intDescription = "length"
                            , replacement = stringCollection.empty.specific.asString
                            }
                            checkInfo

                    Nothing ->
                        Nothing
            )
        ]


stringReplaceChecks : IntoFnCheck
stringReplaceChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck stringCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Just replacementArg ->
                        firstThatConstructsJust
                            [ \() ->
                                Maybe.andThen
                                    (\stringArg ->
                                        case ( checkInfo.firstArg, stringArg ) of
                                            ( Node _ (Expression.Literal toReplace), Node _ (Expression.Literal third) ) ->
                                                if not (String.contains "\u{000D}" toReplace) && not (String.contains toReplace third) then
                                                    Just
                                                        (Rule.errorWithFix
                                                            { message = "String.replace with a pattern not present in the given string will result in the given string"
                                                            , details = [ "You can replace this call by the given string itself." ]
                                                            }
                                                            checkInfo.fnRange
                                                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range stringArg })
                                                        )

                                                else
                                                    Nothing

                                            _ ->
                                                Nothing
                                    )
                                    (thirdArg checkInfo)
                            , \() ->
                                case Normalize.compare checkInfo checkInfo.firstArg replacementArg of
                                    Normalize.ConfirmedEquality ->
                                        Just
                                            (alwaysReturnsLastArgError
                                                (qualifiedToString checkInfo.fn ++ " where the pattern to replace and the replacement are equal")
                                                stringCollection
                                                checkInfo
                                            )

                                    _ ->
                                        Nothing
                            ]
                            ()

                    Nothing ->
                        Nothing
            )
        ]


stringAppendChecks : IntoFnCheck
stringAppendChecks =
    intoFnCheckOnlyCall (collectionUnionChecks { leftElementsStayOnTheLeft = True } stringCollection)


stringConcatChecks : IntoFnCheck
stringConcatChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableFlatFromListChecks stringCollection
        , onSpecificFnCallCanBeCombinedCheck { args = [], earlierFn = Fn.List.repeat, combinedFn = Fn.String.repeat }
        , onSpecificFnCallCanBeCombinedCheck { args = [], earlierFn = Fn.List.intersperse, combinedFn = Fn.String.join }
        ]


stringJoinChecks : IntoFnCheck
stringJoinChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ callOnEmptyReturnsCheck { resultAsString = stringCollection.empty.specific.asString } listCollection
            , flatIntersperseWithEmptySeparatorIsEquivalentToFnCheck stringCollection Fn.String.concat
            ]
        )


stringRepeatChecks : IntoFnCheck
stringRepeatChecks =
    intoFnCheckOnlyCall (emptiableFlatRepeatChecks stringCollection)


stringWordsChecks : IntoFnCheck
stringWordsChecks =
    intoFnCheckOnlyCall
        (callOnEmptyReturnsCheck { resultAsString = \_ -> "[ \"\" ]" } stringCollection)


stringLinesChecks : IntoFnCheck
stringLinesChecks =
    intoFnCheckOnlyCall
        (callOnEmptyReturnsCheck { resultAsString = \_ -> "[ \"\" ]" } stringCollection)


stringToListChecks : IntoFnCheck
stringToListChecks =
    onSpecificFnCallReturnsItsLastArgCheck Fn.String.fromList


stringFoldlChecks : IntoFnCheck
stringFoldlChecks =
    intoFnCheckOnlyCall (emptiableFoldChecks stringCollection)


stringFoldrChecks : IntoFnCheck
stringFoldrChecks =
    intoFnCheckOnlyCall (emptiableFoldChecks stringCollection)



-- MAYBE FUNCTIONS


maybeMapChecks : IntoFnCheck
maybeMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks maybeWithJustAsWrap
        , mapOnWrappedChecks maybeWithJustAsWrap
        ]


maybeMapNChecks : IntoFnCheck
maybeMapNChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ wrapperMapNChecks maybeWithJustAsWrap
            , emptiableMapNChecks maybeWithJustAsWrap
            ]
        )


maybeAndThenChecks : IntoFnCheck
maybeAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ wrapperFlatMapChecks maybeWithJustAsWrap
        , emptiableFlatMapChecks maybeWithJustAsWrap
        ]


maybeWithDefaultChecks : IntoFnCheck
maybeWithDefaultChecks =
    withDefaultChecks maybeWithJustAsWrap



-- RESULT FUNCTIONS


resultMapChecks : IntoFnCheck
resultMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks resultWithOkAsWrap
        , mapOnWrappedChecks resultWithOkAsWrap
        ]


resultMapNChecks : IntoFnCheck
resultMapNChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ wrapperMapNChecks resultWithOkAsWrap
            , mapNOrFirstEmptyConstructionChecks resultWithOkAsWrap
            ]
        )


mapWrapErrorInfo :
    ( ModuleName, String )
    -> WrapperProperties otherProperties
    -> { message : String, details : List String }
mapWrapErrorInfo mapFn wrapper =
    let
        wrapFnInErrorInfo : String
        wrapFnInErrorInfo =
            qualifiedToString (qualify wrapper.wrap.fn defaultQualifyResources)
    in
    { message = qualifiedToString mapFn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in " ++ wrapFnInErrorInfo ++ " with the function applied to the value inside"
    , details = [ "You can replace this call by " ++ wrapFnInErrorInfo ++ " with the function directly applied to the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ " itself." ]
    }


resultMapErrorChecks : IntoFnCheck
resultMapErrorChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks resultWithErrAsWrap
        , mapOnWrappedChecks resultWithErrAsWrap
        ]


resultAndThenChecks : IntoFnCheck
resultAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck resultWithOkAsWrap
        , wrapperFlatMapChecks resultWithOkAsWrap
        ]


resultWithDefaultChecks : IntoFnCheck
resultWithDefaultChecks =
    withDefaultChecks resultWithOkAsWrap


resultToMaybeChecks : IntoFnCheck
resultToMaybeChecks =
    intoFnChecksFirstThatConstructsError
        [ unwrapToMaybeChecks resultWithOkAsWrap
        , intoFnCheckOnlyComposition
            (\checkInfo ->
                case checkInfo.earlier.fn of
                    ( [ "Result" ], "Err" ) ->
                        Just
                            { info =
                                { message = qualifiedToString Fn.Result.toMaybe ++ " on an error will result in Nothing"
                                , details = [ "You can replace this call by always Nothing." ]
                                }
                            , fix =
                                compositionReplaceByFix
                                    (qualifiedToString (qualify Fn.Basics.always checkInfo)
                                        ++ " "
                                        ++ qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)
                                    )
                                    checkInfo
                            }

                    _ ->
                        Nothing
            )
        ]


resultFromMaybeWithEmptyValueOnNothingCheck : IntoFnCheck
resultFromMaybeWithEmptyValueOnNothingCheck =
    fromMaybeWithEmptyValueOnNothingCheck resultWithOkAsWrap



-- LIST FUNCTIONS


listAppendChecks : IntoFnCheck
listAppendChecks =
    intoFnCheckOnlyCall
        (collectionUnionChecks { leftElementsStayOnTheLeft = True } listCollection)


listConcatChecks : IntoFnCheck
listConcatChecks =
    intoFnChecksFirstThatConstructsError
        [ onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.List.map, combinedFn = Fn.List.concatMap }
        , emptiableFlatFromListChecks listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case fromListGetLiteral listCollection checkInfo.lookupTable checkInfo.firstArg of
                    Just listLiteral ->
                        case traverse AstHelpers.getListLiteral listLiteral.elements of
                            Just _ ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Expression could be simplified to be a single List"
                                        , details = [ "Try moving all the elements into a single list." ]
                                        }
                                        checkInfo.fnRange
                                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg }
                                            ++ List.concatMap removeBoundariesFix listLiteral.elements
                                        )
                                    )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyCall (mergeConsecutiveFromListLiteralsCheck listCollection)
        ]


listConcatMapChecks : IntoFnCheck
listConcatMapChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (operationWithIdentityIsEquivalentToFnCheck Fn.List.concat)
        , emptiableFlatMapChecks listCollection
        , wrapperFlatMapChecks listCollection
        ]


listIndexedMapChecks : IntoFnCheck
listIndexedMapChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , intoFnCheckOnlyCall (operationWithExtraArgChecks { operationWithoutExtraArg = Fn.List.map })
        ]


listIntersperseChecks : IntoFnCheck
listIntersperseChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , unnecessaryOnWrappedCheck listCollection
        ]


listHeadChecks : IntoFnCheck
listHeadChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (firstThatConstructsJust
                [ callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } listCollection
                , \checkInfo ->
                    Maybe.map
                        (\listArgHead ->
                            Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " on a list with a first element will result in Just that element"
                                , details = [ "You can replace this call by Just the first list element." ]
                                }
                                checkInfo.fnRange
                                (replaceBySubExpressionFix (Node.range checkInfo.firstArg) listArgHead
                                    ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                            (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
                                       ]
                                )
                        )
                        (getListHead checkInfo.lookupTable checkInfo.firstArg)
                , \checkInfo ->
                    AstHelpers.getSpecificFnCall Fn.List.reverse checkInfo.lookupTable checkInfo.firstArg
                        |> Maybe.andThen
                            (\reverseCall ->
                                AstHelpers.getSpecificFnCall Fn.List.sort checkInfo.lookupTable reverseCall.firstArg
                                    |> Maybe.map
                                        (\sortCall ->
                                            Rule.errorWithFix
                                                { message = "List.sort, then List.reverse, then List.head can be combined into List.maximum"
                                                , details = [ "You can replace this call by List.maximum with the same list given to List.sort which is meant for this exact purpose." ]
                                                }
                                                checkInfo.fnRange
                                                (replaceBySubExpressionFix (Node.range checkInfo.firstArg) sortCall.firstArg
                                                    ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                                            (qualifiedToString (qualify Fn.List.maximum checkInfo))
                                                       ]
                                                )
                                        )
                            )
                ]
            )
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.List.sort
            , combinedFn = Fn.List.minimum
            }
        ]


getListHead : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
getListHead lookupTable expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.ListExpr (head :: _) ->
            Just head

        Expression.OperatorApplication "::" _ head _ ->
            Just head

        _ ->
            case AstHelpers.getListSingleton lookupTable expressionNode of
                Just single ->
                    Just single.element

                Nothing ->
                    Nothing


listTailExistsError : List Fix -> CallCheckInfo -> Error {}
listTailExistsError replaceListArgByTailFix checkInfo =
    Rule.errorWithFix
        { message = qualifiedToString checkInfo.fn ++ " on a list with some elements will result in Just the elements after the first"
        , details = [ "You can replace this call by Just the list elements after the first." ]
        }
        checkInfo.fnRange
        (replaceListArgByTailFix
            ++ [ Fix.replaceRangeBy checkInfo.fnRange
                    (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
               ]
        )


listTailChecks : IntoFnCheck
listTailChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } listCollection
            , \checkInfo ->
                case Node.value (AstHelpers.removeParens checkInfo.firstArg) of
                    Expression.ListExpr ((Node headRange _) :: (Node tailFirstRange _) :: _) ->
                        Just
                            (listTailExistsError
                                [ Fix.removeRange { start = headRange.start, end = tailFirstRange.start }
                                ]
                                checkInfo
                            )

                    Expression.OperatorApplication "::" _ _ tail ->
                        Just
                            (listTailExistsError
                                (replaceBySubExpressionFix (Node.range checkInfo.firstArg) tail)
                                checkInfo
                            )

                    _ ->
                        Nothing
            , \checkInfo ->
                case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                    Just _ ->
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in Just []"
                                , details = [ "You can replace this call by Just []." ]
                                }
                                checkInfo.fnRange
                                [ Fix.replaceRangeBy (Node.range checkInfo.firstArg) "[]"
                                , Fix.replaceRangeBy checkInfo.fnRange
                                    (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
                                ]
                            )

                    Nothing ->
                        Nothing
            ]
        )


listMapChecks : IntoFnCheck
listMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks listCollection
        , listMapOnSingletonCheck
        , onSpecificFnCallCanBeCombinedCheck
            { args = [ tupleSecondAccessFunctionProperties ]
            , earlierFn = Fn.Array.toIndexedList
            , combinedFn = Fn.Array.toList
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = [ tupleFirstAccessFunctionProperties ]
            , earlierFn = Fn.Dict.toList
            , combinedFn = Fn.Dict.keys
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = [ tupleSecondAccessFunctionProperties ]
            , earlierFn = Fn.Dict.toList
            , combinedFn = Fn.Dict.values
            }
        ]


listMapNChecks : IntoFnCheck
listMapNChecks =
    intoFnCheckOnlyCall (emptiableMapNChecks listCollection)


listMapOnSingletonCheck : IntoFnCheck
listMapOnSingletonCheck =
    { call =
        \checkInfo ->
            -- we do not re-use mapOnWrappedChecks because that would fix e.g.
            -- map f (if c then List.singleton a else [ b ]) --> map f (List.singleton (if c then a else b))
            -- while we instead fix it to the more compact form
            -- map f [ if c then a else b ]
            case secondArg checkInfo of
                Just listArg ->
                    firstThatConstructsJust
                        [ \() ->
                            case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                                Just wrapped ->
                                    let
                                        mappedValueRange : Range
                                        mappedValueRange =
                                            Node.range wrapped.element

                                        mappingArgRange : Range
                                        mappingArgRange =
                                            Node.range checkInfo.firstArg
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in a singleton list with the function applied to the value inside"
                                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                                            }
                                            checkInfo.fnRange
                                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range listArg }
                                                ++ parenthesizeIfNeededFix wrapped.element
                                                ++ (case checkInfo.callStyle of
                                                        Pipe LeftToRight ->
                                                            [ Fix.insertAt mappedValueRange.start "("
                                                            , Fix.insertAt mappedValueRange.end
                                                                (" |> " ++ checkInfo.extractSourceCode mappingArgRange ++ ")")
                                                            ]

                                                        Pipe RightToLeft ->
                                                            [ Fix.insertAt mappedValueRange.start ("(" ++ checkInfo.extractSourceCode mappingArgRange ++ " <| ")
                                                            , Fix.insertAt mappedValueRange.end ")"
                                                            ]

                                                        Application ->
                                                            [ Fix.insertAt mappedValueRange.start ("(" ++ checkInfo.extractSourceCode mappingArgRange ++ " ")
                                                            , Fix.insertAt mappedValueRange.end ")"
                                                            ]
                                                   )
                                            )
                                        )

                                Nothing ->
                                    Nothing
                        , \() ->
                            case sameInAllBranches (getValueWithNodeRange (listCollection.wrap.getValue checkInfo.lookupTable)) listArg of
                                Just wraps ->
                                    let
                                        mappingArgRange : Range
                                        mappingArgRange =
                                            Node.range checkInfo.firstArg
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in a singleton list with the function applied to the value inside"
                                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                                            }
                                            checkInfo.fnRange
                                            (keepOnlyFix
                                                { parentRange = Range.combine [ checkInfo.fnRange, mappingArgRange ]
                                                , keep = mappingArgRange
                                                }
                                                ++ List.concatMap
                                                    (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value)
                                                    wraps
                                                ++ [ Fix.insertAt checkInfo.parentRange.start "[ "
                                                   , Fix.insertAt checkInfo.parentRange.end " ]"
                                                   ]
                                            )
                                        )

                                Nothing ->
                                    Nothing
                        ]
                        ()

                Nothing ->
                    Nothing
    , composition = (mapOnWrappedChecks listCollection).composition
    }


listMemberChecks : IntoFnCheck
listMemberChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ callOnEmptyReturnsCheck
                { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
                listCollection
            , knownMemberChecks listCollection
            , wrapperMemberChecks listCollection
            ]
        )


listSumChecks : IntoFnCheck
listSumChecks =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck listCollection
        , intoFnCheckOnlyCall
            (firstThatConstructsJust
                [ callOnEmptyReturnsCheck { resultAsString = \_ -> "0" } listCollection
                , \checkInfo ->
                    callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn)
                        ( listCollection, numberForAddProperties )
                        checkInfo
                , \checkInfo ->
                    if checkInfo.expectNaN then
                        callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn)
                            ( listCollection, numberForAddProperties )
                            checkInfo

                    else
                        Nothing
                ]
            )
        ]


listProductChecks : IntoFnCheck
listProductChecks =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck listCollection
        , intoFnCheckOnlyCall
            (firstThatConstructsJust
                [ callOnEmptyReturnsCheck { resultAsString = \_ -> "1" } listCollection
                , \checkInfo ->
                    callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn)
                        ( listCollection, numberForMultiplyProperties )
                        checkInfo
                , \checkInfo ->
                    if checkInfo.expectNaN then
                        callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn)
                            ( listCollection, numberForMultiplyProperties )
                            checkInfo

                    else
                        callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn)
                            ( listCollection, numberNotExpectingNaNForMultiplyProperties )
                            checkInfo
                ]
            )
        ]


listMinimumChecks : IntoFnCheck
listMinimumChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } listCollection)
        , onWrappedReturnsJustItsValueCheck listCollection
        ]


listMaximumChecks : IntoFnCheck
listMaximumChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } listCollection)
        , onWrappedReturnsJustItsValueCheck listCollection
        ]


listFoldlChecks : IntoFnCheck
listFoldlChecks =
    listFoldAnyDirectionChecks


listFoldrChecks : IntoFnCheck
listFoldrChecks =
    listFoldAnyDirectionChecks


listFoldAnyDirectionChecks : IntoFnCheck
listFoldAnyDirectionChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (emptiableFoldChecks listCollection)
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Nothing ->
                        Nothing

                    Just initialArg ->
                        let
                            maybeListArg : Maybe (Node Expression)
                            maybeListArg =
                                thirdArg checkInfo

                            numberBinaryOperationChecks : { identity : Int, two : String, list : String } -> Maybe (Error {})
                            numberBinaryOperationChecks operation =
                                let
                                    fixWith : List Fix -> Error {}
                                    fixWith fixes =
                                        let
                                            replacementOperationAsString : String
                                            replacementOperationAsString =
                                                qualifiedToString ( [ "List" ], operation.list )
                                        in
                                        Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " (" ++ operation.two ++ ") " ++ String.fromInt operation.identity ++ " is the same as " ++ replacementOperationAsString
                                            , details = [ "You can replace this call by " ++ replacementOperationAsString ++ " which is meant for this exact purpose." ]
                                            }
                                            checkInfo.fnRange
                                            fixes
                                in
                                if AstHelpers.getUncomputedNumberValue initialArg == Just (Basics.toFloat operation.identity) then
                                    Just
                                        (fixWith
                                            [ Fix.replaceRangeBy
                                                { start = checkInfo.fnRange.start
                                                , end = (Node.range initialArg).end
                                                }
                                                (qualifiedToString (qualify ( [ "List" ], operation.list ) checkInfo))
                                            ]
                                        )

                                else
                                    case maybeListArg of
                                        Nothing ->
                                            Nothing

                                        Just _ ->
                                            case checkInfo.callStyle of
                                                Pipe LeftToRight ->
                                                    -- list |> fold op initial --> ((list |> List.op) op initial)
                                                    Just
                                                        (fixWith
                                                            [ Fix.insertAt (Node.range initialArg).end ")"
                                                            , Fix.insertAt (Node.range initialArg).start (operation.two ++ " ")
                                                            , Fix.replaceRangeBy
                                                                { start = checkInfo.fnRange.start
                                                                , end = (Node.range checkInfo.firstArg).end
                                                                }
                                                                (qualifiedToString (qualify ( [ "List" ], operation.list ) checkInfo) ++ ")")
                                                            , Fix.insertAt checkInfo.parentRange.start "(("
                                                            ]
                                                        )

                                                -- Pipe RightToLeft | Application ->
                                                _ ->
                                                    -- fold op initial list --> (initial op (List.op list))
                                                    Just
                                                        (fixWith
                                                            [ Fix.insertAt checkInfo.parentRange.end ")"
                                                            , Fix.insertAt (Node.range initialArg).end
                                                                (" "
                                                                    ++ operation.two
                                                                    ++ " ("
                                                                    ++ qualifiedToString (qualify ( [ "List" ], operation.list ) checkInfo)
                                                                )
                                                            , Fix.removeRange
                                                                { start = checkInfo.fnRange.start
                                                                , end = (Node.range initialArg).start
                                                                }
                                                            ]
                                                        )

                            boolBinaryOperationChecks : { two : String, list : String, determining : Bool } -> Bool -> Error {}
                            boolBinaryOperationChecks operation initialIsDetermining =
                                if initialIsDetermining == operation.determining then
                                    let
                                        determiningAsString : String
                                        determiningAsString =
                                            AstHelpers.boolToString operation.determining
                                    in
                                    alwaysResultsInUnparenthesizedConstantError
                                        (qualifiedToString checkInfo.fn ++ " with (" ++ operation.two ++ ") and the initial accumulator " ++ determiningAsString)
                                        { replacement = \res -> qualifiedToString (qualify ( [ "Basics" ], determiningAsString ) res) }
                                        checkInfo

                                else
                                    -- initialIsTrue /= operation.determining
                                    let
                                        replacementOperationAsString : String
                                        replacementOperationAsString =
                                            qualifiedToString ( [ "List" ], operation.list ) ++ " identity"
                                    in
                                    Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " (" ++ operation.two ++ ") " ++ AstHelpers.boolToString (not operation.determining) ++ " is the same as " ++ replacementOperationAsString
                                        , details = [ "You can replace this call by " ++ replacementOperationAsString ++ " which is meant for this exact purpose." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy
                                            { start = checkInfo.fnRange.start, end = (Node.range initialArg).end }
                                            (qualifiedToString (qualify ( [ "List" ], operation.list ) checkInfo)
                                                ++ " "
                                                ++ qualifiedToString (qualify Fn.Basics.identity checkInfo)
                                            )
                                        ]
                        in
                        firstThatConstructsJust
                            [ \() ->
                                if AstHelpers.isSpecificUnappliedBinaryOperation "*" checkInfo checkInfo.firstArg then
                                    numberBinaryOperationChecks { two = "*", list = "product", identity = 1 }

                                else
                                    Nothing
                            , \() ->
                                if AstHelpers.isSpecificUnappliedBinaryOperation "+" checkInfo checkInfo.firstArg then
                                    numberBinaryOperationChecks { two = "+", list = "sum", identity = 0 }

                                else
                                    Nothing
                            , \() ->
                                case Evaluate.getBoolean checkInfo initialArg of
                                    Undetermined ->
                                        Nothing

                                    Determined initialBool ->
                                        if AstHelpers.isSpecificUnappliedBinaryOperation "&&" checkInfo checkInfo.firstArg then
                                            Just (boolBinaryOperationChecks { two = "&&", list = "all", determining = False } initialBool)

                                        else if AstHelpers.isSpecificUnappliedBinaryOperation "||" checkInfo checkInfo.firstArg then
                                            Just (boolBinaryOperationChecks { two = "||", list = "any", determining = True } initialBool)

                                        else
                                            Nothing
                            ]
                            ()
            )
        , { call =
                \checkInfo ->
                    case thirdArg checkInfo of
                        Just listArg ->
                            case AstHelpers.getSpecificFnCall Fn.Set.toList checkInfo.lookupTable listArg of
                                Just setToListCall ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = "To fold a set, you don't need to convert to a List"
                                            , details = [ "Using " ++ qualifiedToString ( [ "Set" ], AstHelpers.qualifiedName checkInfo.fn ) ++ " directly is meant for this exact purpose and will also be faster." ]
                                            }
                                            checkInfo.fnRange
                                            (replaceBySubExpressionFix setToListCall.nodeRange setToListCall.firstArg
                                                ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                                        (qualifiedToString (qualify ( [ "Set" ], AstHelpers.qualifiedName checkInfo.fn ) checkInfo))
                                                   ]
                                            )
                                        )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
          , composition =
                \checkInfo ->
                    case checkInfo.earlier.fn of
                        ( [ "Set" ], "toList" ) ->
                            Just
                                { info =
                                    { message = "To fold a set, you don't need to convert to a List"
                                    , details = [ "Using " ++ qualifiedToString ( [ "Set" ], AstHelpers.qualifiedName checkInfo.later.fn ) ++ " directly is meant for this exact purpose and will also be faster." ]
                                    }
                                , fix =
                                    [ Fix.replaceRangeBy checkInfo.later.fnRange
                                        (qualifiedToString (qualify ( [ "Set" ], AstHelpers.qualifiedName checkInfo.later.fn ) checkInfo))
                                    , Fix.removeRange checkInfo.earlier.removeRange
                                    ]
                                }

                        _ ->
                            Nothing
          }
        ]


listIsEmptyChecks : IntoFnCheck
listIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks listCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Set.toList
            , combinedFn = Fn.Set.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.toList
            , combinedFn = Fn.Array.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.toIndexedList
            , combinedFn = Fn.Array.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.toList
            , combinedFn = Fn.Dict.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.values
            , combinedFn = Fn.Dict.isEmpty
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.keys
            , combinedFn = Fn.Dict.isEmpty
            }
        ]


listLengthChecks : IntoFnCheck
listLengthChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionSizeChecks listCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.toList
            , combinedFn = Fn.Dict.size
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.values
            , combinedFn = Fn.Dict.size
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.keys
            , combinedFn = Fn.Dict.size
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Set.toList
            , combinedFn = Fn.Set.size
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.toList
            , combinedFn = Fn.Array.length
            }
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.toIndexedList
            , combinedFn = Fn.Array.length
            }
        ]


listAllChecks : IntoFnCheck
listAllChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ emptiableAllChecks listCollection
            , collectionAllChecks listCollection
            ]
        )


listAnyChecks : IntoFnCheck
listAnyChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ emptiableAnyChecks listCollection
            , collectionAnyChecks listCollection
            , operationWithEqualsConstantIsEquivalentToFnWithThatConstantCheck Fn.List.member
            ]
        )


listFilterChecks : IntoFnCheck
listFilterChecks =
    emptiableKeepWhenChecks listCollection


listPartitionChecks : IntoFnCheck
listPartitionChecks =
    intoFnCheckOnlyCall (collectionPartitionChecks listCollection)


listFilterMapChecks : IntoFnCheck
listFilterMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableWrapperFilterMapChecks listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
                    firstThatConstructsJust
                        [ \() ->
                            callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with an identity function")
                                ( listCollection, maybeWithJustAsWrap )
                                checkInfo
                        , \() ->
                            case secondArg checkInfo of
                                Just listArg ->
                                    case AstHelpers.getListLiteral listArg of
                                        Just list ->
                                            case
                                                traverse
                                                    (AstHelpers.getSpecificFnCall Fn.Maybe.justVariant checkInfo.lookupTable)
                                                    list
                                            of
                                                Just justCalls ->
                                                    Just
                                                        (Rule.errorWithFix
                                                            { message = "Unnecessary use of " ++ qualifiedToString checkInfo.fn ++ " identity"
                                                            , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                                                            }
                                                            checkInfo.fnRange
                                                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range listArg }
                                                                ++ List.concatMap
                                                                    (\just -> keepOnlyFix { parentRange = just.nodeRange, keep = Node.range just.firstArg })
                                                                    justCalls
                                                            )
                                                        )

                                                Nothing ->
                                                    Nothing

                                        Nothing ->
                                            Nothing

                                Nothing ->
                                    Nothing
                        ]
                        ()

                else
                    Nothing
            )
        ]


listRangeChecks : IntoFnCheck
listRangeChecks =
    intoFnCheckOnlyCall (emptiableRangeChecks listCollection)


listRepeatChecks : IntoFnCheck
listRepeatChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ emptiableRepeatChecks listCollection
            , wrapperRepeatChecks listCollection
            ]
        )


listReverseChecks : IntoFnCheck
listReverseChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableReverseChecks listCollection
        , unnecessaryOnWrappedCheck listCollection
        ]


listSortChecks : IntoFnCheck
listSortChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , unnecessaryOnWrappedCheck listCollection
        , operationDoesNotChangeResultOfOperationCheck
        ]


listSortByChecks : IntoFnCheck
listSortByChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , unnecessaryOnWrappedCheck listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                    Just _ ->
                        Just
                            (alwaysReturnsLastArgError
                                (qualifiedToString checkInfo.fn ++ " with a function that always returns the same constant")
                                listCollection
                                checkInfo
                            )

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyCall (operationWithIdentityIsEquivalentToFnCheck Fn.List.sort)
        , operationDoesNotChangeResultOfOperationCheck
        ]


listSortWithChecks : IntoFnCheck
listSortWithChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , unnecessaryOnWrappedCheck listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                let
                    alwaysAlwaysOrder : Maybe Order
                    alwaysAlwaysOrder =
                        AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg
                            |> Maybe.andThen (AstHelpers.getAlwaysResult checkInfo.lookupTable)
                            |> Maybe.andThen (AstHelpers.getOrder checkInfo.lookupTable)
                in
                case alwaysAlwaysOrder of
                    Just order ->
                        let
                            fixToIdentity : Error {}
                            fixToIdentity =
                                alwaysReturnsLastArgError
                                    (qualifiedToString checkInfo.fn ++ " with a comparison that always returns " ++ AstHelpers.orderToString order)
                                    listCollection
                                    checkInfo
                        in
                        case order of
                            LT ->
                                Just
                                    (operationWithFirstArgIsEquivalentToFnError
                                        { firstArgDescription = "a comparison that always returns LT"
                                        , replacementFn = Fn.List.reverse
                                        }
                                        checkInfo
                                    )

                            EQ ->
                                Just fixToIdentity

                            GT ->
                                Just fixToIdentity

                    Nothing ->
                        Nothing
            )
        ]


listTakeChecks : IntoFnCheck
listTakeChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case Evaluate.getInt checkInfo checkInfo.firstArg of
                    Just length ->
                        callWithNonPositiveIntCanBeReplacedByCheck
                            { int = length
                            , intDescription = "length"
                            , replacement = listCollection.empty.specific.asString
                            }
                            checkInfo

                    Nothing ->
                        Nothing
            )
        ]


listDropChecks : IntoFnCheck
listDropChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck listCollection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                Evaluate.getInt checkInfo checkInfo.firstArg
                    |> Maybe.andThen
                        (\count ->
                            firstThatConstructsJust
                                [ \() ->
                                    callWithNonPositiveIntCheckErrorSituation
                                        { int = count, intDescription = "count", fn = checkInfo.fn }
                                        |> Maybe.map
                                            (\situation -> alwaysReturnsLastArgError situation listCollection checkInfo)
                                , \() -> dropOnSmallerCollectionCheck { dropCount = count } listCollection checkInfo
                                , \() ->
                                    dropOnLargerConstructionFromListLiteralWillRemoveTheseElementsCheck { dropCount = count }
                                        listCollection
                                        checkInfo
                                ]
                                ()
                        )
            )
        ]


listUnzipChecks : IntoFnCheck
listUnzipChecks =
    intoFnCheckOnlyCall
        (callOnEmptyReturnsCheck { resultAsString = \_ -> "( [], [] )" } listCollection)



-- ARRAY FUNCTIONS


arrayToListChecks : IntoFnCheck
arrayToListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck { resultAsString = listCollection.empty.specific.asString } arrayCollection)
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Array.fromList
        , onSpecificFnCallCanBeCombinedCheck
            { args = [], earlierFn = Fn.Array.repeat, combinedFn = Fn.List.repeat }
        ]


arrayToIndexedListChecks : IntoFnCheck
arrayToIndexedListChecks =
    intoFnCheckOnlyCall
        (callOnEmptyReturnsCheck { resultAsString = listCollection.empty.specific.asString } arrayCollection)


arrayFromListChecks : IntoFnCheck
arrayFromListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (emptiableFromListChecks arrayCollection)
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Array.toList
        ]


arrayRepeatChecks : IntoFnCheck
arrayRepeatChecks =
    intoFnCheckOnlyCall (emptiableRepeatChecks arrayCollection)


arrayInitializeChecks : IntoFnCheck
arrayInitializeChecks =
    intoFnCheckOnlyCall (emptiableRepeatChecks arrayCollection)


arrayMapChecks : IntoFnCheck
arrayMapChecks =
    emptiableMapChecks arrayCollection


arrayIndexedMapChecks : IntoFnCheck
arrayIndexedMapChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck arrayCollection
        , intoFnCheckOnlyCall (operationWithExtraArgChecks { operationWithoutExtraArg = Fn.Array.map })
        ]


arrayIsEmptyChecks : IntoFnCheck
arrayIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks arrayCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.fromList
            , combinedFn = Fn.List.isEmpty
            }
        ]


arrayLengthChecks : IntoFnCheck
arrayLengthChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (firstThatConstructsJust
                [ collectionSizeChecks arrayCollection
                , arrayLengthOnArrayRepeatOrInitializeChecks
                ]
            )
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Array.fromList
            , combinedFn = Fn.List.length
            }
        ]


arrayGetChecks : IntoFnCheck
arrayGetChecks =
    intoFnCheckOnlyCall (getChecks arrayCollection)


arrayLengthOnArrayRepeatOrInitializeChecks : CallCheckInfo -> Maybe (Error {})
arrayLengthOnArrayRepeatOrInitializeChecks checkInfo =
    let
        maybeCall : Maybe ( String, { nodeRange : Range, fnRange : Range, firstArg : Node Expression, argsAfterFirst : List (Node Expression) } )
        maybeCall =
            firstThatConstructsJust
                [ \() ->
                    AstHelpers.getSpecificFnCall Fn.Array.repeat checkInfo.lookupTable checkInfo.firstArg
                        |> Maybe.map (Tuple.pair "repeat")
                , \() ->
                    AstHelpers.getSpecificFnCall Fn.Array.initialize checkInfo.lookupTable checkInfo.firstArg
                        |> Maybe.map (Tuple.pair "initialize")
                ]
                ()
    in
    case maybeCall of
        Just ( fnName, call ) ->
            let
                maxFn : String
                maxFn =
                    qualifiedToString (qualify Fn.Basics.max defaultQualifyResources)
            in
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString (qualify checkInfo.fn checkInfo) ++ " on an array created by " ++ qualifiedToString (qualify ( [ "Array" ], fnName ) defaultQualifyResources) ++ " with a given length will result in that length"
                    , details = [ "You can replace this call by " ++ maxFn ++ " 0 with the given length. " ++ maxFn ++ " 0 makes sure that negative given lengths return 0." ]
                    }
                    checkInfo.fnRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range call.firstArg }
                        ++ [ Fix.insertAt checkInfo.parentRange.start (qualifiedToString (qualify Fn.Basics.max checkInfo) ++ " 0 ") ]
                    )
                )

        Nothing ->
            Nothing


arraySetChecks : IntoFnCheck
arraySetChecks =
    setChecks arrayCollection


arraySliceChecks : IntoFnCheck
arraySliceChecks =
    collectionSliceChecks arrayCollection


arrayFilterChecks : IntoFnCheck
arrayFilterChecks =
    emptiableKeepWhenChecks arrayCollection


arrayAppendChecks : IntoFnCheck
arrayAppendChecks =
    intoFnCheckOnlyCall (collectionUnionChecks { leftElementsStayOnTheLeft = True } arrayCollection)


arrayFoldlChecks : IntoFnCheck
arrayFoldlChecks =
    intoFnCheckOnlyCall (emptiableFoldChecks arrayCollection)


arrayFoldrChecks : IntoFnCheck
arrayFoldrChecks =
    intoFnCheckOnlyCall (emptiableFoldChecks arrayCollection)



-- SET FUNCTIONS


setFromListChecks : IntoFnCheck
setFromListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (emptiableFromListChecks setCollection)
        , wrapperFromListSingletonChecks setCollection
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Set.toList
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if checkInfo.expectNaN then
                    Nothing

                else
                    case Node.value checkInfo.firstArg of
                        Expression.ListExpr elements ->
                            case List.map (Normalize.normalizeButKeepRange checkInfo) elements of
                                [] ->
                                    Nothing

                                first :: rest ->
                                    allValuesDifferent
                                        { message = "Set.fromList on a list with a duplicate key will only keep one of them"
                                        , details = [ "Maybe one of the keys was supposed to be a different value? If not, you can remove one of the duplicate keys." ]
                                        }
                                        first
                                        rest

                        _ ->
                            Nothing
            )
        ]


allValuesDifferent : { message : String, details : List String } -> Node Expression -> List (Node Expression) -> Maybe (Error {})
allValuesDifferent errorDetails (Node keyRange keyValue) otherKeysToCheck =
    case otherKeysToCheck of
        first :: rest ->
            if List.any (\(Node _ otherKey) -> otherKey == keyValue) otherKeysToCheck then
                Just
                    (Rule.errorWithFix
                        errorDetails
                        keyRange
                        [ Fix.removeRange
                            { start = keyRange.start
                            , end = (Node.range first).start
                            }
                        ]
                    )

            else
                allValuesDifferent errorDetails first rest

        [] ->
            -- key is the last element
            -- so it can't be equal to any other key
            Nothing


setIsEmptyChecks : IntoFnCheck
setIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks setCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Set.fromList
            , combinedFn = Fn.List.isEmpty
            }
        ]


setSizeChecks : IntoFnCheck
setSizeChecks =
    intoFnCheckOnlyCall (collectionSizeChecks setCollection)


setMemberChecks : IntoFnCheck
setMemberChecks =
    intoFnCheckOnlyCall (collectionMemberChecks setCollection)


setInsertChecks : IntoFnCheck
setInsertChecks =
    intoFnCheckOnlyCall (collectionInsertChecks setCollection)


setRemoveChecks : IntoFnCheck
setRemoveChecks =
    collectionRemoveChecks setCollection


setFilterChecks : IntoFnCheck
setFilterChecks =
    emptiableKeepWhenChecks setCollection


setPartitionChecks : IntoFnCheck
setPartitionChecks =
    intoFnCheckOnlyCall (collectionPartitionChecks setCollection)


setIntersectChecks : IntoFnCheck
setIntersectChecks =
    collectionIntersectChecks setCollection


setDiffChecks : IntoFnCheck
setDiffChecks =
    intoFnCheckOnlyCall (collectionDiffChecks setCollection)


setUnionChecks : IntoFnCheck
setUnionChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionUnionChecks { leftElementsStayOnTheLeft = True } setCollection)
        , withTwoEqualArgumentsReturnsLastCheck
        ]


setMapChecks : IntoFnCheck
setMapChecks =
    emptiableMapChecks setCollection


setToListChecks : IntoFnCheck
setToListChecks =
    intoFnCheckOnlyCall (emptiableToListChecks setCollection)


setFoldlChecks : IntoFnCheck
setFoldlChecks =
    intoFnCheckOnlyCall (emptiableFoldChecks setCollection)


setFoldrChecks : IntoFnCheck
setFoldrChecks =
    intoFnCheckOnlyCall (emptiableFoldChecks setCollection)



-- DICT FUNCTIONS


dictFromListChecks : IntoFnCheck
dictFromListChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (emptiableFromListChecks dictCollection)
        , onSpecificFnCallReturnsItsLastArgCheck Fn.Dict.toList
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if checkInfo.expectNaN then
                    Nothing

                else
                    case Node.value checkInfo.firstArg of
                        Expression.ListExpr elements ->
                            let
                                toEntry : Node Expression -> { entryRange : Range, first : Maybe (Node Expression) }
                                toEntry entry =
                                    { entryRange = Node.range entry
                                    , first =
                                        AstHelpers.getTuple2 checkInfo.lookupTable entry
                                            |> Maybe.map (\tuple -> Normalize.normalizeButKeepRange checkInfo tuple.first)
                                    }

                                knownEntryTuples : List { entryRange : Range, first : Maybe (Node Expression) }
                                knownEntryTuples =
                                    List.map toEntry elements
                            in
                            case knownEntryTuples of
                                [] ->
                                    Nothing

                                first :: rest ->
                                    allKeysDifferent first rest

                        _ ->
                            Nothing
            )
        ]


allKeysDifferent : { entryRange : Range, first : Maybe (Node Expression) } -> List { entryRange : Range, first : Maybe (Node Expression) } -> Maybe (Error {})
allKeysDifferent entry otherEntriesToCheck =
    case otherEntriesToCheck of
        nextEntry :: restOfEntries ->
            case entry.first of
                Just firstKey ->
                    if isAnyTheSameAsBy firstKey otherEntriesToCheck then
                        Just
                            (Rule.errorWithFix
                                { message =
                                    "Dict.fromList on entries with a duplicate key will only keep the last entry"
                                , details =
                                    [ "Maybe one of the keys was supposed to be a different value? If not, you can remove earlier entries with duplicate keys." ]
                                }
                                (Node.range firstKey)
                                [ Fix.removeRange
                                    { start = entry.entryRange.start
                                    , end = nextEntry.entryRange.start
                                    }
                                ]
                            )

                    else
                        allKeysDifferent nextEntry restOfEntries

                Nothing ->
                    allKeysDifferent nextEntry restOfEntries

        [] ->
            -- entry is the last element
            -- so it can't be equal to any other key
            Nothing


isAnyTheSameAsBy : Node Expression -> List { a | first : Maybe (Node Expression) } -> Bool
isAnyTheSameAsBy first rest =
    List.any
        (\node ->
            case node.first of
                Just element ->
                    Node.value element == Node.value first

                Nothing ->
                    False
        )
        rest


dictIsEmptyChecks : IntoFnCheck
dictIsEmptyChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionIsEmptyChecks dictCollection)
        , onSpecificFnCallCanBeCombinedCheck
            { args = []
            , earlierFn = Fn.Dict.fromList
            , combinedFn = Fn.List.isEmpty
            }
        ]


dictSizeChecks : IntoFnCheck
dictSizeChecks =
    intoFnCheckOnlyCall (collectionSizeChecks dictCollection)


dictMemberChecks : IntoFnCheck
dictMemberChecks =
    intoFnCheckOnlyCall (collectionMemberChecks dictCollection)


dictRemoveChecks : IntoFnCheck
dictRemoveChecks =
    collectionRemoveChecks dictCollection


dictFilterChecks : IntoFnCheck
dictFilterChecks =
    emptiableKeepWhenWithExtraArgChecks dictCollection


dictPartitionChecks : IntoFnCheck
dictPartitionChecks =
    intoFnCheckOnlyCall (emptiablePartitionWithExtraArgChecks dictCollection)


dictMapChecks : IntoFnCheck
dictMapChecks =
    emptiableMapWithExtraArgChecks dictCollection


dictIntersectChecks : IntoFnCheck
dictIntersectChecks =
    collectionIntersectChecks dictCollection


dictDiffChecks : IntoFnCheck
dictDiffChecks =
    intoFnCheckOnlyCall (collectionDiffChecks dictCollection)


dictUnionChecks : IntoFnCheck
dictUnionChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (collectionUnionChecks { leftElementsStayOnTheLeft = False } dictCollection)
        , withTwoEqualArgumentsReturnsLastCheck
        ]


dictToListChecks : IntoFnCheck
dictToListChecks =
    intoFnCheckOnlyCall (emptiableToListChecks dictCollection)


dictFoldlChecks : IntoFnCheck
dictFoldlChecks =
    intoFnCheckOnlyCall (emptiableFoldWithExtraArgChecks dictCollection)


dictFoldrChecks : IntoFnCheck
dictFoldrChecks =
    intoFnCheckOnlyCall (emptiableFoldWithExtraArgChecks dictCollection)



-- PLATFORM.CMD FUNCTIONS


platformCmdBatchChecks : IntoFnCheck
platformCmdBatchChecks =
    emptiableFlatFromListChecks cmdCollection


platformCmdMapChecks : IntoFnCheck
platformCmdMapChecks =
    emptiableMapChecks cmdCollection



-- PLATFORM.SUB FUNCTIONS


platformSubBatchChecks : IntoFnCheck
platformSubBatchChecks =
    emptiableFlatFromListChecks subCollection


platformSubChecks : IntoFnCheck
platformSubChecks =
    emptiableMapChecks subCollection



-- TASK FUNCTIONS


taskMapChecks : IntoFnCheck
taskMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks taskWithSucceedAsWrap
        , mapOnWrappedChecks taskWithSucceedAsWrap
        ]


taskMapNChecks : IntoFnCheck
taskMapNChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ wrapperMapNChecks taskWithSucceedAsWrap
            , mapNOrFirstEmptyConstructionChecks taskWithSucceedAsWrap
            ]
        )


taskAndThenChecks : IntoFnCheck
taskAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck taskWithSucceedAsWrap
        , wrapperFlatMapChecks taskWithSucceedAsWrap
        ]


taskMapErrorChecks : IntoFnCheck
taskMapErrorChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks taskWithFailAsWrap
        , mapOnWrappedChecks taskWithFailAsWrap
        ]


taskOnErrorChecks : IntoFnCheck
taskOnErrorChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck taskWithFailAsWrap
        , wrapperFlatMapChecks taskWithFailAsWrap
        ]


taskSequenceChecks : IntoFnCheck
taskSequenceChecks =
    intoFnChecksFirstThatConstructsError
        [ listOfWrapperSequenceChecks taskWithSucceedAsWrap
        , intoFnCheckOnlyCall
            (sequenceOrFirstEmptyChecks ( listCollection, taskWithSucceedAsWrap ))
        ]



-- HTML.ATTRIBUTES FUNCTIONS


getTupleWithSpecificSecondBoolExpressionNode : Bool -> ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
getTupleWithSpecificSecondBoolExpressionNode specificBool lookupTable expressionNode =
    AstHelpers.getTuple2Literal expressionNode
        |> Maybe.andThen
            (\tuple -> AstHelpers.getSpecificBool specificBool lookupTable tuple.second)
        |> Maybe.map (\_ -> expressionNode)


htmlAttributesClassListFalseElementError : CallCheckInfo -> { message : String, details : List String }
htmlAttributesClassListFalseElementError checkInfo =
    { message = "In a " ++ qualifiedToString checkInfo.fn ++ ", a tuple paired with False can be removed"
    , details = [ "You can remove the tuple list element where the second part is False." ]
    }


htmlAttributesClassListChecks : IntoFnCheck
htmlAttributesClassListChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ \checkInfo ->
                case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                    Just single ->
                        case AstHelpers.getTuple2Literal single.element of
                            Just tuple ->
                                case AstHelpers.getBool checkInfo.lookupTable tuple.second of
                                    Just bool ->
                                        if bool then
                                            let
                                                replacementFn : ( ModuleName, String )
                                                replacementFn =
                                                    Fn.Html.Attributes.class
                                            in
                                            Just
                                                (Rule.errorWithFix
                                                    { message = qualifiedToString checkInfo.fn ++ " with a single tuple paired with True can be replaced with " ++ qualifiedToString replacementFn
                                                    , details = [ "You can replace this call by " ++ qualifiedToString replacementFn ++ " with the String from the single tuple list element." ]
                                                    }
                                                    checkInfo.fnRange
                                                    (replaceBySubExpressionFix (Node.range checkInfo.firstArg) tuple.first
                                                        ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                                                (qualifiedToString (qualify replacementFn checkInfo))
                                                           ]
                                                    )
                                                )

                                        else
                                            Just
                                                (Rule.errorWithFix (htmlAttributesClassListFalseElementError checkInfo)
                                                    checkInfo.fnRange
                                                    [ Fix.replaceRangeBy (Node.range checkInfo.firstArg) "[]" ]
                                                )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            , \checkInfo ->
                case AstHelpers.getListLiteral checkInfo.firstArg of
                    Just (tuple0 :: tuple1 :: tuple2Up) ->
                        case findMapNeighboring (\el -> getTupleWithSpecificSecondBoolExpressionNode False checkInfo.lookupTable el) (tuple0 :: tuple1 :: tuple2Up) of
                            Just classPart ->
                                Just
                                    (Rule.errorWithFix (htmlAttributesClassListFalseElementError checkInfo)
                                        checkInfo.fnRange
                                        (listLiteralRemoveElementFix classPart)
                                    )

                            Nothing ->
                                Nothing

                    _ ->
                        Nothing
            , \checkInfo ->
                case AstHelpers.getCollapsedCons checkInfo.firstArg of
                    Just classParts ->
                        case findMapNeighboring (\el -> getTupleWithSpecificSecondBoolExpressionNode False checkInfo.lookupTable el) classParts.consed of
                            Just classPart ->
                                Just
                                    (Rule.errorWithFix (htmlAttributesClassListFalseElementError checkInfo)
                                        checkInfo.fnRange
                                        (collapsedConsRemoveElementFix
                                            { toRemove = classPart
                                            , tailRange = Node.range classParts.tail
                                            }
                                        )
                                    )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            ]
        )



-- JSON.DECODE FUNCTIONS


jsonDecodeMapChecks : IntoFnCheck
jsonDecodeMapChecks =
    intoFnChecksFirstThatConstructsError
        [ emptiableMapChecks jsonDecoderWithSucceedAsWrap
        , mapOnWrappedChecks jsonDecoderWithSucceedAsWrap
        ]


jsonDecodeMapNChecks : IntoFnCheck
jsonDecodeMapNChecks =
    intoFnCheckOnlyCall
        (firstThatConstructsJust
            [ wrapperMapNChecks jsonDecoderWithSucceedAsWrap
            , mapNOrFirstEmptyConstructionChecks jsonDecoderWithSucceedAsWrap
            ]
        )


jsonDecodeAndThenChecks : IntoFnCheck
jsonDecodeAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck jsonDecoderWithSucceedAsWrap
        , wrapperFlatMapChecks jsonDecoderWithSucceedAsWrap
        ]


jsonDecodeOneOfChecks : IntoFnCheck
jsonDecodeOneOfChecks =
    oneOfChecks



-- RANDOM FUNCTIONS


randomUniformChecks : IntoFnCheck
randomUniformChecks =
    intoFnCheckOnlyCall (oneOfConstantsWithOneAndRestListChecks randomGeneratorWrapper)


randomWeightedChecks : IntoFnCheck
randomWeightedChecks =
    intoFnCheckOnlyCall (oneOfWeightedConstantsWithOneAndRestChecks randomGeneratorWrapper)


randomListChecks : IntoFnCheck
randomListChecks =
    intoFnCheckOnlyCall (sequenceRepeatChecks randomGeneratorWrapper)


randomMapChecks : IntoFnCheck
randomMapChecks =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (mapIdentityChecks randomGeneratorWrapper)
        , mapOnWrappedChecks randomGeneratorWrapper
        , nonEmptiableWrapperMapAlwaysChecks randomGeneratorWrapper
        ]


randomAndThenChecks : IntoFnCheck
randomAndThenChecks =
    intoFnChecksFirstThatConstructsError
        [ wrapperFlatMapChecks randomGeneratorWrapper
        , intoFnCheckOnlyCall (nonEmptiableWrapperFlatMapAlwaysChecks randomGeneratorWrapper)
        ]



-- PARSER FUNCTIONS


parserOneOfChecks : IntoFnCheck
parserOneOfChecks =
    oneOfChecks



-- PARSER.ADVANCED FUNCTIONS


parserAdvancedOneOfChecks : IntoFnCheck
parserAdvancedOneOfChecks =
    oneOfChecks



-- TEST FUNCTIONS


testConcatChecks : IntoFnCheck
testConcatChecks =
    -- does not use emptiableFlatFromListChecks because test has no simple Test.none
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck listCollection
        , intoFnCheckOnlyCall flatFromListsSpreadFlatFromListElementsCheck
        ]



-- TYPE PROPERTIES


type alias TypeProperties properties =
    { properties
        | represents : String
    }


{-| Properties of a type that either holds some data or is "empty" with the given properties.
-}
type alias EmptiableProperties emptySpecificProperties otherProperties =
    { otherProperties
        | empty : TypeSubsetProperties emptySpecificProperties
    }


{-| Properties of a structure type that will always have data inside, for example a non-empty list, a `Test`, a `Benchmark` or a tree (but not a forest).

This can be really valuable, for example when you want to know whether the function of a map or flatMap will always be called.

The way this type is defined,
it is impossible to have one type that has both `EmptiableProperties` and `NonEmptiableProperties`

-}
type alias NonEmptiableProperties otherProperties =
    { otherProperties | empty : { invalid : () } }


{-| Properties of a type that has a construction function that takes one value.

Example "wrap" construction functions: `Just`, `Err`, `List.singleton` and `[ a ]`
Note that for example `Cmd.batch [ a ]` is not a "wrap" because it keeps the type of the inner value `a`

-}
type alias WrapperProperties otherProperties =
    { otherProperties
        | wrap : ConstructWithOneValueProperties
    }


{-| Properties of a type that can be constructed from a list, like String with String.fromList.
See `ConstructionFromList`
-}
type alias ConstructibleFromListProperties otherProperties =
    { otherProperties
        | fromList : ConstructionFromList
    }


{-| How the type can be constructed from a list.

  - `ConstructionAsList`: the type is an alias to a list, like
      - `type alias Forest a = List (Tree a)`
      - `type alias TreePath = List Index`
      - `type alias Options = List Option`
  - `ConstructionFromListCall`: the type can be constructed using a function that takes one list as the only argument, like
      - String.fromList : List Char -> String\`

-}
type ConstructionFromList
    = ConstructionAsList
    | ConstructionFromListCall ( ModuleName, String )


{-| Properties of a type with with multiple elements.
-}
type alias CollectionProperties otherProperties =
    { otherProperties
        | elements :
            { countDescription : String
            , determineCount : Infer.Resources {} -> Node Expression -> Maybe CollectionSize
            , get :
                Infer.Resources {}
                -> Node Expression
                ->
                    Maybe
                        { known : List (Node Expression)
                        , -- whether every contained element known.
                          -- E.g. in x :: xs we just know the first, not all elements
                          allKnown : Bool
                        }
            }
    }


{-| Properties of a type that under specific operations has a constant element that "annihilates"
all others, which means any application with one such element results in that element:

    a && False
    --> False

    False && a
    --> False

    List.all identity [ a, False, b ]
    --> False

    a || True
    --> True

    List.any identity [ a, True, b ]
    --> True

    Set.intersection Set.empty set
    --> Set.empty

    Set.intersection set Set.empty
    --> Set.empty

Even NaN falls into this category with +, \*, min, max:

    10 * Basics.max 10 (0 / 0) + 10
    --> 0 / 0 (NaN)

    10 - Basics.min 10 (0 / 0) * 10
    --> 0 / 0

And some properties only hold when `expectNaN` is not enabled, e.g.

    10 * 0
    --> 0

    List.product [ a, 0, b ]
    --> 0

    Basics.max a (1 / 0)
    --> 1 / 0 (Infinity)

    List.maximum [ a, 1 / 0, b ]
    --> Just (1 / 0)

More info: <https://en.wikipedia.org/wiki/Absorbing_element>

-}
type alias AbsorbableProperties otherProperties =
    { otherProperties
        | absorbing : ConstantProperties
    }


{-| Properties of a type that has a function that can change each current value given a function.
Examples are `Result.mapError`, `List.map` or `Random.map`.

Types with a `map` function typically have associated `WrapperProperties`.
For now, this has to be explicitly specified!

-}
type alias MappableProperties otherProperties =
    { otherProperties
        | mapFn : ( ModuleName, String )
    }


{-| Common properties of a specific set of values for a type.

Examples:

  - a task that is known to fail
  - a non-empty list with exactly one element
  - an empty string

The first 2 are examples of a subset with `ConstructWithOneValueProperties`,
the last one is an example of a subset with `ConstantProperties`

-}
type alias TypeSubsetProperties specificProperties =
    { specific : specificProperties
    , kind : specificProperties -> TypeSubsetKindProperties
    }


{-| Properties of a set of values.

  - Only one value is possible, like Cmd.none or [] → Constant
  - Multiple values are possible, like `Ok anyValue` or `[ onlyElementAnyValue ]`? → `A`/`An` depending on the indefinite article in front of the description

-}
type TypeSubsetKindProperties
    = ConstructWithOneValue ConstructWithOneValueProperties
    | Constant ConstantProperties


type alias ConstantProperties =
    { description : String
    , asString : QualifyResources {} -> String
    , is : Infer.Resources {} -> Node Expression -> Bool
    }


type alias ConstructWithOneValueProperties =
    { description : ConstructWithOneValueDescription
    , fn : ( ModuleName, String )
    , getValue : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
    }


type ConstructWithOneValueDescription
    = A String
    | An String


isInTypeSubset : TypeSubsetProperties specificProperties -> Infer.Resources res -> Node Expression -> Bool
isInTypeSubset typeSubsetProperties resources expressionNode =
    case typeSubsetProperties.kind typeSubsetProperties.specific of
        Constant constantProperties ->
            constantProperties.is (extractInferResources resources) expressionNode

        ConstructWithOneValue constructWithOneValue ->
            isJust (constructWithOneValue.getValue resources.lookupTable expressionNode)


typeSubsetDescriptionIndefinite : TypeSubsetProperties specificProperties -> String
typeSubsetDescriptionIndefinite typeSubsetProperties =
    case typeSubsetProperties.kind typeSubsetProperties.specific of
        Constant constantProperties ->
            constantProperties.description

        ConstructWithOneValue constructWithOneValue ->
            constructWithOneValueDescriptionIndefinite constructWithOneValue.description


typeSubsetDescriptionDefinite : String -> TypeSubsetProperties specificProperties -> String
typeSubsetDescriptionDefinite definiteArticle typeSubsetProperties =
    case typeSubsetProperties.kind typeSubsetProperties.specific of
        Constant constantProperties ->
            constantProperties.description

        ConstructWithOneValue constructWithOneValue ->
            constructWithOneValueDescriptionDefinite definiteArticle constructWithOneValue.description


typeSubsetDescriptionWithoutArticle : TypeSubsetProperties specificProperties -> String
typeSubsetDescriptionWithoutArticle typeSubsetProperties =
    case typeSubsetProperties.kind typeSubsetProperties.specific of
        Constant constantProperties ->
            constantProperties.description

        ConstructWithOneValue constructWithOneValue ->
            constructWithOneValueDescriptionWithoutArticle constructWithOneValue.description


{-| Create `ConstantProperties` for a value with a given fully qualified name.
-}
constantFnProperties : ( ModuleName, String ) -> ConstantProperties
constantFnProperties fullyQualified =
    { description = qualifiedToString (qualify fullyQualified defaultQualifyResources)
    , is =
        \res expr ->
            isJust (AstHelpers.getSpecificValueOrFn fullyQualified res.lookupTable expr)
    , asString =
        \res -> qualifiedToString (qualify fullyQualified res)
    }


{-| Create `ConstructWithOneValueProperties` for a function call with a given fully qualified name with a given `ConstructWithOneValueDescription`.
-}
fnCallConstructWithOneValueProperties : ConstructWithOneValueDescription -> ( ModuleName, String ) -> ConstructWithOneValueProperties
fnCallConstructWithOneValueProperties description fullyQualified =
    { description = description
    , fn = fullyQualified
    , getValue =
        \lookupTable expr ->
            Maybe.map .firstArg (AstHelpers.getSpecificFnCall fullyQualified lookupTable expr)
    }


getEmptyExpressionNode :
    Infer.Resources a
    -> EmptiableProperties empty otherProperties
    -> Node Expression
    -> Maybe (Node Expression)
getEmptyExpressionNode resources emptiable expressionNode =
    if isInTypeSubset emptiable.empty resources expressionNode then
        Just expressionNode

    else
        Nothing


getAbsorbingExpressionNode : AbsorbableProperties otherProperties -> Infer.Resources res -> Node Expression -> Maybe (Node Expression)
getAbsorbingExpressionNode absorbable inferResources expressionNode =
    if absorbable.absorbing.is (extractInferResources inferResources) expressionNode then
        Just expressionNode

    else
        Nothing


getValueWithNodeRange :
    (Node Expression -> Maybe (Node Expression))
    -> Node Expression
    -> Maybe { value : Node Expression, nodeRange : Range }
getValueWithNodeRange getValue expressionNode =
    Maybe.map (\value -> { value = value, nodeRange = Node.range expressionNode })
        (getValue expressionNode)


fromListGetLiteral : ConstructibleFromListProperties otherProperties -> ModuleNameLookupTable -> Node Expression -> Maybe { constructionNodeRange : Range, literalRange : Range, elements : List (Node Expression) }
fromListGetLiteral constructibleFromList lookupTable expressionNode =
    case constructibleFromList.fromList of
        ConstructionAsList ->
            case AstHelpers.removeParens expressionNode of
                Node listLiteralRange (Expression.ListExpr listElements) ->
                    Just { constructionNodeRange = Node.range expressionNode, literalRange = listLiteralRange, elements = listElements }

                _ ->
                    Nothing

        ConstructionFromListCall fromListFn ->
            case AstHelpers.getSpecificFnCall fromListFn lookupTable expressionNode of
                Just fromListCall ->
                    case AstHelpers.removeParens fromListCall.firstArg of
                        Node listLiteralRange (Expression.ListExpr listElements) ->
                            Just { constructionNodeRange = Node.range expressionNode, literalRange = listLiteralRange, elements = listElements }

                        _ ->
                            Nothing

                Nothing ->
                    Nothing


constructWithOneValueDescriptionIndefinite : ConstructWithOneValueDescription -> String
constructWithOneValueDescriptionIndefinite incomingArgDescription =
    case incomingArgDescription of
        A description ->
            "a " ++ description

        An description ->
            "an " ++ description


constructWithOneValueDescriptionDefinite : String -> ConstructWithOneValueDescription -> String
constructWithOneValueDescriptionDefinite startWithDefiniteArticle referenceArgDescription =
    case referenceArgDescription of
        A description ->
            startWithDefiniteArticle ++ " " ++ description

        An description ->
            startWithDefiniteArticle ++ " " ++ description


constructWithOneValueDescriptionWithoutArticle : ConstructWithOneValueDescription -> String
constructWithOneValueDescriptionWithoutArticle referenceArgDescription =
    case referenceArgDescription of
        A description ->
            description

        An description ->
            description


extractQualifyResources : QualifyResources a -> QualifyResources {}
extractQualifyResources resources =
    { importLookup = resources.importLookup
    , moduleBindings = resources.moduleBindings
    , localBindings = resources.localBindings
    }


extractInferResources : Infer.Resources a -> Infer.Resources {}
extractInferResources resources =
    { lookupTable = resources.lookupTable
    , inferredConstants = resources.inferredConstants
    }


emptyAsString : QualifyResources a -> EmptiableProperties ConstantProperties otherProperties -> String
emptyAsString qualifyResources emptiable =
    emptiable.empty.specific.asString (extractQualifyResources qualifyResources)


boolForAndProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
boolForAndProperties =
    { represents = "bool"
    , empty = { specific = boolTrueConstant, kind = Constant }
    , absorbing = boolFalseConstant
    }


boolForOrProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
boolForOrProperties =
    { represents = "bool"
    , empty = { specific = boolFalseConstant, kind = Constant }
    , absorbing = boolTrueConstant
    }


boolTrueConstant : ConstantProperties
boolTrueConstant =
    { description = qualifiedToString (qualify Fn.Basics.trueVariant defaultQualifyResources)
    , is = \res expr -> Evaluate.getBoolean res expr == Determined True
    , asString = \res -> qualifiedToString (qualify Fn.Basics.trueVariant res)
    }


boolFalseConstant : ConstantProperties
boolFalseConstant =
    { description = qualifiedToString (qualify Fn.Basics.falseVariant defaultQualifyResources)
    , is = \res expr -> Evaluate.getBoolean res expr == Determined False
    , asString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res)
    }


numberForAddProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
numberForAddProperties =
    { represents = "number"
    , empty = { specific = number0ConstantSpecific, kind = Constant }
    , absorbing = numberNaNConstantSpecific
    }


{-| Be aware that in elm, 0 is not absorbing for `(*)` (see `AbsorbableProperties`) because NaN can "overwrite" it.

    0 * (0 / 0)
    --> 0 / 0 (NaN)

In fact, NaN is an absorbing element for `(*)`.
If `expectingNaN` is not enabled, use `numberNotExpectingNaNForMultiplyProperties`.

-}
numberForMultiplyProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
numberForMultiplyProperties =
    { represents = "number"
    , empty = { specific = number1ConstantSpecific, kind = Constant }
    , absorbing = numberNaNConstantSpecific
    }


{-| If `expectingNaN` is enabled, 0 is not absorbing for `(*)` (see `AbsorbableProperties`) because NaN can "overwrite" it.

    0 * (0 / 0)
    --> 0 / 0 (NaN)

If that's the case, use `numberForMultiplyProperties`.

Not having `expectingNaN` enabled however, 0 _is_ absorbing, so we can now simplify e.g.

    List.product [ a, 0, b ]
    --> 0

(see `callOnListWithAbsorbingElement`)

-}
numberNotExpectingNaNForMultiplyProperties : TypeProperties (EmptiableProperties ConstantProperties (AbsorbableProperties {}))
numberNotExpectingNaNForMultiplyProperties =
    { represents = "number"
    , empty = { specific = number1ConstantSpecific, kind = Constant }
    , absorbing = number0ConstantSpecific
    }


number0ConstantSpecific : ConstantProperties
number0ConstantSpecific =
    { description = "0"
    , is = \res expr -> Evaluate.getNumber res expr == Just 0
    , asString = \_ -> "0"
    }


number1ConstantSpecific : ConstantProperties
number1ConstantSpecific =
    { description = "1"
    , is = \res expr -> Evaluate.getNumber res expr == Just 1
    , asString = \_ -> "1"
    }


numberNaNConstantSpecific : ConstantProperties
numberNaNConstantSpecific =
    { description = "NaN"
    , is =
        \res expr ->
            case AstHelpers.removeParens expr of
                Node _ (Expression.OperatorApplication "/" _ dividend divisor) ->
                    (Evaluate.getNumber res dividend == Just 0)
                        && (Evaluate.getNumber res divisor == Just 0)

                _ ->
                    False
    , asString = \_ -> "(0 / 0)"
    }


randomGeneratorWrapper : TypeProperties (NonEmptiableProperties (WrapperProperties (MappableProperties {})))
randomGeneratorWrapper =
    { represents = "random generator"
    , wrap = randomGeneratorConstantConstruct
    , empty = { invalid = () }
    , mapFn = Fn.Random.map
    }


randomGeneratorConstantConstruct : ConstructWithOneValueProperties
randomGeneratorConstantConstruct =
    fnCallConstructWithOneValueProperties (A "constant generator") Fn.Random.constant


maybeWithJustAsWrap : TypeProperties (EmptiableProperties ConstantProperties (WrapperProperties (MappableProperties {})))
maybeWithJustAsWrap =
    { represents = "maybe"
    , empty = { specific = constantFnProperties Fn.Maybe.nothingVariant, kind = Constant }
    , wrap = maybeJustConstruct
    , mapFn = Fn.Maybe.map
    }


maybeJustConstruct : ConstructWithOneValueProperties
maybeJustConstruct =
    fnCallConstructWithOneValueProperties (A "just maybe") Fn.Maybe.justVariant


resultWithOkAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
resultWithOkAsWrap =
    { represents = "result"
    , wrap = resultOkayConstruct
    , empty = { specific = resultErrorConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Result.map
    }


resultOkayConstruct : ConstructWithOneValueProperties
resultOkayConstruct =
    fnCallConstructWithOneValueProperties (An "okay result") Fn.Result.okVariant


resultErrorConstruct : ConstructWithOneValueProperties
resultErrorConstruct =
    fnCallConstructWithOneValueProperties (An "error") Fn.Result.errVariant


resultWithErrAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
resultWithErrAsWrap =
    { represents = "result"
    , wrap = resultErrorConstruct
    , empty = { specific = resultOkayConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Result.mapError
    }


taskWithSucceedAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
taskWithSucceedAsWrap =
    { represents = "task"
    , wrap = taskSucceedingConstruct
    , empty = { specific = taskFailingConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Task.map
    }


taskSucceedingConstruct : ConstructWithOneValueProperties
taskSucceedingConstruct =
    fnCallConstructWithOneValueProperties (A "succeeding task") Fn.Task.succeed


taskFailingConstruct : ConstructWithOneValueProperties
taskFailingConstruct =
    fnCallConstructWithOneValueProperties (A "failing task") Fn.Task.fail


taskWithFailAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
taskWithFailAsWrap =
    { represents = "task"
    , wrap = taskFailingConstruct
    , empty = { specific = taskSucceedingConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Task.mapError
    }


jsonDecoderWithSucceedAsWrap : TypeProperties (WrapperProperties (EmptiableProperties ConstructWithOneValueProperties (MappableProperties {})))
jsonDecoderWithSucceedAsWrap =
    { represents = "json decoder"
    , wrap = jsonDecoderSucceedingConstruct
    , empty = { specific = jsonDecoderFailingConstruct, kind = ConstructWithOneValue }
    , mapFn = Fn.Json.Decode.map
    }


jsonDecoderSucceedingConstruct : ConstructWithOneValueProperties
jsonDecoderSucceedingConstruct =
    fnCallConstructWithOneValueProperties (A "succeeding decoder") Fn.Json.Decode.succeed


jsonDecoderFailingConstruct : ConstructWithOneValueProperties
jsonDecoderFailingConstruct =
    fnCallConstructWithOneValueProperties (A "failing decoder") Fn.Json.Decode.fail


listCollection : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties (WrapperProperties (ConstructibleFromListProperties (MappableProperties {})))))
listCollection =
    { represents = "list"
    , empty = { specific = listEmptyConstantSpecific, kind = Constant }
    , elements =
        { get = listGetElements
        , countDescription = "length"
        , determineCount = listDetermineLength
        }
    , wrap = listSingletonConstruct
    , mapFn = Fn.List.map
    , fromList = ConstructionAsList
    }


listEmptyConstantSpecific : ConstantProperties
listEmptyConstantSpecific =
    { description = "[]"
    , is = \_ expr -> AstHelpers.getListLiteral expr == Just []
    , asString = \_ -> "[]"
    }


listSingletonConstruct : ConstructWithOneValueProperties
listSingletonConstruct =
    { description = A "singleton list"
    , fn = Fn.List.singleton
    , getValue =
        \lookupTable expr ->
            Maybe.map .element (AstHelpers.getListSingleton lookupTable expr)
    }


listGetElements : Infer.Resources a -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
listGetElements resources =
    firstThatConstructsJust
        [ \expressionNode ->
            expressionNode
                |> AstHelpers.getListLiteral
                |> Maybe.map (\list -> { known = list, allKnown = True })
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.List.singleton resources.lookupTable
                |> Maybe.map (\singletonCall -> { known = [ singletonCall.firstArg ], allKnown = True })
        , \expressionNode ->
            case AstHelpers.removeParens expressionNode of
                Node _ (Expression.OperatorApplication "::" _ head tail) ->
                    case listGetElements resources tail of
                        Just tailElements ->
                            Just { known = head :: tailElements.known, allKnown = tailElements.allKnown }

                        Nothing ->
                            Just { known = [ head ], allKnown = False }

                _ ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.removeParens expressionNode of
                Node _ (Expression.OperatorApplication "++" _ leftList rightList) ->
                    case
                        listGetElements resources leftList
                            |> Maybe.map
                                (\lefts ->
                                    ( lefts
                                    , if lefts.allKnown then
                                        listGetElements resources rightList

                                      else
                                        Nothing
                                    )
                                )
                    of
                        Just ( leftElements, Just rightElements ) ->
                            Just { allKnown = rightElements.allKnown, known = leftElements.known ++ rightElements.known }

                        Just ( leftElements, Nothing ) ->
                            Just { known = leftElements.known, allKnown = False }

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        ]


listDetermineLength : Infer.Resources a -> Node Expression -> Maybe CollectionSize
listDetermineLength resources =
    firstThatConstructsJust
        [ \expressionNode ->
            expressionNode
                |> AstHelpers.getListLiteral
                |> Maybe.map (\list -> Exactly (List.length list))
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.List.singleton resources.lookupTable
                |> Maybe.map (\_ -> Exactly 1)
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.List.repeat resources.lookupTable expressionNode of
                Just repeatCall ->
                    Evaluate.getInt resources repeatCall.firstArg
                        |> Maybe.map (\n -> Exactly (max 0 n))

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.List.range resources.lookupTable expressionNode of
                Just rangeCall ->
                    case ( Evaluate.getInt resources rangeCall.firstArg, Maybe.andThen (Evaluate.getInt resources) (List.head rangeCall.argsAfterFirst) ) of
                        ( Just start, Just end ) ->
                            Just (Exactly (max 0 (end - start)))

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.removeParens expressionNode of
                Node _ (Expression.OperatorApplication "::" _ _ right) ->
                    maybeCollectionSizeAdd1 (listDetermineLength resources right)

                _ ->
                    Nothing
        ]


stringCollection : TypeProperties (CollectionProperties (WrapperProperties (EmptiableProperties ConstantProperties (ConstructibleFromListProperties {}))))
stringCollection =
    { represents = "string"
    , empty = { specific = stringEmptyConstantSpecific, kind = Constant }
    , elements =
        { countDescription = "length"
        , determineCount = stringDetermineLength
        , get = stringGetElements
        }
    , wrap = singleCharConstruct
    , fromList = ConstructionFromListCall Fn.String.fromList
    }


stringEmptyConstantSpecific : ConstantProperties
stringEmptyConstantSpecific =
    { description = emptyStringAsString
    , asString = \_ -> emptyStringAsString
    , is = \_ (Node _ expr) -> expr == Expression.Literal ""
    }


singleCharConstruct : ConstructWithOneValueProperties
singleCharConstruct =
    fnCallConstructWithOneValueProperties (A "single-char string") Fn.String.fromChar


stringDetermineLength : Infer.Resources res -> Node Expression -> Maybe CollectionSize
stringDetermineLength resources =
    firstThatConstructsJust
        [ \expressionNode ->
            case AstHelpers.removeParens expressionNode of
                Node _ (Expression.Literal string) ->
                    Just (Exactly (String.length string))

                _ ->
                    Nothing
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.String.fromChar resources.lookupTable
                |> Maybe.map (\_ -> Exactly 1)
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.String.fromList resources.lookupTable
                |> Maybe.andThen (\fromListCall -> listDetermineLength resources fromListCall.firstArg)
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.String.cons resources.lookupTable
                |> Maybe.andThen
                    (\consCall ->
                        maybeCollectionSizeAdd1
                            (Maybe.andThen (stringDetermineLength resources) (List.head consCall.argsAfterFirst))
                    )
        ]


maybeCollectionSizeAdd1 : Maybe CollectionSize -> Maybe CollectionSize
maybeCollectionSizeAdd1 collectionSize =
    case collectionSize of
        Nothing ->
            Just NotEmpty

        Just NotEmpty ->
            Just NotEmpty

        Just (Exactly tailLength) ->
            Just (Exactly (1 + tailLength))


stringGetElements : Infer.Resources res -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
stringGetElements resources =
    firstThatConstructsJust
        [ \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.String.fromChar resources.lookupTable
                |> Maybe.map (\fromCharCall -> { known = [ fromCharCall.firstArg ], allKnown = True })
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.String.fromList resources.lookupTable
                |> Maybe.andThen (\fromListCall -> listGetElements resources fromListCall.firstArg)
        ]


arrayCollection : TypeProperties (CollectionProperties (ConstructibleFromListProperties (EmptiableProperties ConstantProperties {})))
arrayCollection =
    { represents = "array"
    , empty = { specific = constantFnProperties Fn.Array.empty, kind = Constant }
    , elements =
        { countDescription = "length"
        , determineCount = arrayDetermineLength
        , get = arrayGetElements
        }
    , fromList = ConstructionFromListCall Fn.Array.fromList
    }


arrayGetElements : Infer.Resources a -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
arrayGetElements resources =
    firstThatConstructsJust
        [ \expressionNode ->
            case AstHelpers.getSpecificValueOrFn Fn.Array.empty resources.lookupTable expressionNode of
                Just _ ->
                    Just { known = [], allKnown = True }

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.Array.fromList resources.lookupTable expressionNode of
                Just fromListCall ->
                    listGetElements resources fromListCall.firstArg

                Nothing ->
                    Nothing
        ]


arrayDetermineLength : Infer.Resources a -> Node Expression -> Maybe CollectionSize
arrayDetermineLength resources =
    firstThatConstructsJust
        [ \expressionNode ->
            case AstHelpers.getSpecificValueOrFn Fn.Array.empty resources.lookupTable expressionNode of
                Just _ ->
                    Just (Exactly 0)

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.Array.fromList resources.lookupTable expressionNode of
                Just fromListCall ->
                    listDetermineLength resources fromListCall.firstArg

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.Array.repeat resources.lookupTable expressionNode of
                Just repeatCall ->
                    Evaluate.getInt resources repeatCall.firstArg
                        |> Maybe.map (\n -> Exactly (max 0 n))

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.Array.initialize resources.lookupTable expressionNode of
                Just repeatCall ->
                    Evaluate.getInt resources repeatCall.firstArg
                        |> Maybe.map (\n -> Exactly (max 0 n))

                Nothing ->
                    Nothing
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.Array.push resources.lookupTable
                |> Maybe.andThen
                    (\pushCall ->
                        maybeCollectionSizeAdd1
                            (Maybe.andThen (arrayDetermineLength resources) (List.head pushCall.argsAfterFirst))
                    )
        ]


setCollection : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties (WrapperProperties (ConstructibleFromListProperties {}))))
setCollection =
    { represents = "set"
    , empty = { specific = constantFnProperties Fn.Set.empty, kind = Constant }
    , elements =
        { countDescription = "size"
        , determineCount = setDetermineSize
        , get = setGetElements
        }
    , wrap = setSingletonConstruct
    , fromList = ConstructionFromListCall Fn.Set.fromList
    }


setSingletonConstruct : ConstructWithOneValueProperties
setSingletonConstruct =
    fnCallConstructWithOneValueProperties (A "singleton set") Fn.Set.singleton


setGetElements : Infer.Resources a -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
setGetElements resources =
    firstThatConstructsJust
        [ \expressionNode ->
            case AstHelpers.getSpecificValueOrFn Fn.Set.empty resources.lookupTable expressionNode of
                Just _ ->
                    Just { known = [], allKnown = True }

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.Set.singleton resources.lookupTable expressionNode of
                Just singletonCall ->
                    Just { known = [ singletonCall.firstArg ], allKnown = True }

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.Set.fromList resources.lookupTable expressionNode of
                Just fromListCall ->
                    case listGetElements resources fromListCall.firstArg of
                        Just listElements ->
                            case traverse getComparableWithExpressionNode listElements.known of
                                Just comparableElements ->
                                    Just
                                        { known = uniqueBy .comparable comparableElements |> List.map .expressionNode
                                        , allKnown = listElements.allKnown
                                        }

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        ]


getComparableWithExpressionNode : Node Expression -> Maybe { comparable : List Expression, expressionNode : Node Expression }
getComparableWithExpressionNode expressionNode =
    getComparableExpression expressionNode
        |> Maybe.map (\comparable -> { comparable = comparable, expressionNode = expressionNode })


setDetermineSize : Infer.Resources res -> Node Expression -> Maybe CollectionSize
setDetermineSize resources =
    firstThatConstructsJust
        [ \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificValueOrFn Fn.Set.empty resources.lookupTable
                |> Maybe.map (\_ -> Exactly 0)
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.Set.singleton resources.lookupTable
                |> Maybe.map (\_ -> Exactly 1)
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.Set.fromList resources.lookupTable expressionNode of
                Just fromListCall ->
                    case listGetElements resources fromListCall.firstArg of
                        Just listElements ->
                            if listElements.allKnown then
                                case traverse getComparableExpression listElements.known of
                                    Just comparableListElements ->
                                        comparableListElements |> unique |> List.length |> Exactly |> Just

                                    Nothing ->
                                        case listElements.known of
                                            [] ->
                                                Nothing

                                            _ :: [] ->
                                                Just (Exactly 1)

                                            _ :: _ :: _ ->
                                                Just NotEmpty

                            else
                                case listElements.known of
                                    [] ->
                                        Nothing

                                    _ :: _ ->
                                        Just NotEmpty

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        ]


dictCollection : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties (ConstructibleFromListProperties {})))
dictCollection =
    { represents = "dict"
    , empty = { specific = constantFnProperties Fn.Dict.empty, kind = Constant }
    , elements =
        { countDescription = "size"
        , determineCount = dictDetermineSize
        , get = dictGetValues
        }
    , fromList = ConstructionFromListCall Fn.Dict.fromList
    }


dictDetermineSize :
    Infer.Resources a
    -> Node Expression
    -> Maybe CollectionSize
dictDetermineSize resources =
    firstThatConstructsJust
        [ \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificValueOrFn Fn.Dict.empty resources.lookupTable
                |> Maybe.map (\_ -> Exactly 0)
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.Dict.singleton resources.lookupTable
                |> Maybe.andThen
                    (\singletonCall ->
                        case singletonCall.argsAfterFirst of
                            _ :: [] ->
                                Just (Exactly 1)

                            _ ->
                                Nothing
                    )
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall Fn.Dict.fromList resources.lookupTable expressionNode of
                Just fromListCall ->
                    case listGetElements resources fromListCall.firstArg of
                        Just listElements ->
                            if listElements.allKnown then
                                case traverse (getTupleWithComparableFirst resources.lookupTable) listElements.known of
                                    Just comparableKeyExpressions ->
                                        comparableKeyExpressions |> uniqueBy .comparableFirst |> List.length |> Exactly |> Just

                                    Nothing ->
                                        case listElements.known of
                                            [] ->
                                                Nothing

                                            _ :: [] ->
                                                Just (Exactly 1)

                                            _ :: _ :: _ ->
                                                Just NotEmpty

                            else
                                case listElements.known of
                                    [] ->
                                        Nothing

                                    _ :: _ ->
                                        Just NotEmpty

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        ]


dictGetValues : Infer.Resources res -> Node Expression -> Maybe { known : List (Node Expression), allKnown : Bool }
dictGetValues resources =
    firstThatConstructsJust
        [ \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificValueOrFn Fn.Dict.empty resources.lookupTable
                |> Maybe.map (\_ -> { known = [], allKnown = True })
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.Dict.singleton resources.lookupTable
                |> Maybe.andThen
                    (\singletonCall ->
                        case singletonCall.argsAfterFirst of
                            singletonValue :: [] ->
                                Just { known = [ singletonValue ], allKnown = True }

                            _ ->
                                Nothing
                    )
        , \expressionNode ->
            expressionNode
                |> AstHelpers.getSpecificFnCall Fn.Dict.fromList resources.lookupTable
                |> Maybe.andThen
                    (\fromListCall ->
                        case listGetElements resources fromListCall.firstArg of
                            Just listElements ->
                                if listElements.allKnown then
                                    case traverse (getTupleWithComparableFirst resources.lookupTable) listElements.known of
                                        Just tuplesWithComparableKey ->
                                            Just
                                                { known = uniqueBy .comparableFirst tuplesWithComparableKey |> List.map .second
                                                , allKnown = True
                                                }

                                        Nothing ->
                                            Nothing

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )
        ]


getTupleWithComparableFirst : ModuleNameLookupTable -> Node Expression -> Maybe { comparableFirst : List Expression, second : Node Expression }
getTupleWithComparableFirst lookupTable expressionNode =
    case AstHelpers.getTuple2 lookupTable expressionNode of
        Just tuple ->
            case getComparableExpression tuple.first of
                Just comparableFirst ->
                    Just { comparableFirst = comparableFirst, second = tuple.second }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


cmdCollection : TypeProperties (EmptiableProperties ConstantProperties {})
cmdCollection =
    { represents = "command"
    , empty = { specific = constantFnProperties Fn.Platform.Cmd.none, kind = Constant }
    }


subCollection : TypeProperties (EmptiableProperties ConstantProperties {})
subCollection =
    { represents = "subscription"
    , empty = { specific = constantFnProperties Fn.Platform.Sub.none, kind = Constant }
    }



-- CHECKS FOR GENERIC TYPES
{-
   Abstract names used for operations in the generic checks below

   #### "flat"

   Turns a wrapper of a type into _that exact type_.

   Examples:

       String.concat : List String -> String
       Test.concat : List Test -> Test
       Range.combine : List Range -> Range
       Bytes.Encode.sequence : List Encoder -> Encoder
       Result.Extra.merge : Result a a -> a
       Platform.Cmd.batch : List (Cmd a) -> Cmd a
       List.concat : List (List a) -> List a
       Maybe.Extra.join : Maybe (Maybe a) -> Maybe a
       List.NonEmpty.concat : Nonempty (Nonempty a) -> Nonempty a
       Parser.Sequence.concat : List (Parser (List a)) -> Parser (List a) -- by lambda-phi

   Note the difference to "sequence":

   #### "sequence"

   Turns a wrapper of a type with an argument `a` into _that type with argument `wrapper a`_.

   Examples:

       Random.Extra.sequence : List (Generator a) -> Generator (List a)
       Json.Extra.sequence : List (Decoder a) -> Decoder (List a)
       Task.sequence : List (Task x a) -> Task x (List a)
       Maybe.Extra.combineArray : Array (Maybe a) -> Maybe (Array a)
       -- ↓ don't exist
       Result.Maybe.toMaybe : Result x (Maybe a) -> Maybe (Result x e)
       Maybe.Function.applyIfJust : Maybe (a -> b) -> (a -> Maybe b)
       Result.Function.applyIfOk : Result x (a -> b) -> (a -> Result x b)
       List.Function.feedEach : List (a -> b) -> (a -> List b)

   #### someName ++ otherName

   You'll find names that are a combination of two operation names like "sequenceRepeat" or "sequenceMap".
   How to interpret them?

   "sequenceMap ..args" for example is equivalent to `sequence (map ..args)` which means:
   First map the type, then apply a sequence operation to the result.

   This intuition works for all these.
   So "sequenceRepeat ..args" is equivalent to `sequence (repeat ..args)` which means:
   First create the collection using repeat, then apply a sequence operation to the result.

   #### handle ++ "when"

   For each value, only "handle" it when the given function results in True.

   Examples:

       Array.Extra.removeWhen : (a -> Bool) -> Array a -> Array a
       List.Extra.setIf : (a -> Bool) -> a -> List a -> List a
       List.Extra.updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
       Set.filter : (comparable -> Bool) -> Set comparable -> Set comparable
       Maybe.Extra.filter : (a -> Bool) -> Maybe a -> Maybe a
-}


oneOfChecks : IntoFnCheck
oneOfChecks =
    onWrappedReturnsItsValueCheck listCollection


{-| Checks for a "oneOfConstants" operation:

    oneOfConstants firstPossibility [] --> wrap firstPossibility

so for example

    Random.uniform firstPossibility []
    --> Random.constant firstPossibility

Note that this is different (mostly in terms of error info) from both e.g.

    List.NonEmpty.fromCons firstState [] --> List.NonEmpty.singleton firstState

    Set.insert first Set.empty --> Set.singleton first

(For the second, you can use `collectionInsertChecks`.)

Note also that "oneOfConstants" is different from "oneOf":

    Codec.oneOf : Codec a -> List (Codec a) -> Codec a
    -- vs
    Random.uniform : a -> List a -> Random.Generator a

where the simplification would be

    Code.oneOf codec [] --> codec

-}
oneOfConstantsWithOneAndRestListChecks : WrapperProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
oneOfConstantsWithOneAndRestListChecks wrapper checkInfo =
    case secondArg checkInfo of
        Just otherOptionsArg ->
            if listCollection.empty.specific.is (extractInferResources checkInfo) otherOptionsArg then
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with one possible value will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with that value"
                        , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the first given value." ]
                        }
                        checkInfo.fnRange
                        (Fix.replaceRangeBy checkInfo.fnRange
                            (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                            :: keepOnlyFix
                                { parentRange = checkInfo.parentRange
                                , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                }
                        )
                    )

            else
                Nothing

        Nothing ->
            Nothing


{-| The "oneOfWeightedConstants" checks

    oneOfWeightedConstants ( w, a ) [] --> wrap a

    oneOfWeightedConstants weighted [] --> wrap (Tuple.first weighted)

Examples of such functions:

    Random.weighted : ( Float, a ) -> List ( Float, a ) -> Generator a
    -- ↓ doesn't exist
    SucceedingFuzzer.frequencyValues : ( Float, a ) -> List ( Float, a ) -> SucceedingFuzzer a

-}
oneOfWeightedConstantsWithOneAndRestChecks : WrapperProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
oneOfWeightedConstantsWithOneAndRestChecks wrapper checkInfo =
    case secondArg checkInfo of
        Just otherOptionsArg ->
            if listCollection.empty.specific.is (extractInferResources checkInfo) otherOptionsArg then
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with one possible value will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with that value"
                        , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the first given value." ]
                        }
                        checkInfo.fnRange
                        (case AstHelpers.getTuple2 checkInfo.lookupTable checkInfo.firstArg of
                            Just tuple ->
                                keepOnlyFix
                                    { parentRange = checkInfo.parentRange
                                    , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                    }
                                    ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                            (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                       ]
                                    ++ replaceBySubExpressionFix (Node.range checkInfo.firstArg) tuple.second

                            Nothing ->
                                let
                                    tupleArgRange : Range
                                    tupleArgRange =
                                        Node.range checkInfo.firstArg
                                in
                                keepOnlyFix
                                    { parentRange = checkInfo.parentRange
                                    , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                    }
                                    ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                            (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                       , Fix.insertAt tupleArgRange.start
                                            ("(" ++ qualifiedToString Fn.Tuple.first ++ " ")
                                       , Fix.insertAt tupleArgRange.end ")"
                                       ]
                        )
                    )

            else
                Nothing

        Nothing ->
            Nothing


{-| The map checks

    map f empty --> empty

    map identity emptiable --> emptiable

If your mapping function also takes extra information like the key or index as an argument, use `emptiableMapWithExtraArgChecks`.

-}
emptiableMapChecks :
    TypeProperties (EmptiableProperties empty otherProperties)
    -> IntoFnCheck
emptiableMapChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall (mapIdentityChecks emptiable)
        , unnecessaryOnEmptyCheck emptiable
        ]


mapIdentityChecks :
    TypeProperties properties
    -> CallCheckInfo
    -> Maybe (Error {})
mapIdentityChecks mappable checkInfo =
    if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
        Just
            (alwaysReturnsLastArgError
                (qualifiedToString checkInfo.fn ++ " with an identity function")
                mappable
                checkInfo
            )

    else
        Nothing


{-| The map checks

    map f empty --> empty

    map (\_ v -> v) emptiable --> emptiable

If your mapping function only takes one value as an argument, use `emptiableMapChecks`.

-}
emptiableMapWithExtraArgChecks : TypeProperties (EmptiableProperties empty otherProperties) -> IntoFnCheck
emptiableMapWithExtraArgChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck emptiable
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                    Just alwaysResult ->
                        if AstHelpers.isIdentity checkInfo.lookupTable alwaysResult then
                            Just
                                (alwaysReturnsLastArgError
                                    (qualifiedToString checkInfo.fn ++ " with a function that maps to the unchanged value")
                                    emptiable
                                    checkInfo
                                )

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )
        ]


{-| The map check

    map f (wrap a) --> wrap (f a)

    map f << wrap --> wrap << f

So for example

    Random.map f (Random.constant a)
    --> Random.constant (f a)

    Random.map f << Random.constant
    --> Random.constant << f

-}
mapOnWrappedChecks : WrapperProperties otherProperties -> IntoFnCheck
mapOnWrappedChecks wrapper =
    { call =
        \checkInfo ->
            case secondArg checkInfo of
                Just wrapperArg ->
                    firstThatConstructsJust
                        [ \() ->
                            case wrapper.wrap.getValue checkInfo.lookupTable wrapperArg of
                                Just wrappedValue ->
                                    let
                                        mappedValueRange : Range
                                        mappedValueRange =
                                            Node.range wrappedValue

                                        mappingArgRange : Range
                                        mappingArgRange =
                                            Node.range checkInfo.firstArg
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            (mapWrapErrorInfo checkInfo.fn wrapper)
                                            checkInfo.fnRange
                                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range wrapperArg }
                                                ++ parenthesizeIfNeededFix wrappedValue
                                                ++ (case checkInfo.callStyle of
                                                        Pipe LeftToRight ->
                                                            [ Fix.insertAt mappedValueRange.start "("
                                                            , Fix.insertAt mappedValueRange.end
                                                                (" |> " ++ checkInfo.extractSourceCode mappingArgRange ++ ")")
                                                            ]

                                                        Pipe RightToLeft ->
                                                            [ Fix.insertAt mappedValueRange.start ("(" ++ checkInfo.extractSourceCode mappingArgRange ++ " <| ")
                                                            , Fix.insertAt mappedValueRange.end ")"
                                                            ]

                                                        Application ->
                                                            [ Fix.insertAt mappedValueRange.start ("(" ++ checkInfo.extractSourceCode mappingArgRange ++ " ")
                                                            , Fix.insertAt mappedValueRange.end ")"
                                                            ]
                                                   )
                                            )
                                        )

                                Nothing ->
                                    Nothing
                        , \() ->
                            case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) wrapperArg of
                                Just wraps ->
                                    let
                                        mappingArgRange : Range
                                        mappingArgRange =
                                            Node.range checkInfo.firstArg

                                        removeWrapCalls : List Fix
                                        removeWrapCalls =
                                            List.concatMap
                                                (\wrap ->
                                                    keepOnlyFix
                                                        { parentRange = wrap.nodeRange
                                                        , keep = Node.range wrap.value
                                                        }
                                                )
                                                wraps
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            (mapWrapErrorInfo checkInfo.fn wrapper)
                                            checkInfo.fnRange
                                            (case checkInfo.callStyle of
                                                Pipe LeftToRight ->
                                                    [ Fix.removeRange { start = checkInfo.fnRange.start, end = mappingArgRange.start }
                                                    , Fix.insertAt mappingArgRange.end
                                                        (" |> " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                                    ]
                                                        ++ removeWrapCalls

                                                Pipe RightToLeft ->
                                                    Fix.replaceRangeBy
                                                        { start = checkInfo.parentRange.start, end = mappingArgRange.start }
                                                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " <| ")
                                                        :: removeWrapCalls

                                                Application ->
                                                    [ Fix.replaceRangeBy
                                                        { start = checkInfo.parentRange.start, end = mappingArgRange.start }
                                                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " (")
                                                    , Fix.insertAt checkInfo.parentRange.end ")"
                                                    ]
                                                        ++ removeWrapCalls
                                            )
                                        )

                                Nothing ->
                                    Nothing
                        ]
                        ()

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            case ( checkInfo.earlier.fn == wrapper.wrap.fn, checkInfo.later.args ) of
                ( True, (Node mapperFunctionRange _) :: _ ) ->
                    Just
                        { info = mapWrapErrorInfo checkInfo.later.fn wrapper
                        , fix =
                            [ Fix.replaceRangeBy checkInfo.later.range
                                (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                            , Fix.replaceRangeBy checkInfo.earlier.range
                                (checkInfo.extractSourceCode mapperFunctionRange)
                            ]
                        }

                _ ->
                    Nothing
    }


{-| The map check

    map (always a) wrapper --> wrap a

    map << always --> always << wrap

So for example

    Random.map (always a) generator
    --> Random.constant a

    Random.map << always
    --: a -> Generator a -> Generator a
    --> always << Random.constant

-}
nonEmptiableWrapperMapAlwaysChecks : NonEmptiableProperties (WrapperProperties otherProperties) -> IntoFnCheck
nonEmptiableWrapperMapAlwaysChecks wrapper =
    { call =
        \checkInfo ->
            case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just (Node alwaysMapResultRange alwaysMapResult) ->
                    let
                        ( leftParenIfRequired, rightParenIfRequired ) =
                            if needsParens alwaysMapResult then
                                ( "(", ")" )

                            else
                                ( "", "" )
                    in
                    Just
                        (case secondArg checkInfo of
                            Nothing ->
                                Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " with a function that always maps to the same value will always result in " ++ qualifiedToString wrapper.wrap.fn ++ " with that value"
                                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the value produced by the mapper function." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy
                                        { start = checkInfo.parentRange.start, end = alwaysMapResultRange.start }
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo)
                                            ++ " ("
                                            ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo)
                                            ++ " "
                                            ++ leftParenIfRequired
                                        )
                                    , Fix.replaceRangeBy
                                        { start = alwaysMapResultRange.end, end = checkInfo.parentRange.end }
                                        (rightParenIfRequired ++ ")")
                                    ]

                            Just _ ->
                                Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " with a function that always maps to the same value will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with that value"
                                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the value produced by the mapper function." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy
                                        { start = checkInfo.parentRange.start, end = alwaysMapResultRange.start }
                                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo)
                                            ++ " "
                                            ++ leftParenIfRequired
                                        )
                                    , Fix.replaceRangeBy
                                        { start = alwaysMapResultRange.end, end = checkInfo.parentRange.end }
                                        rightParenIfRequired
                                    ]
                        )

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            case ( ( checkInfo.earlier.fn, checkInfo.earlier.args ), checkInfo.later.args ) of
                ( ( ( [ "Basics" ], "always" ), [] ), [] ) ->
                    let
                        equivalent : String
                        equivalent =
                            qualifiedToString wrapper.wrap.fn ++ ", then `always`"
                    in
                    Just
                        { info =
                            { message = qualifiedToString checkInfo.later.fn ++ " with a function that always maps to the same value is equivalent to " ++ equivalent
                            , details = [ "You can replace this call by " ++ equivalent ++ "." ]
                            }
                        , fix =
                            [ Fix.replaceRangeBy checkInfo.earlier.fnRange
                                (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                            , Fix.replaceRangeBy checkInfo.later.fnRange
                                (qualifiedToString (qualify Fn.Basics.always checkInfo))
                            ]
                        }

                _ ->
                    Nothing
    }


{-| The map checks

    flatMap (always emptyConstant) emptyConstant --> emptyConstant

    flatMap f empty --> empty

So for example

    List.concatMap (always []) list --> []

    List.concatMap f [] --> []

-}
emptiableFlatMapChecks : EmptiableProperties ConstantProperties otherProperties -> IntoFnCheck
emptiableFlatMapChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck emptiable
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case constructs (sameInAllBranches (getEmptyExpressionNode checkInfo emptiable)) checkInfo.lookupTable checkInfo.firstArg of
                    Just _ ->
                        Just
                            (alwaysResultsInUnparenthesizedConstantError
                                (qualifiedToString checkInfo.fn ++ " with a function that will always return " ++ emptiable.empty.specific.description)
                                { replacement = emptiable.empty.specific.asString }
                                checkInfo
                            )

                    Nothing ->
                        Nothing
            )
        ]


{-| `flatMap f` on a wrapped value is equivalent to `f`

    flatMap f (wrap a) --> f a

    flatMap f << wrap --> f

    flatMap wrap wrapper --> wrapper

    flatMap (\a -> wrap b) wrapper --> map (\a -> b) wrapper

    flatMap (wrap << f) wrapper --> map f wrapper

So for example

    List.concatMap f [ a ] --> f a

    List.concatMap f << List.singleton --> f

-}
wrapperFlatMapChecks :
    TypeProperties (WrapperProperties (MappableProperties otherProperties))
    -> IntoFnCheck
wrapperFlatMapChecks wrapper =
    intoFnChecksFirstThatConstructsError
        [ { call =
                \checkInfo ->
                    case secondArg checkInfo of
                        Just maybeArg ->
                            case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) maybeArg of
                                Just wrapCalls ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as applying the function to the value from " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description
                                            , details = [ "You can replace this call by the function directly applied to the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ "." ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                                                :: List.concatMap (\justCall -> replaceBySubExpressionFix justCall.nodeRange justCall.value) wrapCalls
                                            )
                                        )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
          , composition =
                \checkInfo ->
                    case ( wrapper.wrap.fn == checkInfo.earlier.fn, checkInfo.later.args ) of
                        ( True, (Node functionRange _) :: [] ) ->
                            Just
                                { info =
                                    { message = qualifiedToString checkInfo.later.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as applying the function to the value from " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description
                                    , details = [ "You can replace this composition by the function given to " ++ qualifiedToString checkInfo.later.fn ++ "." ]
                                    }
                                , fix =
                                    Fix.removeRange checkInfo.earlier.removeRange
                                        :: keepOnlyFix { parentRange = checkInfo.later.range, keep = functionRange }
                                }

                        _ ->
                            Nothing
          }
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getSpecificValueOrFn wrapper.wrap.fn checkInfo.lookupTable checkInfo.firstArg of
                    Just _ ->
                        Just
                            (alwaysReturnsLastArgError
                                (qualifiedToString checkInfo.fn ++ " with a function equivalent to " ++ qualifiedToString (qualify wrapper.wrap.fn defaultQualifyResources))
                                wrapper
                                checkInfo
                            )

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case constructsOrComposesInto wrapper.wrap checkInfo.lookupTable checkInfo.firstArg of
                    Just withoutWrap ->
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " with a function that always returns " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as " ++ qualifiedToString wrapper.mapFn ++ " with the function returning the value inside"
                                , details = [ "You can replace this call by " ++ qualifiedToString wrapper.mapFn ++ " with the function returning the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ "." ]
                                }
                                checkInfo.fnRange
                                (Fix.replaceRangeBy checkInfo.fnRange
                                    (qualifiedToString (qualify wrapper.mapFn checkInfo))
                                    :: withoutWrap
                                )
                            )

                    Nothing ->
                        Nothing
            )
        ]


withDefaultChecks :
    WrapperProperties (EmptiableProperties empty otherProperties)
    -> IntoFnCheck
withDefaultChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck emptiable
        , intoFnCheckOnlyCall (emptiableWithDefaultChecks emptiable)
        ]


{-| The flatMap check

    flatMap (always nextWrapper) wrapper --> nextWrapper

So for example

    Random.andThen (always nextGenerator) generator
    --> nextGenerator

-}
nonEmptiableWrapperFlatMapAlwaysChecks :
    TypeProperties (NonEmptiableProperties (WrapperProperties otherProperties))
    -> CallCheckInfo
    -> Maybe (Error {})
nonEmptiableWrapperFlatMapAlwaysChecks wrapper checkInfo =
    case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
        Just alwaysResult ->
            Just
                (let
                    replacementAndFix : { replacementDescription : String, fix : List Fix }
                    replacementAndFix =
                        case secondArg checkInfo of
                            Nothing ->
                                { replacementDescription = "always with the " ++ wrapper.represents ++ " produced by the function"
                                , fix = keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg }
                                }

                            Just _ ->
                                { replacementDescription = "the " ++ wrapper.represents ++ " produced by the function"
                                , fix = replaceBySubExpressionFix checkInfo.parentRange alwaysResult
                                }
                 in
                 Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with a function that always returns to the same " ++ wrapper.represents ++ " will result in that " ++ wrapper.represents
                    , details = [ "You can replace this call by " ++ replacementAndFix.replacementDescription ++ "." ]
                    }
                    checkInfo.fnRange
                    replacementAndFix.fix
                )

        Nothing ->
            Nothing


{-| The "withDefault" checks

    withDefault default empty --> default

so for example

    Result.withDefault "okay" (Err 1)
    --> "okay"

-}
emptiableWithDefaultChecks :
    EmptiableProperties empty otherProperties
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableWithDefaultChecks emptiable checkInfo =
    case secondArg checkInfo of
        Just emptiableArg ->
            case sameInAllBranches (getEmptyExpressionNode checkInfo emptiable) emptiableArg of
                Just _ ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on " ++ typeSubsetDescriptionIndefinite emptiable.empty ++ " will result in the default value"
                            , details = [ "You can replace this call by the default value." ]
                            }
                            checkInfo.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


unwrapToMaybeChecks :
    WrapperProperties (EmptiableProperties empty otherProperties)
    -> IntoFnCheck
unwrapToMaybeChecks emptiableWrapper =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsJustItsValueCheck emptiableWrapper
        , intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck
                { resultAsString = \res -> qualifiedToString (qualify Fn.Maybe.nothingVariant res) }
                emptiableWrapper
            )
        ]


fromMaybeWithEmptyValueOnNothingCheck : WrapperProperties (EmptiableProperties ConstructWithOneValueProperties otherProperties) -> IntoFnCheck
fromMaybeWithEmptyValueOnNothingCheck wrapper =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Just maybeArg ->
                        case sameInAllBranches (AstHelpers.getSpecificValueOrFn Fn.Maybe.nothingVariant checkInfo.lookupTable) maybeArg of
                            Just _ ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " on Nothing will result in " ++ qualifiedToString (qualify wrapper.empty.specific.fn checkInfo) ++ " with the given first value"
                                        , details = [ "You can replace this call by " ++ qualifiedToString (qualify wrapper.empty.specific.fn checkInfo) ++ " with the given first value." ]
                                        }
                                        checkInfo.fnRange
                                        (Fix.replaceRangeBy checkInfo.fnRange
                                            (qualifiedToString (qualify wrapper.empty.specific.fn checkInfo))
                                            :: keepOnlyFix
                                                { parentRange = checkInfo.parentRange
                                                , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                                }
                                        )
                                    )

                            Nothing ->
                                Nothing

                    Nothing ->
                        Nothing
            )
        , { call =
                \checkInfo ->
                    case secondArg checkInfo of
                        Just maybeArg ->
                            case sameInAllBranches (AstHelpers.getSpecificFnCall Fn.Maybe.justVariant checkInfo.lookupTable) maybeArg of
                                Just justCalls ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " on a just maybe will result in " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " with the value inside"
                                            , details = [ "You can replace this call by " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " with the value inside the given just maybe." ]
                                            }
                                            checkInfo.fnRange
                                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range maybeArg }
                                                ++ List.map
                                                    (\justCall ->
                                                        Fix.replaceRangeBy justCall.fnRange
                                                            (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                                    )
                                                    justCalls
                                            )
                                        )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
          , composition =
                \checkInfo ->
                    case ( checkInfo.earlier.fn, checkInfo.later.args ) of
                        ( ( [ "Maybe" ], "Just" ), _ :: [] ) ->
                            Just
                                { info =
                                    { message = qualifiedToString checkInfo.later.fn ++ " on a just maybe will result in " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " with the value inside"
                                    , details = [ "You can replace this call by " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ "." ]
                                    }
                                , fix =
                                    [ Fix.removeRange checkInfo.later.removeRange
                                    , Fix.replaceRangeBy checkInfo.earlier.range (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                    ]
                                }

                        _ ->
                            Nothing
          }
        ]


{-| The "flatFromList" checks

    flatFromList []
    --> empty

    flatFromList [ emptiable ]
    --> emptiable

    flatFromList [ aEmptiable, empty, bEmptiable ]
    --> flatFromList [ aEmptiable, bEmptiable ]

    flatFromList [ a, flatFromList [ b, c ], d ]
    --> flatFromList [ a, b, c, d ]

So for example with `emptiableFlatFromListChecks stringCollection`

    String.concat []
    --> ""

    String.concat [ string ]
    --> string

    String.concat [ "hello", "", "world" ]
    --> String.concat [ "hello", "world" ]

    String.concat [ "a", String.concat [ b, c ], d ]
    --> String.concat [ "a", b, c, d ]

-}
emptiableFlatFromListChecks : EmptiableProperties ConstantProperties otherProperties -> IntoFnCheck
emptiableFlatFromListChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ onWrappedReturnsItsValueCheck listCollection
        , intoFnCheckOnlyCall (callOnEmptyReturnsCheck { resultAsString = emptiable.empty.specific.asString } listCollection)
        , intoFnCheckOnlyCall
            (\checkInfo ->
                callOnFromListWithIrrelevantEmptyElement (qualifiedToString (qualify checkInfo.fn defaultQualifyResources))
                    ( listCollection, emptiable )
                    checkInfo
            )
        , intoFnCheckOnlyCall flatFromListsSpreadFlatFromListElementsCheck
        ]


{-| The "flatFromList" check

    flatFromList [ a, flatFromList [ b, c ], d ]
    --> flatFromList [ a, b, c, d ]

So for example

    String.concat [ "a", String.concat [ b, c ], d ]
    --> String.concat [ "a", b, c, d ]

-}
flatFromListsSpreadFlatFromListElementsCheck : CallCheckInfo -> Maybe (Error {})
flatFromListsSpreadFlatFromListElementsCheck checkInfo =
    case AstHelpers.getListLiteral checkInfo.firstArg of
        Just listLiteralElements ->
            let
                getFlatFromListOnLiteralCheck : Node Expression -> Maybe { callNodeRange : Range, literalRange : Range }
                getFlatFromListOnLiteralCheck expressionNode =
                    AstHelpers.getSpecificFnCall checkInfo.fn checkInfo.lookupTable expressionNode
                        |> Maybe.andThen
                            (\flatFromListCall ->
                                case flatFromListCall.firstArg of
                                    Node literalRange (Expression.ListExpr _) ->
                                        Just { callNodeRange = Node.range expressionNode, literalRange = literalRange }

                                    _ ->
                                        Nothing
                            )
            in
            case List.filterMap getFlatFromListOnLiteralCheck listLiteralElements of
                nestedFromListLiteral0 :: nestedFromListLiteral1Up ->
                    let
                        flatFromListOnLiteralDescription : String
                        flatFromListOnLiteralDescription =
                            qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " call"
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "Nested " ++ flatFromListOnLiteralDescription ++ "s can be spread"
                            , details = [ "You can move the elements from the inner " ++ flatFromListOnLiteralDescription ++ "s to inside this outer " ++ flatFromListOnLiteralDescription ++ "." ]
                            }
                            checkInfo.fnRange
                            (List.concatMap
                                (\nestedFromListLiteral ->
                                    keepOnlyFix { parentRange = nestedFromListLiteral.callNodeRange, keep = rangeWithoutBoundaries nestedFromListLiteral.literalRange }
                                )
                                (nestedFromListLiteral0 :: nestedFromListLiteral1Up)
                            )
                        )

                [] ->
                    Nothing

        Nothing ->
            Nothing


callOnFromListWithIrrelevantEmptyElement :
    String
    ->
        ( TypeProperties (ConstructibleFromListProperties otherProperties)
        , EmptiableProperties empty elementOtherProperties
        )
    -> CallCheckInfo
    -> Maybe (Error {})
callOnFromListWithIrrelevantEmptyElement situation ( constructibleFromList, emptiableElement ) checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just collectionArg ->
            case fromListGetLiteral constructibleFromList checkInfo.lookupTable collectionArg of
                Just listLiteral ->
                    case findMapNeighboring (getEmptyExpressionNode checkInfo emptiableElement) listLiteral.elements of
                        Just emptyLiteralAndNeighbors ->
                            Just
                                (Rule.errorWithFix
                                    { message = situation ++ " on a " ++ constructibleFromList.represents ++ " containing an irrelevant " ++ typeSubsetDescriptionWithoutArticle emptiableElement.empty
                                    , details = [ "Including " ++ typeSubsetDescriptionDefinite "the" emptiableElement.empty ++ " in the " ++ constructibleFromList.represents ++ " does not change the result of this call. You can remove the " ++ typeSubsetDescriptionWithoutArticle emptiableElement.empty ++ " element." ]
                                    }
                                    (Node.range emptyLiteralAndNeighbors.found)
                                    (listLiteralRemoveElementFix emptyLiteralAndNeighbors)
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| The check to unify neighboring constructions from a literal list in a literal list

    fromList construction
        with
        [ a
        , fromList construction with [ b, c ]
        , fromList construction with [ d, e ]
        , fromList construction with [ f, g ]
        , h
        ]

    -->
    fromList construction
        with
        [ a
        , fromList construction with [ b, c, d, e, f, g ]
        , h
        ]

-}
mergeConsecutiveFromListLiteralsCheck : ConstructibleFromListProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
mergeConsecutiveFromListLiteralsCheck constructibleFromList checkInfo =
    case Maybe.andThen (fromListGetLiteral constructibleFromList checkInfo.lookupTable) (fullyAppliedLastArg checkInfo) of
        Just listLiteral ->
            case mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn Nothing [] constructibleFromList checkInfo listLiteral.elements of
                firstFix :: fixesAfterFirst ->
                    Just
                        (Rule.errorWithFix
                            { message = "Consecutive " ++ constructionFromListOnLiteralDescription constructibleFromList.fromList ++ "s can be merged"
                            , details = [ "Try moving all the elements from consecutive " ++ constructionFromListOnLiteralDescription constructibleFromList.fromList ++ "s so that they form a single list." ]
                            }
                            checkInfo.fnRange
                            (firstFix :: fixesAfterFirst)
                        )

                [] ->
                    Nothing

        Nothing ->
            Nothing


mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn : Maybe { literalInsertLocation : Location, elementEndLocation : Location } -> List Fix -> ConstructibleFromListProperties otherProperties -> { resources | lookupTable : ModuleNameLookupTable, extractSourceCode : Range -> String } -> List (Node Expression) -> List Fix
mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn maybeBefore fixEnding constructibleFromList resources listElements =
    case listElements of
        [] ->
            fixEnding

        first :: afterFirst ->
            case fromListGetLiteral constructibleFromList resources.lookupTable first of
                Nothing ->
                    mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn Nothing fixEnding constructibleFromList resources afterFirst

                Just firstFromListLiteral ->
                    case maybeBefore of
                        Just before ->
                            mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn
                                (Just
                                    { literalInsertLocation = before.literalInsertLocation
                                    , elementEndLocation = (Node.range first).end
                                    }
                                )
                                (Fix.removeRange
                                    { start = before.elementEndLocation, end = (Node.range first).end }
                                    :: Fix.insertAt before.literalInsertLocation
                                        (", " ++ resources.extractSourceCode (rangeWithoutBoundaries firstFromListLiteral.literalRange))
                                    :: fixEnding
                                )
                                constructibleFromList
                                resources
                                afterFirst

                        Nothing ->
                            mergeConsecutiveFromListLiteralsFixWithBeforeEndingIn
                                (Just
                                    { literalInsertLocation = endWithoutBoundary firstFromListLiteral.literalRange
                                    , elementEndLocation = (Node.range first).end
                                    }
                                )
                                fixEnding
                                constructibleFromList
                                resources
                                afterFirst


{-| The range checks

    range 5 4 --> empty

So for example

    List.range 5 4 --> []

-}
emptiableRangeChecks : EmptiableProperties ConstantProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
emptiableRangeChecks emptiable checkInfo =
    case secondArg checkInfo of
        Just rangeEndArg ->
            case ( Evaluate.getInt checkInfo checkInfo.firstArg, Evaluate.getInt checkInfo rangeEndArg ) of
                ( Just rangeStartValue, Just rangeEndValue ) ->
                    if rangeStartValue > rangeEndValue then
                        Just
                            (resultsInConstantError
                                (qualifiedToString checkInfo.fn ++ " with a start index greater than the end index")
                                emptiable.empty.specific.asString
                                checkInfo
                            )

                    else
                        Nothing

                ( Nothing, _ ) ->
                    Nothing

                ( _, Nothing ) ->
                    Nothing

        Nothing ->
            Nothing


{-| Turn `yourFn identity` into `replacementFn`. If `replacementFn` should be `identity`, use `alwaysReturnsLastArgError` instead

Can be used to for example

  - turn `traverse identity` into `sequence`
  - turn `List.filterMap identity` into `Maybe.Extra.values`
  - turn `List.Extra.minimumBy identity` into `List.minimum`

-}
operationWithIdentityIsEquivalentToFnCheck : ( ModuleName, String ) -> CallCheckInfo -> Maybe (Error {})
operationWithIdentityIsEquivalentToFnCheck replacementFn checkInfo =
    if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
        Just
            (operationWithFirstArgIsEquivalentToFnError
                { replacementFn = replacementFn
                , firstArgDescription = "an identity function"
                }
                checkInfo
            )

    else
        Nothing


{-| Check for a function that given an empty separator is equivalent to a given replacement operation.

    flatIntersperse empty something
    --> replacementOperation something

So for example

    List.Extra.intercalate : List a -> List (List a) -> List a
    List.Extra.intercalate [] list --> List.concat list

    DList.intersperse : DList a -> List (DList a) -> DList a
    DList.intersperse DList.empty dList --> DList.concat dList

Note that this really only applies to "flat-intersperse-like" functions, not for example

    concatMap identity type --> concat type
    -- where identity would be the "empty function"

for that specific example, there is `operationWithIdentityIsEquivalentToFnCheck`

-}
flatIntersperseWithEmptySeparatorIsEquivalentToFnCheck : EmptiableProperties empty otherProperties -> ( ModuleName, String ) -> CallCheckInfo -> Maybe (Error {})
flatIntersperseWithEmptySeparatorIsEquivalentToFnCheck elementProperties replacementFn checkInfo =
    if isInTypeSubset elementProperties.empty checkInfo checkInfo.firstArg then
        Just
            (operationWithFirstArgIsEquivalentToFnError
                { replacementFn = replacementFn
                , firstArgDescription = "separator " ++ typeSubsetDescriptionWithoutArticle elementProperties.empty
                }
                checkInfo
            )

    else
        Nothing


operationWithFirstArgIsEquivalentToFnError : { firstArgDescription : String, replacementFn : ( ModuleName, String ) } -> CallCheckInfo -> Error {}
operationWithFirstArgIsEquivalentToFnError config checkInfo =
    Rule.errorWithFix
        { message = qualifiedToString checkInfo.fn ++ " with " ++ config.firstArgDescription ++ " is the same as " ++ qualifiedToString config.replacementFn
        , details = [ "You can replace this call by " ++ qualifiedToString config.replacementFn ++ "." ]
        }
        checkInfo.fnRange
        [ Fix.replaceRangeBy
            (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
            (qualifiedToString (qualify config.replacementFn checkInfo))
        ]


{-| Map where the usual map function has an extra argument with special information.

For example `indexedMap` also supplied an index. Not using the index would be identical to `map`.

Another example would be [`List.Extra.indexedFoldl`](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#indexedFoldl) which also supplies the current index.
Not using the path would be identical to `List.foldl`.

-}
operationWithExtraArgChecks : { operationWithoutExtraArg : ( ModuleName, String ) } -> CallCheckInfo -> Maybe (Error {})
operationWithExtraArgChecks config checkInfo =
    case getReplaceAlwaysByItsResultFix checkInfo.lookupTable checkInfo.firstArg of
        Just replaceAlwaysByFunctionResult ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with a function that ignores the first argument is the same as " ++ qualifiedToString config.operationWithoutExtraArg
                    , details = [ "You can replace this call by " ++ qualifiedToString config.operationWithoutExtraArg ++ "." ]
                    }
                    checkInfo.fnRange
                    (Fix.replaceRangeBy checkInfo.fnRange
                        (qualifiedToString (qualify config.operationWithoutExtraArg checkInfo))
                        :: replaceAlwaysByFunctionResult
                    )
                )

        Nothing ->
            Nothing


getReplaceAlwaysByItsResultFix : ModuleNameLookupTable -> Node Expression -> Maybe (List Fix)
getReplaceAlwaysByItsResultFix lookupTable expressionNode =
    case AstHelpers.removeParens expressionNode of
        Node _ (Expression.LambdaExpression lambda) ->
            case lambda.args of
                firstArg :: argsAfterFirst ->
                    case AstHelpers.removeParensFromPattern firstArg of
                        Node _ Pattern.AllPattern ->
                            case argsAfterFirst of
                                [] ->
                                    Just (keepOnlyFix { parentRange = Node.range expressionNode, keep = Node.range lambda.expression })

                                (Node secondRange _) :: _ ->
                                    Just
                                        [ Fix.removeRange { start = (Node.range firstArg).start, end = secondRange.start } ]

                        _ ->
                            Nothing

                [] ->
                    Nothing

        _ ->
            case AstHelpers.getSpecificFnCall Fn.Basics.always lookupTable expressionNode of
                Just alwaysCall ->
                    Just
                        (replaceBySubExpressionFix alwaysCall.nodeRange alwaysCall.firstArg)

                Nothing ->
                    Nothing


wrapperMemberChecks : TypeProperties (WrapperProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
wrapperMemberChecks wrapper checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just wrapperArg ->
            case wrapper.wrap.getValue checkInfo.lookupTable wrapperArg of
                Just wrapValue ->
                    let
                        needleArgRange : Range
                        needleArgRange =
                            Node.range checkInfo.firstArg
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ " are equal." ]
                            }
                            checkInfo.fnRange
                            (List.concat
                                [ keepOnlyFix
                                    { parentRange = checkInfo.parentRange
                                    , keep = Range.combine [ needleArgRange, Node.range wrapValue ]
                                    }
                                , [ Fix.replaceRangeBy
                                        (rangeBetweenExclusive ( needleArgRange, Node.range wrapValue ))
                                        " == "
                                  ]
                                , parenthesizeIfNeededFix wrapValue
                                ]
                            )
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


knownMemberChecks : TypeProperties (CollectionProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
knownMemberChecks collection checkInfo =
    if checkInfo.expectNaN then
        Nothing

    else
        case fullyAppliedLastArg checkInfo of
            Just collectionArg ->
                case collection.elements.get (extractInferResources checkInfo) collectionArg of
                    Just collectionElements ->
                        let
                            needleArg : Node Expression
                            needleArg =
                                checkInfo.firstArg

                            needleArgNormalized : Node Expression
                            needleArgNormalized =
                                Normalize.normalize checkInfo needleArg

                            isNeedle : Node Expression -> Bool
                            isNeedle element =
                                Normalize.compareWithoutNormalization
                                    (Normalize.normalize checkInfo element)
                                    needleArgNormalized
                                    == Normalize.ConfirmedEquality
                        in
                        if List.any isNeedle collectionElements.known then
                            Just
                                (resultsInConstantError
                                    (qualifiedToString checkInfo.fn ++ " on a " ++ collection.represents ++ " which contains the given element")
                                    (\res -> qualifiedToString (qualify Fn.Basics.trueVariant res))
                                    checkInfo
                                )

                        else
                            Nothing

                    Nothing ->
                        Nothing

            Nothing ->
                Nothing


{-| Replace a call on a collection containing an absorbing element to the absorbing element.
See `AbsorbableProperties` for details.

    operation ..args.. (collection containing absorbing)
    --> absorbing

So for example with `( listCollection, boolForAndProperties )`

    List.product [ a, 0, b ]
    --> 0

with `( listCollection, numberNotExpectingNaNForMultiplyProperties )` and a check for identity

    List.all identity [ a, False, b ]
    --> False

-}
callOnCollectionWithAbsorbingElementChecks :
    String
    -> ( TypeProperties (CollectionProperties otherProperties), AbsorbableProperties elementOtherProperties )
    -> CallCheckInfo
    -> Maybe (Error {})
callOnCollectionWithAbsorbingElementChecks situation ( collection, elementAbsorbable ) checkInfo =
    case Maybe.andThen (collection.elements.get (extractInferResources checkInfo)) (fullyAppliedLastArg checkInfo) of
        Just elements ->
            case findMap (getAbsorbingExpressionNode elementAbsorbable checkInfo) elements.known of
                Just absorbingElement ->
                    Just
                        (Rule.errorWithFix
                            { message = situation ++ " on a " ++ collection.represents ++ " with " ++ elementAbsorbable.absorbing.description ++ " will result in " ++ elementAbsorbable.absorbing.description
                            , details =
                                [ "You can replace this call by " ++ elementAbsorbable.absorbing.asString defaultQualifyResources ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange absorbingElement)
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


emptiableAllChecks : EmptiableProperties empty otherProperties -> CallCheckInfo -> Maybe (Error {})
emptiableAllChecks emptiable =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck
            { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.trueVariant res) }
            emptiable
        , \checkInfo ->
            case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just alwaysResult ->
                    case Evaluate.getBoolean checkInfo alwaysResult of
                        Determined True ->
                            Just
                                (alwaysResultsInUnparenthesizedConstantError
                                    (qualifiedToString checkInfo.fn ++ " with a function that will always return True")
                                    { replacement = \res -> qualifiedToString (qualify Fn.Basics.trueVariant res) }
                                    checkInfo
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        ]


collectionAllChecks : TypeProperties (CollectionProperties (ConstructibleFromListProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionAllChecks collection =
    firstThatConstructsJust
        [ \checkInfo ->
            if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
                firstThatConstructsJust
                    [ \() ->
                        callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn ++ " with an identity function")
                            ( collection, boolForAndProperties )
                            checkInfo
                    , \() ->
                        callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with an identity function")
                            ( collection, boolForAndProperties )
                            checkInfo
                    ]
                    ()

            else
                Nothing
        , \checkInfo ->
            case AstHelpers.getSpecificValueOrFn Fn.Basics.not checkInfo.lookupTable checkInfo.firstArg of
                Just _ ->
                    case Maybe.andThen (collection.elements.get (extractInferResources checkInfo)) (fullyAppliedLastArg checkInfo) of
                        Just elements ->
                            firstThatConstructsJust
                                [ \() ->
                                    if List.any (boolTrueConstant.is (extractInferResources checkInfo)) elements.known then
                                        Just
                                            (Rule.errorWithFix
                                                { message = qualifiedToString checkInfo.fn ++ " with `not` on a " ++ collection.represents ++ " with True will result in False"
                                                , details =
                                                    [ "You can replace this call by False." ]
                                                }
                                                checkInfo.fnRange
                                                [ Fix.replaceRangeBy checkInfo.parentRange
                                                    (qualifiedToString (qualify Fn.Basics.falseVariant checkInfo))
                                                ]
                                            )

                                    else
                                        Nothing
                                , \() ->
                                    callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with `not`")
                                        ( collection, { empty = { specific = boolFalseConstant, kind = Constant } } )
                                        checkInfo
                                ]
                                ()

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        ]


emptiableAnyChecks : EmptiableProperties empty otherProperties -> CallCheckInfo -> Maybe (Error {})
emptiableAnyChecks emptiable =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck
            { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
            emptiable
        , \checkInfo ->
            case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just alwaysResult ->
                    case Evaluate.getBoolean checkInfo alwaysResult of
                        Determined False ->
                            Just
                                (alwaysResultsInUnparenthesizedConstantError
                                    (qualifiedToString checkInfo.fn ++ " with a function that will always return False")
                                    { replacement = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
                                    checkInfo
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        ]


collectionAnyChecks : TypeProperties (CollectionProperties (ConstructibleFromListProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionAnyChecks collection =
    firstThatConstructsJust
        [ \checkInfo ->
            if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
                firstThatConstructsJust
                    [ \() ->
                        callOnCollectionWithAbsorbingElementChecks (qualifiedToString checkInfo.fn ++ " with an identity function")
                            ( collection, boolForOrProperties )
                            checkInfo
                    , \() ->
                        callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with an identity function")
                            ( collection, boolForOrProperties )
                            checkInfo
                    ]
                    ()

            else
                Nothing
        , \checkInfo ->
            case AstHelpers.getSpecificValueOrFn Fn.Basics.not checkInfo.lookupTable checkInfo.firstArg of
                Just _ ->
                    case Maybe.andThen (collection.elements.get (extractInferResources checkInfo)) (fullyAppliedLastArg checkInfo) of
                        Just elements ->
                            firstThatConstructsJust
                                [ \() ->
                                    if List.any (boolFalseConstant.is (extractInferResources checkInfo)) elements.known then
                                        Just
                                            (Rule.errorWithFix
                                                { message = qualifiedToString checkInfo.fn ++ " with `not` on a " ++ collection.represents ++ " with False will result in True"
                                                , details =
                                                    [ "You can replace this call by True." ]
                                                }
                                                checkInfo.fnRange
                                                [ Fix.replaceRangeBy checkInfo.parentRange
                                                    (qualifiedToString (qualify Fn.Basics.trueVariant checkInfo))
                                                ]
                                            )

                                    else
                                        Nothing
                                , \() ->
                                    callOnFromListWithIrrelevantEmptyElement (qualifiedToString checkInfo.fn ++ " with `not`")
                                        ( collection, { empty = { specific = boolTrueConstant, kind = Constant } } )
                                        checkInfo
                                ]
                                ()

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        ]


{-| The check

    fn (\el -> el == constant) something
    --> replacementFn constant something

So for example

    List.any (\el -> el == constant) list
    --> List.member constant list

    List.Extra.removeIfIndex (\i -> i == constant) list
    --> List.Extra.removeAt constant list

    -- ↓ doesn't exist
    Set.Extra.filterNot (\k -> k == constant) set
    --> Set.remove constant

-}
operationWithEqualsConstantIsEquivalentToFnWithThatConstantCheck : ( ModuleName, String ) -> CallCheckInfo -> Maybe (Error {})
operationWithEqualsConstantIsEquivalentToFnWithThatConstantCheck replacementFn checkInfo =
    case isEqualToConstantFunction checkInfo.firstArg of
        Nothing ->
            Nothing

        Just equatedTo ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with a check for equality with a specific value can be replaced by " ++ qualifiedToString replacementFn ++ " with that value"
                    , details = [ "You can replace this call by " ++ qualifiedToString replacementFn ++ " with the specific value to find which meant for this exact purpose." ]
                    }
                    checkInfo.fnRange
                    (Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementFn checkInfo))
                        :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) equatedTo.constant
                    )
                )


isEqualToConstantFunction : Node Expression -> Maybe { constant : Node Expression }
isEqualToConstantFunction rawNode =
    case Node.value (AstHelpers.removeParens rawNode) of
        Expression.Application ((Node _ (Expression.PrefixOperator "==")) :: expr :: []) ->
            Just { constant = expr }

        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.VarPattern var) ] ->
                    case Node.value (AstHelpers.removeParens lambda.expression) of
                        Expression.OperatorApplication "==" _ left right ->
                            let
                                nodeToFind : Expression
                                nodeToFind =
                                    Expression.FunctionOrValue [] var
                            in
                            if (Node.value left == nodeToFind) && not (expressionContainsVariable var (Node.value right)) then
                                Just { constant = right }

                            else if (Node.value right == nodeToFind) && not (expressionContainsVariable var (Node.value left)) then
                                Just { constant = left }

                            else
                                Nothing

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


expressionContainsVariable : String -> Expression -> Bool
expressionContainsVariable variableToFind expressionNode =
    case expressionNode of
        Expression.FunctionOrValue [] variable ->
            variable == variableToFind

        nonVariableExpressionNode ->
            List.any (\(Node _ sub) -> expressionContainsVariable variableToFind sub)
                (AstHelpers.subExpressions nonVariableExpressionNode)


{-| The sequence checks `sequenceOnCollectionWithKnownEmptyElementCheck` and `sequenceOnFromListWithEmptyIgnoresLaterElementsCheck`
-}
sequenceOrFirstEmptyChecks :
    ( TypeProperties (CollectionProperties (ConstructibleFromListProperties collectionOtherProperties))
    , EmptiableProperties empty (WrapperProperties elementOtherProperties)
    )
    -> CallCheckInfo
    -> Maybe (Error {})
sequenceOrFirstEmptyChecks ( collection, elementEmptiable ) =
    firstThatConstructsJust
        [ sequenceOnCollectionWithKnownEmptyElementCheck ( collection, elementEmptiable )
        , sequenceOnFromListWithEmptyIgnoresLaterElementsCheck ( collection, elementEmptiable )
        ]


{-| The sequence check

    sequence (construction fromList [ a, empty, b ])
    --> sequence (construction fromList [ a, empty ])

So for example

    Task.sequence [ aTask, Task.fail x, bTask ]
    --> Task.sequence [ aTask, Task.fail x ]

-}
sequenceOnFromListWithEmptyIgnoresLaterElementsCheck :
    ( TypeProperties (ConstructibleFromListProperties constructibleFromListOtherProperties), EmptiableProperties empty elementOtherProperties )
    -> CallCheckInfo
    -> Maybe (Error {})
sequenceOnFromListWithEmptyIgnoresLaterElementsCheck ( constructibleFromList, elementEmptiable ) checkInfo =
    case fromListGetLiteral constructibleFromList checkInfo.lookupTable checkInfo.firstArg of
        Just listLiteral ->
            case findMapNeighboring (\el -> getEmptyExpressionNode checkInfo elementEmptiable el) listLiteral.elements of
                Just emptyAndNeighbors ->
                    case emptyAndNeighbors.after of
                        Just _ ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on a " ++ constructibleFromList.represents ++ " containing " ++ typeSubsetDescriptionIndefinite elementEmptiable.empty ++ " early will ignore later elements"
                                    , details = [ "You can remove all " ++ constructibleFromList.represents ++ " elements after " ++ typeSubsetDescriptionDefinite "the first" elementEmptiable.empty ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.removeRange
                                        { start = (Node.range emptyAndNeighbors.found).end
                                        , end = endWithoutBoundary listLiteral.literalRange
                                        }
                                    ]
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| The sequence check

    sequence (collection containing some wrapped elements, then empty, then other elements)
    --> empty

So for example

    Task.sequence [ Task.succeed a, Task.fail x, task ]
    --> Task.fail x

-}
sequenceOnCollectionWithKnownEmptyElementCheck :
    ( TypeProperties (CollectionProperties collectionOtherProperties), EmptiableProperties empty (WrapperProperties elementOtherProperties) )
    -> CallCheckInfo
    -> Maybe (Error {})
sequenceOnCollectionWithKnownEmptyElementCheck ( collection, elementEmptiable ) checkInfo =
    case collection.elements.get (extractInferResources checkInfo) checkInfo.firstArg of
        Just elements ->
            case List.filter (\el -> isNothing (elementEmptiable.wrap.getValue checkInfo.lookupTable el)) elements.known of
                firstNonWrappedElement :: _ ->
                    if isInTypeSubset elementEmptiable.empty (extractInferResources checkInfo) firstNonWrappedElement then
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " on a " ++ collection.represents ++ " containing " ++ typeSubsetDescriptionIndefinite elementEmptiable.empty ++ " will result in " ++ typeSubsetDescriptionDefinite "the first" elementEmptiable.empty
                                , details = [ "You can replace this call by " ++ typeSubsetDescriptionDefinite "the first" elementEmptiable.empty ++ " in the " ++ collection.represents ++ "." ]
                                }
                                checkInfo.fnRange
                                (replaceBySubExpressionFix checkInfo.parentRange firstNonWrappedElement)
                            )

                    else
                        Nothing

                [] ->
                    Nothing

        Nothing ->
            Nothing


listOfWrapperSequenceChecks : WrapperProperties (MappableProperties otherProperties) -> IntoFnCheck
listOfWrapperSequenceChecks wrapper =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (callOnEmptyReturnsCheck
                { resultAsString =
                    \res -> qualifiedToString (qualify wrapper.wrap.fn res) ++ " []"
                }
                listCollection
            )
        , onWrappedIsEquivalentToMapWrapOnValueCheck ( listCollection, wrapper )
        , intoFnCheckOnlyCall (sequenceOnCollectionWithAllElementsWrapped ( listCollection, wrapper ))
        ]


{-| The sequence check

    sequence (collection with each element being wrapped)
    --> wrap (collection with each value)

so for example

    Task.sequence [ Task.succeed a, Task.succeed b ]
    --> Task.succeed [ a, b ]

-}
sequenceOnCollectionWithAllElementsWrapped :
    ( TypeProperties (CollectionProperties otherCollectionProperties), WrapperProperties elementOtherProperties )
    -> CallCheckInfo
    -> Maybe (Error {})
sequenceOnCollectionWithAllElementsWrapped ( collection, elementWrapper ) checkInfo =
    case collection.elements.get (extractInferResources checkInfo) checkInfo.firstArg of
        Just elements ->
            if elements.allKnown then
                case traverse (getValueWithNodeRange (elementWrapper.wrap.getValue checkInfo.lookupTable)) elements.known of
                    Just wrappeds ->
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " on a " ++ collection.represents ++ " where each element is " ++ constructWithOneValueDescriptionIndefinite elementWrapper.wrap.description ++ " will result in " ++ qualifiedToString elementWrapper.wrap.fn ++ " on the values inside"
                                , details = [ "You can replace this call by " ++ qualifiedToString elementWrapper.wrap.fn ++ " on a list where each element is replaced by its value inside " ++ constructWithOneValueDescriptionDefinite "the" elementWrapper.wrap.description ++ "." ]
                                }
                                checkInfo.fnRange
                                (Fix.replaceRangeBy
                                    checkInfo.fnRange
                                    (qualifiedToString (qualify elementWrapper.wrap.fn checkInfo))
                                    :: List.concatMap
                                        (\wrapped -> keepOnlyFix { parentRange = wrapped.nodeRange, keep = Node.range wrapped.value })
                                        wrappeds
                                )
                            )

                    Nothing ->
                        Nothing

            else
                Nothing

        Nothing ->
            Nothing


{-| The "sequenceRepeat" operation checks

    sequenceRepeat 0 wrapper --> wrap []

    sequenceRepeat 1 wrapper --> map List.singleton wrapper

    sequenceRepeat n (wrap a) --> wrap (List.repeat n a)

Examples of such functions:

    Random.list : Int -> Generator a -> Generator (List a)
    Parser.repeat : Int -> Parser a -> Parser (List a) -- by dasch

-}
sequenceRepeatChecks : WrapperProperties (MappableProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
sequenceRepeatChecks wrapper =
    firstThatConstructsJust
        [ \checkInfo ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just lengthInt ->
                    firstThatConstructsJust
                        [ \() ->
                            case lengthInt of
                                1 ->
                                    let
                                        replacement : QualifyResources res -> String
                                        replacement res =
                                            qualifiedToString (qualify wrapper.mapFn res)
                                                ++ " "
                                                ++ qualifiedToString (qualify Fn.List.singleton res)
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " 1 will result in " ++ replacement defaultQualifyResources
                                            , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
                                            }
                                            checkInfo.fnRange
                                            [ Fix.replaceRangeBy
                                                (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                                (replacement checkInfo)
                                            ]
                                        )

                                _ ->
                                    Nothing
                        , \() ->
                            callWithNonPositiveIntCheckErrorSituation { fn = checkInfo.fn, int = lengthInt, intDescription = "length" }
                                |> Maybe.map
                                    (\situation ->
                                        alwaysResultsInConstantError situation
                                            { replacement =
                                                \res -> qualifiedToString (qualify Fn.Random.constant res) ++ " []"
                                            , replacementNeedsParens = True
                                            }
                                            checkInfo
                                    )
                        ]
                        ()

                Nothing ->
                    Nothing
        , \checkInfo ->
            case secondArg checkInfo of
                Just elementArg ->
                    case AstHelpers.getSpecificFnCall wrapper.wrap.fn checkInfo.lookupTable elementArg of
                        Just wrapCall ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " with " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with " ++ qualifiedToString Fn.List.repeat ++ " with the value in " ++ constructWithOneValueDescriptionDefinite "that" wrapper.wrap.description
                                    , details = [ "You can replace the call by " ++ qualifiedToString wrapper.wrap.fn ++ " with " ++ qualifiedToString Fn.List.repeat ++ " with the same length and the value inside " ++ constructWithOneValueDescriptionDefinite "the given" wrapper.wrap.description ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (replaceBySubExpressionFix wrapCall.nodeRange wrapCall.firstArg
                                        ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                                (qualifiedToString (qualify Fn.List.repeat checkInfo))
                                           , Fix.insertAt checkInfo.parentRange.start
                                                (qualifiedToString (qualify wrapper.wrap.fn checkInfo)
                                                    ++ " ("
                                                )
                                           , Fix.insertAt checkInfo.parentRange.end ")"
                                           ]
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        ]


{-| Checks for a repeat function that flattens the resulting list into the same type

    flatRepeat -1 emptiable --> empty

    flatRepeat n empty --> empty

    flatRepeat 1 emptiable --> emptiable

Examples of such functions:

    String.Graphemes.repeat : Int -> String -> String
    Animation.repeat : Int -> Animation.Step msg -> Animation.Step msg
    applyTimes : Int -> (a -> a) -> a -> a

-}
emptiableFlatRepeatChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
emptiableFlatRepeatChecks emptiable =
    firstThatConstructsJust
        [ \checkInfo ->
            case secondArg checkInfo of
                Just emptiableArg ->
                    if emptiable.empty.specific.is (extractInferResources checkInfo) emptiableArg then
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " with " ++ emptiable.empty.specific.description ++ " will result in " ++ emptiable.empty.specific.description
                                , details = [ "You can replace this call by " ++ emptiable.empty.specific.description ++ "." ]
                                }
                                checkInfo.fnRange
                                (keepOnlyFix
                                    { parentRange = checkInfo.parentRange
                                    , keep = Node.range emptiableArg
                                    }
                                )
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
        , \checkInfo ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just intValue ->
                    firstThatConstructsJust
                        [ \() ->
                            case intValue of
                                1 ->
                                    Just
                                        (alwaysReturnsLastArgError (qualifiedToString checkInfo.fn ++ " 1")
                                            { represents = emptiable.represents ++ " to repeat" }
                                            checkInfo
                                        )

                                _ ->
                                    Nothing
                        , \() ->
                            callWithNonPositiveIntCanBeReplacedByCheck
                                { int = intValue
                                , intDescription = "length"
                                , replacement = emptiable.empty.specific.asString
                                }
                                checkInfo
                        ]
                        ()

                _ ->
                    Nothing
        ]


{-| The Folding/reducing checks

    fold f initial empty
    --> initial

    fold (\_ soFar -> soFar) initial emptiable
    --> initial

which applies to for example

    Graph.fold : (NodeContext n e -> b -> b) -> b -> Graph n e -> b

but also functions like

    Either.foldl foldOnLeft initial (Either.Right r) --> initial

    Effects.apply f initial Effects.none

Any other argument order is not supported:

    Maybe.Extra.unwrap initial f Nothing
    -- not simplified

    Result.Extra.unwrap initial f (Err x)
    -- not simplified

    RemoteData.unwrap initial f (Err x)
    -- not simplified

If your fold function takes two arguments, use `emptiableFoldWithExtraArgChecks`

-}
emptiableFoldChecks :
    TypeProperties (EmptiableProperties empty otherProperties)
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableFoldChecks emptiable =
    firstThatConstructsJust
        [ foldToUnchangedAccumulatorCheck emptiable
        , foldOnEmptyChecks emptiable
        ]


foldOnEmptyChecks : EmptiableProperties empty otherProperties -> CallCheckInfo -> Maybe (Error {})
foldOnEmptyChecks emptiable checkInfo =
    case checkInfo.argsAfterFirst of
        initialArg :: emptiableArg :: [] ->
            if isInTypeSubset emptiable.empty checkInfo emptiableArg then
                Just
                    (returnsArgError
                        (qualifiedToString checkInfo.fn ++ " on " ++ typeSubsetDescriptionIndefinite emptiable.empty)
                        { argRepresents = "initial accumulator"
                        , arg = initialArg
                        }
                        checkInfo
                    )

            else
                Nothing

        _ ->
            Nothing


foldToUnchangedAccumulatorCheck : TypeProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
foldToUnchangedAccumulatorCheck typeProperties checkInfo =
    case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
        Just reduceAlwaysResult ->
            if AstHelpers.isIdentity checkInfo.lookupTable reduceAlwaysResult then
                let
                    replacement : { description : String, fix : List Fix }
                    replacement =
                        case checkInfo.argsAfterFirst of
                            -- fold (\_ -> identity)
                            [] ->
                                { description = "`always` because the incoming accumulator will be returned, no matter which " ++ typeProperties.represents ++ " is supplied next"
                                , fix =
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo))
                                    ]
                                }

                            -- fold (\_ -> identity) initial
                            _ :: [] ->
                                { description = "`always` with the given initial accumulator"
                                , fix =
                                    [ Fix.replaceRangeBy
                                        (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo))
                                    ]
                                }

                            -- fully applied
                            initialArg :: _ :: _ ->
                                { description = "the given initial accumulator"
                                , fix =
                                    keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range initialArg }
                                }
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with a function that always returns the unchanged accumulator will result in the initial accumulator"
                        , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                        }
                        checkInfo.fnRange
                        replacement.fix
                    )

            else
                Nothing

        Nothing ->
            Nothing


{-| Folding/reducing checks with a reduce function that not only takes the current element but more information as an extra argument

    fold f initial empty --> initial

    fold (\_ _ soFar -> soFar) emptiable --> initial

which applies to for example

    Dict.foldl : (k -> v -> b -> b) -> b -> Dict.Dict k v -> b
    Graph.Tree.levelOrder : (l -> Forest l -> b -> b) -> b -> Tree l -> b

If your fold function does not have an extra arg, use `emptiableFoldChecks`.

-}
emptiableFoldWithExtraArgChecks :
    TypeProperties (EmptiableProperties empty otherProperties)
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableFoldWithExtraArgChecks emptiable =
    firstThatConstructsJust
        [ foldToUnchangedAccumulatorWithExtraArgCheck emptiable
        , foldOnEmptyChecks emptiable
        ]


foldToUnchangedAccumulatorWithExtraArgCheck : TypeProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
foldToUnchangedAccumulatorWithExtraArgCheck typeProperties checkInfo =
    let
        maybeReduceFunctionResult : Maybe (Node Expression)
        maybeReduceFunctionResult =
            checkInfo.firstArg
                |> AstHelpers.getAlwaysResult checkInfo.lookupTable
                |> Maybe.andThen (AstHelpers.getAlwaysResult checkInfo.lookupTable)
    in
    case maybeReduceFunctionResult of
        Just reduceAlwaysResult ->
            if AstHelpers.isIdentity checkInfo.lookupTable reduceAlwaysResult then
                let
                    replacement : { description : String, fix : List Fix }
                    replacement =
                        case checkInfo.argsAfterFirst of
                            -- fold (\_ -> identity)
                            [] ->
                                { description = "`always` because the incoming accumulator will be returned, no matter which " ++ typeProperties.represents ++ " is supplied next"
                                , fix =
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo))
                                    ]
                                }

                            -- fold (\_ -> identity) initial
                            _ :: [] ->
                                { description = "`always` with the given initial accumulator"
                                , fix =
                                    [ Fix.replaceRangeBy
                                        (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                        (qualifiedToString (qualify Fn.Basics.always checkInfo))
                                    ]
                                }

                            -- fully applied
                            initialArg :: _ :: _ ->
                                { description = "the given initial accumulator"
                                , fix =
                                    keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range initialArg }
                                }
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with a function that always returns the unchanged accumulator will result in the initial accumulator"
                        , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                        }
                        checkInfo.fnRange
                        replacement.fix
                    )

            else
                Nothing

        Nothing ->
            Nothing


emptiableRepeatChecks : CollectionProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
emptiableRepeatChecks collection checkInfo =
    case Evaluate.getInt checkInfo checkInfo.firstArg of
        Just intValue ->
            callWithNonPositiveIntCanBeReplacedByCheck
                { int = intValue
                , intDescription = collection.elements.countDescription
                , replacement = collection.empty.specific.asString
                }
                checkInfo

        Nothing ->
            Nothing


wrapperRepeatChecks : CollectionProperties (WrapperProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
wrapperRepeatChecks wrapper checkInfo =
    case Evaluate.getInt checkInfo checkInfo.firstArg of
        Just 1 ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with " ++ wrapper.elements.countDescription ++ " 1 will result in " ++ qualifiedToString wrapper.wrap.fn
                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ "." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy
                        (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                    ]
                )

        Just _ ->
            Nothing

        Nothing ->
            Nothing


getChecks : TypeProperties (CollectionProperties (EmptiableProperties empty otherProperties)) -> CallCheckInfo -> Maybe (Error {})
getChecks collection =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.specific.asString } collection
        , \checkInfo ->
            Evaluate.getInt checkInfo checkInfo.firstArg
                |> Maybe.andThen (indexAccessChecks collection checkInfo)
        ]


indexAccessChecks : TypeProperties (CollectionProperties otherProperties) -> CallCheckInfo -> Int -> Maybe (Error {})
indexAccessChecks collection checkInfo n =
    if n < 0 then
        Just
            (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with negative index")
                { replacement = maybeWithJustAsWrap.empty.specific.asString }
                checkInfo
            )

    else
        case secondArg checkInfo of
            Just arg ->
                firstThatConstructsJust
                    [ \() ->
                        case collection.elements.get (extractInferResources checkInfo) arg of
                            Just literalElements ->
                                case List.drop n literalElements.known |> List.head of
                                    Just element ->
                                        Just
                                            (Rule.errorWithFix
                                                { message = "The element returned by " ++ qualifiedToString checkInfo.fn ++ " is known"
                                                , details = [ "You can replace this call by Just the targeted element." ]
                                                }
                                                checkInfo.fnRange
                                                (replaceBySubExpressionFix (Node.range arg) element
                                                    ++ [ Fix.replaceRangeBy (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                                            (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo))
                                                       ]
                                                )
                                            )

                                    Nothing ->
                                        if literalElements.allKnown then
                                            Just
                                                (Rule.errorWithFix
                                                    { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return " ++ qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)
                                                    , details = [ "You can replace this call by Nothing." ]
                                                    }
                                                    checkInfo.fnRange
                                                    [ Fix.replaceRangeBy checkInfo.parentRange (qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)) ]
                                                )

                                        else
                                            Nothing

                            Nothing ->
                                Nothing
                    , \() ->
                        AstHelpers.getSpecificFnCall Fn.Array.repeat checkInfo.lookupTable arg
                            |> Maybe.andThen
                                (\repeatCall ->
                                    List.head repeatCall.argsAfterFirst
                                        |> Maybe.andThen
                                            (\repeatSecondArg ->
                                                case Evaluate.getInt checkInfo repeatCall.firstArg of
                                                    Just repeatArgInt ->
                                                        if n < repeatArgInt then
                                                            Just
                                                                (Rule.errorWithFix
                                                                    { message = "The element returned by " ++ qualifiedToString checkInfo.fn ++ " is known"
                                                                    , details = [ "You can replace this call by Just the repeated element." ]
                                                                    }
                                                                    checkInfo.fnRange
                                                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                                                        (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo) ++ " " ++ checkInfo.extractSourceCode (Node.range repeatSecondArg))
                                                                    ]
                                                                )

                                                        else
                                                            Just
                                                                (Rule.errorWithFix
                                                                    { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return " ++ qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)
                                                                    , details = [ "You can replace this call by Nothing." ]
                                                                    }
                                                                    checkInfo.fnRange
                                                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                                                        (qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo))
                                                                    ]
                                                                )

                                                    Nothing ->
                                                        Nothing
                                            )
                                )
                    , \() ->
                        AstHelpers.getSpecificFnCall Fn.Array.initialize checkInfo.lookupTable arg
                            |> Maybe.andThen
                                (\initializeCall ->
                                    List.head initializeCall.argsAfterFirst
                                        |> Maybe.andThen
                                            (\repeatSecondArg ->
                                                case Evaluate.getInt checkInfo initializeCall.firstArg of
                                                    Just initializeArgInt ->
                                                        if n < initializeArgInt then
                                                            Just
                                                                (Rule.errorWithFix
                                                                    { message = "The element returned by " ++ qualifiedToString checkInfo.fn ++ " is known"
                                                                    , details = [ "You can replace this call by Just the function directly applied to the index." ]
                                                                    }
                                                                    checkInfo.fnRange
                                                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                                                        (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo) ++ " (" ++ checkInfo.extractSourceCode (Node.range repeatSecondArg) ++ " " ++ String.fromInt n ++ ")")
                                                                    ]
                                                                )

                                                        else
                                                            Just
                                                                (Rule.errorWithFix
                                                                    { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return " ++ qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo)
                                                                    , details = [ "You can replace this call by Nothing." ]
                                                                    }
                                                                    checkInfo.fnRange
                                                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                                                        (qualifiedToString (qualify Fn.Maybe.nothingVariant checkInfo))
                                                                    ]
                                                                )

                                                    Nothing ->
                                                        Nothing
                                            )
                                )
                    ]
                    ()

            Nothing ->
                Nothing


setChecks : TypeProperties (CollectionProperties (EmptiableProperties empty otherProperties)) -> IntoFnCheck
setChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case Evaluate.getInt checkInfo checkInfo.firstArg of
                    Just n ->
                        if n < 0 then
                            Just
                                (alwaysReturnsLastArgError
                                    (qualifiedToString checkInfo.fn ++ " with negative index")
                                    collection
                                    checkInfo
                                )

                        else
                            case secondArg checkInfo of
                                Just replacementArg ->
                                    setOnKnownElementChecks collection checkInfo n (Node.range replacementArg)

                                Nothing ->
                                    Nothing

                    Nothing ->
                        Nothing
            )
        ]


setOnKnownElementChecks :
    TypeProperties (CollectionProperties otherProperties)
    -> CallCheckInfo
    -> Int
    -> Range
    -> Maybe (Error {})
setOnKnownElementChecks collection checkInfo n replacementArgRange =
    case thirdArg checkInfo of
        Just collectionArg ->
            case collection.elements.get (extractInferResources checkInfo) collectionArg of
                Just literalElements ->
                    case List.drop n literalElements.known |> List.head of
                        Just element ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " will replace a known element"
                                    , details = [ "You can directly replace the element at the given index in the " ++ collection.represents ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range collectionArg }
                                        ++ [ Fix.replaceRangeBy (Node.range element) (checkInfo.extractSourceCode replacementArgRange) ]
                                    )
                                )

                        Nothing ->
                            if literalElements.allKnown then
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return the same given " ++ collection.represents
                                        , details = [ "You can replace this call by the given " ++ collection.represents ++ "." ]
                                        }
                                        checkInfo.fnRange
                                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range collectionArg })
                                    )

                            else
                                Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


emptiableReverseChecks : EmptiableProperties empty otherProperties -> IntoFnCheck
emptiableReverseChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck emptiable
        , toggleFnChecks
        ]


{-| The drop check

    drop n (collection with size <= n) --> empty

So for example

    List.drop 3 [ a, b ] --> []

-}
dropOnSmallerCollectionCheck : { dropCount : Int } -> TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
dropOnSmallerCollectionCheck config collection checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just listArg ->
            case listDetermineLength checkInfo listArg of
                Just (Exactly length) ->
                    if config.dropCount >= length then
                        Just
                            (alwaysResultsInUnparenthesizedConstantError
                                (qualifiedToString checkInfo.fn ++ " with a count greater than or equal to the given " ++ collection.represents ++ "'s length")
                                { replacement = collection.empty.specific.asString }
                                checkInfo
                            )

                    else
                        Nothing

                _ ->
                    Nothing

        Nothing ->
            Nothing


{-| The drop check

    drop n (fromList on list with size > n)
    --> (fromList on list with the first n elements removed)

So for example

    Array.drop 2 (Array.fromList [ a, b, c ])
    --> Array.fromList [ c ]

-}
dropOnLargerConstructionFromListLiteralWillRemoveTheseElementsCheck : { dropCount : Int } -> TypeProperties (ConstructibleFromListProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
dropOnLargerConstructionFromListLiteralWillRemoveTheseElementsCheck config constructibleFromList checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just lastArg ->
            case fromListGetLiteral constructibleFromList checkInfo.lookupTable lastArg of
                Just fromListLiteral ->
                    case List.drop config.dropCount fromListLiteral.elements of
                        (Node elementAfterDroppedRange _) :: _ ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " with a count less than the given " ++ constructibleFromList.represents ++ "'s length will remove these elements"
                                    , details = [ "You can remove the first " ++ String.fromInt config.dropCount ++ " elements from the " ++ constructionFromListOnLiteralDescription constructibleFromList.fromList ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range lastArg }
                                        ++ [ Fix.removeRange
                                                { start = startWithoutBoundary fromListLiteral.literalRange
                                                , end = elementAfterDroppedRange.start
                                                }
                                           ]
                                    )
                                )

                        [] ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


emptiableMapNChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
emptiableMapNChecks emptiable checkInfo =
    if List.any (isInTypeSubset emptiable.empty checkInfo) checkInfo.argsAfterFirst then
        Just
            (alwaysResultsInUnparenthesizedConstantError
                (qualifiedToString checkInfo.fn ++ " with any " ++ emptiable.represents ++ " being " ++ emptiable.empty.specific.description)
                { replacement = emptiable.empty.specific.asString }
                checkInfo
            )

    else
        Nothing


{-| When all arguments of a fully applied `mapN` are wrapped,
apply the given function to the values inside and wrap the whole thing again:

    map2 f (wrap first) (wrap second)
    --> wrap (f first second)

For example given `resultWithOkAsWrap`:

    Result.map2 f (Ok first) (Ok second)
    --> Ok (f first second)

This is pretty similar to `listOfWrapperSequenceChecks` where we look at arguments instead of list elements.

-}
wrapperMapNChecks : TypeProperties (WrapperProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
wrapperMapNChecks wrapper checkInfo =
    if List.length checkInfo.argsAfterFirst == (checkInfo.argCount - 1) then
        -- fully applied
        case traverse (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) checkInfo.argsAfterFirst of
            Just wraps ->
                let
                    wrapFnDescription : String
                    wrapFnDescription =
                        qualifiedToString (qualify wrapper.wrap.fn defaultQualifyResources)
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " where each " ++ wrapper.represents ++ " is " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in " ++ wrapFnDescription ++ " on the values inside"
                        , details = [ "You can replace this call by " ++ wrapFnDescription ++ " with the function applied to the values inside each " ++ constructWithOneValueDescriptionWithoutArticle wrapper.wrap.description ++ "." ]
                        }
                        checkInfo.fnRange
                        (keepOnlyFix
                            { parentRange = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                            , keep = Node.range checkInfo.firstArg
                            }
                            ++ List.concatMap (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value) wraps
                            ++ (case checkInfo.callStyle of
                                    Pipe LeftToRight ->
                                        [ Fix.insertAt checkInfo.parentRange.end
                                            (" |> " ++ qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                        ]

                                    Pipe RightToLeft ->
                                        [ Fix.insertAt checkInfo.parentRange.start
                                            (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " <| ")
                                        ]

                                    Application ->
                                        [ Fix.insertAt checkInfo.parentRange.end ")"
                                        , Fix.insertAt checkInfo.parentRange.start (qualifiedToString (qualify wrapper.wrap.fn checkInfo) ++ " (")
                                        ]
                               )
                        )
                    )

            Nothing ->
                Nothing

    else
        Nothing


{-| If we find an empty argument given to the `mapN`, we either

  - replace the whole call by the first empty argument if all earlier arguments are wrapped

        map3 f (wrap first) empty thirdWrapper
        --> empty

        map2 f empty secondWrapper
        --> empty

    For example given `resultWithOkAsWrap`:

        Result.map3 f (Ok x) (Err y) thirdResult
        --> Err y

  - ignore arguments after the known empty argument because they will never have an effect on the result

        map3 f emptyOrWrappedWeDoNotKnow empty thirdWrapper
        --> map2 f emptyOrWrappedWeDoNotKnow empty

    For example given `resultWithOkAsWrap`:

        Result.map3 f errorOrOkWeDoNotKnow (Err x) thirdResult
        --> Result.map2 f errorOrOkWeDoNotKnow (Err x)

This is pretty similar to `listSequenceOrFirstEmptyChecks` where we look at arguments instead of list elements.

-}
mapNOrFirstEmptyConstructionChecks :
    WrapperProperties (EmptiableProperties empty otherProperties)
    -> CallCheckInfo
    -> Maybe (Error {})
mapNOrFirstEmptyConstructionChecks emptiable checkInfo =
    case findMapAndAllBefore (getEmptyExpressionNode checkInfo emptiable) checkInfo.argsAfterFirst of
        -- no empty arg found
        Nothing ->
            Nothing

        Just emptyAndBefore ->
            case traverse (\el -> emptiable.wrap.getValue checkInfo.lookupTable el) emptyAndBefore.before of
                -- all args before are known to not be empty
                Just _ ->
                    let
                        replacement : { description : String, fix : List Fix }
                        replacement =
                            case checkInfo.argCount - (1 + List.length checkInfo.argsAfterFirst) of
                                -- fully applied
                                0 ->
                                    { description = typeSubsetDescriptionDefinite "the first" emptiable.empty
                                    , fix = replaceBySubExpressionFix checkInfo.parentRange emptyAndBefore.found
                                    }

                                -- one arg curried
                                1 ->
                                    { description =
                                        "always with " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                    , fix =
                                        replaceBySubExpressionFix checkInfo.parentRange emptyAndBefore.found
                                            ++ [ Fix.insertAt checkInfo.parentRange.start (qualifiedToString (qualify Fn.Basics.always checkInfo) ++ " ") ]
                                    }

                                -- multiple args curried
                                atLeast2 ->
                                    let
                                        lambdaStart : String
                                        lambdaStart =
                                            "\\" ++ String.repeat atLeast2 "_ " ++ "-> "
                                    in
                                    { description =
                                        lambdaStart ++ "with " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                    , fix =
                                        replaceBySubExpressionFix checkInfo.parentRange emptyAndBefore.found
                                            ++ [ Fix.insertAt checkInfo.parentRange.start ("(" ++ lambdaStart)
                                               , Fix.insertAt checkInfo.parentRange.end ")"
                                               ]
                                    }
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " where we know " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty ++ " will result in " ++ typeSubsetDescriptionDefinite "that" emptiable.empty
                            , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                            }
                            checkInfo.fnRange
                            replacement.fix
                        )

                -- some args before could be empty
                Nothing ->
                    let
                        keptArgCount : Int
                        keptArgCount =
                            List.length emptyAndBefore.before + 1
                    in
                    if keptArgCount == (checkInfo.argCount - 1) then
                        -- last arg is empty
                        Nothing

                    else
                        -- there are args (curried or present) after the known empty arg
                        let
                            replacementMap : ( ModuleName, String )
                            replacementMap =
                                ( AstHelpers.qualifiedModuleName checkInfo.fn, "map" ++ String.fromInt keptArgCount )

                            keptRange : Range
                            keptRange =
                                Range.combine
                                    (checkInfo.fnRange
                                        :: Node.range emptyAndBefore.found
                                        :: List.map Node.range emptyAndBefore.before
                                    )

                            replacement : { description : String, fix : List Fix }
                            replacement =
                                case checkInfo.argCount - (1 + List.length checkInfo.argsAfterFirst) of
                                    -- fully applied
                                    0 ->
                                        { fix =
                                            [ Fix.removeRange
                                                { start = keptRange.end, end = checkInfo.parentRange.end }
                                            , Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementMap checkInfo))
                                            , Fix.removeRange
                                                { start = checkInfo.parentRange.start, end = keptRange.start }
                                            ]
                                        , description =
                                            qualifiedToString replacementMap ++ " with the same arguments until " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                        }

                                    -- one arg curried
                                    1 ->
                                        { fix =
                                            [ Fix.replaceRangeBy
                                                { start = keptRange.end, end = checkInfo.parentRange.end }
                                                ")"
                                            , Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementMap checkInfo))
                                            , Fix.replaceRangeBy
                                                { start = checkInfo.parentRange.start
                                                , end = keptRange.start
                                                }
                                                (qualifiedToString (qualify Fn.Basics.always checkInfo) ++ " (")
                                            ]
                                        , description =
                                            "always with " ++ qualifiedToString replacementMap ++ " with the same arguments until " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                        }

                                    -- multiple args curried
                                    atLeast2 ->
                                        let
                                            lambdaStart : String
                                            lambdaStart =
                                                "\\" ++ String.repeat atLeast2 "_ " ++ "-> "
                                        in
                                        { fix =
                                            [ Fix.replaceRangeBy
                                                { start = keptRange.end, end = checkInfo.parentRange.end }
                                                ")"
                                            , Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementMap checkInfo))
                                            , Fix.replaceRangeBy
                                                { start = checkInfo.parentRange.start, end = keptRange.start }
                                                ("(" ++ lambdaStart)
                                            ]
                                        , description =
                                            lambdaStart ++ "with " ++ qualifiedToString replacementMap ++ " with the same arguments until " ++ typeSubsetDescriptionDefinite "the first" emptiable.empty
                                        }
                        in
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " with " ++ typeSubsetDescriptionIndefinite emptiable.empty ++ " early will ignore later arguments"
                                , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                                }
                                checkInfo.fnRange
                                replacement.fix
                            )


emptiableWrapperFilterMapChecks : TypeProperties (WrapperProperties (EmptiableProperties ConstantProperties (MappableProperties otherProperties))) -> IntoFnCheck
emptiableWrapperFilterMapChecks emptiableWrapper =
    intoFnChecksFirstThatConstructsError
        [ intoFnCheckOnlyCall
            (\checkInfo ->
                case constructs (sameInAllBranches (AstHelpers.getSpecificFnCall Fn.Maybe.justVariant checkInfo.lookupTable)) checkInfo.lookupTable checkInfo.firstArg of
                    Just justCalls ->
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " with a function that will always return Just is the same as " ++ qualifiedToString emptiableWrapper.mapFn
                                , details = [ "You can remove the `Just`s and replace the call by " ++ qualifiedToString emptiableWrapper.mapFn ++ "." ]
                                }
                                checkInfo.fnRange
                                (Fix.replaceRangeBy checkInfo.fnRange
                                    (qualifiedToString (qualify emptiableWrapper.mapFn checkInfo))
                                    :: List.concatMap (\call -> replaceBySubExpressionFix call.nodeRange call.firstArg) justCalls
                                )
                            )

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getSpecificValueOrFn Fn.Maybe.justVariant checkInfo.lookupTable checkInfo.firstArg of
                    Just _ ->
                        Just
                            (alwaysReturnsLastArgError
                                (qualifiedToString checkInfo.fn ++ " with a function that will always return Just")
                                emptiableWrapper
                                checkInfo
                            )

                    Nothing ->
                        Nothing
            )
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case constructs (sameInAllBranches (AstHelpers.getSpecificValueOrFn Fn.Maybe.nothingVariant checkInfo.lookupTable)) checkInfo.lookupTable checkInfo.firstArg of
                    Just _ ->
                        Just
                            (alwaysResultsInUnparenthesizedConstantError
                                (qualifiedToString checkInfo.fn ++ " with a function that will always return Nothing")
                                { replacement = emptiableWrapper.empty.specific.asString }
                                checkInfo
                            )

                    Nothing ->
                        Nothing
            )
        , mapToOperationWithIdentityCanBeCombinedToOperationChecks emptiableWrapper
        , unnecessaryOnEmptyCheck emptiableWrapper
        ]


identityFunctionProperties :
    { description : String
    , is : Infer.Resources {} -> Node Expression -> Bool
    , asString : QualifyResources {} -> String
    , fn : ( ModuleName, String )
    }
identityFunctionProperties =
    { description = "an identity function"
    , is = \res expr -> AstHelpers.isIdentity res.lookupTable expr
    , asString = \res -> qualifiedToString (qualify Fn.Basics.identity res)
    , fn = Fn.Basics.identity
    }


tupleFirstAccessFunctionProperties :
    { description : String
    , is : Infer.Resources {} -> Node Expression -> Bool
    , asString : QualifyResources {} -> String
    , fn : ( ModuleName, String )
    }
tupleFirstAccessFunctionProperties =
    { description = "Tuple.first"
    , fn = Fn.Tuple.first
    , is = \res expr -> AstHelpers.isTupleFirstAccess res.lookupTable expr
    , asString = \res -> qualifiedToString (qualify Fn.Tuple.first res)
    }


tupleSecondAccessFunctionProperties :
    { description : String
    , is : Infer.Resources {} -> Node Expression -> Bool
    , asString : QualifyResources {} -> String
    , fn : ( ModuleName, String )
    }
tupleSecondAccessFunctionProperties =
    { description = "Tuple.second"
    , fn = Fn.Tuple.second
    , is = \res expr -> AstHelpers.isTupleSecondAccess res.lookupTable expr
    , asString = \res -> qualifiedToString (qualify Fn.Tuple.second res)
    }


mapToOperationWithIdentityCanBeCombinedToOperationChecks : MappableProperties otherProperties -> IntoFnCheck
mapToOperationWithIdentityCanBeCombinedToOperationChecks mappable =
    let
        checkIntoFn : ( ModuleName, String ) -> IntoFnCheck
        checkIntoFn combinedFn =
            onSpecificFnCallCanBeCombinedCheck
                { args = [ identityFunctionProperties ]
                , earlierFn = mappable.mapFn
                , combinedFn = combinedFn
                }
    in
    { call = \checkInfo -> (checkIntoFn checkInfo.fn).call checkInfo
    , composition = \checkInfo -> (checkIntoFn checkInfo.later.fn).composition checkInfo
    }


{-| Simplify this operation after a given call to `earlierFn` into a given `combinedFn`.

Examples:

  - `List.concat (List.map f list) --> List.concatMap f list` (same for sequence+map to traverse etc)
  - `List.concat << List.map f --> List.concatMap f`
  - `Parser/Decoder/Random/...sequence (List.repeat n x) --> Parser/Decoder/Random/...repeat n x`
  - `String.concat (List.repeat n x) --> String.repeat n x`
  - `Animation.loop (List.repeat n x) --> Animation.repeat n x` using [`mdgriffith/elm-style-animation`](https://package.elm-lang.org/packages/mdgriffith/elm-style-animation/4.0.0/)
  - `FormattedText.concat (List.intersperse s list) --> FormattedText.join s list` using [`NoRedInk/elm-formatted-text-19`](https://package.elm-lang.org/packages/NoRedInk/elm-formatted-text-19/1.0.0/)
  - `List.filterMap identity (List.map f list) --> List.filterMap f list`
  - `List.filterMap identity << List.map f --> List.filterMap f`
  - `traverse identity (map f a) --> traverse f a`
  - those listed in `onSpecificFnCallCanBeCombinedCheck`

-}
onSpecificFnCallCanBeCombinedCheck :
    { args : List { argProperties | description : String, is : Infer.Resources {} -> Node Expression -> Bool }
    , earlierFn : ( ModuleName, String )
    , combinedFn : ( ModuleName, String )
    }
    -> IntoFnCheck
onSpecificFnCallCanBeCombinedCheck config =
    let
        laterOperationDescription : ( ModuleName, String ) -> String
        laterOperationDescription laterFn =
            case config.args of
                [] ->
                    qualifiedToString (qualify laterFn defaultQualifyResources)

                firstArg :: argsAfterFirst ->
                    qualifiedToString (qualify laterFn defaultQualifyResources)
                        ++ " with "
                        ++ firstArg.description
                        ++ String.concat (List.map (\arg -> " and " ++ arg.description) argsAfterFirst)

        laterInitArgsAreValidForOperation : List (Node Expression) -> Infer.Resources res -> Bool
        laterInitArgsAreValidForOperation laterInitArgs resources =
            List.all identity
                (List.map2 (\arg argConfig -> argConfig.is (extractInferResources resources) arg)
                    laterInitArgs
                    config.args
                )
    in
    { call =
        \checkInfo ->
            case Maybe.andThen (AstHelpers.getSpecificFnCall config.earlierFn checkInfo.lookupTable) (fullyAppliedLastArg checkInfo) of
                Just fromFnCall ->
                    if laterInitArgsAreValidForOperation (listFilledInit ( checkInfo.firstArg, checkInfo.argsAfterFirst )) checkInfo then
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString config.earlierFn ++ ", then " ++ laterOperationDescription checkInfo.fn ++ " can be combined into " ++ qualifiedToString config.combinedFn
                                , details = [ "You can replace this call by " ++ qualifiedToString config.combinedFn ++ " with the same arguments given to " ++ qualifiedToString config.earlierFn ++ " which is meant for this exact purpose." ]
                                }
                                checkInfo.fnRange
                                (Fix.replaceRangeBy
                                    fromFnCall.fnRange
                                    (qualifiedToString (qualify config.combinedFn checkInfo))
                                    :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = fromFnCall.nodeRange }
                                )
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if
                onlyLastArgIsCurried checkInfo.later
                    && (checkInfo.earlier.fn == config.earlierFn)
                    && laterInitArgsAreValidForOperation checkInfo.later.args checkInfo
            then
                Just
                    { info =
                        { message = qualifiedToString config.earlierFn ++ ", then " ++ laterOperationDescription checkInfo.later.fn ++ " can be combined into " ++ qualifiedToString config.combinedFn
                        , details = [ "You can replace this composition by " ++ qualifiedToString config.combinedFn ++ " with the same arguments given to " ++ qualifiedToString config.earlierFn ++ " which is meant for this exact purpose." ]
                        }
                    , fix =
                        [ Fix.replaceRangeBy
                            checkInfo.earlier.fnRange
                            (qualifiedToString (qualify config.combinedFn checkInfo))
                        , Fix.removeRange checkInfo.later.removeRange
                        ]
                    }

            else
                Nothing
    }


{-| The check

    operation (wrap a) --> map wrap a

    operation << wrap --> map wrap

So for example

    Task.sequence [ task ]
    --: Task x (List a)
    --> Task.map List.singleton task

    Task.sequence << List.singleton
    --: Task x a -> Task x (List a)
    --> Task.map List.singleton

Note that some functions called "sequence" have equal element and result types, like

    Bytes.Encode.sequence  [ encoder ]
    --: Bytes.Encode.Encoder

    Bytes.Encode.sequence << List.singleton
    --: Bytes.Encode.Encoder -> Bytes.Encode.Encoder

which means you can simplify them to `encoder`/`identity` using `unnecessaryOnWrappedCheck`.

-}
onWrappedIsEquivalentToMapWrapOnValueCheck :
    ( WrapperProperties wrapperOtherProperties, WrapperProperties (MappableProperties elementOtherProperties) )
    -> IntoFnCheck
onWrappedIsEquivalentToMapWrapOnValueCheck ( wrapper, valueMappable ) =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Just lastArg ->
                    case wrapper.wrap.getValue checkInfo.lookupTable lastArg of
                        Just wrappedValue ->
                            let
                                replacement : QualifyResources a -> String
                                replacement qualifyResources =
                                    qualifiedToString (qualify valueMappable.mapFn qualifyResources)
                                        ++ " "
                                        ++ qualifiedToString (qualify wrapper.wrap.fn qualifyResources)
                            in
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as " ++ replacement defaultQualifyResources ++ " on the value inside"
                                    , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ " on the value inside the singleton list." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (replacement checkInfo)
                                        :: replaceBySubExpressionFix (Node.range lastArg) wrappedValue
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == wrapper.wrap.fn) then
                let
                    replacement : QualifyResources a -> String
                    replacement qualifyResources =
                        qualifiedToString (qualify valueMappable.mapFn qualifyResources)
                            ++ " "
                            ++ qualifiedToString (qualify wrapper.wrap.fn qualifyResources)
                in
                Just
                    { info =
                        { message = qualifiedToString checkInfo.later.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " is the same as " ++ replacement defaultQualifyResources ++ " on the value inside"
                        , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
                        }
                    , fix = compositionReplaceByFix (replacement checkInfo) checkInfo
                    }

            else
                Nothing
    }


pipelineChecks :
    { commentRanges : List Range
    , extractSourceCode : Range -> String
    , nodeRange : Range
    , pipedInto : Node Expression
    , arg : Node Expression
    , direction : LeftOrRightDirection
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , lookupTable : ModuleNameLookupTable
    }
    -> Maybe (Error {})
pipelineChecks =
    firstThatConstructsJust
        [ \checkInfo -> pipingIntoCompositionChecks { commentRanges = checkInfo.commentRanges, extractSourceCode = checkInfo.extractSourceCode } checkInfo.direction checkInfo.pipedInto
        , \checkInfo -> fullyAppliedLambdaInPipelineChecks { nodeRange = checkInfo.nodeRange, function = checkInfo.pipedInto, firstArgument = checkInfo.arg }
        , \checkInfo ->
            case AstHelpers.getRecordAccessFunction checkInfo.pipedInto of
                Just fieldName ->
                    accessingRecordChecks
                        { parentRange = checkInfo.nodeRange
                        , record = checkInfo.arg
                        , fieldRange = Node.range checkInfo.pipedInto
                        , fieldName = fieldName
                        , importRecordTypeAliases = checkInfo.importRecordTypeAliases
                        , moduleRecordTypeAliases = checkInfo.moduleRecordTypeAliases
                        , lookupTable = checkInfo.lookupTable
                        }
                        |> Maybe.map (\e -> Rule.errorWithFix e.info (Node.range checkInfo.pipedInto) e.fix)

                Nothing ->
                    Nothing
        ]


fullyAppliedLambdaInPipelineChecks : { nodeRange : Range, firstArgument : Node Expression, function : Node Expression } -> Maybe (Error {})
fullyAppliedLambdaInPipelineChecks checkInfo =
    case Node.value checkInfo.function of
        Expression.ParenthesizedExpression (Node lambdaRange (Expression.LambdaExpression lambda)) ->
            case Node.value (AstHelpers.removeParens checkInfo.firstArgument) of
                Expression.OperatorApplication "|>" _ _ _ ->
                    Nothing

                Expression.OperatorApplication "<|" _ _ _ ->
                    Nothing

                _ ->
                    appliedLambdaError
                        { nodeRange = checkInfo.nodeRange
                        , lambdaRange = lambdaRange
                        , lambda = lambda
                        }

        _ ->
            Nothing


pipingIntoCompositionChecks :
    { commentRanges : List Range, extractSourceCode : Range -> String }
    -> LeftOrRightDirection
    -> Node Expression
    -> Maybe (Error {})
pipingIntoCompositionChecks context compositionDirection expressionNode =
    let
        ( opToFind, replacement ) =
            case compositionDirection of
                RightToLeft ->
                    ( "<<", "<|" )

                LeftToRight ->
                    ( ">>", "|>" )

        pipingIntoCompositionChecksHelp : Node Expression -> Maybe { opToReplaceRange : Range, fixes : List Fix, firstStepIsComposition : Bool }
        pipingIntoCompositionChecksHelp subExpression =
            case Node.value subExpression of
                Expression.ParenthesizedExpression inParens ->
                    case pipingIntoCompositionChecksHelp inParens of
                        Nothing ->
                            Nothing

                        Just error ->
                            if error.firstStepIsComposition then
                                -- parens can safely be removed
                                Just
                                    { error
                                        | fixes =
                                            removeBoundariesFix subExpression ++ error.fixes
                                    }

                            else
                                -- inside parenthesis is checked separately because
                                -- the parens here can't safely be removed
                                Nothing

                Expression.OperatorApplication symbol _ left right ->
                    let
                        continuedSearch : Maybe { opToReplaceRange : Range, fixes : List Fix, firstStepIsComposition : Bool }
                        continuedSearch =
                            case compositionDirection of
                                RightToLeft ->
                                    pipingIntoCompositionChecksHelp left

                                LeftToRight ->
                                    pipingIntoCompositionChecksHelp right
                    in
                    if symbol == replacement then
                        Maybe.map (\errors -> { errors | firstStepIsComposition = False })
                            continuedSearch

                    else if symbol == opToFind then
                        let
                            opToFindRange : Range
                            opToFindRange =
                                findOperatorRange
                                    { operator = opToFind
                                    , commentRanges = context.commentRanges
                                    , extractSourceCode = context.extractSourceCode
                                    , leftRange = Node.range left
                                    , rightRange = Node.range right
                                    }
                        in
                        Just
                            { opToReplaceRange = opToFindRange
                            , fixes =
                                Fix.replaceRangeBy opToFindRange replacement
                                    :: (case continuedSearch of
                                            Nothing ->
                                                []

                                            Just additionalErrorsFound ->
                                                additionalErrorsFound.fixes
                                       )
                            , firstStepIsComposition = True
                            }

                    else
                        Nothing

                _ ->
                    Nothing
    in
    case pipingIntoCompositionChecksHelp expressionNode of
        Nothing ->
            Nothing

        Just error ->
            Just
                (Rule.errorWithFix
                    { message = "Use " ++ replacement ++ " instead of " ++ opToFind
                    , details =
                        [ "Because of the precedence of operators, using " ++ opToFind ++ " at this location is the same as using " ++ replacement ++ "."
                        , "To make it more idiomatic in Elm and generally easier to read, please use " ++ replacement ++ " instead. You may need to remove some parentheses to do this."
                        , "Here is an example:"
                            ++ (case compositionDirection of
                                    RightToLeft ->
                                        leftPipeExample

                                    LeftToRight ->
                                        rightPipeExample
                               )
                        ]
                    }
                    error.opToReplaceRange
                    error.fixes
                )


rightPipeExample : String
rightPipeExample =
    """
Before: data |> fn1 |> (fn2 >> fn3)
After:  data |> fn1 |>  fn2 |> fn3"""


leftPipeExample : String
leftPipeExample =
    """
Before: (fn3 << fn2) <| fn1 <| data
After:   fn3 <| fn2  <| fn1 <| data"""



-- GENERIC CHECKS


{-| Condense applying the same function with equal arguments (except the last one) twice in sequence into one.
This applies to functions that are equivalent to identity when operating on the result another such function.

Examples of such functions:

  - one argument: `Simplify.expectNaN`, `Review.Rule.providesFixesForModuleRule`, `List.sort`, `List.Extra.unique`, [`AVL.Set.clear`](https://package.elm-lang.org/packages/owanturist/elm-avl-dict/2.1.0/AVL-Set#clear)
  - two arguments: `List.filter f`, `List.Extra.filterNot f`, `List.Extra.takeWhile/dropWhile(Right) f`, `List.sortBy f`, `List.Extra.uniqueBy f`
  - three arguments: `Array.set i new`, `Array.Extra.resizelRepeat l pad`, `List.Extra.setAt i new`

Note that `update` or `setWhere` operations for example _can_ have an effect even after the same operation has already been applied.

For operations that toggle between 2 states, like `reverse` or `List.Extra.swapAt i j`, use `toggleCallChecks`

-}
operationDoesNotChangeResultOfOperationCheck : IntoFnCheck
operationDoesNotChangeResultOfOperationCheck =
    { call =
        \checkInfo ->
            case Maybe.andThen (AstHelpers.getSpecificFnCall checkInfo.fn checkInfo.lookupTable) (fullyAppliedLastArg checkInfo) of
                Just lastArgCall ->
                    let
                        areAllArgsEqual : Bool
                        areAllArgsEqual =
                            List.all
                                (\( arg, lastArgCallArg ) ->
                                    Normalize.compare checkInfo arg lastArgCallArg == Normalize.ConfirmedEquality
                                )
                                (List.map2 Tuple.pair
                                    (listFilledInit ( checkInfo.firstArg, checkInfo.argsAfterFirst ))
                                    (listFilledInit ( lastArgCall.firstArg, lastArgCall.argsAfterFirst ))
                                )
                    in
                    if areAllArgsEqual then
                        Just
                            (Rule.errorWithFix
                                { message =
                                    case checkInfo.argCount of
                                        1 ->
                                            "Unnecessary " ++ qualifiedToString checkInfo.fn ++ " after " ++ qualifiedToString checkInfo.fn

                                        _ ->
                                            "Unnecessary " ++ qualifiedToString checkInfo.fn ++ " after equivalent " ++ qualifiedToString checkInfo.fn
                                , details = [ "You can remove this additional operation." ]
                                }
                                checkInfo.fnRange
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = lastArgCall.nodeRange })
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            let
                areAllArgsEqual : () -> Bool
                areAllArgsEqual () =
                    List.all
                        (\( arg, earlierArg ) ->
                            Normalize.compare checkInfo arg earlierArg == Normalize.ConfirmedEquality
                        )
                        (List.map2 Tuple.pair checkInfo.later.args checkInfo.earlier.args)
            in
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == checkInfo.later.fn) && areAllArgsEqual () then
                Just
                    { info =
                        { message =
                            case checkInfo.later.argCount of
                                1 ->
                                    "Unnecessary " ++ qualifiedToString checkInfo.later.fn ++ " after " ++ qualifiedToString checkInfo.earlier.fn

                                _ ->
                                    "Unnecessary " ++ qualifiedToString checkInfo.later.fn ++ " after equivalent " ++ qualifiedToString checkInfo.earlier.fn
                        , details = [ "You can remove this additional operation." ]
                        }
                    , fix = [ Fix.removeRange checkInfo.later.removeRange ]
                    }

            else
                Nothing
    }


{-| When a function's first 2 arguments are equal it will return the last argument.

    f a a
    --> a

Examples:

    Dict.union dict dict
    --> dict

    Set.intersect set set
    --> set

-}
withTwoEqualArgumentsReturnsLastCheck : IntoFnCheck
withTwoEqualArgumentsReturnsLastCheck =
    intoFnCheckOnlyCall
        (\checkInfo ->
            case secondArg checkInfo of
                Just secondArg_ ->
                    case Normalize.compare checkInfo checkInfo.firstArg secondArg_ of
                        Normalize.ConfirmedEquality ->
                            Just
                                (returnsArgError
                                    (qualifiedToString checkInfo.fn ++ " with equal first and second arguments")
                                    { arg = secondArg_, argRepresents = "last argument" }
                                    checkInfo
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        )


toggleFnChecks : IntoFnCheck
toggleFnChecks =
    { call = \checkInfo -> (onSpecificFnCallReturnsItsLastArgCheck checkInfo.fn).call checkInfo
    , composition = \checkInfo -> (onSpecificFnCallReturnsItsLastArgCheck checkInfo.later.fn).composition checkInfo
    }


{-| Chaining two operations that are inverses of each other and therefore cancel each other out.
For example

    Array.fromList (Array.toList array)
    --> array

    Array.fromList << Array.toList
    --> identity

    Array.toList (Array.fromList list)
    --> list

    Array.toList << Array.fromList
    --> identity

These usually exist in pairs, like above so make sure to add this check for both functions.
But there are exceptions!

    Set.fromList (Set.toList set) --> set

    Set.fromList << Set.toList --> identity

This will always work because `Set.toList` will never produce a list with duplicate elements. However!

    Set.toList (Set.fromList list) --> list

    Set.toList << Set.fromList --> identity

would be an incorrect fix. See for example

    Set.toList (Set.fromList [ 0, 0 ])
    --> not [ 0, 0 ] but actually [ 0 ]

-}
onSpecificFnCallReturnsItsLastArgCheck : ( ModuleName, String ) -> IntoFnCheck
onSpecificFnCallReturnsItsLastArgCheck inverseFn =
    { call =
        \checkInfo ->
            case AstHelpers.getSpecificFnCall inverseFn checkInfo.lookupTable checkInfo.firstArg of
                Just call ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString inverseFn ++ ", then " ++ qualifiedToString checkInfo.fn ++ " cancels each other out"
                            , details = [ "You can replace this call by the argument given to " ++ qualifiedToString inverseFn ++ "." ]
                            }
                            checkInfo.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range call.firstArg })
                        )

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if checkInfo.earlier.fn == inverseFn then
                Just
                    (compositionAlwaysReturnsIncomingError
                        (qualifiedToString checkInfo.earlier.fn ++ ", then " ++ qualifiedToString checkInfo.later.fn ++ " cancels each other out")
                        checkInfo
                    )

            else
                Nothing
    }


{-| The wrapper check

    fn .. (wrap a) --> wrap a
    fn .. << wrap --> wrap

So for example

    List.reverse [ a ] --> [ a ]

    List.reverse << List.singleton --> List.singleton

-}
unnecessaryOnWrappedCheck : WrapperProperties otherProperties -> IntoFnCheck
unnecessaryOnWrappedCheck wrapper =
    { call = unnecessaryCallOnCheck { specific = wrapper.wrap, kind = ConstructWithOneValue }
    , composition = unnecessaryCompositionAfterCheck wrapper.wrap
    }


unnecessaryCallOnCheck : TypeSubsetProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
unnecessaryCallOnCheck constructable checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just constructableArg ->
            let
                getConstructable : Node Expression -> Maybe ()
                getConstructable expressionNode =
                    if isInTypeSubset constructable checkInfo expressionNode then
                        Just ()

                    else
                        Nothing
            in
            case sameInAllBranches getConstructable constructableArg of
                Just _ ->
                    Just
                        (Rule.errorWithFix
                            (operationDoesNotChangeSpecificLastArgErrorInfo { fn = checkInfo.fn, specific = constructable })
                            checkInfo.fnRange
                            (keepOnlyFix
                                { parentRange = checkInfo.parentRange
                                , keep = Node.range constructableArg
                                }
                            )
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| The operation is equivalent to identity when applied on an empty value:

    fn .. empty --> empty

    -- only when a constructor fn exists
    fn .. << emptyConstructor --> emptyConstructor

Examples

    List.sort []
    --> []

    Json.Decode.map f (Json.Decode.fail x)
    --> Json.Decode.fail x

    Json.Decode.map f << Json.Decode.fail
    --> Json.Decode.fail

    Task.mapError f (Task.succeed a)
    --> Task.succeed a

    Task.mapError f << Task.succeed
    --> Task.succeed

-}
unnecessaryOnEmptyCheck : EmptiableProperties empty otherProperties -> IntoFnCheck
unnecessaryOnEmptyCheck emptiable =
    { call = unnecessaryCallOnCheck emptiable.empty
    , composition =
        case emptiable.empty.kind emptiable.empty.specific of
            Constant _ ->
                \_ -> Nothing

            ConstructWithOneValue constructWithOneValue ->
                \checkInfo -> unnecessaryCompositionAfterCheck constructWithOneValue checkInfo
    }


callOnEmptyReturnsCheck :
    { resultAsString : QualifyResources {} -> String }
    -> EmptiableProperties empty otherProperties
    -> CallCheckInfo
    -> Maybe (Error {})
callOnEmptyReturnsCheck config collection checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just lastArg ->
            if isInTypeSubset collection.empty (extractInferResources checkInfo) lastArg then
                let
                    resultDescription : String
                    resultDescription =
                        config.resultAsString defaultQualifyResources
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on " ++ typeSubsetDescriptionIndefinite collection.empty ++ " will result in " ++ resultDescription
                        , details = [ "You can replace this call by " ++ resultDescription ++ "." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange
                            (config.resultAsString (extractQualifyResources checkInfo))
                        ]
                    )

            else
                Nothing

        Nothing ->
            Nothing


unnecessaryCompositionAfterCheck :
    ConstructWithOneValueProperties
    -> CompositionIntoCheckInfo
    -> Maybe ErrorInfoAndFix
unnecessaryCompositionAfterCheck construct checkInfo =
    if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == construct.fn) then
        Just
            { info =
                { message = qualifiedToString checkInfo.later.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite construct.description ++ " will result in " ++ constructWithOneValueDescriptionDefinite "the unchanged" construct.description
                , details = [ "You can replace this composition by " ++ qualifiedToString (qualify construct.fn checkInfo) ++ "." ]
                }
            , fix =
                [ Fix.removeRange checkInfo.later.removeRange ]
            }

    else
        Nothing


callWithNonPositiveIntCanBeReplacedByCheck :
    { int : number
    , intDescription : String
    , replacement : QualifyResources {} -> String
    }
    -> CallCheckInfo
    -> Maybe (Error {})
callWithNonPositiveIntCanBeReplacedByCheck config checkInfo =
    callWithNonPositiveIntCheckErrorSituation { fn = checkInfo.fn, int = config.int, intDescription = config.intDescription }
        |> Maybe.map
            (\situation ->
                alwaysResultsInUnparenthesizedConstantError situation
                    { replacement = config.replacement }
                    checkInfo
            )


callWithNonPositiveIntCheckErrorSituation :
    { fn : ( ModuleName, String )
    , int : number
    , intDescription : String
    }
    -> Maybe String
callWithNonPositiveIntCheckErrorSituation config =
    if config.int <= 0 then
        let
            lengthDescription : String
            lengthDescription =
                if config.int < 0 then
                    "negative " ++ config.intDescription

                else
                    config.intDescription ++ " 0"
        in
        Just
            (qualifiedToString config.fn ++ " with " ++ lengthDescription)

    else
        Nothing


{-| The last argument of a fully applied function (the given `argCount` specifies what is considered "fully applied").

For example, `fullyAppliedLastArg` on `Array.set 3 "Hitagi"` would return `Nothing`
while `fullyAppliedLastArg` on `Array.set 3 "Hitagi" arr` would return `Just arr`.

-}
fullyAppliedLastArg : { callInfo | firstArg : Node Expression, argsAfterFirst : List (Node Expression), argCount : Int } -> Maybe (Node Expression)
fullyAppliedLastArg callInfo =
    List.drop (callInfo.argCount - 1) (callInfo.firstArg :: callInfo.argsAfterFirst) |> List.head


onlyLastArgIsCurried : { function | args : List (Node Expression), argCount : Int } -> Bool
onlyLastArgIsCurried functionInfo =
    List.length functionInfo.args == (functionInfo.argCount - 1)


{-| This operation is equivalent to identity when called on a wrapped value.

    operation (wrap a) --> a

    operation << wrap --> identity

For example

    List.sum [ a ] --> a

    Cmd.batch [ a ] --> a

    List.sum << List.singleton --> identity

    Cmd.batch << List.singleton --> identity

-}
onWrappedReturnsItsValueCheck : WrapperProperties otherProperties -> IntoFnCheck
onWrappedReturnsItsValueCheck wrapper =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Just wrapperArg ->
                    case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) wrapperArg of
                        Nothing ->
                            Nothing

                        Just wraps ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in the value inside"
                                    , details = [ "You can replace this call by the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range wrapperArg }
                                        ++ List.concatMap (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value) wraps
                                    )
                                )

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == wrapper.wrap.fn) then
                Just
                    (compositionAlwaysReturnsIncomingError
                        (qualifiedToString (qualify checkInfo.later.fn defaultQualifyResources) ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will always result in the value inside")
                        checkInfo
                    )

            else
                Nothing
    }


{-| This operation is equivalent to Just when called on a wrapped value.

    operation (wrap a) --> Just a

    operation << wrap --> Just

For example

    List.minimum [ a ] --> Just a

    List.minimum << List.singleton --> Just

    Result.toMaybe (Ok a) --> Just a

    Result.toMaybe << Ok --> Just

-}
onWrappedReturnsJustItsValueCheck : WrapperProperties otherProperties -> IntoFnCheck
onWrappedReturnsJustItsValueCheck wrapper =
    { call =
        \checkInfo ->
            case fullyAppliedLastArg checkInfo of
                Just withWrapArg ->
                    case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) withWrapArg of
                        Just wraps ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will result in Just the value inside"
                                    , details = [ "You can replace this call by Just the value inside " ++ constructWithOneValueDescriptionDefinite "the" wrapper.wrap.description ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.removeRange { start = (Node.range withWrapArg).end, end = checkInfo.parentRange.end }
                                        :: List.concatMap (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value) wraps
                                        ++ [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = (Node.range withWrapArg).start }
                                                (qualifiedToString (qualify Fn.Maybe.justVariant checkInfo) ++ " ")
                                           ]
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    , composition =
        \checkInfo ->
            if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == wrapper.wrap.fn) then
                Just
                    { info =
                        { message = qualifiedToString checkInfo.later.fn ++ " on " ++ constructWithOneValueDescriptionIndefinite wrapper.wrap.description ++ " will always result in Just the value inside"
                        , details = [ "You can replace this call by Just." ]
                        }
                    , fix = compositionReplaceByFnFix Fn.Maybe.justVariant checkInfo
                    }

            else
                Nothing
    }


{-| The filter checks

    keepWhen f empty --> empty

    keepWhen (\_ -> True) emptiable --> emptiable

    keepWhen (\_ -> False) emptiable --> empty

If your function only takes two arguments like `Dict.filter`, use `emptiableKeepWhenWithExtraArgChecks`

-}
emptiableKeepWhenChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> IntoFnCheck
emptiableKeepWhenChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck emptiable
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                    Just constantFunctionResult ->
                        keepWhenWithConstantFunctionResultChecks constantFunctionResult emptiable checkInfo

                    Nothing ->
                        Nothing
            )
        ]


keepWhenWithConstantFunctionResultChecks : Node Expression -> TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
keepWhenWithConstantFunctionResultChecks constantFunctionResult emptiable checkInfo =
    case Evaluate.getBoolean checkInfo constantFunctionResult of
        Determined True ->
            Just
                (alwaysReturnsLastArgError
                    (qualifiedToString checkInfo.fn ++ " with a function that will always return True")
                    emptiable
                    checkInfo
                )

        Determined False ->
            Just
                (alwaysResultsInUnparenthesizedConstantError
                    (qualifiedToString checkInfo.fn ++ " with a function that will always return False")
                    { replacement = emptiable.empty.specific.asString }
                    checkInfo
                )

        Undetermined ->
            Nothing


{-| Filter checks where the function takes 2 arguments

    keepWhenWithExtraArg f empty --> empty

    keepWhenWithExtraArg (\_ _ -> True) emptiable --> emptiable

    keepWhenWithExtraArg (\_ _ -> False) emptiable --> empty

If your function only takes one argument like `List.filter`, use `emptiableKeepWhenChecks`

-}
emptiableKeepWhenWithExtraArgChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> IntoFnCheck
emptiableKeepWhenWithExtraArgChecks emptiable =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck emptiable
        , intoFnCheckOnlyCall
            (\checkInfo ->
                let
                    maybeFilterFunctionResult : Maybe (Node Expression)
                    maybeFilterFunctionResult =
                        checkInfo.firstArg
                            |> AstHelpers.getAlwaysResult checkInfo.lookupTable
                            |> Maybe.andThen (AstHelpers.getAlwaysResult checkInfo.lookupTable)
                in
                case maybeFilterFunctionResult of
                    Just constantFunctionResult ->
                        keepWhenWithConstantFunctionResultChecks constantFunctionResult emptiable checkInfo

                    Nothing ->
                        Nothing
            )
        ]


collectionRemoveChecks : CollectionProperties (EmptiableProperties empty otherProperties) -> IntoFnCheck
collectionRemoveChecks collection =
    unnecessaryOnEmptyCheck collection


collectionSliceChecks : EmptiableProperties ConstantProperties otherProperties -> IntoFnCheck
collectionSliceChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                case secondArg checkInfo of
                    Just endArg ->
                        firstThatConstructsJust
                            [ \() ->
                                if Normalize.areAllTheSame checkInfo checkInfo.firstArg [ endArg ] then
                                    Just
                                        (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with equal start and end index")
                                            { replacement = collection.empty.specific.asString }
                                            checkInfo
                                        )

                                else
                                    Nothing
                            , \() ->
                                case Evaluate.getInt checkInfo endArg of
                                    Just endInt ->
                                        firstThatConstructsJust
                                            [ \() ->
                                                case endInt of
                                                    0 ->
                                                        Just
                                                            (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with end index 0")
                                                                { replacement = collection.empty.specific.asString }
                                                                checkInfo
                                                            )

                                                    _ ->
                                                        Nothing
                                            , \() ->
                                                case Evaluate.getInt checkInfo checkInfo.firstArg of
                                                    Just startInt ->
                                                        if startInt > endInt then
                                                            if startInt >= 0 && endInt >= 0 then
                                                                Just
                                                                    (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with a start index greater than the end index")
                                                                        { replacement = collection.empty.specific.asString }
                                                                        checkInfo
                                                                    )

                                                            else if startInt <= -1 && endInt <= -1 then
                                                                Just
                                                                    (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with a negative start index closer to the right than the negative end index")
                                                                        { replacement = collection.empty.specific.asString }
                                                                        checkInfo
                                                                    )

                                                            else
                                                                Nothing

                                                        else
                                                            Nothing

                                                    Nothing ->
                                                        Nothing
                                            ]
                                            ()

                                    Nothing ->
                                        Nothing
                            ]
                            ()

                    Nothing ->
                        Nothing
            )
        ]


collectionIntersectChecks : CollectionProperties (EmptiableProperties ConstantProperties otherProperties) -> IntoFnCheck
collectionIntersectChecks collection =
    intoFnChecksFirstThatConstructsError
        [ unnecessaryOnEmptyCheck collection
        , intoFnCheckOnlyCall
            (\checkInfo ->
                if collection.empty.specific.is (extractInferResources checkInfo) checkInfo.firstArg then
                    Just
                        (alwaysResultsInUnparenthesizedConstantError
                            (qualifiedToString checkInfo.fn ++ " on " ++ collection.empty.specific.description)
                            { replacement = collection.empty.specific.asString }
                            checkInfo
                        )

                else
                    Nothing
            )
        , withTwoEqualArgumentsReturnsLastCheck
        ]


collectionDiffChecks : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionDiffChecks collection =
    firstThatConstructsJust
        [ \checkInfo ->
            if collection.empty.specific.is (extractInferResources checkInfo) checkInfo.firstArg then
                Just
                    (alwaysResultsInUnparenthesizedConstantError
                        (qualifiedToString checkInfo.fn ++ " " ++ emptyAsString checkInfo collection)
                        { replacement = collection.empty.specific.asString }
                        checkInfo
                    )

            else
                Nothing
        , \checkInfo ->
            case secondArg checkInfo of
                Just collectionArg ->
                    if isInTypeSubset collection.empty (extractInferResources checkInfo) collectionArg then
                        Just
                            (Rule.errorWithFix
                                { message = "Unnecessary " ++ qualifiedToString checkInfo.fn ++ " with " ++ emptyAsString checkInfo collection
                                , details = [ "You can replace this call by the given first " ++ collection.represents ++ "." ]
                                }
                                checkInfo.fnRange
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
        ]


collectionUnionChecks : { leftElementsStayOnTheLeft : Bool } -> TypeProperties (CollectionProperties (ConstructibleFromListProperties (EmptiableProperties empty otherProperties))) -> CallCheckInfo -> Maybe (Error {})
collectionUnionChecks config collection =
    firstThatConstructsJust
        [ \checkInfo ->
            if isInTypeSubset collection.empty checkInfo checkInfo.firstArg then
                Just
                    (alwaysReturnsLastArgError
                        (qualifiedToString checkInfo.fn ++ " " ++ typeSubsetDescriptionWithoutArticle collection.empty)
                        collection
                        checkInfo
                    )

            else
                Nothing
        , \checkInfo ->
            case secondArg checkInfo of
                Just secondArg_ ->
                    if isInTypeSubset collection.empty checkInfo secondArg_ then
                        Just
                            (Rule.errorWithFix
                                { message = "Unnecessary " ++ qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " with " ++ typeSubsetDescriptionIndefinite collection.empty
                                , details = [ "You can replace this call by the given first " ++ collection.represents ++ "." ]
                                }
                                checkInfo.fnRange
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                            )

                    else
                        collectionUnionWithLiteralsChecks config
                            { first = checkInfo.firstArg
                            , second = secondArg_
                            , operationRange = checkInfo.fnRange
                            , operation = qualifiedToString (qualify checkInfo.fn defaultQualifyResources)
                            }
                            collection
                            checkInfo

                Nothing ->
                    Nothing
        ]


constructionFromListOnLiteralDescription : ConstructionFromList -> String
constructionFromListOnLiteralDescription fromListConstruction =
    case fromListConstruction of
        ConstructionAsList ->
            "list literal"

        ConstructionFromListCall fn ->
            qualifiedToString fn ++ " call"


collectionUnionWithLiteralsChecks :
    { leftElementsStayOnTheLeft : Bool }
    ->
        { first : Node Expression
        , second : Node Expression
        , operationRange : Range
        , operation : String
        }
    -> CollectionProperties (ConstructibleFromListProperties otherProperties)
    ->
        { checkInfo
            | lookupTable : ModuleNameLookupTable
            , extractSourceCode : Range -> String
            , parentRange : Range
        }
    -> Maybe (Error {})
collectionUnionWithLiteralsChecks config operationInfo collection checkInfo =
    case fromListGetLiteral collection checkInfo.lookupTable operationInfo.second of
        Just literalListSecond ->
            case fromListGetLiteral collection checkInfo.lookupTable operationInfo.first of
                Just literalListFirst ->
                    let
                        fromListLiteralDescription : String
                        fromListLiteralDescription =
                            constructionFromListOnLiteralDescription collection.fromList
                    in
                    Just
                        (Rule.errorWithFix
                            { message = operationInfo.operation ++ " on " ++ fromListLiteralDescription ++ "s can be turned into a single " ++ fromListLiteralDescription
                            , details = [ "Try moving all the elements into a single " ++ fromListLiteralDescription ++ "." ]
                            }
                            operationInfo.operationRange
                            (if config.leftElementsStayOnTheLeft then
                                keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range operationInfo.second }
                                    ++ [ Fix.insertAt
                                            (rangeWithoutBoundaries literalListSecond.literalRange).start
                                            (checkInfo.extractSourceCode (rangeWithoutBoundaries literalListFirst.literalRange) ++ ",")
                                       ]

                             else
                                keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range operationInfo.first }
                                    ++ [ Fix.insertAt
                                            (rangeWithoutBoundaries literalListFirst.literalRange).start
                                            (checkInfo.extractSourceCode (rangeWithoutBoundaries literalListSecond.literalRange) ++ ",")
                                       ]
                            )
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


collectionInsertChecks : CollectionProperties (EmptiableProperties empty (WrapperProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionInsertChecks collection checkInfo =
    case secondArg checkInfo of
        Just collectionArg ->
            if isInTypeSubset collection.empty checkInfo collectionArg then
                Just
                    (Rule.errorWithFix
                        { message = "Use " ++ qualifiedToString collection.wrap.fn ++ " instead of inserting in " ++ typeSubsetDescriptionIndefinite collection.empty
                        , details = [ "You can replace this call by " ++ qualifiedToString collection.wrap.fn ++ "." ]
                        }
                        checkInfo.fnRange
                        (replaceBySubExpressionFix checkInfo.parentRange checkInfo.firstArg
                            ++ [ Fix.insertAt checkInfo.parentRange.start
                                    (qualifiedToString (qualify collection.wrap.fn checkInfo) ++ " ")
                               ]
                        )
                    )

            else
                Nothing

        Nothing ->
            Nothing


collectionMemberChecks : CollectionProperties (EmptiableProperties empty otherProperties) -> CallCheckInfo -> Maybe (Error {})
collectionMemberChecks collection =
    callOnEmptyReturnsCheck
        { resultAsString = \res -> qualifiedToString (qualify Fn.Basics.falseVariant res) }
        collection


collectionIsEmptyChecks : TypeProperties (CollectionProperties (EmptiableProperties empty otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionIsEmptyChecks collection checkInfo =
    case collection.elements.determineCount (extractInferResources checkInfo) checkInfo.firstArg of
        Just (Exactly 0) ->
            Just
                (resultsInConstantError
                    (qualifiedToString checkInfo.fn ++ " on " ++ typeSubsetDescriptionIndefinite collection.empty)
                    (\res -> qualifiedToString (qualify Fn.Basics.trueVariant res))
                    checkInfo
                )

        Just _ ->
            Just
                (resultsInConstantError
                    (qualifiedToString checkInfo.fn ++ " on this " ++ collection.represents)
                    (\res -> qualifiedToString (qualify Fn.Basics.falseVariant res))
                    checkInfo
                )

        Nothing ->
            Nothing


collectionSizeChecks : TypeProperties (CollectionProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
collectionSizeChecks collection checkInfo =
    case collection.elements.determineCount (extractInferResources checkInfo) checkInfo.firstArg of
        Just (Exactly size) ->
            Just
                (Rule.errorWithFix
                    { message = "The " ++ collection.elements.countDescription ++ " of the " ++ collection.represents ++ " is " ++ String.fromInt size
                    , details = [ "The " ++ collection.elements.countDescription ++ " of the " ++ collection.represents ++ " can be determined by looking at the code." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange (String.fromInt size) ]
                )

        _ ->
            Nothing


emptiableFromListChecks : EmptiableProperties ConstantProperties otherProperties -> CallCheckInfo -> Maybe (Error {})
emptiableFromListChecks collection =
    callOnEmptyReturnsCheck { resultAsString = collection.empty.specific.asString } listCollection


wrapperFromListSingletonChecks : WrapperProperties otherProperties -> IntoFnCheck
wrapperFromListSingletonChecks wrapper =
    { call =
        \checkInfo ->
            case listCollection.wrap.getValue checkInfo.lookupTable checkInfo.firstArg of
                Nothing ->
                    Nothing

                Just listSingletonValue ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside"
                            , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside the singleton list." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix (Node.range checkInfo.firstArg) listSingletonValue
                                ++ [ Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify wrapper.wrap.fn checkInfo)) ]
                            )
                        )
    , composition =
        \checkInfo ->
            if checkInfo.earlier.fn == listCollection.wrap.fn then
                Just
                    { info =
                        { message = qualifiedToString checkInfo.later.fn ++ " on a singleton list will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside"
                        , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ "." ]
                        }
                    , fix =
                        compositionReplaceByFnFix wrapper.wrap.fn checkInfo
                    }

            else
                Nothing
    }


emptiableToListChecks :
    EmptiableProperties empty otherProperties
    -> CallCheckInfo
    -> Maybe (Error {})
emptiableToListChecks collection =
    callOnEmptyReturnsCheck { resultAsString = listCollection.empty.specific.asString } collection


{-| The partition checks

    partition f empty --> empty

    partition (\_ -> True) emptiable --> ( emptiable, empty )

    partition (\_ -> False) emptiable --> ( empty, emptiable )

If your function takes two arguments like `Dict.partition`, use `emptiablePartitionWithExtraArgChecks`.

-}
collectionPartitionChecks : TypeProperties (CollectionProperties (EmptiableProperties ConstantProperties otherProperties)) -> CallCheckInfo -> Maybe (Error {})
collectionPartitionChecks collection =
    firstThatConstructsJust
        [ partitionOnEmptyChecks collection
        , \checkInfo ->
            case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just constantFunctionResult ->
                    partitionWithConstantFunctionResult constantFunctionResult collection checkInfo

                Nothing ->
                    Nothing
        ]


partitionOnEmptyChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
partitionOnEmptyChecks emptiable =
    callOnEmptyReturnsCheck
        { resultAsString = \res -> "( " ++ emptiable.empty.specific.asString res ++ ", " ++ emptiable.empty.specific.asString res ++ " )" }
        emptiable


partitionWithConstantFunctionResult : Node Expression -> TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
partitionWithConstantFunctionResult constantFunctionResult collection checkInfo =
    case Evaluate.getBoolean checkInfo constantFunctionResult of
        Determined True ->
            case secondArg checkInfo of
                Just (Node listArgRange _) ->
                    Just
                        (Rule.errorWithFix
                            { message = "All elements will go to the first " ++ collection.represents
                            , details = [ "Since the predicate function always returns True, the second " ++ collection.represents ++ " will always be " ++ collection.empty.specific.description ++ "." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = listArgRange.start } "( "
                            , Fix.insertAt listArgRange.end (", " ++ emptyAsString checkInfo collection ++ " )")
                            ]
                        )

                Nothing ->
                    Nothing

        Determined False ->
            Just
                (Rule.errorWithFix
                    { message = "All elements will go to the second " ++ collection.represents
                    , details = [ "Since the predicate function always returns False, the first " ++ collection.represents ++ " will always be " ++ collection.empty.specific.description ++ "." ]
                    }
                    checkInfo.fnRange
                    (case secondArg checkInfo of
                        Just listArg ->
                            [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range listArg).start } ("( " ++ emptyAsString checkInfo collection ++ ", ")
                            , Fix.insertAt (Node.range listArg).end " )"
                            ]

                        Nothing ->
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                ("("
                                    ++ qualifiedToString (qualify Fn.Tuple.pair checkInfo)
                                    ++ " "
                                    ++ emptyAsString checkInfo collection
                                    ++ ")"
                                )
                            ]
                    )
                )

        Undetermined ->
            Nothing


{-| partition checks where the function takes two arguments

    partition f empty --> empty

    partition (\_ _ -> True) emptiable --> ( emptiable, empty )

    partition (\_ _ -> False) emptiable --> ( empty, emptiable )

If your function only takes one argument like `List.partition`, use `collectionPartitionChecks`

-}
emptiablePartitionWithExtraArgChecks : TypeProperties (EmptiableProperties ConstantProperties otherProperties) -> CallCheckInfo -> Maybe (Error {})
emptiablePartitionWithExtraArgChecks emptiable =
    firstThatConstructsJust
        [ partitionOnEmptyChecks emptiable
        , \checkInfo ->
            let
                maybePartitionFunctionResult : Maybe (Node Expression)
                maybePartitionFunctionResult =
                    checkInfo.firstArg
                        |> AstHelpers.getAlwaysResult checkInfo.lookupTable
                        |> Maybe.andThen (AstHelpers.getAlwaysResult checkInfo.lookupTable)
            in
            case maybePartitionFunctionResult of
                Just constantFunctionResult ->
                    partitionWithConstantFunctionResult constantFunctionResult emptiable checkInfo

                Nothing ->
                    Nothing
        ]


type CollectionSize
    = Exactly Int
    | NotEmpty


replaceSingleElementListBySingleValue : ModuleNameLookupTable -> Node Expression -> Maybe (List Fix)
replaceSingleElementListBySingleValue lookupTable expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.ListExpr (listElement :: []) ->
            Just (replaceBySubExpressionFix (Node.range expressionNode) listElement)

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "singleton")) :: _ :: []) ->
            if ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just [ "List" ] then
                Just [ Fix.removeRange fnRange ]

            else
                Nothing

        Expression.IfBlock _ thenBranch elseBranch ->
            combineSingleElementFixes lookupTable [ thenBranch, elseBranch ] []

        Expression.CaseExpression caseOf ->
            combineSingleElementFixes lookupTable (List.map Tuple.second caseOf.cases) []

        _ ->
            Nothing


combineSingleElementFixes : ModuleNameLookupTable -> List (Node Expression) -> List Fix -> Maybe (List Fix)
combineSingleElementFixes lookupTable nodes soFar =
    case nodes of
        [] ->
            Just soFar

        node :: restOfNodes ->
            case replaceSingleElementListBySingleValue lookupTable node of
                Nothing ->
                    Nothing

                Just fixes ->
                    combineSingleElementFixes lookupTable restOfNodes (fixes ++ soFar)



-- RECORD UPDATE


recordUpdateChecks : Range -> Node String -> List (Node Expression.RecordSetter) -> Maybe (Error {})
recordUpdateChecks recordUpdateRange recordVariable fields =
    case findMapNeighboring (getUnnecessaryRecordUpdateSetter (Node.value recordVariable)) fields of
        Just unnecessarySetterAndNeighbors ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary field assignment"
                    , details = [ "The field is being set to its own value." ]
                    }
                    unnecessarySetterAndNeighbors.found.valueAccessRange
                    (case unnecessarySetterAndNeighbors.before of
                        Just (Node prevRange _) ->
                            [ Fix.removeRange { start = prevRange.end, end = unnecessarySetterAndNeighbors.found.setterRange.end } ]

                        Nothing ->
                            case unnecessarySetterAndNeighbors.after of
                                Nothing ->
                                    -- it's the only setter
                                    keepOnlyFix { parentRange = recordUpdateRange, keep = Node.range recordVariable }

                                Just (Node afterRange _) ->
                                    -- It's the first setter, so we can remove until the second setter
                                    [ Fix.removeRange { start = unnecessarySetterAndNeighbors.found.setterRange.start, end = afterRange.start } ]
                    )
                )

        Nothing ->
            Nothing


getUnnecessaryRecordUpdateSetter : String -> Node ( Node String, Node Expression ) -> Maybe { valueAccessRange : Range, setterRange : Range }
getUnnecessaryRecordUpdateSetter recordVariableName (Node setterRange ( Node _ field, valueNode )) =
    case AstHelpers.getAccessingRecord valueNode of
        Just accessingRecord ->
            case accessingRecord.record of
                Node _ (Expression.FunctionOrValue [] recordVariable) ->
                    if field == accessingRecord.field && recordVariableName == recordVariable then
                        Just { setterRange = setterRange, valueAccessRange = accessingRecord.range }

                    else
                        Nothing

                _ ->
                    Nothing

        Nothing ->
            Nothing



-- IF


type alias IfCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , importLookup : ImportLookup
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , nodeRange : Range
    , condition : Node Expression
    , trueBranch : Node Expression
    , falseBranch : Node Expression
    }


targetIfKeyword : Range -> Range
targetIfKeyword ifExpressionRange =
    let
        ifStart : Location
        ifStart =
            ifExpressionRange.start
    in
    { start = ifStart
    , end = { ifStart | column = ifStart.column + 2 }
    }


ifChecks :
    IfCheckInfo
    -> Maybe { errors : Error {}, rangesToIgnore : RangeDict () }
ifChecks =
    firstThatConstructsJust
        [ \checkInfo ->
            case Evaluate.getBoolean checkInfo checkInfo.condition of
                Determined determinedConditionResultIsTrue ->
                    let
                        branch : { expressionNode : Node Expression, name : String }
                        branch =
                            if determinedConditionResultIsTrue then
                                { expressionNode = checkInfo.trueBranch, name = "then" }

                            else
                                { expressionNode = checkInfo.falseBranch, name = "else" }
                    in
                    Just
                        { errors =
                            Rule.errorWithFix
                                { message = "The condition will always evaluate to " ++ AstHelpers.boolToString determinedConditionResultIsTrue
                                , details = [ "The expression can be replaced by what is inside the '" ++ branch.name ++ "' branch." ]
                                }
                                (targetIfKeyword checkInfo.nodeRange)
                                (replaceBySubExpressionFix checkInfo.nodeRange branch.expressionNode)
                        , rangesToIgnore = RangeDict.singleton (Node.range checkInfo.condition) ()
                        }

                Undetermined ->
                    Nothing
        , \checkInfo ->
            case ( Evaluate.getBoolean checkInfo checkInfo.trueBranch, Evaluate.getBoolean checkInfo checkInfo.falseBranch ) of
                ( Determined True, Determined False ) ->
                    Just
                        { errors =
                            Rule.errorWithFix
                                { message = "The if expression's value is the same as the condition"
                                , details = [ "The expression can be replaced by the condition." ]
                                }
                                (targetIfKeyword checkInfo.nodeRange)
                                (replaceBySubExpressionFix checkInfo.nodeRange checkInfo.condition)
                        , rangesToIgnore = RangeDict.empty
                        }

                ( Determined False, Determined True ) ->
                    Just
                        { errors =
                            Rule.errorWithFix
                                { message = "The if expression's value is the inverse of the condition"
                                , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                                }
                                (targetIfKeyword checkInfo.nodeRange)
                                (replaceBySubExpressionFix checkInfo.nodeRange checkInfo.condition
                                    ++ [ Fix.insertAt checkInfo.nodeRange.start
                                            (qualifiedToString (qualify Fn.Basics.not checkInfo) ++ " ")
                                       ]
                                )
                        , rangesToIgnore = RangeDict.empty
                        }

                _ ->
                    Nothing
        , \checkInfo ->
            case Normalize.compare checkInfo checkInfo.trueBranch checkInfo.falseBranch of
                Normalize.ConfirmedEquality ->
                    Just
                        { errors =
                            Rule.errorWithFix
                                { message = "The values in both branches is the same."
                                , details = [ "The expression can be replaced by the contents of either branch." ]
                                }
                                (targetIfKeyword checkInfo.nodeRange)
                                (replaceBySubExpressionFix checkInfo.nodeRange checkInfo.trueBranch)
                        , rangesToIgnore = RangeDict.empty
                        }

                _ ->
                    Nothing
        ]



-- CASE OF


caseOfChecks : List (CaseOfCheckInfo -> Maybe (Error {}))
caseOfChecks =
    [ sameBodyForCaseOfChecks
    , booleanCaseOfChecks
    , destructuringCaseOfChecks
    , caseOfWithUnreachableCasesChecks
    ]


type alias CaseOfCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    , moduleCustomTypes : Dict String { variantNames : Set String }
    , importCustomTypes : Dict ModuleName (Dict String { variantNames : Set String })
    , extractSourceCode : Range -> String
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , parentRange : Range
    , caseOf : Expression.CaseBlock
    }


sameBodyForCaseOfChecks :
    CaseOfCheckInfo
    -> Maybe (Error {})
sameBodyForCaseOfChecks context =
    case context.caseOf.cases of
        [] ->
            Nothing

        ( firstPattern, firstBody ) :: rest ->
            let
                restPatterns : List (Node Pattern)
                restPatterns =
                    List.map Tuple.first rest
            in
            if
                introducesVariableOrUsesTypeConstructor context (firstPattern :: restPatterns)
                    || not (Normalize.areAllTheSame context firstBody (List.map Tuple.second rest))
            then
                Nothing

            else
                let
                    firstBodyRange : Range
                    firstBodyRange =
                        Node.range firstBody
                in
                Just
                    (Rule.errorWithFix
                        { message = "Unnecessary case expression"
                        , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                        }
                        (caseKeyWordRange context.parentRange)
                        [ Fix.removeRange { start = context.parentRange.start, end = firstBodyRange.start }
                        , Fix.removeRange { start = firstBodyRange.end, end = context.parentRange.end }
                        ]
                    )


caseKeyWordRange : Range -> Range
caseKeyWordRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 4 }
    }


introducesVariableOrUsesTypeConstructor :
    { a | lookupTable : ModuleNameLookupTable, customTypesToReportInCases : Set ( ModuleName, ConstructorName ) }
    -> List (Node Pattern)
    -> Bool
introducesVariableOrUsesTypeConstructor resources nodesToLookAt =
    case nodesToLookAt of
        [] ->
            False

        node :: remaining ->
            case Node.value node of
                Pattern.VarPattern _ ->
                    True

                Pattern.RecordPattern _ ->
                    True

                Pattern.AsPattern _ _ ->
                    True

                Pattern.ParenthesizedPattern pattern ->
                    introducesVariableOrUsesTypeConstructor resources (pattern :: remaining)

                Pattern.TuplePattern nodes ->
                    introducesVariableOrUsesTypeConstructor resources (nodes ++ remaining)

                Pattern.UnConsPattern first rest ->
                    introducesVariableOrUsesTypeConstructor resources (first :: rest :: remaining)

                Pattern.ListPattern nodes ->
                    introducesVariableOrUsesTypeConstructor resources (nodes ++ remaining)

                Pattern.NamedPattern variantQualified nodes ->
                    case ModuleNameLookupTable.fullModuleNameFor resources.lookupTable node of
                        Just moduleName ->
                            if Set.member ( moduleName, variantQualified.name ) resources.customTypesToReportInCases then
                                introducesVariableOrUsesTypeConstructor resources (nodes ++ remaining)

                            else
                                True

                        Nothing ->
                            True

                _ ->
                    introducesVariableOrUsesTypeConstructor resources remaining


booleanCaseOfChecks : CaseOfCheckInfo -> Maybe (Error {})
booleanCaseOfChecks checkInfo =
    case checkInfo.caseOf.cases of
        ( firstPattern, Node firstRange _ ) :: ( Node secondPatternRange _, Node secondExprRange _ ) :: [] ->
            case AstHelpers.getBoolPattern checkInfo.lookupTable firstPattern of
                Just isTrueFirst ->
                    let
                        expressionRange : Range
                        expressionRange =
                            Node.range checkInfo.caseOf.expression
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "Replace `case..of` by an `if` condition"
                            , details =
                                [ "The idiomatic way to check for a condition is to use an `if` expression."
                                , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
                                ]
                            }
                            (Node.range firstPattern)
                            (if isTrueFirst then
                                [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = expressionRange.start } "if "
                                , Fix.replaceRangeBy { start = expressionRange.end, end = firstRange.start } " then "
                                , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                                ]

                             else
                                [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = expressionRange.start } "if not ("
                                , Fix.replaceRangeBy { start = expressionRange.end, end = firstRange.start } ") then "
                                , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                                ]
                            )
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


destructuringCaseOfChecks :
    CaseOfCheckInfo
    -> Maybe (Error {})
destructuringCaseOfChecks checkInfo =
    case checkInfo.caseOf.cases of
        ( rawSinglePattern, Node bodyRange _ ) :: [] ->
            let
                singlePattern : Node Pattern
                singlePattern =
                    AstHelpers.removeParensFromPattern rawSinglePattern
            in
            if isSimpleDestructurePattern singlePattern then
                let
                    exprRange : Range
                    exprRange =
                        Node.range checkInfo.caseOf.expression

                    caseIndentation : String
                    caseIndentation =
                        String.repeat (checkInfo.parentRange.start.column - 1) " "

                    bodyIndentation : String
                    bodyIndentation =
                        String.repeat (bodyRange.start.column - 1) " "
                in
                Just
                    (Rule.errorWithFix
                        { message = "Use a let expression to destructure data"
                        , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                        }
                        (Node.range singlePattern)
                        [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = exprRange.start }
                            ("let " ++ checkInfo.extractSourceCode (Node.range singlePattern) ++ " = ")
                        , Fix.replaceRangeBy { start = exprRange.end, end = bodyRange.start }
                            ("\n" ++ caseIndentation ++ "in\n" ++ bodyIndentation)
                        ]
                    )

            else
                Nothing

        _ ->
            Nothing


isSimpleDestructurePattern : Node Pattern -> Bool
isSimpleDestructurePattern (Node _ pattern) =
    case pattern of
        Pattern.TuplePattern _ ->
            True

        Pattern.RecordPattern _ ->
            True

        Pattern.VarPattern _ ->
            True

        _ ->
            False


caseOfWithUnreachableCasesChecks : CaseOfCheckInfo -> Maybe (Error {})
caseOfWithUnreachableCasesChecks checkInfo =
    caseOfWithUnreachableCasesChecksOn
        { casedExpressionNode = checkInfo.caseOf.expression
        , cases = checkInfo.caseOf.cases
        }
        checkInfo
        |> Maybe.map
            (\error ->
                let
                    maybeOnlyMatchingCase : Maybe { index : Int, expressionNode : Node Expression }
                    maybeOnlyMatchingCase =
                        case error.fix.casedExpressionReplace.replacement of
                            "()" ->
                                error.fix.cases
                                    |> List.indexedMap (\index fix -> { index = index, fix = fix })
                                    |> findMap
                                        (\case_ ->
                                            case case_.fix of
                                                UnreachableCaseRemove _ ->
                                                    Nothing

                                                UnreachableCaseReplace replace ->
                                                    Just { index = case_.index, expressionNode = replace.expressionNode }
                                        )

                            _ ->
                                Nothing
                in
                case maybeOnlyMatchingCase of
                    Nothing ->
                        Rule.errorWithFix { message = error.message, details = error.details }
                            error.range
                            (unreachableCasesFixToReviewFixes error.fix)

                    Just onlyMatchingCase ->
                        Rule.errorWithFix
                            { message = error.message
                            , details =
                                error.details
                                    ++ [ "Since only a single case branch will remain after removing the unreachable ones, you can replace this case-of by the result of the "
                                            ++ indexthToString onlyMatchingCase.index
                                            ++ " case."
                                       ]
                            }
                            error.range
                            (replaceBySubExpressionFix checkInfo.parentRange onlyMatchingCase.expressionNode)
            )


type alias UnreachableCasesFix =
    { casedExpressionReplace : { range : Range, replacement : String }
    , cases : List UnreachableCaseFix
    }


type UnreachableCaseFix
    = UnreachableCaseRemove { patternRange : Range, expressionRange : Range }
    | UnreachableCaseReplace { range : Range, replacement : String, expressionNode : Node Expression }


unreachableCasesFixToReviewFixes : UnreachableCasesFix -> List Fix
unreachableCasesFixToReviewFixes unreachableCasesFix =
    Fix.replaceRangeBy unreachableCasesFix.casedExpressionReplace.range unreachableCasesFix.casedExpressionReplace.replacement
        :: List.concat
            (List.map2
                (\unreachableCaseFix unreachableCaseFixBefore ->
                    case unreachableCaseFix of
                        UnreachableCaseReplace unreachableCaseReplace ->
                            [ Fix.replaceRangeBy unreachableCaseReplace.range unreachableCaseReplace.replacement ]

                        UnreachableCaseRemove caseRanges ->
                            [ Fix.removeRange
                                { start =
                                    { row =
                                        case unreachableCaseFixBefore of
                                            Nothing ->
                                                caseRanges.patternRange.start.row

                                            Just (UnreachableCaseRemove beforeCaseRanges) ->
                                                beforeCaseRanges.expressionRange.end.row + 1

                                            Just (UnreachableCaseReplace beforeUnreachableCaseReplace) ->
                                                (Node.range beforeUnreachableCaseReplace.expressionNode).end.row + 1
                                    , column = 0
                                    }
                                , end =
                                    { row = caseRanges.expressionRange.end.row + 1, column = 0 }
                                }
                            ]
                )
                unreachableCasesFix.cases
                (Nothing :: List.map Just unreachableCasesFix.cases)
            )


caseOfWithUnreachableCasesChecksOn :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseOfWithUnreachableCasesChecksOn config =
    firstThatConstructsJust
        [ caseVariantOfWithUnreachableCasesChecks config
        , caseListLiteralOfWithUnreachableCasesChecks config
        , caseConsOfWithUnreachableCasesChecks config
        , caseTuple2OfWithUnreachableCasesChecks config
        , caseTuple3OfWithUnreachableCasesChecks config
        ]


caseTuple2OfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseTuple2OfWithUnreachableCasesChecks config checkInfo =
    case AstHelpers.getTuple2 checkInfo.lookupTable config.casedExpressionNode of
        Nothing ->
            Nothing

        Just casedTuple ->
            let
                maybeTuplePatternCases : Maybe (List { firstPatternNode : Node Pattern, secondPatternNode : Node Pattern, expressionNode : Node Expression })
                maybeTuplePatternCases =
                    traverse
                        (\( patternNode, expressionNode ) ->
                            case patternNode of
                                Node _ (Pattern.TuplePattern (firstPatternNode :: secondPatternNode :: [])) ->
                                    Just { firstPatternNode = firstPatternNode, secondPatternNode = secondPatternNode, expressionNode = expressionNode }

                                _ ->
                                    Nothing
                        )
                        config.cases
            in
            case maybeTuplePatternCases of
                Nothing ->
                    Nothing

                Just tuplePatternCases ->
                    firstThatConstructsJust
                        [ caseOfWithUnreachableCasesChecksOn
                            { casedExpressionNode = casedTuple.first
                            , cases =
                                List.map (\case_ -> ( case_.firstPatternNode, case_.expressionNode ))
                                    tuplePatternCases
                            }
                        , caseOfWithUnreachableCasesChecksOn
                            { casedExpressionNode = casedTuple.second
                            , cases =
                                List.map (\case_ -> ( case_.secondPatternNode, case_.expressionNode ))
                                    tuplePatternCases
                            }
                        ]
                        checkInfo


caseTuple3OfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseTuple3OfWithUnreachableCasesChecks config checkInfo =
    case config.casedExpressionNode of
        Node _ (Expression.TupledExpression (casedTupleFirstNode :: casedTupleSecondNode :: casedTupleThirdNode :: [])) ->
            let
                maybeTuplePatternCases : Maybe (List { firstPatternNode : Node Pattern, secondPatternNode : Node Pattern, thirdPatternNode : Node Pattern, expressionNode : Node Expression })
                maybeTuplePatternCases =
                    traverse
                        (\( patternNode, expressionNode ) ->
                            case patternNode of
                                Node _ (Pattern.TuplePattern (firstPatternNode :: secondPatternNode :: thirdPatternNode :: [])) ->
                                    Just { firstPatternNode = firstPatternNode, secondPatternNode = secondPatternNode, thirdPatternNode = thirdPatternNode, expressionNode = expressionNode }

                                _ ->
                                    Nothing
                        )
                        config.cases
            in
            case maybeTuplePatternCases of
                Nothing ->
                    Nothing

                Just tuplePatternCases ->
                    firstThatConstructsJust
                        [ caseOfWithUnreachableCasesChecksOn
                            { casedExpressionNode = casedTupleFirstNode
                            , cases =
                                List.map (\case_ -> ( case_.firstPatternNode, case_.expressionNode ))
                                    tuplePatternCases
                            }
                        , caseOfWithUnreachableCasesChecksOn
                            { casedExpressionNode = casedTupleSecondNode
                            , cases =
                                List.map (\case_ -> ( case_.secondPatternNode, case_.expressionNode ))
                                    tuplePatternCases
                            }
                        , caseOfWithUnreachableCasesChecksOn
                            { casedExpressionNode = casedTupleThirdNode
                            , cases =
                                List.map (\case_ -> ( case_.thirdPatternNode, case_.expressionNode ))
                                    tuplePatternCases
                            }
                        ]
                        checkInfo

        _ ->
            Nothing


caseVariantOfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseVariantOfWithUnreachableCasesChecks config checkInfo =
    let
        maybeVariantCaseOf :
            Maybe
                { cased : { moduleName : ModuleName, name : String, attachments : List (Node Expression), customTypeVariantNames : Set String }
                , cases : List { patternRange : Range, name : String, attachments : List (Node Pattern), expressionNode : Node Expression }
                }
        maybeVariantCaseOf =
            case AstHelpers.getValueOrFnOrFnCall config.casedExpressionNode of
                Nothing ->
                    Nothing

                Just valueOrCall ->
                    let
                        maybeValueModule : Maybe { name : ModuleName, customTypes : Dict String { variantNames : Set String } }
                        maybeValueModule =
                            case ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable valueOrCall.fnRange of
                                Just [] ->
                                    Just { name = [], customTypes = checkInfo.moduleCustomTypes }

                                Just (moduleNamePart0 :: moduleNamePart1Up) ->
                                    Dict.get (moduleNamePart0 :: moduleNamePart1Up) checkInfo.importCustomTypes
                                        |> Maybe.map
                                            (\customTypes ->
                                                { name = moduleNamePart0 :: moduleNamePart1Up, customTypes = customTypes }
                                            )

                                Nothing ->
                                    Nothing
                    in
                    case maybeValueModule of
                        Nothing ->
                            Nothing

                        Just valueModule ->
                            Maybe.map2
                                (\customTypeWithVariant cases ->
                                    { cased =
                                        { moduleName = valueModule.name
                                        , name = valueOrCall.fnName
                                        , attachments = valueOrCall.args
                                        , customTypeVariantNames = customTypeWithVariant.variantNames
                                        }
                                    , cases = cases
                                    }
                                )
                                (getCustomTypeWithVariant valueOrCall.fnName valueModule.customTypes)
                                (traverse getVariantCase config.cases)
    in
    case maybeVariantCaseOf of
        Nothing ->
            Nothing

        Just variantCaseOf ->
            case Set.size variantCaseOf.cased.customTypeVariantNames of
                1 ->
                    caseSingleVariantWithUnreachableCasesCheck
                        { cased = variantCaseOf.cased
                        , casedExpressionRange = Node.range config.casedExpressionNode
                        , cases = variantCaseOf.cases
                        }
                        checkInfo

                -- >= 2 possible variants
                _ ->
                    Just
                        (caseMultiVariantWithUnreachableCasesError
                            { cased = variantCaseOf.cased
                            , casedExpressionRange = Node.range config.casedExpressionNode
                            , cases = variantCaseOf.cases
                            }
                            checkInfo
                        )


caseSingleVariantWithUnreachableCasesCheck :
    { cased : { moduleName : ModuleName, name : String, attachments : List (Node Expression), customTypeVariantNames : Set String }
    , casedExpressionRange : Range
    , cases : List { patternRange : Range, name : String, attachments : List (Node Pattern), expressionNode : Node Expression }
    }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseSingleVariantWithUnreachableCasesCheck variantCaseOf checkInfo =
    variantCaseOf.cased.attachments
        |> List.foldl
            (\attachment soFar ->
                case traverse listFilledFromList soFar.remainingCaseVariantAttachmentListsFilled of
                    Just caseVariantAttachmentListsFilled ->
                        { index = soFar.index + 1
                        , remainingCaseVariantAttachmentListsFilled =
                            List.map listFilledTail caseVariantAttachmentListsFilled
                        , attachmentAndCasesList =
                            soFar.attachmentAndCasesList
                                |> (::) { index = soFar.index, attachment = attachment, cases = List.map listFilledHead caseVariantAttachmentListsFilled }
                        }

                    -- case curried variant of → compiler error
                    Nothing ->
                        { index = 0
                        , remainingCaseVariantAttachmentListsFilled = []
                        , attachmentAndCasesList = []
                        }
            )
            { index = 0
            , attachmentAndCasesList = []
            , remainingCaseVariantAttachmentListsFilled =
                List.map
                    (\case_ ->
                        List.map (\patternNode -> ( patternNode, case_.expressionNode ))
                            case_.attachments
                    )
                    variantCaseOf.cases
            }
        |> .attachmentAndCasesList
        |> findMap
            (\casedVariantAttachmentAndCases ->
                caseOfWithUnreachableCasesChecksOn
                    { casedExpressionNode = casedVariantAttachmentAndCases.attachment
                    , cases = casedVariantAttachmentAndCases.cases
                    }
                    checkInfo
                    |> Maybe.map
                        (\attachmentError ->
                            { attachmentError
                                | fix =
                                    { casedExpressionReplace =
                                        { range = variantCaseOf.casedExpressionRange
                                        , replacement =
                                            toNestedTupleFix
                                                (List.indexedMap
                                                    (\i (Node argRange _) ->
                                                        if i == casedVariantAttachmentAndCases.index then
                                                            attachmentError.fix.casedExpressionReplace.replacement

                                                        else
                                                            checkInfo.extractSourceCode argRange
                                                    )
                                                    variantCaseOf.cased.attachments
                                                )
                                        }
                                    , cases =
                                        List.map2
                                            (\variantPattern patternAttachmentError ->
                                                case patternAttachmentError of
                                                    UnreachableCaseRemove remove ->
                                                        UnreachableCaseRemove remove

                                                    UnreachableCaseReplace replace ->
                                                        UnreachableCaseReplace
                                                            { range = variantPattern.patternRange
                                                            , replacement =
                                                                toNestedTupleFix
                                                                    (List.indexedMap
                                                                        (\i (Node attachmentRange _) ->
                                                                            if i == casedVariantAttachmentAndCases.index then
                                                                                replace.replacement

                                                                            else
                                                                                checkInfo.extractSourceCode attachmentRange
                                                                        )
                                                                        variantPattern.attachments
                                                                    )
                                                            , expressionNode = replace.expressionNode
                                                            }
                                            )
                                            variantCaseOf.cases
                                            attachmentError.fix.cases
                                    }
                            }
                        )
            )


caseMultiVariantWithUnreachableCasesError :
    { cased : { moduleName : ModuleName, name : String, attachments : List (Node Expression), customTypeVariantNames : Set String }
    , casedExpressionRange : Range
    , cases : List { patternRange : Range, name : String, attachments : List (Node Pattern), expressionNode : Node Expression }
    }
    ->
        { checkInfo
            | parentRange : Range
            , extractSourceCode : Range -> String
        }
    -> { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseMultiVariantWithUnreachableCasesError variantCaseOf checkInfo =
    let
        caseIndexesMatchingOnDifferentVariant : List Int
        caseIndexesMatchingOnDifferentVariant =
            variantCaseOf.cases
                |> List.indexedMap (\caseIndex variant -> { index = caseIndex, variant = variant })
                |> List.filterMap
                    (\case_ ->
                        if case_.variant.name /= variantCaseOf.cased.name then
                            Just case_.index

                        else
                            Nothing
                    )
    in
    { message = "Unreachable case branches"
    , details =
        [ "The value between case ... of is a known "
            ++ qualifiedToString (qualify ( variantCaseOf.cased.moduleName, variantCaseOf.cased.name ) defaultQualifyResources)
            ++ " variant. However, the "
            ++ (caseIndexesMatchingOnDifferentVariant |> List.map indexthToString |> String.join " and ")
            ++ " case matches on a different variant which means you can remove it."
        ]
    , range =
        findMap
            (\case_ ->
                if case_.name /= variantCaseOf.cased.name then
                    Just case_.patternRange

                else
                    Nothing
            )
            variantCaseOf.cases
            |> Maybe.withDefault (caseKeyWordRange checkInfo.parentRange)
    , fix =
        { casedExpressionReplace =
            { range = variantCaseOf.casedExpressionRange
            , replacement =
                toNestedTupleFix
                    (List.map (\(Node argRange _) -> checkInfo.extractSourceCode argRange)
                        variantCaseOf.cased.attachments
                    )
            }
        , cases =
            List.map
                (\variantCase ->
                    if variantCase.name == variantCaseOf.cased.name then
                        UnreachableCaseReplace
                            { range = variantCase.patternRange
                            , replacement =
                                toNestedTupleFix
                                    (List.map (\(Node argRange _) -> checkInfo.extractSourceCode argRange)
                                        variantCase.attachments
                                    )
                            , expressionNode = variantCase.expressionNode
                            }

                    else
                        UnreachableCaseRemove
                            { patternRange = variantCase.patternRange
                            , expressionRange = Node.range variantCase.expressionNode
                            }
                )
                variantCaseOf.cases
        }
    }


getVariantCase : ( Node Pattern, Node Expression ) -> Maybe { patternRange : Range, expressionNode : Node Expression, name : String, attachments : List (Node Pattern) }
getVariantCase ( patternNode, expressionNode ) =
    case AstHelpers.removeParensFromPattern patternNode of
        Node patternRange (Pattern.NamedPattern qualified attachments) ->
            Just { patternRange = patternRange, expressionNode = expressionNode, name = qualified.name, attachments = attachments }

        _ ->
            Nothing


getCustomTypeWithVariant : String -> Dict String { variantNames : Set String } -> Maybe { name : String, variantNames : Set String }
getCustomTypeWithVariant variantName customTypes =
    customTypes
        |> Dict.foldl
            (\customTypeName customTypeInfo soFar ->
                case soFar of
                    Just found ->
                        Just found

                    Nothing ->
                        if Set.member variantName customTypeInfo.variantNames then
                            Just { name = customTypeName, variantNames = customTypeInfo.variantNames }

                        else
                            Nothing
            )
            Nothing


caseListLiteralOfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseListLiteralOfWithUnreachableCasesChecks config checkInfo =
    let
        maybeListPatternCases : Maybe (List { patternRange : Range, expressionNode : Node Expression, pattern : ListPattern })
        maybeListPatternCases =
            traverse
                (\( patternNode, expressionNode ) ->
                    Maybe.map
                        (\listPattern ->
                            { patternRange = Node.range patternNode, expressionNode = expressionNode, pattern = listPattern }
                        )
                        (getListPattern (Node.value patternNode))
                )
                config.cases
    in
    Maybe.map2
        (\casedListLiteralElements listPatternCases ->
            caseListLiteralOfWithUnreachableCasesError
                { casedExpressionRange = Node.range config.casedExpressionNode
                , listPatternCases = listPatternCases
                , casedListLiteralElements = casedListLiteralElements
                }
                checkInfo
        )
        (AstHelpers.getListLiteral config.casedExpressionNode)
        maybeListPatternCases


caseListLiteralOfWithUnreachableCasesError :
    { casedExpressionRange : Range
    , casedListLiteralElements : List (Node Expression)
    , listPatternCases : List { patternRange : Range, expressionNode : Node Expression, pattern : ListPattern }
    }
    ->
        { checkInfo
            | extractSourceCode : Range -> String
            , parentRange : Range
        }
    -> { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseListLiteralOfWithUnreachableCasesError config checkInfo =
    let
        casedListLiteralLength : Int
        casedListLiteralLength =
            List.length config.casedListLiteralElements

        isUnnecessaryListPattern : ListPattern -> Bool
        isUnnecessaryListPattern listPattern =
            case listPattern of
                ListLiteralPattern listLiteralPatternELements ->
                    List.length listLiteralPatternELements /= casedListLiteralLength

                ConsPattern consPattern ->
                    List.length (listFilledToList consPattern.beginningElements) >= (casedListLiteralLength + 1)

        alwaysMatchedBeginningElementCount : Int
        alwaysMatchedBeginningElementCount =
            config.listPatternCases
                |> List.filterMap
                    (\listPatternCase ->
                        case listPatternCase.pattern of
                            ListLiteralPattern _ ->
                                Nothing

                            ConsPattern consPattern ->
                                Just (List.length (listFilledToList consPattern.beginningElements))
                    )
                |> List.minimum
                |> Maybe.withDefault casedListLiteralLength

        unnecessaryListPatternIndexes : List Int
        unnecessaryListPatternIndexes =
            config.listPatternCases
                |> List.indexedMap (\caseIndex case_ -> { index = caseIndex, case_ = case_ })
                |> List.filterMap
                    (\caseAndIndex ->
                        if isUnnecessaryListPattern caseAndIndex.case_.pattern then
                            Just caseAndIndex.index

                        else
                            Nothing
                    )
    in
    { message = "Unreachable case branches"
    , details =
        [ "The value between case ... of is a known list of length "
            ++ String.fromInt casedListLiteralLength
            ++ ". However, the "
            ++ (unnecessaryListPatternIndexes |> List.map indexthToString |> String.join " and ")
            ++ " case matches on a list with a different length which means you can remove it."
        ]
    , range =
        findMap
            (\case_ ->
                if isUnnecessaryListPattern case_.pattern then
                    Just case_.patternRange

                else
                    Nothing
            )
            config.listPatternCases
            |> Maybe.withDefault (caseKeyWordRange checkInfo.parentRange)
    , fix =
        { casedExpressionReplace =
            { range = config.casedExpressionRange
            , replacement =
                listLiteralToNestedTupleFix
                    { tupledBeginningLength = alwaysMatchedBeginningElementCount
                    , elements =
                        List.map (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                            config.casedListLiteralElements
                    }
            }
        , cases =
            List.map
                (\listPatternCase ->
                    if isUnnecessaryListPattern listPatternCase.pattern then
                        UnreachableCaseRemove
                            { patternRange = listPatternCase.patternRange
                            , expressionRange = Node.range listPatternCase.expressionNode
                            }

                    else
                        UnreachableCaseReplace
                            { range = listPatternCase.patternRange
                            , expressionNode = listPatternCase.expressionNode
                            , replacement =
                                case listPatternCase.pattern of
                                    ListLiteralPattern listPatternElements ->
                                        listLiteralToNestedTupleFix
                                            { tupledBeginningLength = alwaysMatchedBeginningElementCount
                                            , elements =
                                                List.map (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                                                    listPatternElements
                                            }

                                    ConsPattern consPattern ->
                                        collapsedConsToNestedTupleFix
                                            { tupledBeginningLength = alwaysMatchedBeginningElementCount
                                            , beginningElements =
                                                listFilledMap (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                                                    consPattern.beginningElements
                                            , tail = checkInfo.extractSourceCode (Node.range consPattern.tail)
                                            }
                            }
                )
                config.listPatternCases
        }
    }


caseConsOfWithUnreachableCasesChecks :
    { casedExpressionNode : Node Expression, cases : List ( Node Pattern, Node Expression ) }
    -> CaseOfCheckInfo
    -> Maybe { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseConsOfWithUnreachableCasesChecks config checkInfo =
    let
        maybeListPatternCases : Maybe (List { patternRange : Range, expressionNode : Node Expression, pattern : ListPattern })
        maybeListPatternCases =
            traverse
                (\( patternNode, expressionNode ) ->
                    Maybe.map
                        (\listPattern ->
                            { patternRange = Node.range patternNode, expressionNode = expressionNode, pattern = listPattern }
                        )
                        (getListPattern (Node.value patternNode))
                )
                config.cases
    in
    Maybe.map2
        (\casedCons listPatternCases ->
            caseConsOfWithUnreachableCasesError
                { casedExpressionRange = Node.range config.casedExpressionNode
                , casedCons = casedCons
                , listPatternCases = listPatternCases
                }
                checkInfo
        )
        (getCollapsedCons (Node.value config.casedExpressionNode))
        maybeListPatternCases


caseConsOfWithUnreachableCasesError :
    { casedExpressionRange : Range
    , casedCons : { beginningElements : ( Node Expression, List (Node Expression) ), tail : Node Expression }
    , listPatternCases : List { patternRange : Range, expressionNode : Node Expression, pattern : ListPattern }
    }
    ->
        { checkInfo
            | extractSourceCode : Range -> String
            , parentRange : Range
        }
    -> { message : String, details : List String, range : Range, fix : UnreachableCasesFix }
caseConsOfWithUnreachableCasesError config checkInfo =
    let
        casedBeginningElementCount : Int
        casedBeginningElementCount =
            listFilledLength config.casedCons.beginningElements

        isUnnecessaryListPattern : ListPattern -> Bool
        isUnnecessaryListPattern listPattern =
            case listPattern of
                ListLiteralPattern listLiteralPatternELements ->
                    List.length listLiteralPatternELements < casedBeginningElementCount

                ConsPattern _ ->
                    False

        alwaysMatchedBeginningElementCount : Int
        alwaysMatchedBeginningElementCount =
            config.listPatternCases
                |> List.filterMap
                    (\listPatternCase ->
                        case listPatternCase.pattern of
                            ListLiteralPattern _ ->
                                Nothing

                            ConsPattern consPattern ->
                                Just (List.length (listFilledToList consPattern.beginningElements))
                    )
                |> List.minimum
                |> Maybe.withDefault casedBeginningElementCount

        unnecessaryListPatternIndexes : List Int
        unnecessaryListPatternIndexes =
            config.listPatternCases
                |> List.indexedMap (\caseIndex case_ -> { index = caseIndex, case_ = case_ })
                |> List.filterMap
                    (\caseAndIndex ->
                        if isUnnecessaryListPattern caseAndIndex.case_.pattern then
                            Just caseAndIndex.index

                        else
                            Nothing
                    )
    in
    { message = "Unreachable case branches"
    , details =
        [ "The value between case ... of is a list of length >= "
            ++ String.fromInt casedBeginningElementCount
            ++ ". However, the "
            ++ (unnecessaryListPatternIndexes |> List.map indexthToString |> String.join " and ")
            ++ " case matches on a shorter list which means you can remove it."
        ]
    , range =
        findMap
            (\case_ ->
                if isUnnecessaryListPattern case_.pattern then
                    Just case_.patternRange

                else
                    Nothing
            )
            config.listPatternCases
            |> Maybe.withDefault (caseKeyWordRange checkInfo.parentRange)
    , fix =
        { casedExpressionReplace =
            { range = config.casedExpressionRange
            , replacement =
                collapsedConsToNestedTupleFix
                    { tupledBeginningLength = alwaysMatchedBeginningElementCount
                    , beginningElements =
                        listFilledMap (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                            config.casedCons.beginningElements
                    , tail = checkInfo.extractSourceCode (Node.range config.casedCons.tail)
                    }
            }
        , cases =
            List.map
                (\listPatternCase ->
                    if isUnnecessaryListPattern listPatternCase.pattern then
                        UnreachableCaseRemove
                            { patternRange = listPatternCase.patternRange
                            , expressionRange = Node.range listPatternCase.expressionNode
                            }

                    else
                        UnreachableCaseReplace
                            { range = listPatternCase.patternRange
                            , expressionNode = listPatternCase.expressionNode
                            , replacement =
                                case listPatternCase.pattern of
                                    ListLiteralPattern listPatternElements ->
                                        listLiteralToNestedTupleFix
                                            { tupledBeginningLength = alwaysMatchedBeginningElementCount
                                            , elements =
                                                List.map (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                                                    listPatternElements
                                            }

                                    ConsPattern consPattern ->
                                        collapsedConsToNestedTupleFix
                                            { tupledBeginningLength = alwaysMatchedBeginningElementCount
                                            , beginningElements =
                                                listFilledMap (\(Node elementRange _) -> checkInfo.extractSourceCode elementRange)
                                                    consPattern.beginningElements
                                            , tail = checkInfo.extractSourceCode (Node.range consPattern.tail)
                                            }
                            }
                )
                config.listPatternCases
        }
    }


type ListPattern
    = ListLiteralPattern (List (Node Pattern))
    | ConsPattern { beginningElements : ( Node Pattern, List (Node Pattern) ), tail : Node Pattern }


getListPattern : Pattern -> Maybe ListPattern
getListPattern pattern =
    case pattern of
        Pattern.ListPattern elementPatterns ->
            Just (ListLiteralPattern elementPatterns)

        nonListLiteralPattern ->
            Maybe.map ConsPattern (getCollapsedConsPattern nonListLiteralPattern)


getCollapsedConsPattern : Pattern -> Maybe { beginningElements : ( Node Pattern, List (Node Pattern) ), tail : Node Pattern }
getCollapsedConsPattern pattern =
    case pattern of
        Pattern.UnConsPattern head tail ->
            case getCollapsedConsPattern (Node.value tail) of
                Nothing ->
                    Just { beginningElements = ( head, [] ), tail = tail }

                Just tailCollapsed ->
                    Just { beginningElements = ( head, listFilledToList tailCollapsed.beginningElements ), tail = tailCollapsed.tail }

        _ ->
            Nothing


getCollapsedCons : Expression -> Maybe { beginningElements : ( Node Expression, List (Node Expression) ), tail : Node Expression }
getCollapsedCons expression =
    case expression of
        Expression.OperatorApplication "::" _ head tail ->
            case getCollapsedCons (Node.value tail) of
                Just tailCollapsed ->
                    Just { beginningElements = ( head, listFilledToList tailCollapsed.beginningElements ), tail = tailCollapsed.tail }

                Nothing ->
                    Just { beginningElements = ( head, [] ), tail = tail }

        _ ->
            Nothing


indexthToString : Int -> String
indexthToString index =
    let
        suffix : String
        suffix =
            case (index + 1) |> remainderBy 10 of
                1 ->
                    "st"

                2 ->
                    "nd"

                3 ->
                    "rd"

                _ ->
                    "th"
    in
    String.fromInt (index + 1) ++ suffix



-- NEGATION


negationChecks : { parentRange : Range, negatedExpression : Node Expression } -> Maybe (Error {})
negationChecks checkInfo =
    case AstHelpers.removeParens checkInfo.negatedExpression of
        Node range (Expression.Negation negatedValue) ->
            let
                doubleNegationRange : Range
                doubleNegationRange =
                    { start = checkInfo.parentRange.start
                    , end = { row = range.start.row, column = range.start.column + 1 }
                    }
            in
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary double number negation"
                    , details = [ "Negating a number twice is the same as the number itself." ]
                    }
                    doubleNegationRange
                    (replaceBySubExpressionFix checkInfo.parentRange negatedValue)
                )

        _ ->
            Nothing



-- FULLY APPLIED PREFIX OPERATORS


fullyAppliedPrefixOperatorError :
    { operator : String
    , operatorRange : Range
    , left : Node Expression
    , right : Node Expression
    }
    -> Error {}
fullyAppliedPrefixOperatorError checkInfo =
    Rule.errorWithFix
        { message = "Use the infix form (a + b) over the prefix form ((+) a b)"
        , details = [ "The prefix form is generally more unfamiliar to Elm developers, and therefore it is nicer when the infix form is used." ]
        }
        checkInfo.operatorRange
        [ Fix.removeRange { start = checkInfo.operatorRange.start, end = (Node.range checkInfo.left).start }
        , Fix.insertAt (Node.range checkInfo.right).start (checkInfo.operator ++ " ")
        ]



-- APPLIED LAMBDA


appliedLambdaError : { nodeRange : Range, lambdaRange : Range, lambda : Expression.Lambda } -> Maybe (Error {})
appliedLambdaError checkInfo =
    case checkInfo.lambda.args of
        (Node unitRange Pattern.UnitPattern) :: otherPatterns ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary unit argument"
                    , details =
                        [ "This function is expecting a unit, but also passing it directly."
                        , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                        ]
                    }
                    unitRange
                    (case otherPatterns of
                        [] ->
                            replaceBySubExpressionFix checkInfo.nodeRange checkInfo.lambda.expression

                        secondPattern :: _ ->
                            Fix.removeRange { start = unitRange.start, end = (Node.range secondPattern).start }
                                :: keepOnlyAndParenthesizeFix { parentRange = checkInfo.nodeRange, keep = checkInfo.lambdaRange }
                    )
                )

        (Node allRange Pattern.AllPattern) :: otherPatterns ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary wildcard argument argument"
                    , details =
                        [ "This function is being passed an argument that is directly ignored."
                        , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                        ]
                    }
                    allRange
                    (case otherPatterns of
                        [] ->
                            replaceBySubExpressionFix checkInfo.nodeRange checkInfo.lambda.expression

                        secondPattern :: _ ->
                            Fix.removeRange { start = allRange.start, end = (Node.range secondPattern).start }
                                :: keepOnlyAndParenthesizeFix { parentRange = checkInfo.nodeRange, keep = checkInfo.lambdaRange }
                    )
                )

        _ ->
            Nothing



-- LET IN


letInChecks : Expression.LetBlock -> Maybe (Error {})
letInChecks letBlock =
    case Node.value letBlock.expression of
        Expression.LetExpression _ ->
            let
                letRange : Range
                letRange =
                    letKeyWordRange (Node.range letBlock.expression)
            in
            Just
                (Rule.errorWithFix
                    { message = "Let blocks can be joined together"
                    , details = [ "Let blocks can contain multiple declarations, and there is no advantage to having multiple chained let expressions rather than one longer let expression." ]
                    }
                    letRange
                    (case listLast letBlock.declarations of
                        Just (Node lastDeclRange _) ->
                            [ Fix.replaceRangeBy { start = lastDeclRange.end, end = letRange.end } "\n" ]

                        Nothing ->
                            []
                    )
                )

        _ ->
            Nothing


letKeyWordRange : Range -> Range
letKeyWordRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 3 }
    }



-- RECORD ACCESS


accessingRecordChecks :
    { parentRange : Range
    , fieldName : String
    , fieldRange : Range
    , record : Node Expression
    , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
    , moduleRecordTypeAliases : Dict String (List String)
    , lookupTable : ModuleNameLookupTable
    }
    -> Maybe ErrorInfoAndFix
accessingRecordChecks checkInfo =
    case Node.value (AstHelpers.removeParens checkInfo.record) of
        Expression.RecordExpr fields ->
            accessingRecordWithKnownFieldsChecks
                { nodeRange = checkInfo.parentRange
                , maybeRecordNameRange = Nothing
                , fieldName = checkInfo.fieldName
                , knownFields = fields
                }

        Expression.RecordUpdateExpression (Node recordNameRange _) setFields ->
            accessingRecordWithKnownFieldsChecks
                { nodeRange = checkInfo.parentRange
                , maybeRecordNameRange = Just recordNameRange
                , fieldName = checkInfo.fieldName
                , knownFields = setFields
                }

        Expression.LetExpression letIn ->
            Just
                { info =
                    { message = "Accessing a field outside a let...in will result in accessing it in its result"
                    , details = [ "You can replace accessing this record outside the let...in by accessing its result record after `in`." ]
                    }
                , fix =
                    keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.record }
                        ++ replaceSubExpressionByRecordAccessFix checkInfo.fieldName letIn.expression
                }

        Expression.IfBlock _ thenBranch elseBranch ->
            distributeFieldAccess "an if...then...else" [ thenBranch, elseBranch ] checkInfo

        Expression.CaseExpression caseOf ->
            distributeFieldAccess "a case...of" (List.map Tuple.second caseOf.cases) checkInfo

        _ ->
            case getRecordTypeAliasConstructorCall checkInfo.record checkInfo of
                Just recordTypeAliasConstructorCall ->
                    if List.length recordTypeAliasConstructorCall.fieldNames == List.length recordTypeAliasConstructorCall.args then
                        accessingRecordWithKnownFieldsChecks
                            { nodeRange = checkInfo.parentRange
                            , fieldName = checkInfo.fieldName
                            , maybeRecordNameRange = Nothing
                            , knownFields =
                                List.map2 (\name arg -> Node.empty ( Node.empty name, arg ))
                                    recordTypeAliasConstructorCall.fieldNames
                                    recordTypeAliasConstructorCall.args
                            }

                    else
                        Nothing

                Nothing ->
                    Nothing


accessingRecordCompositionChecks : CompositionCheckInfo -> Maybe (Error {})
accessingRecordCompositionChecks checkInfo =
    case AstHelpers.getRecordAccessFunction checkInfo.later.node of
        Just accessFunctionFieldName ->
            firstThatConstructsJust
                [ \() ->
                    case constructs (getRecordWithKnownFields checkInfo) checkInfo.lookupTable checkInfo.earlier.node of
                        Just recordWithKnownField ->
                            accessingRecordWithKnownFieldsChecks
                                { nodeRange = recordWithKnownField.range
                                , fieldName = accessFunctionFieldName
                                , maybeRecordNameRange = recordWithKnownField.maybeRecordNameRange
                                , knownFields = recordWithKnownField.knownFields
                                }
                                |> Maybe.map
                                    (\e ->
                                        Rule.errorWithFix e.info
                                            (Node.range checkInfo.later.node)
                                            (Fix.removeRange checkInfo.later.removeRange :: e.fix)
                                    )

                        Nothing ->
                            Nothing
                , \() ->
                    case getRecordTypeAliasConstructorCall checkInfo.earlier.node checkInfo of
                        Just recordTypeAliasConstructorCall ->
                            if List.length recordTypeAliasConstructorCall.fieldNames == (List.length recordTypeAliasConstructorCall.args + 1) then
                                case recordTypeAliasConstructorCall.fieldNames of
                                    -- alias to {} (which is actually allowed by elm)
                                    [] ->
                                        Nothing

                                    firstFirstName :: afterFirstFieldName ->
                                        if listFilledLast ( firstFirstName, afterFirstFieldName ) == accessFunctionFieldName then
                                            Just
                                                (Rule.errorWithFix
                                                    { message = "Unnecessary constructing a record around a field that is then being accessed"
                                                    , details = [ "This composition will construct a record with the incoming value stored in the field " ++ wrapInBackticks accessFunctionFieldName ++ ". This exact field is then immediately accessed which means you can replace this composition by identity." ]
                                                    }
                                                    (Node.range checkInfo.later.node)
                                                    [ Fix.removeRange checkInfo.earlier.removeRange
                                                    , Fix.replaceRangeBy (Node.range checkInfo.later.node)
                                                        (qualifiedToString (qualify Fn.Basics.identity checkInfo))
                                                    ]
                                                )

                                        else
                                            let
                                                maybeAccessedFieldExpressionNode : Maybe (Node Expression)
                                                maybeAccessedFieldExpressionNode =
                                                    findMap
                                                        (\field ->
                                                            if field.name == accessFunctionFieldName then
                                                                Just field.arg

                                                            else
                                                                Nothing
                                                        )
                                                        (List.map2 (\name arg -> { name = name, arg = arg })
                                                            recordTypeAliasConstructorCall.fieldNames
                                                            recordTypeAliasConstructorCall.args
                                                        )
                                            in
                                            case maybeAccessedFieldExpressionNode of
                                                Just accessedFieldExpressionNode ->
                                                    Just
                                                        (Rule.errorWithFix
                                                            { message = "Accessing a field of a record where we know that field's value will return that field's value"
                                                            , details = [ "This composition will construct a record where we known the value of the field " ++ wrapInBackticks accessFunctionFieldName ++ ". This exact field is then immediately accessed which means you can replace this composition by `always` with that value." ]
                                                            }
                                                            (Node.range checkInfo.later.node)
                                                            (replaceBySubExpressionFix (Node.range checkInfo.earlier.node) accessedFieldExpressionNode
                                                                ++ [ Fix.insertAt (Node.range checkInfo.earlier.node).start
                                                                        (qualifiedToString (qualify Fn.Basics.always checkInfo) ++ " ")
                                                                   , Fix.removeRange checkInfo.later.removeRange
                                                                   ]
                                                            )
                                                        )

                                                -- compile-time error
                                                Nothing ->
                                                    Nothing

                            else
                                Nothing

                        Nothing ->
                            Nothing
                ]
                ()

        Nothing ->
            Nothing


getRecordWithKnownFields :
    { checkInfo
        | importRecordTypeAliases : Dict ModuleName (Dict String (List String))
        , moduleRecordTypeAliases : Dict String (List String)
        , lookupTable : ModuleNameLookupTable
    }
    -> Node Expression
    -> Maybe { range : Range, maybeRecordNameRange : Maybe Range, knownFields : List (Node ( Node String, Node Expression )) }
getRecordWithKnownFields resources expressionNode =
    case AstHelpers.removeParens expressionNode of
        Node range (Expression.RecordExpr fields) ->
            Just
                { range = range
                , maybeRecordNameRange = Nothing
                , knownFields = fields
                }

        Node range (Expression.RecordUpdateExpression (Node recordNameRange _) setFields) ->
            Just
                { range = range
                , maybeRecordNameRange = Just recordNameRange
                , knownFields = setFields
                }

        nonRecordLiteralExpressionNode ->
            case getRecordTypeAliasConstructorCall nonRecordLiteralExpressionNode resources of
                Just recordTypeAliasConstructorCall ->
                    if List.length recordTypeAliasConstructorCall.fieldNames == List.length recordTypeAliasConstructorCall.args then
                        Just
                            { range = Node.range nonRecordLiteralExpressionNode
                            , maybeRecordNameRange = Nothing
                            , knownFields =
                                List.map2 (\name arg -> Node.empty ( Node.empty name, arg ))
                                    recordTypeAliasConstructorCall.fieldNames
                                    recordTypeAliasConstructorCall.args
                            }

                    else
                        Nothing

                Nothing ->
                    Nothing


accessingRecordWithKnownFieldsChecks :
    { nodeRange : Range
    , maybeRecordNameRange : Maybe Range
    , fieldName : String
    , knownFields : List (Node Expression.RecordSetter)
    }
    -> Maybe ErrorInfoAndFix
accessingRecordWithKnownFieldsChecks checkInfo =
    let
        maybeMatchingSetterValue : Maybe (Node Expression)
        maybeMatchingSetterValue =
            findMap
                (\(Node _ ( Node _ setterField, setterValue )) ->
                    if setterField == checkInfo.fieldName then
                        Just setterValue

                    else
                        Nothing
                )
                checkInfo.knownFields
    in
    case maybeMatchingSetterValue of
        Just setter ->
            Just
                { info =
                    { message = "Accessing a field of a record where we know that field's value will return that field's value"
                    , details = [ "You can replace accessing this record by just that field's value." ]
                    }
                , fix = replaceBySubExpressionFix checkInfo.nodeRange setter
                }

        Nothing ->
            case checkInfo.maybeRecordNameRange of
                Just recordNameRange ->
                    Just
                        { info =
                            { message = "Updating a record, then accessing an unchanged field will result in that field from the unchanged record"
                            , details = [ "You can replace accessing this record by just the original record variable inside the record update." ]
                            }
                        , fix =
                            [ Fix.removeRange { start = checkInfo.nodeRange.start, end = recordNameRange.start }
                            , Fix.replaceRangeBy { start = recordNameRange.end, end = checkInfo.nodeRange.end } ("." ++ checkInfo.fieldName)
                            ]
                        }

                Nothing ->
                    Nothing


distributeFieldAccess :
    String
    -> List (Node Expression)
    ->
        { checkInfo
            | parentRange : Range
            , record : Node Expression
            , fieldName : String
            , importRecordTypeAliases : Dict ModuleName (Dict String (List String))
            , moduleRecordTypeAliases : Dict String (List String)
            , lookupTable : ModuleNameLookupTable
        }
    -> Maybe ErrorInfoAndFix
distributeFieldAccess kind branches checkInfo =
    let
        recordWithKnownFieldsInAllBranches : Maybe (List (Node Expression))
        recordWithKnownFieldsInAllBranches =
            traverse
                (\surfaceBranch ->
                    sameInAllBranches
                        (\innerBranch ->
                            getRecordWithKnownFields checkInfo innerBranch
                                |> Maybe.map (\_ -> innerBranch)
                        )
                        surfaceBranch
                )
                branches
                |> Maybe.map List.concat
    in
    case recordWithKnownFieldsInAllBranches of
        Just records ->
            Just
                { info =
                    { message = "Accessing a field outside " ++ kind ++ " will result in accessing it in each branch"
                    , details = [ "You can replace accessing this record outside " ++ kind ++ " by accessing the record inside each branch." ]
                    }
                , fix =
                    keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.record }
                        ++ List.concatMap (\recordWithKnownFields -> replaceSubExpressionByRecordAccessFix checkInfo.fieldName recordWithKnownFields) records
                }

        Nothing ->
            Nothing


getRecordTypeAliasConstructorCall :
    Node Expression
    ->
        { checkInfo
            | importRecordTypeAliases : Dict ModuleName (Dict String (List String))
            , moduleRecordTypeAliases : Dict String (List String)
            , lookupTable : ModuleNameLookupTable
        }
    -> Maybe { nodeRange : Range, args : List (Node Expression), fieldNames : List String }
getRecordTypeAliasConstructorCall expressionNode checkInfo =
    case AstHelpers.getValueOrFnOrFnCall expressionNode of
        Nothing ->
            Nothing

        Just valueOrFnOrCall ->
            case ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable valueOrFnOrCall.fnRange of
                Nothing ->
                    Nothing

                Just [] ->
                    Dict.get valueOrFnOrCall.fnName checkInfo.moduleRecordTypeAliases
                        |> Maybe.map
                            (\fieldNames ->
                                { nodeRange = valueOrFnOrCall.nodeRange
                                , args = valueOrFnOrCall.args
                                , fieldNames = fieldNames
                                }
                            )

                Just (moduleNamePart0 :: moduleNamePart1Up) ->
                    case Dict.get (moduleNamePart0 :: moduleNamePart1Up) checkInfo.importRecordTypeAliases of
                        Nothing ->
                            Nothing

                        Just importModuleRecordTypeAliases ->
                            Dict.get valueOrFnOrCall.fnName importModuleRecordTypeAliases
                                |> Maybe.map
                                    (\fieldNames ->
                                        { nodeRange = valueOrFnOrCall.nodeRange
                                        , args = valueOrFnOrCall.args
                                        , fieldNames = fieldNames
                                        }
                                    )



-- FIX HELPERS


parenthesizeIfNeededFix : Node Expression -> List Fix
parenthesizeIfNeededFix (Node expressionRange expression) =
    if needsParens expression then
        parenthesizeFix expressionRange

    else
        []


parenthesizeFix : Range -> List Fix
parenthesizeFix toSurround =
    [ Fix.insertAt toSurround.start "("
    , Fix.insertAt toSurround.end ")"
    ]


keepOnlyFix : { parentRange : Range, keep : Range } -> List Fix
keepOnlyFix config =
    [ Fix.removeRange
        { start = config.parentRange.start
        , end = config.keep.start
        }
    , Fix.removeRange
        { start = config.keep.end
        , end = config.parentRange.end
        }
    ]


keepOnlyAndParenthesizeFix : { parentRange : Range, keep : Range } -> List Fix
keepOnlyAndParenthesizeFix config =
    [ Fix.replaceRangeBy { start = config.parentRange.start, end = config.keep.start } "("
    , Fix.replaceRangeBy { start = config.keep.end, end = config.parentRange.end } ")"
    ]


replaceBySubExpressionFix : Range -> Node Expression -> List Fix
replaceBySubExpressionFix outerRange (Node exprRange exprValue) =
    if needsParens exprValue then
        keepOnlyAndParenthesizeFix { parentRange = outerRange, keep = exprRange }

    else
        keepOnlyFix { parentRange = outerRange, keep = exprRange }


replaceSubExpressionByRecordAccessFix : String -> Node Expression -> List Fix
replaceSubExpressionByRecordAccessFix fieldName (Node exprRange exprValue) =
    if needsParens exprValue then
        [ Fix.insertAt exprRange.start "("
        , Fix.insertAt exprRange.end (")." ++ fieldName)
        ]

    else
        [ Fix.insertAt exprRange.end ("." ++ fieldName) ]


rangeBetweenExclusive : ( Range, Range ) -> Range
rangeBetweenExclusive ( aRange, bRange ) =
    case Range.compareLocations aRange.start bRange.start of
        GT ->
            { start = bRange.end, end = aRange.start }

        -- EQ | LT
        _ ->
            { start = aRange.end, end = bRange.start }


{-| Takes the ranges of two neighboring elements and
returns a range that includes the specified element and everything between them.

This is useful when you can't use `replaceBySubExpressionFix` and `keepOnlyFix` because there is no
existing node that could be kept.

For example, you might want to remove `|> identity` in `f |> g |> identity`. `elm-syntax` might represent this as (simplified)

    Op (Var "f") "|>" (Op (Var "g") "|>" (Var "identity"))

In practice, you will check this syntax tree recursively, leading to situations where we only know

  - the previous/next element which we want to keep
  - and the current element which we want to remove

-}
andBetweenRange : { excluded : Range, included : Range } -> Range
andBetweenRange ranges =
    case Range.compare ranges.excluded ranges.included of
        LT ->
            { start = ranges.excluded.end, end = ranges.included.end }

        -- GT | EQ ->
        _ ->
            { start = ranges.included.start, end = ranges.excluded.start }


listLiteralToNestedTupleFix : { tupledBeginningLength : Int, elements : List String } -> String
listLiteralToNestedTupleFix config =
    case config.elements of
        [] ->
            "()"

        element0 :: element1Up ->
            let
                tupledElements : ( String, List String )
                tupledElements =
                    ( element0, List.take (config.tupledBeginningLength - 1) element1Up )
            in
            case List.drop (config.tupledBeginningLength - 1) element1Up of
                [] ->
                    toNestedTupleFix (listFilledToList tupledElements)

                firstUntupledElementRange :: afterFirstUntupledElementRange ->
                    toNestedTupleFix
                        (listFilledToList tupledElements
                            ++ [ "[ "
                                    ++ String.join ", " (firstUntupledElementRange :: afterFirstUntupledElementRange)
                                    ++ " ]"
                               ]
                        )


collapsedConsToNestedTupleFix : { tupledBeginningLength : Int, beginningElements : ( String, List String ), tail : String } -> String
collapsedConsToNestedTupleFix config =
    toNestedTupleFix
        (listFilledHead config.beginningElements
            :: List.take (config.tupledBeginningLength - 1) (listFilledTail config.beginningElements)
            ++ [ String.join " :: "
                    (List.drop (config.tupledBeginningLength - 1) (listFilledTail config.beginningElements)
                        ++ [ config.tail ]
                    )
               ]
        )


toNestedTupleFix : List String -> String
toNestedTupleFix parts =
    case parts of
        [] ->
            "()"

        part0 :: parts1Up ->
            toNestedTupleFixFromPartial ( part0, parts1Up )


toNestedTupleFixFromPartial : ( String, List String ) -> String
toNestedTupleFixFromPartial parts =
    case parts of
        ( only, [] ) ->
            only

        ( first, second :: thirdUp ) ->
            "(" ++ first ++ ", " ++ toNestedTupleFixFromPartial ( second, thirdUp ) ++ ")"


rangeContainsLocation : Location -> Range -> Bool
rangeContainsLocation location =
    \range ->
        not
            ((Range.compareLocations location range.start == LT)
                || (Range.compareLocations location range.end == GT)
            )


rangeWithoutBoundaries : Range -> Range
rangeWithoutBoundaries range =
    { start = startWithoutBoundary range
    , end = endWithoutBoundary range
    }


startWithoutBoundary : Range -> Location
startWithoutBoundary range =
    { row = range.start.row, column = range.start.column + 1 }


endWithoutBoundary : Range -> Location
endWithoutBoundary range =
    { row = range.end.row, column = range.end.column - 1 }


removeBoundariesFix : Node a -> List Fix
removeBoundariesFix (Node nodeRange _) =
    keepOnlyFix { parentRange = nodeRange, keep = rangeWithoutBoundaries nodeRange }


leftBoundaryRange : Range -> Range
leftBoundaryRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 1 }
    }


{-| Shortcut for `alwaysResultsInConstantError` with `replacementNeedsParens = False`.

If you want to replace to something like `Just []`,
use `alwaysResultsInConstantError` with `replacementNeedsParens = True`.

-}
alwaysResultsInUnparenthesizedConstantError :
    String
    -> { replacement : QualifyResources {} -> String }
    -> CallCheckInfo
    -> Error {}
alwaysResultsInUnparenthesizedConstantError usingSituation config =
    alwaysResultsInConstantError usingSituation
        { replacement = config.replacement
        , replacementNeedsParens = False
        }


{-| Regardless of what the next incoming value will be, the result is already determined to be a given constant.

For example, `List.repeat 0` will always return [], whatever the argument will be.

If your function also always returns a constant but it does not have an irrelevant next argument,
like `List.range 1 0`, use `resultsInConstantError`

-}
alwaysResultsInConstantError :
    String
    ->
        { replacement : QualifyResources {} -> String
        , replacementNeedsParens : Bool
        }
    -> CallCheckInfo
    -> Error {}
alwaysResultsInConstantError usingSituation config checkInfo =
    let
        addNecessaryParens : String -> String
        addNecessaryParens string =
            if config.replacementNeedsParens then
                "(" ++ string ++ ")"

            else
                string

        replacement : QualifyResources {} -> String
        replacement =
            case checkInfo.argCount - (1 + List.length checkInfo.argsAfterFirst) of
                -- fully applied
                0 ->
                    config.replacement

                -- one arg curried
                1 ->
                    \res ->
                        qualifiedToString (qualify Fn.Basics.always res) ++ " " ++ addNecessaryParens (config.replacement res)

                -- multiple args curried
                atLeast2 ->
                    \res ->
                        "(\\" ++ String.repeat atLeast2 "_ " ++ "-> " ++ config.replacement res ++ ")"
    in
    Rule.errorWithFix
        { message = usingSituation ++ " will always result in " ++ config.replacement defaultQualifyResources
        , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
        }
        checkInfo.fnRange
        [ Fix.replaceRangeBy checkInfo.parentRange (replacement (extractQualifyResources checkInfo)) ]


{-| The result in the given situation is determined to be a given constant.

For example, `List.range 1 0` will return [].

If your function also always returns a constant but has an irrelevant next argument,
like `List.repeat 0`, use `alwaysResultsInConstantError`

-}
resultsInConstantError : String -> (QualifyResources {} -> String) -> CallCheckInfo -> Error {}
resultsInConstantError usingSituation replacement checkInfo =
    Rule.errorWithFix
        { message = usingSituation ++ " will result in " ++ replacement defaultQualifyResources
        , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
        }
        checkInfo.fnRange
        [ Fix.replaceRangeBy checkInfo.parentRange (replacement (extractQualifyResources checkInfo)) ]


compositionReplaceByFix :
    String
    -> { checkInfo | later : { later | range : Range }, earlier : { earlier | removeRange : Range } }
    -> List Fix
compositionReplaceByFix replacement checkInfo =
    [ Fix.replaceRangeBy checkInfo.later.range replacement
    , Fix.removeRange checkInfo.earlier.removeRange
    ]


compositionReplaceByFnFix :
    ( ModuleName, String )
    -> QualifyResources { checkInfo | later : { later | range : Range }, earlier : { earlier | removeRange : Range } }
    -> List Fix
compositionReplaceByFnFix replacementFn checkInfo =
    compositionReplaceByFix (qualifiedToString (qualify replacementFn checkInfo)) checkInfo


operationDoesNotChangeSpecificLastArgErrorInfo : { fn : ( ModuleName, String ), specific : TypeSubsetProperties otherTypeSubsetProperties } -> { message : String, details : List String }
operationDoesNotChangeSpecificLastArgErrorInfo config =
    let
        specificLastArgReference : String
        specificLastArgReference =
            typeSubsetDescriptionDefinite "the given" config.specific
    in
    { message = qualifiedToString config.fn ++ " on " ++ typeSubsetDescriptionIndefinite config.specific ++ " will result in " ++ specificLastArgReference
    , details = [ "You can replace this call by " ++ specificLastArgReference ++ "." ]
    }


{-| In your specific situation, the last incoming argument will always be returned unchanged.

For example, `List.map identity` will not change whatever list comes next. It is equivalent to `identity`

Use `returnsArgError` with the given last arg as `arg` when the last arg is already present.

-}
alwaysReturnsLastArgError :
    String
    -> TypeProperties otherProperties
    -> QualifyResources { a | fnRange : Range, parentRange : Range, argCount : Int, firstArg : Node Expression, argsAfterFirst : List (Node Expression) }
    -> Error {}
alwaysReturnsLastArgError usingSpecificSituation lastArgProperties checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just lastArg ->
            returnsArgError usingSpecificSituation { arg = lastArg, argRepresents = lastArgProperties.represents } checkInfo

        Nothing ->
            -- Not enough arguments
            let
                replacement : QualifyResources res -> String
                replacement =
                    case checkInfo.argCount - (1 + List.length checkInfo.argsAfterFirst) - 1 of
                        0 ->
                            \res -> qualifiedToString (qualify Fn.Basics.identity res)

                        1 ->
                            \res -> qualifiedToString (qualify Fn.Basics.always res) ++ " " ++ qualifiedToString (qualify Fn.Basics.identity res)

                        atLeast2 ->
                            \res -> "(\\" ++ String.repeat atLeast2 "_ " ++ "-> " ++ qualifiedToString (qualify Fn.Basics.identity res) ++ ")"
            in
            Rule.errorWithFix
                { message = usingSpecificSituation ++ " will always return the same given " ++ lastArgProperties.represents
                , details =
                    [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange (replacement checkInfo) ]


{-| In your specific situation, the given arg will always be returned unchanged.

Use `alwaysReturnsLastArgError` when the last arg could be absent and it would still not change, like with `List.map identity`.

-}
returnsArgError :
    String
    ->
        { argRepresents : String
        , arg : Node Expression
        }
    -> QualifyResources { a | fnRange : Range, parentRange : Range }
    -> Error {}
returnsArgError usingSituation config checkInfo =
    Rule.errorWithFix
        { message = usingSituation ++ " will always return the same given " ++ config.argRepresents
        , details =
            [ "You can replace this call by the " ++ config.argRepresents ++ " itself." ]
        }
        checkInfo.fnRange
        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range config.arg })


{-| `ErrorInfoAndFix` for when a specific composition is equivalent to identity, e.g. `Just >> Maybe.withDefault x`.
-}
compositionAlwaysReturnsIncomingError : String -> CompositionIntoCheckInfo -> ErrorInfoAndFix
compositionAlwaysReturnsIncomingError message checkInfo =
    if checkInfo.isEmbeddedInComposition then
        { info =
            { message = message
            , details = [ "You can remove these two functions." ]
            }
        , fix =
            [ Fix.removeRange checkInfo.earlier.removeRange
            , Fix.removeRange checkInfo.later.removeRange
            ]
        }

    else
        { info =
            { message = message
            , details = [ "You can replace this composition by identity." ]
            }
        , fix = compositionReplaceByFnFix Fn.Basics.identity checkInfo
        }


{-| Use in combination with
`findMapNeighboring` where finding returns the element's node.

Works for patterns and expressions.

-}
listLiteralRemoveElementFix : { before : Maybe (Node element), found : Node element, after : Maybe (Node element) } -> List Fix
listLiteralRemoveElementFix toRemove =
    case ( toRemove.before, toRemove.after ) of
        -- found the only element
        ( Nothing, Nothing ) ->
            [ Fix.removeRange (Node.range toRemove.found) ]

        -- found first element
        ( Nothing, Just (Node afterRange _) ) ->
            [ Fix.removeRange
                { start = (Node.range toRemove.found).start
                , end = afterRange.start
                }
            ]

        -- found after first element
        ( Just (Node beforeRange _), _ ) ->
            [ Fix.removeRange
                { start = beforeRange.end
                , end = (Node.range toRemove.found).end
                }
            ]


{-| Use in combination with
`findMapNeighboring` where finding returns the element's node.

Works for patterns and expressions.

-}
collapsedConsRemoveElementFix :
    { toRemove : { before : Maybe (Node element), after : Maybe (Node element), found : Node element }
    , tailRange : Range
    }
    -> List Fix
collapsedConsRemoveElementFix config =
    case ( config.toRemove.before, config.toRemove.after ) of
        -- found the only consed element
        ( Nothing, Nothing ) ->
            [ Fix.removeRange
                { start = (Node.range config.toRemove.found).start, end = config.tailRange.start }
            ]

        -- found first consed element
        ( Nothing, Just (Node afterRange _) ) ->
            [ Fix.removeRange
                { start = (Node.range config.toRemove.found).start
                , end = afterRange.start
                }
            ]

        -- found after first consed element
        ( Just (Node beforeRange _), _ ) ->
            [ Fix.removeRange
                { start = beforeRange.end
                , end = (Node.range config.toRemove.found).end
                }
            ]


{-| Detect and provide removing fixes for a function that will return a specific construct

    \a -> construct b
    --> \a -> b

    \a -> if cond then construct b else construct c -- and other branching
    --> \a -> if cond then b else c

    f << construct -- nd other compositions where construct is last
    --> f

-}
constructsOrComposesInto :
    { constructWithOneValue
        | fn : ( ModuleName, String )
        , getValue : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
    }
    -> ModuleNameLookupTable
    -> Node Expression
    -> Maybe (List Fix)
constructsOrComposesInto constructWithOneValue lookupTable expressionNode =
    findMap (\f -> f ())
        [ \() ->
            expressionNode
                |> constructs
                    (sameInAllBranches (\expr -> getValueWithNodeRange (constructWithOneValue.getValue lookupTable) expr))
                    lookupTable
                |> Maybe.map
                    (\wrapCalls ->
                        List.concatMap (\call -> replaceBySubExpressionFix call.nodeRange call.value) wrapCalls
                    )
        , \() ->
            expressionNode
                |> getCompositionToLast
                |> Maybe.andThen
                    (\compositionToLast ->
                        compositionToLast.last
                            |> AstHelpers.getSpecificValueOrFn constructWithOneValue.fn lookupTable
                            |> Maybe.map
                                (\_ ->
                                    [ Fix.removeRange
                                        (andBetweenRange { included = Node.range compositionToLast.last, excluded = Node.range compositionToLast.earlier })
                                    ]
                                )
                    )
        ]



-- STRING


wrapInBackticks : String -> String
wrapInBackticks s =
    "`" ++ s ++ "`"



-- PARSERS


{-| Get the last function in `earlier` and the earliest function in `later` that's not itself a composition.

E.g. for `(i << h) << (g << f)`

    getInnerComposition { earlier = (g << f), later = (i << h) }
    --> { earlier = g, later = h }

which works for nested parens and any combination of `>>` and `<<`.

The returned `removeEarlier/LaterRange` can be used together with `Fix.removeRange` to only remove one side of the composition.
The returned `isEmbeddedInComposition` is true if there are other functions composed before `earlier` or after `later`.

-}
getInnerComposition :
    { compositionInfo
        | earlier : Node Expression
        , later : Node Expression
    }
    ->
        { earlier :
            { node : Node Expression
            , removeRange : Range
            }
        , later :
            { node : Node Expression
            , removeRange : Range
            }
        , isEmbeddedInComposition : Bool
        }
getInnerComposition compositionInfo =
    let
        laterAsComposition : Maybe { earliest : Node Expression, later : Node Expression }
        laterAsComposition =
            getCompositionFromEarliest compositionInfo.later

        earlierAsComposition : Maybe { earlier : Node Expression, last : Node Expression }
        earlierAsComposition =
            getCompositionToLast compositionInfo.earlier
    in
    { earlier =
        case earlierAsComposition of
            Just earlier ->
                { node = earlier.last
                , removeRange =
                    andBetweenRange { included = Node.range earlier.last, excluded = Node.range earlier.earlier }
                }

            Nothing ->
                { node = compositionInfo.earlier
                , removeRange =
                    andBetweenRange { included = Node.range compositionInfo.earlier, excluded = Node.range compositionInfo.later }
                }
    , later =
        case laterAsComposition of
            Just later ->
                { node = later.earliest
                , removeRange =
                    andBetweenRange { included = Node.range later.earliest, excluded = Node.range later.later }
                }

            Nothing ->
                { node = compositionInfo.later
                , removeRange =
                    andBetweenRange { included = Node.range compositionInfo.later, excluded = Node.range compositionInfo.earlier }
                }
    , isEmbeddedInComposition =
        isJust earlierAsComposition || isJust laterAsComposition
    }


{-| The function applied later than all the others in a composition chain and the function directly before.

E.g. `(f << g) << h` would return `Just { earlier = g, last = f }`

-}
getCompositionToLast : Node Expression -> Maybe { earlier : Node Expression, last : Node Expression }
getCompositionToLast expressionNode =
    case getFullComposition expressionNode of
        Just fullComposition ->
            case getCompositionToLast fullComposition.composedLater of
                Just actualLast ->
                    Just actualLast

                Nothing ->
                    Just { earlier = fullComposition.earlier, last = fullComposition.composedLater }

        Nothing ->
            Nothing


{-| The function applied earlier than all the others in a composition chain and the function directly before.

E.g. `f << (g << h)` would return `Just { later = g, earliest = h }`

-}
getCompositionFromEarliest : Node Expression -> Maybe { earliest : Node Expression, later : Node Expression }
getCompositionFromEarliest expressionNode =
    case getFullComposition expressionNode of
        Just fullComposition ->
            case getCompositionFromEarliest fullComposition.earlier of
                Just actualEarlier ->
                    Just actualEarlier

                Nothing ->
                    Just { earliest = fullComposition.earlier, later = fullComposition.composedLater }

        Nothing ->
            Nothing


{-| Unlike `AstHelpers.getComposition` which only looks at the earliest 2 composed functions, e.g. `f << g` for `f << g << h`.
`getFullComposition` returns the later part as an expression, e.g. `{ earlier = f, composedLater = g << h }`.
-}
getFullComposition : Node Expression -> Maybe { earlier : Node Expression, composedLater : Node Expression }
getFullComposition expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.OperatorApplication "<<" _ composedLater earlier ->
            Just { earlier = earlier, composedLater = composedLater }

        Expression.OperatorApplication ">>" _ earlier composedLater ->
            Just { earlier = earlier, composedLater = composedLater }

        _ ->
            Nothing


needsParens : Expression -> Bool
needsParens expr =
    case expr of
        Expression.Application _ ->
            True

        Expression.OperatorApplication _ _ _ _ ->
            True

        Expression.IfBlock _ _ _ ->
            True

        Expression.Negation _ ->
            True

        Expression.LetExpression _ ->
            True

        Expression.CaseExpression _ ->
            True

        Expression.LambdaExpression _ ->
            True

        Expression.UnitExpr ->
            False

        Expression.CharLiteral _ ->
            False

        Expression.Integer _ ->
            False

        Expression.Hex _ ->
            False

        Expression.Floatable _ ->
            False

        Expression.Literal _ ->
            False

        Expression.GLSLExpression _ ->
            False

        Expression.PrefixOperator _ ->
            False

        Expression.RecordAccessFunction _ ->
            False

        Expression.RecordAccess _ _ ->
            False

        Expression.FunctionOrValue _ _ ->
            False

        Expression.ParenthesizedExpression _ ->
            False

        Expression.TupledExpression _ ->
            False

        Expression.ListExpr _ ->
            False

        Expression.RecordExpr _ ->
            False

        Expression.RecordUpdateExpression _ _ ->
            False

        -- IMPOSSIBLE --
        Expression.Operator _ ->
            False


{-| Take one argument and return a value that matches a given parser.
-}
constructs :
    (Node Expression -> Maybe specific)
    -> ModuleNameLookupTable
    -> Node Expression
    -> Maybe specific
constructs getSpecific lookupTable expressionNode =
    case AstHelpers.getSpecificFnCall Fn.Basics.always lookupTable expressionNode of
        Just alwaysCall ->
            getSpecific alwaysCall.firstArg

        Nothing ->
            case Node.value (AstHelpers.removeParens expressionNode) of
                Expression.LambdaExpression lambda ->
                    case lambda.args of
                        _ :: [] ->
                            getSpecific lambda.expression

                        _ ->
                            Nothing

                _ ->
                    Nothing


sameInAllBranches :
    (Node Expression -> Maybe info)
    -> Node Expression
    -> Maybe (List info)
sameInAllBranches getSpecific baseExpressionNode =
    case getSpecific baseExpressionNode of
        Just specific ->
            Just [ specific ]

        Nothing ->
            case Node.value (AstHelpers.removeParens baseExpressionNode) of
                Expression.LetExpression letIn ->
                    sameInAllBranches getSpecific letIn.expression

                Expression.IfBlock _ thenBranch elseBranch ->
                    traverse
                        (\branchExpression -> sameInAllBranches getSpecific branchExpression)
                        [ thenBranch, elseBranch ]
                        |> Maybe.map List.concat

                Expression.CaseExpression caseOf ->
                    traverse
                        (\( _, caseExpression ) -> sameInAllBranches getSpecific caseExpression)
                        caseOf.cases
                        |> Maybe.map List.concat

                _ ->
                    Nothing


getComparableExpression : Node Expression -> Maybe (List Expression)
getComparableExpression =
    getComparableExpressionHelper 1


getComparableExpressionHelper : Int -> Node Expression -> Maybe (List Expression)
getComparableExpressionHelper sign (Node _ expression) =
    case expression of
        Expression.Integer int ->
            Just [ Expression.Integer (sign * int) ]

        Expression.Hex hex ->
            Just [ Expression.Integer (sign * hex) ]

        Expression.Floatable float ->
            Just [ Expression.Floatable (toFloat sign * float) ]

        Expression.Negation expr ->
            getComparableExpressionHelper (-1 * sign) expr

        Expression.Literal string ->
            Just [ Expression.Literal string ]

        Expression.CharLiteral char ->
            Just [ Expression.CharLiteral char ]

        Expression.ParenthesizedExpression expr ->
            getComparableExpressionHelper 1 expr

        Expression.TupledExpression exprs ->
            exprs
                |> traverse (getComparableExpressionHelper 1)
                |> Maybe.map List.concat

        Expression.ListExpr exprs ->
            exprs
                |> traverse (getComparableExpressionHelper 1)
                |> Maybe.map List.concat

        _ ->
            Nothing



-- LIST HELPERS


listLast : List a -> Maybe a
listLast list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just (listFilledLast ( head, tail ))


listFilledFromList : List a -> Maybe ( a, List a )
listFilledFromList list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            Just ( head, tail )


listFilledLength : ( a, List a ) -> Int
listFilledLength ( _, tail ) =
    1 + List.length tail


listFilledToList : ( a, List a ) -> List a
listFilledToList ( head, tail ) =
    head :: tail


listFilledHead : ( a, List a ) -> a
listFilledHead ( head, _ ) =
    head


listFilledTail : ( a, List a ) -> List a
listFilledTail ( _, tail ) =
    tail


listFilledLast : ( a, List a ) -> a
listFilledLast ( head, tail ) =
    case tail of
        [] ->
            head

        tailHead :: tailTail ->
            listFilledLast ( tailHead, tailTail )


listFilledInit : ( a, List a ) -> List a
listFilledInit ( head, tail ) =
    case tail of
        [] ->
            []

        tailHead :: tailTail ->
            head :: listFilledInit ( tailHead, tailTail )


listFilledMap : (a -> b) -> ( a, List a ) -> ( b, List b )
listFilledMap elementChange ( head, tail ) =
    ( elementChange head, List.map elementChange tail )


findMap : (a -> Maybe b) -> List a -> Maybe b
findMap mapper nodes =
    case nodes of
        [] ->
            Nothing

        node :: rest ->
            case mapper node of
                Just value ->
                    Just value

                Nothing ->
                    findMap mapper rest


firstThatConstructsJust : List (a -> Maybe b) -> a -> Maybe b
firstThatConstructsJust remainingChecks data =
    findMap (\checkFn -> checkFn data) remainingChecks


findMapNeighboringAfter : Maybe a -> (a -> Maybe b) -> List a -> Maybe { before : Maybe a, found : b, after : Maybe a }
findMapNeighboringAfter before tryMap list =
    case list of
        [] ->
            Nothing

        now :: after ->
            case tryMap now of
                Just found ->
                    Just { before = before, found = found, after = after |> List.head }

                Nothing ->
                    findMapNeighboringAfter (Just now) tryMap after


findMapNeighboring : (a -> Maybe b) -> List a -> Maybe { before : Maybe a, found : b, after : Maybe a }
findMapNeighboring tryMap list =
    findMapNeighboringAfter Nothing tryMap list


findMapAndAllBefore : (a -> Maybe b) -> List a -> Maybe { before : List a, found : b }
findMapAndAllBefore tryMap list =
    foldUntilOkFrom []
        (\el beforeReversed ->
            case tryMap el of
                Nothing ->
                    Err (el :: beforeReversed)

                Just found ->
                    Ok
                        { found = found
                        , before = List.reverse beforeReversed
                        }
        )
        list
        |> Result.toMaybe


{-| A fold that can stop early (→ `Ok`) instead of traversing the whole list.

    [ 4, 8, -1, 2 ]
        -- take from the right while not negative
        |> foldUntilOkFrom []
            (\n beforeReversed ->
                if n < 0 then
                    Ok (List.reverse beforeReversed) -- stop the fold
                else
                    Err (n :: beforeReversed)
            )
        |> Result.map
    --> [ 4, 8 ]

-}
foldUntilOkFrom : folded -> (a -> folded -> Result folded b) -> List a -> Result folded b
foldUntilOkFrom initialFolded mapOrFoldFurther list =
    case list of
        [] ->
            Err initialFolded

        head :: tail ->
            case mapOrFoldFurther head initialFolded of
                Ok found ->
                    Ok found

                Err newFolded ->
                    foldUntilOkFrom newFolded mapOrFoldFurther tail


traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f list =
    traverseHelp f list []


traverseHelp : (a -> Maybe b) -> List a -> List b -> Maybe (List b)
traverseHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                Just a ->
                    traverseHelp f tail (a :: acc)

                Nothing ->
                    Nothing

        [] ->
            Just (List.reverse acc)


unique : List a -> List a
unique list =
    uniqueBy identity list


uniqueBy : (a -> b) -> List a -> List a
uniqueBy toAspectThatShouldBeUnique list =
    uniqueByHelp toAspectThatShouldBeUnique [] list []


uniqueByHelp : (a -> b) -> List b -> List a -> List a -> List a
uniqueByHelp toAspectThatShouldBeUnique existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                firstAspect : b
                firstAspect =
                    toAspectThatShouldBeUnique first
            in
            if List.member firstAspect existing then
                uniqueByHelp toAspectThatShouldBeUnique existing rest accumulator

            else
                uniqueByHelp toAspectThatShouldBeUnique (firstAspect :: existing) rest (first :: accumulator)



-- MAYBE HELPERS


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Nothing ->
            True

        Just _ ->
            False


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Nothing ->
            []

        Just content ->
            [ content ]
