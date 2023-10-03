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


### Basics functions

    identity x
    --> x

    f >> identity
    --> f

    always x y
    --> x

    f >> always x
    --> always x


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
    --> []

    String.lines ""
    --> []

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

    List.product []
    --> 1

    List.product [ a ]
    --> a

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

    List.any f []
    --> True

    List.any (always False) list
    --> False

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

    List.take 0 list
    --> []

    List.drop 0 list
    --> list

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

    Array.get n Array.empty
    --> Nothing

    Array.get 1 (Array.fromList [ a, b, c ])
    --> Just b

    Array.get 100 (Array.fromList [ a, b, c ])
    --> Nothing

    Array.get -1 array
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


### Sets

    Set.fromList []
    --> Set.empty

    Set.fromList [ a ]
    --> Set.singleton a

    Set.fromList (Set.toList set)
    --> set

    Set.map f Set.empty -- same for Set.filter, Set.remove...
    --> Set.empty

    Set.map identity set
    --> set

    Set.isEmpty Set.empty
    --> True

    Set.member x Set.empty
    --> False

    Set.toList Set.empty
    --> []

    Set.length Set.empty
    --> 0

    Set.intersect Set.empty set
    --> Set.empty

    Set.diff Set.empty set
    --> Set.empty

    Set.diff set Set.empty
    --> set

    Set.union set Set.empty
    --> set

    Set.union (Set.fromList [ a, b ]) (Set.fromList [ c, d ])
    --> Set.fromList [ a, b, c, d]

    Set.insert x Set.empty
    --> Set.singleton x

    -- same for foldr
    List.foldl f x (Set.toList set)
    --> Set.foldl f x set

    Set.partition f Set.empty
    --> ( Set.empty, Set.empty )

    Set.partition (always True) set
    --> ( set, Set.empty )

    -- The following simplifications for Set.foldl also work for Set.foldr
    Set.foldl f initial Set.empty
    --> initial

    Set.foldl (\_ soFar -> soFar) initial set
    --> initial


### Dict

    Dict.fromList []
    --> Dict.empty

    Dict.fromList (Dict.toList dict)
    --> dict

    Dict.isEmpty Dict.empty
    --> True

    Dict.toList Dict.empty
    --> []

    Dict.size Dict.empty
    --> 0

    Dict.member x Dict.empty
    --> False

    Dict.intersect Dict.empty dict
    --> Dict.empty

    Dict.diff Dict.empty dict
    --> Dict.empty

    Dict.diff dict Dict.empty
    --> dict

    Dict.union dict Dict.empty
    --> dict

    Dict.union (Dict.fromList [ a, b ]) (Dict.fromList [ c, d ])
    --> Dict.fromList [ c, d, a, b ]

    Dict.partition f Dict.empty
    --> ( Dict.empty, Dict.empty )

    Dict.partition (always True) dict
    --> ( dict, Dict.empty )

    Dict.partition (always False) dict
    --> ( Dict.empty, dict )

    List.map Tuple.first (Dict.toList dict)
    --> Dict.keys dict

    List.map Tuple.second (Dict.toList dict)
    --> Dict.values dict


### Cmd / Sub

All of these also apply for `Sub`.

    Cmd.batch []
    --> Cmd.none

    Cmd.batch [ a ]
    --> a

    Cmd.batch [ a, Cmd.none, b ]
    --> Cmd.batch [ a, b ]

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
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Simplify.AstHelpers as AstHelpers exposing (emptyStringAsString, qualifiedToString)
import Simplify.Evaluate as Evaluate
import Simplify.Infer as Infer
import Simplify.Match as Match exposing (Match(..))
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


{-| Configuration for this rule. Create a new one with [`defaults`](#defaults) and use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) to alter it.
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
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : ModuleName
    , exposedVariantTypes : Exposed
    , commentRanges : List Range
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
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleContext ->
            { customTypesToReportInCases = Set.empty
            , exposedVariants =
                Dict.singleton moduleContext.moduleName
                    moduleContext.exposedVariants
            }
        )


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable metadata extractSourceCode fullAst projectContext ->
            let
                moduleExposedVariantTypes : Exposed
                moduleExposedVariantTypes =
                    moduleExposingContext fullAst.moduleDefinition

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
            , exposedVariantTypes = moduleExposedVariantTypes
            , importLookup =
                createImportLookup
                    { imports = imports
                    , importExposedVariants = projectContext.exposedVariants
                    }
            , commentRanges = []
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


moduleExposingContext : Node Elm.Syntax.Module.Module -> Exposed
moduleExposingContext moduleHeader =
    case Elm.Syntax.Module.exposingList (Node.value moduleHeader) of
        Exposing.All _ ->
            ExposedAll

        Exposing.Explicit some ->
            ExposedSome
                (List.foldl
                    (\(Node _ expose) acc ->
                        case AstHelpers.getTypeExposeIncludingVariants expose of
                            Just name ->
                                Set.insert name acc

                            Nothing ->
                                acc
                    )
                    Set.empty
                    some
                )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { customTypesToReportInCases = Set.empty
    , exposedVariants = Dict.union newContext.exposedVariants previousContext.exposedVariants
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
    in
    ( if List.isEmpty unknownTypesToIgnore then
        []

      else
        [ errorForUnknownIgnoredConstructor unknownTypesToIgnore ]
    , { customTypesToReportInCases = customTypesToReportInCases
      , exposedVariants = dependencyExposedVariants
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
    }



-- DECLARATION VISITOR


declarationVisitor : Node Declaration -> ModuleContext -> ModuleContext
declarationVisitor declarationNode context =
    case Node.value declarationNode of
        Declaration.CustomTypeDeclaration variantType ->
            let
                variantTypeName : String
                variantTypeName =
                    Node.value variantType.name
            in
            if isExposedFrom context.exposedVariantTypes variantTypeName then
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
            -> CheckInfo
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

                    _ ->
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
            let
                dotFieldRange : Range
                dotFieldRange =
                    { start = (Node.range record).end, end = fieldRange.end }

                maybeErrorInfoAndFix : Maybe ErrorInfoAndFix
                maybeErrorInfoAndFix =
                    case Node.value (AstHelpers.removeParens record) of
                        Expression.RecordExpr setters ->
                            recordAccessChecks
                                { nodeRange = expressionRange
                                , maybeRecordNameRange = Nothing
                                , fieldName = fieldName
                                , setters = setters
                                }

                        Expression.RecordUpdateExpression (Node recordNameRange _) setters ->
                            recordAccessChecks
                                { nodeRange = expressionRange
                                , maybeRecordNameRange = Just recordNameRange
                                , fieldName = fieldName
                                , setters = setters
                                }

                        Expression.LetExpression letIn ->
                            Just (injectRecordAccessIntoLetExpression dotFieldRange letIn.expression fieldName)

                        Expression.IfBlock _ thenBranch elseBranch ->
                            distributeFieldAccess "an if/then/else" dotFieldRange [ thenBranch, elseBranch ] fieldName

                        Expression.CaseExpression caseOf ->
                            distributeFieldAccess "a case/of" dotFieldRange (List.map Tuple.second caseOf.cases) fieldName

                        _ ->
                            Nothing
            in
            onlyMaybeError
                (maybeErrorInfoAndFix
                    |> Maybe.map (\e -> Rule.errorWithFix e.info dotFieldRange e.fix)
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


type alias CheckInfo =
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


secondArg : CheckInfo -> Maybe (Node Expression)
secondArg checkInfo =
    checkInfo.secondArg


thirdArg : CheckInfo -> Maybe (Node Expression)
thirdArg checkInfo =
    checkInfo.thirdArg


type alias ErrorInfoAndFix =
    { info : { message : String, details : List String }
    , fix : List Fix
    }


functionCallChecks : Dict ( ModuleName, String ) ( Int, CheckInfo -> Maybe (Error {}) )
functionCallChecks =
    -- The number of arguments is used to determine how many arguments to pass to the check function.
    -- This corresponds to the number of arguments that the function to check is expected to have.
    -- Any additional arguments will be ignored in order to avoid removing too many arguments
    -- when replacing the entire argument, which is quite common.
    Dict.fromList
        [ ( ( [ "Basics" ], "identity" ), ( 1, basicsIdentityChecks ) )
        , ( ( [ "Basics" ], "always" ), ( 2, basicsAlwaysChecks ) )
        , ( ( [ "Basics" ], "not" ), ( 1, basicsNotChecks ) )
        , ( ( [ "Basics" ], "negate" ), ( 1, basicsNegateChecks ) )
        , ( ( [ "Tuple" ], "first" ), ( 1, tupleFirstChecks ) )
        , ( ( [ "Tuple" ], "second" ), ( 1, tupleSecondChecks ) )
        , ( ( [ "Tuple" ], "pair" ), ( 2, tuplePairChecks ) )
        , ( ( [ "Maybe" ], "map" ), ( 2, maybeMapChecks ) )
        , ( ( [ "Maybe" ], "map2" ), ( 3, maybeMapNChecks ) )
        , ( ( [ "Maybe" ], "map3" ), ( 4, maybeMapNChecks ) )
        , ( ( [ "Maybe" ], "map4" ), ( 5, maybeMapNChecks ) )
        , ( ( [ "Maybe" ], "map5" ), ( 6, maybeMapNChecks ) )
        , ( ( [ "Maybe" ], "andThen" ), ( 2, maybeAndThenChecks ) )
        , ( ( [ "Maybe" ], "withDefault" ), ( 2, withDefaultChecks maybeWithJustAsWrap ) )
        , ( ( [ "Result" ], "map" ), ( 2, resultMapChecks ) )
        , ( ( [ "Result" ], "map2" ), ( 3, resultMapNChecks ) )
        , ( ( [ "Result" ], "map3" ), ( 4, resultMapNChecks ) )
        , ( ( [ "Result" ], "map4" ), ( 5, resultMapNChecks ) )
        , ( ( [ "Result" ], "map5" ), ( 6, resultMapNChecks ) )
        , ( ( [ "Result" ], "mapError" ), ( 2, resultMapErrorChecks ) )
        , ( ( [ "Result" ], "andThen" ), ( 2, resultAndThenChecks ) )
        , ( ( [ "Result" ], "withDefault" ), ( 2, withDefaultChecks resultWithOkAsWrap ) )
        , ( ( [ "Result" ], "toMaybe" ), ( 1, unwrapToMaybeChecks resultWithOkAsWrap ) )
        , ( ( [ "Result" ], "fromMaybe" ), ( 2, resultFromMaybeChecks ) )
        , ( ( [ "List" ], "append" ), ( 2, collectionUnionChecks listCollection ) )
        , ( ( [ "List" ], "head" ), ( 1, listHeadChecks ) )
        , ( ( [ "List" ], "tail" ), ( 1, listTailChecks ) )
        , ( ( [ "List" ], "member" ), ( 2, listMemberChecks ) )
        , ( ( [ "List" ], "map" ), ( 2, listMapChecks ) )
        , ( ( [ "List" ], "filter" ), ( 2, emptiableFilterChecks listCollection ) )
        , ( ( [ "List" ], "filterMap" ), ( 2, listFilterMapChecks ) )
        , ( ( [ "List" ], "concat" ), ( 1, listConcatChecks ) )
        , ( ( [ "List" ], "concatMap" ), ( 2, listConcatMapChecks ) )
        , ( ( [ "List" ], "indexedMap" ), ( 2, listIndexedMapChecks ) )
        , ( ( [ "List" ], "intersperse" ), ( 2, listIntersperseChecks ) )
        , ( ( [ "List" ], "sum" ), ( 1, listSumChecks ) )
        , ( ( [ "List" ], "product" ), ( 1, listProductChecks ) )
        , ( ( [ "List" ], "minimum" ), ( 1, listMinimumChecks ) )
        , ( ( [ "List" ], "maximum" ), ( 1, listMaximumChecks ) )
        , ( ( [ "List" ], "foldl" ), ( 3, listFoldlChecks ) )
        , ( ( [ "List" ], "foldr" ), ( 3, listFoldrChecks ) )
        , ( ( [ "List" ], "all" ), ( 2, listAllChecks ) )
        , ( ( [ "List" ], "any" ), ( 2, listAnyChecks ) )
        , ( ( [ "List" ], "range" ), ( 2, listRangeChecks ) )
        , ( ( [ "List" ], "length" ), ( 1, collectionSizeChecks listCollection ) )
        , ( ( [ "List" ], "repeat" ), ( 2, listRepeatChecks ) )
        , ( ( [ "List" ], "isEmpty" ), ( 1, collectionIsEmptyChecks listCollection ) )
        , ( ( [ "List" ], "partition" ), ( 2, collectionPartitionChecks listCollection ) )
        , ( ( [ "List" ], "reverse" ), ( 1, listReverseChecks ) )
        , ( ( [ "List" ], "sort" ), ( 1, listSortChecks ) )
        , ( ( [ "List" ], "sortBy" ), ( 2, listSortByChecks ) )
        , ( ( [ "List" ], "sortWith" ), ( 2, listSortWithChecks ) )
        , ( ( [ "List" ], "take" ), ( 2, listTakeChecks ) )
        , ( ( [ "List" ], "drop" ), ( 2, listDropChecks ) )
        , ( ( [ "List" ], "map2" ), ( 3, emptiableMapNChecks listCollection ) )
        , ( ( [ "List" ], "map3" ), ( 4, emptiableMapNChecks listCollection ) )
        , ( ( [ "List" ], "map4" ), ( 5, emptiableMapNChecks listCollection ) )
        , ( ( [ "List" ], "map5" ), ( 6, emptiableMapNChecks listCollection ) )
        , ( ( [ "List" ], "unzip" ), ( 1, listUnzipChecks ) )
        , ( ( [ "Array" ], "toList" ), ( 1, arrayToListChecks ) )
        , ( ( [ "Array" ], "toIndexedList" ), ( 1, arrayToIndexedListChecks ) )
        , ( ( [ "Array" ], "fromList" ), ( 1, arrayFromListChecks ) )
        , ( ( [ "Array" ], "map" ), ( 2, emptiableMapChecks arrayCollection ) )
        , ( ( [ "Array" ], "indexedMap" ), ( 2, arrayIndexedMapChecks ) )
        , ( ( [ "Array" ], "filter" ), ( 2, emptiableFilterChecks arrayCollection ) )
        , ( ( [ "Array" ], "isEmpty" ), ( 1, collectionIsEmptyChecks arrayCollection ) )
        , ( ( [ "Array" ], "length" ), ( 1, arrayLengthChecks ) )
        , ( ( [ "Array" ], "repeat" ), ( 2, arrayRepeatChecks ) )
        , ( ( [ "Array" ], "initialize" ), ( 2, arrayInitializeChecks ) )
        , ( ( [ "Array" ], "append" ), ( 2, collectionUnionChecks arrayCollection ) )
        , ( ( [ "Array" ], "get" ), ( 2, getChecks arrayCollection ) )
        , ( ( [ "Array" ], "set" ), ( 3, setChecks arrayCollection ) )
        , ( ( [ "Array" ], "foldl" ), ( 3, arrayFoldlChecks ) )
        , ( ( [ "Array" ], "foldr" ), ( 3, arrayFoldrChecks ) )
        , ( ( [ "Set" ], "map" ), ( 2, emptiableMapChecks setCollection ) )
        , ( ( [ "Set" ], "filter" ), ( 2, emptiableFilterChecks setCollection ) )
        , ( ( [ "Set" ], "remove" ), ( 2, collectionRemoveChecks setCollection ) )
        , ( ( [ "Set" ], "isEmpty" ), ( 1, collectionIsEmptyChecks setCollection ) )
        , ( ( [ "Set" ], "size" ), ( 1, collectionSizeChecks setCollection ) )
        , ( ( [ "Set" ], "member" ), ( 2, collectionMemberChecks setCollection ) )
        , ( ( [ "Set" ], "fromList" ), ( 1, setFromListChecks ) )
        , ( ( [ "Set" ], "toList" ), ( 1, emptiableToListChecks setCollection ) )
        , ( ( [ "Set" ], "partition" ), ( 2, collectionPartitionChecks setCollection ) )
        , ( ( [ "Set" ], "intersect" ), ( 2, collectionIntersectChecks setCollection ) )
        , ( ( [ "Set" ], "diff" ), ( 2, collectionDiffChecks setCollection ) )
        , ( ( [ "Set" ], "union" ), ( 2, collectionUnionChecks setCollection ) )
        , ( ( [ "Set" ], "insert" ), ( 2, collectionInsertChecks setCollection ) )
        , ( ( [ "Set" ], "foldl" ), ( 3, setFoldlChecks ) )
        , ( ( [ "Set" ], "foldr" ), ( 3, setFoldrChecks ) )
        , ( ( [ "Dict" ], "isEmpty" ), ( 1, collectionIsEmptyChecks dictCollection ) )
        , ( ( [ "Dict" ], "fromList" ), ( 1, dictFromListChecks ) )
        , ( ( [ "Dict" ], "toList" ), ( 1, emptiableToListChecks dictCollection ) )
        , ( ( [ "Dict" ], "size" ), ( 1, collectionSizeChecks dictCollection ) )
        , ( ( [ "Dict" ], "member" ), ( 2, collectionMemberChecks dictCollection ) )
        , ( ( [ "Dict" ], "partition" ), ( 2, collectionPartitionChecks dictCollection ) )
        , ( ( [ "Dict" ], "intersect" ), ( 2, collectionIntersectChecks dictCollection ) )
        , ( ( [ "Dict" ], "diff" ), ( 2, collectionDiffChecks dictCollection ) )
        , ( ( [ "Dict" ], "union" ), ( 2, collectionUnionChecks dictCollection ) )
        , ( ( [ "String" ], "toList" ), ( 1, stringToListChecks ) )
        , ( ( [ "String" ], "fromList" ), ( 1, stringFromListChecks ) )
        , ( ( [ "String" ], "isEmpty" ), ( 1, collectionIsEmptyChecks stringCollection ) )
        , ( ( [ "String" ], "concat" ), ( 1, stringConcatChecks ) )
        , ( ( [ "String" ], "join" ), ( 2, stringJoinChecks ) )
        , ( ( [ "String" ], "length" ), ( 1, collectionSizeChecks stringCollection ) )
        , ( ( [ "String" ], "repeat" ), ( 2, stringRepeatChecks ) )
        , ( ( [ "String" ], "replace" ), ( 3, stringReplaceChecks ) )
        , ( ( [ "String" ], "words" ), ( 1, stringWordsChecks ) )
        , ( ( [ "String" ], "lines" ), ( 1, stringLinesChecks ) )
        , ( ( [ "String" ], "reverse" ), ( 1, stringReverseChecks ) )
        , ( ( [ "String" ], "slice" ), ( 3, stringSliceChecks ) )
        , ( ( [ "String" ], "left" ), ( 2, stringLeftChecks ) )
        , ( ( [ "String" ], "right" ), ( 2, stringRightChecks ) )
        , ( ( [ "String" ], "append" ), ( 2, collectionUnionChecks stringCollection ) )
        , ( ( [ "String" ], "foldl" ), ( 3, stringFoldlChecks ) )
        , ( ( [ "String" ], "foldr" ), ( 3, stringFoldrChecks ) )
        , ( ( [ "Platform", "Cmd" ], "batch" ), ( 1, subAndCmdBatchChecks cmdCollection ) )
        , ( ( [ "Platform", "Cmd" ], "map" ), ( 2, emptiableMapChecks cmdCollection ) )
        , ( ( [ "Platform", "Sub" ], "batch" ), ( 1, subAndCmdBatchChecks subCollection ) )
        , ( ( [ "Platform", "Sub" ], "map" ), ( 2, emptiableMapChecks subCollection ) )
        , ( ( [ "Task" ], "map" ), ( 2, taskMapChecks ) )
        , ( ( [ "Task" ], "map2" ), ( 3, taskMapNChecks ) )
        , ( ( [ "Task" ], "map3" ), ( 4, taskMapNChecks ) )
        , ( ( [ "Task" ], "map4" ), ( 5, taskMapNChecks ) )
        , ( ( [ "Task" ], "map5" ), ( 6, taskMapNChecks ) )
        , ( ( [ "Task" ], "andThen" ), ( 2, taskAndThenChecks ) )
        , ( ( [ "Task" ], "mapError" ), ( 2, taskMapErrorChecks ) )
        , ( ( [ "Task" ], "onError" ), ( 2, taskOnErrorChecks ) )
        , ( ( [ "Task" ], "sequence" ), ( 1, taskSequenceChecks ) )
        , ( ( [ "Json", "Decode" ], "oneOf" ), ( 1, oneOfChecks ) )
        , ( ( [ "Json", "Decode" ], "map" ), ( 2, jsonDecodeMapChecks ) )
        , ( ( [ "Json", "Decode" ], "map2" ), ( 3, jsonDecodeMapNChecks ) )
        , ( ( [ "Json", "Decode" ], "map3" ), ( 4, jsonDecodeMapNChecks ) )
        , ( ( [ "Json", "Decode" ], "map4" ), ( 5, jsonDecodeMapNChecks ) )
        , ( ( [ "Json", "Decode" ], "map5" ), ( 6, jsonDecodeMapNChecks ) )
        , ( ( [ "Json", "Decode" ], "map6" ), ( 7, jsonDecodeMapNChecks ) )
        , ( ( [ "Json", "Decode" ], "map7" ), ( 8, jsonDecodeMapNChecks ) )
        , ( ( [ "Json", "Decode" ], "map8" ), ( 9, jsonDecodeMapNChecks ) )
        , ( ( [ "Json", "Decode" ], "andThen" ), ( 2, jsonDecodeAndThenChecks ) )
        , ( ( [ "Html", "Attributes" ], "classList" ), ( 1, htmlAttributesClassListChecks ) )
        , ( ( [ "Parser" ], "oneOf" ), ( 1, oneOfChecks ) )
        , ( ( [ "Parser", "Advanced" ], "oneOf" ), ( 1, oneOfChecks ) )
        , ( ( [ "Random" ], "uniform" ), ( 2, randomUniformChecks ) )
        , ( ( [ "Random" ], "weighted" ), ( 2, randomWeightedChecks ) )
        , ( ( [ "Random" ], "list" ), ( 2, randomListChecks ) )
        , ( ( [ "Random" ], "map" ), ( 2, randomMapChecks ) )
        , ( ( [ "Random" ], "andThen" ), ( 2, randomAndThenChecks ) )
        ]


type alias OperatorCheckInfo =
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


operatorApplicationChecks : Dict String (OperatorCheckInfo -> Maybe (Error {}))
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


type alias CompositionCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , importLookup : ImportLookup
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
    [ basicsIdentityCompositionChecks
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


compositionIntoChecks : Dict ( ModuleName, String ) ( Int, CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix )
compositionIntoChecks =
    Dict.fromList
        [ ( ( [ "Basics" ], "always" ), ( 2, basicsAlwaysCompositionChecks ) )
        , ( ( [ "Basics" ], "not" ), ( 1, toggleCompositionChecks ) )
        , ( ( [ "Basics" ], "negate" ), ( 1, toggleCompositionChecks ) )
        , ( ( [ "String" ], "reverse" ), ( 1, stringReverseCompositionChecks ) )
        , ( ( [ "String" ], "fromList" ), ( 1, stringFromListCompositionChecks ) )
        , ( ( [ "String" ], "toList" ), ( 1, stringToListCompositionChecks ) )
        , ( ( [ "String" ], "concat" ), ( 1, stringConcatCompositionChecks ) )
        , ( ( [ "Tuple" ], "first" ), ( 1, tupleFirstCompositionChecks ) )
        , ( ( [ "Tuple" ], "second" ), ( 1, tupleSecondCompositionChecks ) )
        , ( ( [ "Maybe" ], "map" ), ( 2, maybeMapCompositionChecks ) )
        , ( ( [ "Maybe" ], "withDefault" ), ( 2, wrapperWithDefaultChecks maybeWithJustAsWrap ) )
        , ( ( [ "Result" ], "map" ), ( 2, resultMapCompositionChecks ) )
        , ( ( [ "Result" ], "mapError" ), ( 2, resultMapErrorCompositionChecks ) )
        , ( ( [ "Result" ], "toMaybe" ), ( 1, resultToMaybeCompositionChecks ) )
        , ( ( [ "Result" ], "fromMaybe" ), ( 3, wrapperFromMaybeCompositionChecks resultWithOkAsWrap ) )
        , ( ( [ "Result" ], "withDefault" ), ( 2, wrapperWithDefaultChecks resultWithOkAsWrap ) )
        , ( ( [ "List" ], "reverse" ), ( 1, listReverseCompositionChecks ) )
        , ( ( [ "List" ], "sort" ), ( 1, listSortCompositionChecks ) )
        , ( ( [ "List" ], "sortBy" ), ( 2, listSortByCompositionChecks ) )
        , ( ( [ "List" ], "map" ), ( 2, listMapCompositionChecks ) )
        , ( ( [ "List" ], "filterMap" ), ( 2, listFilterMapCompositionChecks ) )
        , ( ( [ "List" ], "intersperse" ), ( 2, listIntersperseCompositionChecks ) )
        , ( ( [ "List" ], "concat" ), ( 1, listConcatCompositionChecks ) )
        , ( ( [ "List" ], "sum" ), ( 1, sumCompositionChecks listCollection ) )
        , ( ( [ "List" ], "product" ), ( 1, productCompositionChecks listCollection ) )
        , ( ( [ "List" ], "minimum" ), ( 1, minimumCompositionChecks listCollection ) )
        , ( ( [ "List" ], "maximum" ), ( 1, maximumCompositionChecks listCollection ) )
        , ( ( [ "List" ], "foldl" ), ( 3, listFoldlCompositionChecks ) )
        , ( ( [ "List" ], "foldr" ), ( 3, listFoldrCompositionChecks ) )
        , ( ( [ "Set" ], "fromList" ), ( 1, setFromListCompositionChecks ) )
        , ( ( [ "Dict" ], "fromList" ), ( 1, dictFromListCompositionChecks ) )
        , ( ( [ "Array" ], "toList" ), ( 1, arrayToListCompositionChecks ) )
        , ( ( [ "Array" ], "fromList" ), ( 1, arrayFromListCompositionChecks ) )
        , ( ( [ "Task" ], "map" ), ( 2, taskMapCompositionChecks ) )
        , ( ( [ "Task" ], "mapError" ), ( 2, taskMapErrorCompositionChecks ) )
        , ( ( [ "Task" ], "sequence" ), ( 1, taskSequenceCompositionChecks ) )
        , ( ( [ "Platform", "Cmd" ], "batch" ), ( 1, batchCompositionChecks ) )
        , ( ( [ "Platform", "Sub" ], "batch" ), ( 1, batchCompositionChecks ) )
        , ( ( [ "Json", "Decode" ], "map" ), ( 2, jsonDecodeMapCompositionChecks ) )
        , ( ( [ "Random" ], "map" ), ( 2, randomMapCompositionChecks ) )
        ]


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


plusChecks : OperatorCheckInfo -> Maybe (Error {})
plusChecks =
    firstThatConstructsJust
        [ addingZeroCheck
        , addingOppositesCheck
        ]


addingZeroCheck : OperatorCheckInfo -> Maybe (Error {})
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


addingOppositesCheck : OperatorCheckInfo -> Maybe (Error {})
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


minusChecks : OperatorCheckInfo -> Maybe (Error {})
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


checkIfMinusResultsInZero : OperatorCheckInfo -> Maybe (Error {})
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


multiplyChecks : OperatorCheckInfo -> Maybe (Error {})
multiplyChecks checkInfo =
    findMap
        (\side ->
            case AstHelpers.getUncomputedNumberValue side.node of
                Just number ->
                    if number == 1 then
                        Just
                            (Rule.errorWithFix
                                { message = "Unnecessary multiplying by 1"
                                , details = [ "You can replace this operation by the " ++ side.otherDescription ++ " number you multiplied by 0." ]
                                }
                                (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
                            )

                    else if number == 0 then
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

                Nothing ->
                    Nothing
        )
        (operationSides checkInfo)


operationSides : OperatorCheckInfo -> List { node : Node Expression, otherNode : Node Expression, otherDescription : String }
operationSides checkInfo =
    [ { node = checkInfo.left, otherNode = checkInfo.right, otherDescription = "right" }
    , { node = checkInfo.right, otherNode = checkInfo.left, otherDescription = "left" }
    ]


divisionChecks : OperatorCheckInfo -> Maybe (Error {})
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


intDivideChecks : OperatorCheckInfo -> Maybe (Error {})
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


plusplusChecks : OperatorCheckInfo -> Maybe (Error {})
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
                { first = checkInfo.left
                , second = checkInfo.right
                , operationRange = checkInfo.operatorRange
                , operation = "++"
                }
                listCollection
                checkInfo
        , \checkInfo ->
            collectionUnionWithLiteralsChecks
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
    -> EmptiableProperties otherProperties
    -> OperatorCheckInfo
    -> Maybe (Error {})
appendEmptyCheck side collection checkInfo =
    if collection.empty.is checkInfo.lookupTable side.node then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary appending " ++ collection.empty.asString defaultQualifyResources
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


consChecks : OperatorCheckInfo -> Maybe (Error {})
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


toggleCallChecks : CheckInfo -> Maybe (Error {})
toggleCallChecks checkInfo =
    onCallToInverseReturnsItsArgumentCheck checkInfo.fn checkInfo


toggleCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
toggleCompositionChecks checkInfo =
    inversesCompositionCheck checkInfo.later.fn checkInfo


{-| Chaining two operations that are inverses of each other and therefore cancel each other out.
For example

    Array.fromList (Array.toList array)
    --> array

    Array.toList (Array.fromList list)
    --> list

Tip: Add `inversesCompositionCheck` for the same thing as a composition check.

These usually exist in pairs, like above so make sure to add this check for both functions.
But there are exceptions!

    Set.fromList (Set.toList set)
    --> set

This will always work because `Set.toList` will never produce a list with duplicate elements. However

    Set.toList (Set.fromList list)
    --> list

would be an incorrect fix. See for example

    Set.toList (Set.fromList [ 0, 0 ])
    --> not [ 0, 0 ] bit actually [ 0 ]

-}
onCallToInverseReturnsItsArgumentCheck : ( ModuleName, String ) -> CheckInfo -> Maybe (Error {})
onCallToInverseReturnsItsArgumentCheck inverseFn checkInfo =
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


{-| Composing two operations that are inverses of each other and therefore cancel each other out.
For example

    -- inversesCompositionCheck { later = ( [ "Array" ], "fromList" ), earlier = ( [ "Array" ], "toList" ) }
    Array.toList >> Array.fromList
    --> identity

    -- inversesCompositionCheck { later = ( [ "Array" ], "toList" ) , earlier = ( [ "Array" ], "fromList" ) }
    Array.fromList >> Array.toList
    --> identity

Tip: Add `onCallToInverseReturnsItsArgumentCheck` for the same thing as a function call check.

These usually exist in pairs, like above so make sure to add this check for both functions.
But there are exceptions!

    Set.fromList << Set.toList --> identity

This will always work because `Set.toList` will never produce a list with duplicate elements. However

    Set.toList << Set.fromList --> identity

would be an incorrect fix. See for example

    Set.toList (Set.fromList [ 0, 0 ])
    --> not [ 0, 0 ] bit actually [ 0 ]

-}
inversesCompositionCheck : ( ModuleName, String ) -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
inversesCompositionCheck earlierInverseFn checkInfo =
    if checkInfo.earlier.fn == earlierInverseFn then
        Just
            (compositionAlwaysReturnsIncomingError
                (qualifiedToString checkInfo.earlier.fn ++ ", then " ++ qualifiedToString checkInfo.later.fn ++ " cancels each other out")
                checkInfo
            )

    else
        Nothing


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
        , fix = compositionReplaceByFnFix ( [ "Basics" ], "identity" ) checkInfo
        }


compositionReplaceByFnFix :
    ( ModuleName, String )
    -> QualifyResources { checkInfo | later : { later | range : Range }, earlier : { earlier | removeRange : Range } }
    -> List Fix
compositionReplaceByFnFix replacementFn checkInfo =
    compositionReplaceByFix (qualifiedToString (qualify replacementFn checkInfo)) checkInfo


compositionReplaceByFix :
    String
    -> { checkInfo | later : { later | range : Range }, earlier : { earlier | removeRange : Range } }
    -> List Fix
compositionReplaceByFix replacement checkInfo =
    [ Fix.replaceRangeBy checkInfo.later.range replacement
    , Fix.removeRange checkInfo.earlier.removeRange
    ]


{-| Get the last function in `earlier` and the earliest function in `later` that's not itself a composition.

E.g. for `(i << h) << (g << f)`

    getInnerComposition { earlier = (g << f), later = (i << h) }
    --> { earlier = g, later = h }

which works for nested parens ans any combination of `>>` and `<<`.

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



-- EQUALITY


equalityChecks : Bool -> OperatorCheckInfo -> Maybe (Error {})
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
                ( AstHelpers.getSpecificFnCall ( [ "Basics" ], "not" ) checkInfo.lookupTable checkInfo.left
                , AstHelpers.getSpecificFnCall ( [ "Basics" ], "not" ) checkInfo.lookupTable checkInfo.right
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


numberComparisonChecks : (Float -> Float -> Bool) -> OperatorCheckInfo -> Maybe (Error {})
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



-- BASICS


basicsIdentityChecks : CheckInfo -> Maybe (Error {})
basicsIdentityChecks checkInfo =
    Just
        (Rule.errorWithFix
            { message = "`identity` should be removed"
            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
            }
            checkInfo.fnRange
            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
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


basicsAlwaysChecks : CheckInfo -> Maybe (Error {})
basicsAlwaysChecks checkInfo =
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


basicsAlwaysCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
basicsAlwaysCompositionChecks checkInfo =
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


basicsNegateChecks : CheckInfo -> Maybe (Error {})
basicsNegateChecks =
    toggleCallChecks


basicsNotChecks : CheckInfo -> Maybe (Error {})
basicsNotChecks =
    firstThatConstructsJust
        [ notOnKnownBoolCheck
        , toggleCallChecks
        , isNotOnBooleanOperatorCheck
        ]


notOnKnownBoolCheck : CheckInfo -> Maybe (Error {})
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


isNotOnBooleanOperatorCheck : CheckInfo -> Maybe (Error {})
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


orChecks : OperatorCheckInfo -> Maybe (Error {})
orChecks =
    firstThatConstructsJust
        [ \checkInfo -> findMap (\side -> orSideChecks side checkInfo) (operationSides checkInfo)
        , findSimilarConditionsError
        ]


type RedundantConditionResolution
    = RemoveFrom Location
    | ReplaceByNoop Bool


findSimilarConditionsError : OperatorCheckInfo -> Maybe (Error {})
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


orSideChecks : { side | node : Node Expression, otherNode : Node Expression, otherDescription : String } -> OperatorCheckInfo -> Maybe (Error {})
orSideChecks side checkInfo =
    case Evaluate.getBoolean checkInfo side.node of
        Determined True ->
            Just
                (Rule.errorWithFix
                    { message = "(||) with any side being True will result in True"
                    , details =
                        [ "You can replace this operation by True."
                        , "Maybe you have hardcoded a value or mistyped a condition?"
                        ]
                    }
                    checkInfo.parentRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.node })
                )

        Determined False ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary check for || False"
                    , details = [ "You can replace this operation by the " ++ side.otherDescription ++ " bool." ]
                    }
                    (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
                )

        Undetermined ->
            Nothing


andChecks : OperatorCheckInfo -> Maybe (Error {})
andChecks =
    firstThatConstructsJust
        [ \checkInfo -> findMap (\side -> andSideChecks side checkInfo) (operationSides checkInfo)
        , findSimilarConditionsError
        ]


andSideChecks : { side | node : Node Expression, otherNode : Node Expression, otherDescription : String } -> OperatorCheckInfo -> Maybe (Error {})
andSideChecks side checkInfo =
    case Evaluate.getBoolean checkInfo side.node of
        Determined True ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary check for && True"
                    , details = [ "You can replace this operation by the " ++ side.otherDescription ++ " bool." ]
                    }
                    (Range.combine [ checkInfo.operatorRange, Node.range side.node ])
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.otherNode })
                )

        Determined False ->
            Just
                (Rule.errorWithFix
                    { message = "(&&) with any side being False will result in False"
                    , details =
                        [ "You can replace this operation by False."
                        , "Maybe you have hardcoded a value or mistyped a condition?"
                        ]
                    }
                    checkInfo.parentRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range side.node })
                )

        Undetermined ->
            Nothing



-- TUPLE


tuplePairChecks : CheckInfo -> Maybe (Error {})
tuplePairChecks checkInfo =
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


tupleFirstChecks : CheckInfo -> Maybe (Error {})
tupleFirstChecks =
    tuplePartChecks
        { part = TupleFirst
        , description = "first"
        , mapUnrelatedFn = ( [ "Tuple" ], "mapSecond" )
        , mapFn = ( [ "Tuple" ], "mapFirst" )
        }


tupleFirstCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
tupleFirstCompositionChecks =
    firstThatConstructsJust
        [ tuplePartCompositionChecks
            { part = TupleFirst
            , description = "first"
            , mapUnrelatedFn = ( [ "Tuple" ], "mapSecond" )
            , mapFn = ( [ "Tuple" ], "mapFirst" )
            }
        , \checkInfo ->
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
                                        (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo) ++ " ")
                                   , Fix.removeRange checkInfo.later.removeRange
                                   ]
                        }

                _ ->
                    Nothing
        ]


tupleSecondChecks : CheckInfo -> Maybe (Error {})
tupleSecondChecks =
    tuplePartChecks
        { part = TupleSecond
        , description = "second"
        , mapFn = ( [ "Tuple" ], "mapSecond" )
        , mapUnrelatedFn = ( [ "Tuple" ], "mapFirst" )
        }


tupleSecondCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
tupleSecondCompositionChecks =
    firstThatConstructsJust
        [ tuplePartCompositionChecks
            { part = TupleSecond
            , description = "second"
            , mapFn = ( [ "Tuple" ], "mapSecond" )
            , mapUnrelatedFn = ( [ "Tuple" ], "mapFirst" )
            }
        , \checkInfo ->
            case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                ( ( [ "Tuple" ], "pair" ), _ :: [] ) ->
                    Just
                        (compositionAlwaysReturnsIncomingError
                            (qualifiedToString (qualify checkInfo.earlier.fn defaultQualifyResources) ++ " with a first part, then " ++ qualifiedToString (qualify checkInfo.later.fn defaultQualifyResources) ++ " will always result in the incoming second part")
                            checkInfo
                        )

                _ ->
                    Nothing
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
    -> CheckInfo
    -> Maybe (Error {})
tuplePartChecks partConfig =
    firstThatConstructsJust
        [ \checkInfo ->
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
                (AstHelpers.getTuple2 checkInfo.firstArg checkInfo.lookupTable)
        , \checkInfo ->
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
        , \checkInfo ->
            case AstHelpers.getSpecificFnCall ( [ "Tuple" ], "mapBoth" ) checkInfo.lookupTable checkInfo.firstArg of
                Just tupleMapBothCall ->
                    case tupleMapBothCall.argsAfterFirst of
                        secondMapperArg :: _ :: [] ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString ( [ "Tuple" ], "mapBoth" ) ++ " before " ++ qualifiedToString checkInfo.fn ++ " is the same as " ++ qualifiedToString partConfig.mapFn
                                    , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the " ++ qualifiedToString ( [ "Tuple" ], "mapBoth" ) ++ " call by " ++ qualifiedToString partConfig.mapFn ++ " with the same " ++ partConfig.description ++ " mapping and tuple." ]
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
        ]


tuplePartCompositionChecks :
    { part : TuplePart, description : String, mapFn : ( ModuleName, String ), mapUnrelatedFn : ( ModuleName, String ) }
    -> CompositionIntoCheckInfo
    -> Maybe ErrorInfoAndFix
tuplePartCompositionChecks partConfig =
    firstThatConstructsJust
        [ \checkInfo ->
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
        , \checkInfo ->
            case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                ( ( [ "Tuple" ], "mapBoth" ), firstMapperArg :: _ :: [] ) ->
                    Just
                        { info =
                            { message = qualifiedToString ( [ "Tuple" ], "mapBoth" ) ++ " before " ++ qualifiedToString checkInfo.later.fn ++ " is the same as " ++ qualifiedToString partConfig.mapFn
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the " ++ qualifiedToString ( [ "Tuple" ], "mapBoth" ) ++ " call by " ++ qualifiedToString partConfig.mapFn ++ " with the same " ++ partConfig.description ++ " mapping." ]
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
        ]



-- STRING


stringToListChecks : CheckInfo -> Maybe (Error {})
stringToListChecks =
    onCallToInverseReturnsItsArgumentCheck ( [ "String" ], "fromList" )


stringToListCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
stringToListCompositionChecks =
    inversesCompositionCheck ( [ "String" ], "fromList" )


stringFromListChecks : CheckInfo -> Maybe (Error {})
stringFromListChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = stringCollection.empty.asString } listCollection
        , wrapperFromListSingletonChecks stringCollection
        , onCallToInverseReturnsItsArgumentCheck ( [ "String" ], "toList" )
        ]


stringFromListCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
stringFromListCompositionChecks =
    firstThatConstructsJust
        [ wrapperFromListSingletonCompositionChecks stringCollection
        , inversesCompositionCheck ( [ "String" ], "toList" )
        ]


stringConcatChecks : CheckInfo -> Maybe (Error {})
stringConcatChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = stringCollection.empty.asString } listCollection
        , callFromCanBeCombinedCheck { fromFn = ( [ "List" ], "repeat" ), combinedFn = ( [ "String" ], "repeat" ) }
        , callFromCanBeCombinedCheck { fromFn = ( [ "List" ], "intersperse" ), combinedFn = ( [ "String" ], "join" ) }
        ]


stringConcatCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
stringConcatCompositionChecks =
    firstThatConstructsJust
        [ compositionFromCanBeCombinedCheck { fromFn = ( [ "List" ], "repeat" ), combinedFn = ( [ "String" ], "repeat" ) }
        , compositionFromCanBeCombinedCheck { fromFn = ( [ "List" ], "intersperse" ), combinedFn = ( [ "String" ], "join" ) }
        ]


stringWordsChecks : CheckInfo -> Maybe (Error {})
stringWordsChecks =
    callOnEmptyReturnsCheck { resultAsString = listCollection.empty.asString } stringCollection


stringLinesChecks : CheckInfo -> Maybe (Error {})
stringLinesChecks =
    callOnEmptyReturnsCheck { resultAsString = listCollection.empty.asString } stringCollection


stringReverseChecks : CheckInfo -> Maybe (Error {})
stringReverseChecks =
    firstThatConstructsJust
        [ emptiableReverseChecks stringCollection
        , callOnWrappedDoesNotChangeItCheck stringCollection
        ]


stringReverseCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
stringReverseCompositionChecks =
    firstThatConstructsJust
        [ compositionAfterWrapIsUnnecessaryCheck stringCollection
        , toggleCompositionChecks
        ]


stringSliceChecks : CheckInfo -> Maybe (Error {})
stringSliceChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck stringCollection
        , \checkInfo ->
            case secondArg checkInfo of
                Just endArg ->
                    firstThatConstructsJust
                        [ \() ->
                            if Normalize.areAllTheSame checkInfo checkInfo.firstArg [ endArg ] then
                                Just
                                    (alwaysResultsInUnparenthesizedConstantError "String.slice with equal start and end index"
                                        { replacement = \_ -> emptyStringAsString }
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
                                                        (alwaysResultsInUnparenthesizedConstantError "String.slice with end index 0"
                                                            { replacement = \_ -> emptyStringAsString }
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
                                                                (alwaysResultsInUnparenthesizedConstantError "String.slice with a start index greater than the end index"
                                                                    { replacement = \_ -> emptyStringAsString }
                                                                    checkInfo
                                                                )

                                                        else if startInt <= -1 && endInt <= -1 then
                                                            Just
                                                                (alwaysResultsInUnparenthesizedConstantError "String.slice with a negative start index closer to the right than the negative end index"
                                                                    { replacement = \_ -> emptyStringAsString }
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
        ]


stringLeftChecks : CheckInfo -> Maybe (Error {})
stringLeftChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck stringCollection
        , \checkInfo ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just length ->
                    callWithNonPositiveIntCanBeReplacedByCheck
                        { int = length
                        , intDescription = "length"
                        , replacement = \_ -> emptyStringAsString
                        }
                        checkInfo

                Nothing ->
                    Nothing
        ]


callWithNonPositiveIntCanBeReplacedByCheck :
    { int : number
    , intDescription : String
    , replacement : QualifyResources {} -> String
    }
    -> CheckInfo
    -> Maybe (Error {})
callWithNonPositiveIntCanBeReplacedByCheck config checkInfo =
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
            (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with " ++ lengthDescription)
                { replacement = config.replacement }
                checkInfo
            )

    else
        Nothing


stringRightChecks : CheckInfo -> Maybe (Error {})
stringRightChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck stringCollection
        , \checkInfo ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just length ->
                    callWithNonPositiveIntCanBeReplacedByCheck
                        { int = length
                        , intDescription = "length"
                        , replacement = \_ -> emptyStringAsString
                        }
                        checkInfo

                Nothing ->
                    Nothing
        ]


stringJoinChecks : CheckInfo -> Maybe (Error {})
stringJoinChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = stringCollection.empty.asString } listCollection
        , \checkInfo ->
            if stringCollection.empty.is checkInfo.lookupTable checkInfo.firstArg then
                let
                    replacementFn : ( ModuleName, String )
                    replacementFn =
                        ( [ "String" ], "concat" )
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString checkInfo.fn ++ " with separator \"\" is the same as " ++ qualifiedToString replacementFn
                        , details = [ "You can replace this call by " ++ qualifiedToString replacementFn ++ "." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end }
                            (qualifiedToString (qualify replacementFn checkInfo))
                        ]
                    )

            else
                Nothing
        ]


stringRepeatChecks : CheckInfo -> Maybe (Error {})
stringRepeatChecks =
    firstThatConstructsJust
        [ \checkInfo ->
            case secondArg checkInfo of
                Just stringArg ->
                    if stringCollection.empty.is checkInfo.lookupTable stringArg then
                        Just
                            (Rule.errorWithFix
                                { message = "String.repeat with " ++ emptyStringAsString ++ " will result in " ++ emptyStringAsString
                                , details = [ "You can replace this call by " ++ emptyStringAsString ++ "." ]
                                }
                                checkInfo.fnRange
                                [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
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
                                        (alwaysReturnsLastArgError "String.repeat 1"
                                            { represents = "string to repeat" }
                                            checkInfo
                                        )

                                _ ->
                                    Nothing
                        , \() ->
                            callWithNonPositiveIntCanBeReplacedByCheck
                                { int = intValue
                                , intDescription = "length"
                                , replacement = \_ -> emptyStringAsString
                                }
                                checkInfo
                        ]
                        ()

                _ ->
                    Nothing
        ]


stringReplaceChecks : CheckInfo -> Maybe (Error {})
stringReplaceChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck stringCollection
        , \checkInfo ->
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
                                            { represents = "string" }
                                            checkInfo
                                        )

                                _ ->
                                    Nothing
                        ]
                        ()

                Nothing ->
                    Nothing
        ]


stringFoldlChecks : CheckInfo -> Maybe (Error {})
stringFoldlChecks =
    emptiableFoldChecks stringCollection


stringFoldrChecks : CheckInfo -> Maybe (Error {})
stringFoldrChecks =
    emptiableFoldChecks stringCollection



-- MAYBE FUNCTIONS


maybeMapChecks : CheckInfo -> Maybe (Error {})
maybeMapChecks =
    firstThatConstructsJust
        [ emptiableMapChecks maybeWithJustAsWrap
        , mapWrapChecks maybeWithJustAsWrap
        ]


maybeMapCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
maybeMapCompositionChecks =
    wrapToMapCompositionChecks maybeWithJustAsWrap


maybeMapNChecks : CheckInfo -> Maybe (Error {})
maybeMapNChecks =
    firstThatConstructsJust
        [ wrapperMapNChecks maybeWithJustAsWrap
        , emptiableMapNChecks maybeWithJustAsWrap
        ]



-- RESULT FUNCTIONS


resultMapChecks : CheckInfo -> Maybe (Error {})
resultMapChecks =
    firstThatConstructsJust
        [ emptiableMapChecks resultWithOkAsWrap
        , mapWrapChecks resultWithOkAsWrap
        ]


resultMapCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
resultMapCompositionChecks =
    wrapToMapCompositionChecks resultWithOkAsWrap


resultMapNChecks : CheckInfo -> Maybe (Error {})
resultMapNChecks =
    firstThatConstructsJust
        [ wrapperMapNChecks resultWithOkAsWrap
        , mapNOrFirstEmptyConstructionChecks resultWithOkAsWrap
        ]


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
    { message = qualifiedToString mapFn ++ " on " ++ descriptionForIndefinite wrapper.wrap.description ++ " will result in " ++ wrapFnInErrorInfo ++ " with the function applied to the value inside"
    , details = [ "You can replace this call by " ++ wrapFnInErrorInfo ++ " with the function directly applied to the value inside " ++ descriptionForDefinite "the" wrapper.wrap.description ++ " itself." ]
    }


resultMapErrorChecks : CheckInfo -> Maybe (Error {})
resultMapErrorChecks =
    firstThatConstructsJust
        [ emptiableMapChecks resultWithErrAsWrap
        , mapWrapChecks resultWithErrAsWrap
        ]


resultMapErrorCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
resultMapErrorCompositionChecks =
    firstThatConstructsJust
        [ wrapToMapCompositionChecks resultWithErrAsWrap
        , unnecessaryCompositionAfterEmptyCheck resultWithErrAsWrap
        ]



-- LIST FUNCTIONS


listConcatChecks : CheckInfo -> Maybe (Error {})
listConcatChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck listCollection
        , callOnWrapReturnsItsValue listCollection
        , callOnListWithIrrelevantEmptyElement listCollection
        , \checkInfo ->
            case Node.value checkInfo.firstArg of
                Expression.ListExpr list ->
                    case list of
                        firstListElement :: restOfListElements ->
                            firstThatConstructsJust
                                [ \() ->
                                    case traverse AstHelpers.getListLiteral list of
                                        Just _ ->
                                            Just
                                                (Rule.errorWithFix
                                                    { message = "Expression could be simplified to be a single List"
                                                    , details = [ "Try moving all the elements into a single list." ]
                                                    }
                                                    checkInfo.fnRange
                                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg }
                                                        ++ List.concatMap removeBoundariesFix (firstListElement :: restOfListElements)
                                                    )
                                                )

                                        Nothing ->
                                            Nothing
                                , \() ->
                                    case findConsecutiveListLiterals firstListElement restOfListElements of
                                        firstFix :: fixesAfterFirst ->
                                            Just
                                                (Rule.errorWithFix
                                                    { message = "Consecutive literal lists can be merged"
                                                    , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                                                    }
                                                    checkInfo.fnRange
                                                    (firstFix :: fixesAfterFirst)
                                                )

                                        [] ->
                                            Nothing
                                ]
                                ()

                        _ ->
                            Nothing

                _ ->
                    Nothing
        , callFromCanBeCombinedCheck
            { fromFn = ( [ "List" ], "map" ), combinedFn = ( [ "List" ], "concatMap" ) }
        ]


listConcatCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listConcatCompositionChecks =
    firstThatConstructsJust
        [ compositionFromCanBeCombinedCheck
            { fromFn = ( [ "List" ], "map" ), combinedFn = ( [ "List" ], "concatMap" ) }
        , onWrapAlwaysReturnsIncomingCompositionCheck listCollection
        ]


callOnListWithIrrelevantEmptyElement :
    { otherProperties
        | empty :
            { empty
                | description : Description
                , is : ModuleNameLookupTable -> Node Expression -> Bool
            }
    }
    -> CheckInfo
    -> Maybe (Error {})
callOnListWithIrrelevantEmptyElement emptiableElement checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just listArg ->
            case AstHelpers.getListLiteral listArg of
                Just list ->
                    case findMapNeighboring (getEmpty checkInfo.lookupTable emptiableElement) list of
                        Just emptyLiteralAndNeighbors ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on a list containing an irrelevant " ++ descriptionWithoutArticle emptiableElement.empty.description
                                    , details = [ "Including " ++ descriptionForDefinite "the" emptiableElement.empty.description ++ " in the list does not change the result of this call. You can remove the " ++ descriptionWithoutArticle emptiableElement.empty.description ++ " element." ]
                                    }
                                    emptyLiteralAndNeighbors.found.range
                                    (listLiteralElementRemoveFix emptyLiteralAndNeighbors)
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


findConsecutiveListLiterals : Node Expression -> List (Node Expression) -> List Fix
findConsecutiveListLiterals firstListElement restOfListElements =
    case ( firstListElement, restOfListElements ) of
        ( Node firstRange (Expression.ListExpr _), ((Node secondRange (Expression.ListExpr _)) as second) :: rest ) ->
            Fix.replaceRangeBy
                { start = { row = firstRange.end.row, column = firstRange.end.column - 1 }
                , end = { row = secondRange.start.row, column = secondRange.start.column + 1 }
                }
                ", "
                :: findConsecutiveListLiterals second rest

        ( _, x :: xs ) ->
            findConsecutiveListLiterals x xs

        _ ->
            []


listConcatMapChecks : CheckInfo -> Maybe (Error {})
listConcatMapChecks =
    firstThatConstructsJust
        [ operationWithIdentityCanBeReplacedChecks { replacementFn = ( [ "List" ], "concat" ) }
        , emptiableAndThenChecks listCollection
        , wrapperAndThenChecks listCollection
        ]


{-| Turn `yourFn identity` into `replacementFn`. If `replacementFn` should be `identity`, use `alwaysReturnsLastArgError` instead

Can be used to for example

  - turn `traverse identity` into `sequence`
  - turn `List.filterMap identity` into `Maybe.Extra.values`
  - turn `List.Extra.minimumBy identity` into `List.minimum`

-}
operationWithIdentityCanBeReplacedChecks : { replacementFn : ( ModuleName, String ) } -> CheckInfo -> Maybe (Error {})
operationWithIdentityCanBeReplacedChecks config checkInfo =
    if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
        Just
            (Rule.errorWithFix
                { message = qualifiedToString checkInfo.fn ++ " with an identity function is the same as " ++ qualifiedToString config.replacementFn
                , details = [ "You can replace this call by " ++ qualifiedToString config.replacementFn ++ "." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy
                    { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end }
                    (qualifiedToString (qualify config.replacementFn checkInfo))
                ]
            )

    else
        Nothing


listIndexedMapChecks : CheckInfo -> Maybe (Error {})
listIndexedMapChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck listCollection
        , operationWithExtraArgChecks { operationWithoutExtraArg = ( [ "List" ], "map" ) }
        ]


{-| Map where the usual map function has an extra argument with special information.

For example `indexedMap` also supplied an index. Not using the index would be identical to `map`.

Another example would be [`List.Extra.indexedFoldl`](https://package.elm-lang.org/packages/elm-community/list-extra/latest/List-Extra#indexedFoldl) which also supplies the current index.
Not using the path would be identical to `List.foldl`.

-}
operationWithExtraArgChecks : { operationWithoutExtraArg : ( ModuleName, String ) } -> CheckInfo -> Maybe (Error {})
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
            case AstHelpers.getSpecificFnCall ( [ "Basics" ], "always" ) lookupTable expressionNode of
                Just alwaysCall ->
                    Just
                        (replaceBySubExpressionFix alwaysCall.nodeRange alwaysCall.firstArg)

                Nothing ->
                    Nothing


listIntersperseChecks : CheckInfo -> Maybe (Error {})
listIntersperseChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck listCollection
        , callOnWrappedDoesNotChangeItCheck listCollection
        ]


listIntersperseCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listIntersperseCompositionChecks =
    compositionAfterWrapIsUnnecessaryCheck listCollection


listHeadChecks : CheckInfo -> Maybe (Error {})
listHeadChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.asString } listCollection
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
                                    (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                               ]
                        )
                )
                (getListHead checkInfo.lookupTable checkInfo.firstArg)
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


listTailExistsError : List Fix -> CheckInfo -> Error {}
listTailExistsError replaceListArgByTailFix checkInfo =
    Rule.errorWithFix
        { message = qualifiedToString checkInfo.fn ++ " on a list with some elements will result in Just the elements after the first"
        , details = [ "You can replace this call by Just the list elements after the first." ]
        }
        checkInfo.fnRange
        (replaceListArgByTailFix
            ++ [ Fix.replaceRangeBy checkInfo.fnRange
                    (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
               ]
        )


listTailChecks : CheckInfo -> Maybe (Error {})
listTailChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.asString } listCollection
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
                            { message = qualifiedToString checkInfo.fn ++ " on a list with a single element will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy (Node.range checkInfo.firstArg) "[]"
                            , Fix.replaceRangeBy checkInfo.fnRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                            ]
                        )

                Nothing ->
                    Nothing
        ]


listMapChecks : CheckInfo -> Maybe (Error {})
listMapChecks =
    firstThatConstructsJust
        [ emptiableMapChecks listCollection
        , listMapOnSingletonCheck
        , dictToListMapChecks
        , arrayToIndexedListToListMapChecks
        ]


listMapOnSingletonCheck : CheckInfo -> Maybe (Error {})
listMapOnSingletonCheck checkInfo =
    case secondArg checkInfo of
        Just listArg ->
            case sameInAllBranches (getValueWithNodeRange (listCollection.wrap.getValue checkInfo.lookupTable)) listArg of
                Determined wraps ->
                    let
                        mappingArgRange : Range
                        mappingArgRange =
                            Node.range checkInfo.firstArg
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list containing the function directly applied to the value inside the given singleton list." ]
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

                Undetermined ->
                    Nothing

        Nothing ->
            Nothing


dictToListMapErrorInfo : { toEntryAspectList : String, tuplePart : String } -> { message : String, details : List String }
dictToListMapErrorInfo info =
    let
        toEntryAspectListAsQualifiedString : String
        toEntryAspectListAsQualifiedString =
            qualifiedToString ( [ "Dict" ], info.toEntryAspectList )
    in
    { message = qualifiedToString ( [ "Dict" ], "toList" ) ++ ", then " ++ qualifiedToString ( [ "List" ], "map" ) ++ " " ++ qualifiedToString ( [ "Tuple" ], info.tuplePart ) ++ " is the same as " ++ toEntryAspectListAsQualifiedString
    , details = [ "Using " ++ toEntryAspectListAsQualifiedString ++ " directly is meant for this exact purpose and will also be faster." ]
    }


dictToListMapChecks : CheckInfo -> Maybe (Error {})
dictToListMapChecks listMapCheckInfo =
    case secondArg listMapCheckInfo of
        Just listArgument ->
            case AstHelpers.getSpecificFnCall ( [ "Dict" ], "toList" ) listMapCheckInfo.lookupTable listArgument of
                Just dictToListCall ->
                    let
                        error : { toEntryAspectList : String, tuplePart : String } -> Error {}
                        error info =
                            Rule.errorWithFix
                                (dictToListMapErrorInfo info)
                                listMapCheckInfo.fnRange
                                (keepOnlyFix { parentRange = Node.range listArgument, keep = Node.range dictToListCall.firstArg }
                                    ++ [ Fix.replaceRangeBy
                                            (Range.combine [ listMapCheckInfo.fnRange, Node.range listMapCheckInfo.firstArg ])
                                            (qualifiedToString (qualify ( [ "Dict" ], info.toEntryAspectList ) listMapCheckInfo))
                                       ]
                                )
                    in
                    if AstHelpers.isTupleFirstAccess listMapCheckInfo.lookupTable listMapCheckInfo.firstArg then
                        Just (error { tuplePart = "first", toEntryAspectList = "keys" })

                    else if AstHelpers.isTupleSecondAccess listMapCheckInfo.lookupTable listMapCheckInfo.firstArg then
                        Just (error { tuplePart = "second", toEntryAspectList = "values" })

                    else
                        Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


arrayToIndexedListToListMapChecks : CheckInfo -> Maybe (Error {})
arrayToIndexedListToListMapChecks listMapCheckInfo =
    case secondArg listMapCheckInfo of
        Just listArgument ->
            case AstHelpers.getSpecificFnCall ( [ "Array" ], "toIndexedList" ) listMapCheckInfo.lookupTable listArgument of
                Just arrayToIndexedList ->
                    if AstHelpers.isTupleSecondAccess listMapCheckInfo.lookupTable listMapCheckInfo.firstArg then
                        let
                            combinedFn : ( ModuleName, String )
                            combinedFn =
                                ( [ "Array" ], "toList" )
                        in
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString ( [ "Array" ], "toIndexedList" ) ++ ", then " ++ qualifiedToString ( [ "List" ], "map" ) ++ " " ++ qualifiedToString ( [ "Tuple" ], "second" ) ++ " is the same as " ++ qualifiedToString combinedFn
                                , details = [ "You can replace this call by " ++ qualifiedToString combinedFn ++ " on the array given to " ++ qualifiedToString ( [ "Array" ], "toIndexedList" ) ++ " which is meant for this exact purpose and will also be faster." ]
                                }
                                listMapCheckInfo.fnRange
                                (keepOnlyFix { parentRange = Node.range listArgument, keep = Node.range arrayToIndexedList.firstArg }
                                    ++ [ Fix.replaceRangeBy
                                            (Range.combine [ listMapCheckInfo.fnRange, Node.range listMapCheckInfo.firstArg ])
                                            (qualifiedToString (qualify combinedFn listMapCheckInfo))
                                       ]
                                )
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


listMapCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listMapCompositionChecks =
    firstThatConstructsJust
        [ wrapToMapCompositionChecks listCollection
        , dictToListIntoListMapCompositionCheck
        , arrayToIndexedListMapCompositionCheck
        ]


dictToListIntoListMapCompositionCheck : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
dictToListIntoListMapCompositionCheck checkInfo =
    case
        ( ( checkInfo.earlier.fn, checkInfo.earlier.args )
        , checkInfo.later.args
        )
    of
        ( ( ( [ "Dict" ], "toList" ), [] ), elementMappingArg :: [] ) ->
            let
                error : { toEntryAspectList : String, tuplePart : String } -> ErrorInfoAndFix
                error info =
                    { info = dictToListMapErrorInfo info
                    , fix = compositionReplaceByFnFix ( [ "Dict" ], info.toEntryAspectList ) checkInfo
                    }
            in
            if AstHelpers.isTupleFirstAccess checkInfo.lookupTable elementMappingArg then
                Just (error { tuplePart = "first", toEntryAspectList = "keys" })

            else if AstHelpers.isTupleSecondAccess checkInfo.lookupTable elementMappingArg then
                Just (error { tuplePart = "second", toEntryAspectList = "values" })

            else
                Nothing

        _ ->
            Nothing


arrayToIndexedListMapCompositionCheck : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
arrayToIndexedListMapCompositionCheck checkInfo =
    case ( checkInfo.earlier.fn, checkInfo.later.args ) of
        ( ( [ "Array" ], "toIndexedList" ), elementMappingArg :: [] ) ->
            if AstHelpers.isTupleSecondAccess checkInfo.lookupTable elementMappingArg then
                let
                    combinedFn : ( ModuleName, String )
                    combinedFn =
                        ( [ "Array" ], "toList" )
                in
                Just
                    { info =
                        { message = qualifiedToString ( [ "Array" ], "toIndexedList" ) ++ ", then " ++ qualifiedToString ( [ "List" ], "map" ) ++ " " ++ qualifiedToString ( [ "Tuple" ], "second" ) ++ " is the same as " ++ qualifiedToString combinedFn
                        , details = [ "You can replace this composition by " ++ qualifiedToString combinedFn ++ " which is meant for this exact purpose and will also be faster." ]
                        }
                    , fix = compositionReplaceByFnFix combinedFn checkInfo
                    }

            else
                Nothing

        _ ->
            Nothing


listMemberChecks : CheckInfo -> Maybe (Error {})
listMemberChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck
            { resultAsString = \res -> qualifiedToString (qualify ( [ "Basics" ], "False" ) res) }
            listCollection
        , \checkInfo ->
            case secondArg checkInfo of
                Just listArg ->
                    let
                        needleArg : Node Expression
                        needleArg =
                            checkInfo.firstArg

                        needleRange : Range
                        needleRange =
                            Node.range needleArg
                    in
                    firstThatConstructsJust
                        [ \() ->
                            if checkInfo.expectNaN then
                                Nothing

                            else
                                let
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
                                if List.any isNeedle (listKnownElements checkInfo.lookupTable listArg) then
                                    Just
                                        (resultsInConstantError
                                            (qualifiedToString checkInfo.fn ++ " on a list which contains the given element")
                                            (\res -> qualifiedToString (qualify ( [ "Basics" ], "True" ) res))
                                            checkInfo
                                        )

                                else
                                    Nothing
                        , \() ->
                            case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                                Just single ->
                                    let
                                        elementRange : Range
                                        elementRange =
                                            Node.range single.element
                                    in
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " on an list with a single element is the same as directly checking for equality"
                                            , details = [ "You can replace this call by checking whether the member to find and the list element are equal." ]
                                            }
                                            checkInfo.fnRange
                                            (List.concat
                                                [ keepOnlyFix
                                                    { parentRange = checkInfo.parentRange
                                                    , keep = Range.combine [ needleRange, elementRange ]
                                                    }
                                                , [ Fix.replaceRangeBy
                                                        (rangeBetweenExclusive ( needleRange, elementRange ))
                                                        " == "
                                                  ]
                                                , parenthesizeIfNeededFix single.element
                                                ]
                                            )
                                        )

                                Nothing ->
                                    Nothing
                        ]
                        ()

                Nothing ->
                    Nothing
        ]


listKnownElements : ModuleNameLookupTable -> Node Expression -> List (Node Expression)
listKnownElements lookupTable expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.ListExpr (el0 :: el1 :: el2Up) ->
            el0 :: el1 :: el2Up

        Expression.OperatorApplication "::" _ head tail ->
            case AstHelpers.getCollapsedCons tail of
                Nothing ->
                    [ head ]

                Just collapsedCons ->
                    head :: collapsedCons.consed

        _ ->
            case AstHelpers.getListSingleton lookupTable expressionNode of
                Nothing ->
                    []

                Just singletonList ->
                    [ singletonList.element ]


listSumChecks : CheckInfo -> Maybe (Error {})
listSumChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = \_ -> "0" } listCollection
        , callOnWrapReturnsItsValue listCollection
        ]


sumCompositionChecks : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
sumCompositionChecks wrapper =
    onWrapAlwaysReturnsIncomingCompositionCheck wrapper


listProductChecks : CheckInfo -> Maybe (Error {})
listProductChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = \_ -> "1" } listCollection
        , callOnWrapReturnsItsValue listCollection
        ]


productCompositionChecks : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
productCompositionChecks wrapper =
    onWrapAlwaysReturnsIncomingCompositionCheck wrapper


listMinimumChecks : CheckInfo -> Maybe (Error {})
listMinimumChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.asString } listCollection
        , callOnWrapReturnsJustItsValue listCollection
        ]


minimumCompositionChecks : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
minimumCompositionChecks wrapper =
    onWrapAlwaysReturnsJustIncomingCompositionCheck wrapper


listMaximumChecks : CheckInfo -> Maybe (Error {})
listMaximumChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.asString } listCollection
        , callOnWrapReturnsJustItsValue listCollection
        ]


maximumCompositionChecks : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
maximumCompositionChecks wrapper =
    onWrapAlwaysReturnsJustIncomingCompositionCheck wrapper


listFoldlChecks : CheckInfo -> Maybe (Error {})
listFoldlChecks =
    listFoldAnyDirectionChecks


listFoldrChecks : CheckInfo -> Maybe (Error {})
listFoldrChecks =
    listFoldAnyDirectionChecks


listFoldAnyDirectionChecks : CheckInfo -> Maybe (Error {})
listFoldAnyDirectionChecks =
    firstThatConstructsJust
        [ emptiableFoldChecks listCollection
        , \checkInfo ->
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
                                            ++ qualifiedToString (qualify ( [ "Basics" ], "identity" ) checkInfo)
                                        )
                                    ]
                    in
                    firstThatConstructsJust
                        [ \() ->
                            case maybeListArg of
                                Just listArg ->
                                    case AstHelpers.getSpecificFnCall ( [ "Set" ], "toList" ) checkInfo.lookupTable listArg of
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
                        , \() ->
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
        ]


listFoldlCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listFoldlCompositionChecks =
    foldAndSetToListCompositionChecks


listFoldrCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listFoldrCompositionChecks =
    foldAndSetToListCompositionChecks


foldAndSetToListCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
foldAndSetToListCompositionChecks checkInfo =
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


listAllChecks : CheckInfo -> Maybe (Error {})
listAllChecks =
    emptiableAllChecks listCollection


emptiableAllChecks : EmptiableProperties otherProperties -> CheckInfo -> Maybe (Error {})
emptiableAllChecks emptiable =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck
            { resultAsString = \res -> qualifiedToString (qualify ( [ "Basics" ], "True" ) res) }
            emptiable
        , \checkInfo ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined True ->
                    Just
                        (alwaysResultsInUnparenthesizedConstantError
                            (qualifiedToString checkInfo.fn ++ " with a function that will always return True")
                            { replacement = \res -> qualifiedToString (qualify ( [ "Basics" ], "True" ) res) }
                            checkInfo
                        )

                _ ->
                    Nothing
        ]


listAnyChecks : CheckInfo -> Maybe (Error {})
listAnyChecks =
    firstThatConstructsJust
        [ emptiableAnyChecks listCollection
        , \checkInfo ->
            case Evaluate.isEqualToSomethingFunction checkInfo.firstArg of
                Nothing ->
                    Nothing

                Just equatedTo ->
                    let
                        replacementFn : ( ModuleName, String )
                        replacementFn =
                            ( [ "List" ], "member" )
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " with a check for equality with a specific value can be replaced by " ++ qualifiedToString replacementFn ++ " with that value"
                            , details = [ "You can replace this call by " ++ qualifiedToString replacementFn ++ " with the specific value to find which meant for this exact purpose." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify replacementFn checkInfo))
                                :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) equatedTo.something
                            )
                        )
        ]


emptiableAnyChecks : EmptiableProperties otherProperties -> CheckInfo -> Maybe (Error {})
emptiableAnyChecks emptiable =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck
            { resultAsString = \res -> qualifiedToString (qualify ( [ "Basics" ], "False" ) res) }
            emptiable
        , \checkInfo ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined False ->
                    Just
                        (alwaysResultsInUnparenthesizedConstantError
                            (qualifiedToString checkInfo.fn ++ " with a function that will always return False")
                            { replacement = \res -> qualifiedToString (qualify ( [ "Basics" ], "False" ) res) }
                            checkInfo
                        )

                _ ->
                    Nothing
        ]


listFilterMapChecks : CheckInfo -> Maybe (Error {})
listFilterMapChecks =
    firstThatConstructsJust
        [ callOnListWithIrrelevantEmptyElement maybeWithJustAsWrap
        , emptiableWrapperFilterMapChecks listCollection
        , \checkInfo ->
            if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
                case secondArg checkInfo of
                    Just listArg ->
                        case AstHelpers.getListLiteral listArg of
                            Just list ->
                                case
                                    traverse
                                        (AstHelpers.getSpecificFnCall ( [ "Maybe" ], "Just" ) checkInfo.lookupTable)
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

            else
                Nothing
        ]


listFilterMapCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listFilterMapCompositionChecks =
    mapToOperationWithIdentityCanBeCombinedToOperationCompositionChecks { mapFn = ( [ "List" ], "map" ) }


emptiableWrapperFilterMapChecks : EmptiableProperties (WrapperProperties { otherProperties | mapFn : ( ModuleName, String ) }) -> CheckInfo -> Maybe (Error {})
emptiableWrapperFilterMapChecks emptiableWrapper =
    firstThatConstructsJust
        [ \checkInfo ->
            case constructs (sameInAllBranches (AstHelpers.getSpecificFnCall ( [ "Maybe" ], "Just" ) checkInfo.lookupTable)) checkInfo.lookupTable checkInfo.firstArg of
                Determined justCalls ->
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

                Undetermined ->
                    Nothing
        , \checkInfo ->
            case AstHelpers.getSpecificValueOrFn ( [ "Maybe" ], "Just" ) checkInfo.lookupTable checkInfo.firstArg of
                Just _ ->
                    Just
                        (alwaysReturnsLastArgError
                            (qualifiedToString checkInfo.fn ++ " with a function that will always return Just")
                            emptiableWrapper
                            checkInfo
                        )

                Nothing ->
                    Nothing
        , \checkInfo ->
            case constructs (sameInAllBranches (AstHelpers.getSpecificValueOrFn ( [ "Maybe" ], "Nothing" ) checkInfo.lookupTable)) checkInfo.lookupTable checkInfo.firstArg of
                Determined _ ->
                    Just
                        (alwaysResultsInUnparenthesizedConstantError
                            (qualifiedToString checkInfo.fn ++ " with a function that will always return Nothing")
                            { replacement = emptiableWrapper.empty.asString }
                            checkInfo
                        )

                Undetermined ->
                    Nothing
        , mapToOperationWithIdentityCanBeCombinedToOperationChecks { mapFn = emptiableWrapper.mapFn }
        , callOnEmptyReturnsEmptyCheck emptiableWrapper
        ]


mapToOperationWithIdentityCanBeCombinedToOperationChecks : { mapFn : ( ModuleName, String ) } -> CheckInfo -> Maybe (Error {})
mapToOperationWithIdentityCanBeCombinedToOperationChecks config checkInfo =
    case secondArg checkInfo of
        Just mappableArg ->
            if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
                case AstHelpers.getSpecificFnCall config.mapFn checkInfo.lookupTable mappableArg of
                    Just mapCall ->
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString config.mapFn ++ " and " ++ qualifiedToString checkInfo.fn ++ " identity can be combined using " ++ qualifiedToString checkInfo.fn
                                , details = [ qualifiedToString checkInfo.fn ++ " is meant for this exact purpose and will also be faster." ]
                                }
                                checkInfo.fnRange
                                (replaceBySubExpressionFix checkInfo.parentRange mappableArg
                                    ++ [ Fix.replaceRangeBy mapCall.fnRange
                                            (qualifiedToString (qualify checkInfo.fn checkInfo))
                                       ]
                                )
                            )

                    Nothing ->
                        Nothing

            else
                Nothing

        Nothing ->
            Nothing


mapToOperationWithIdentityCanBeCombinedToOperationCompositionChecks : { mapFn : ( ModuleName, String ) } -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
mapToOperationWithIdentityCanBeCombinedToOperationCompositionChecks config checkInfo =
    case checkInfo.later.args of
        elementToMaybeMappingArg :: [] ->
            if AstHelpers.isIdentity checkInfo.lookupTable elementToMaybeMappingArg then
                case ( checkInfo.earlier.fn == config.mapFn, checkInfo.earlier.args ) of
                    ( True, _ :: [] ) ->
                        Just
                            { info =
                                { message = qualifiedToString config.mapFn ++ " and " ++ qualifiedToString checkInfo.later.fn ++ " identity can be combined using " ++ qualifiedToString checkInfo.later.fn
                                , details = [ qualifiedToString checkInfo.later.fn ++ " is meant for this exact purpose and will also be faster." ]
                                }
                            , fix =
                                [ Fix.replaceRangeBy checkInfo.earlier.fnRange
                                    (qualifiedToString (qualify checkInfo.later.fn checkInfo))
                                , Fix.removeRange checkInfo.later.removeRange
                                ]
                            }

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing


{-| Simplify this operation after a given call to `fromFn` into a given `combinedFn`.

Examples:

  - `List.concat (List.map f list) --> List.concatMap f list` (same for sequence+map to traverse etc)
  - `Parser/Decoder/Random/...sequence (List.repeat n x) --> Parser/Decoder/Random/...repeat n x`
  - `String.concat (List.repeat n x) --> String.repeat n x`
  - `Animation.loop (List.repeat n x) --> Animation.repeat n x` using [`mdgriffith/elm-style-animation`](https://package.elm-lang.org/packages/mdgriffith/elm-style-animation/4.0.0/)
  - `FormattedText.concat (List.intersperse s list) --> FormattedText.join s list` using [`NoRedInk/elm-formatted-text-19`](https://package.elm-lang.org/packages/NoRedInk/elm-formatted-text-19/1.0.0/)

Use in combination with `compositionFromCanBeCombinedCheck`

-}
callFromCanBeCombinedCheck :
    { fromFn : ( ModuleName, String ), combinedFn : ( ModuleName, String ) }
    -> CheckInfo
    -> Maybe (Error {})
callFromCanBeCombinedCheck config checkInfo =
    case AstHelpers.getSpecificFnCall config.fromFn checkInfo.lookupTable checkInfo.firstArg of
        Just fromFnCall ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString config.fromFn ++ ", then " ++ qualifiedToString checkInfo.fn ++ " can be combined into " ++ qualifiedToString config.combinedFn
                    , details = [ "You can replace this call by " ++ qualifiedToString config.combinedFn ++ " with the same arguments given to " ++ qualifiedToString config.fromFn ++ " which is meant for this exact purpose." ]
                    }
                    checkInfo.fnRange
                    (Fix.replaceRangeBy
                        fromFnCall.fnRange
                        (qualifiedToString (qualify config.combinedFn checkInfo))
                        :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = fromFnCall.nodeRange }
                    )
                )

        Nothing ->
            Nothing


{-| Simplify this operation after a given call to `fromFn` into a given `combinedFn`,
like `List.concat << List.map f --> List.concatMap f`.

Use in combination with `callFromCanBeCombinedCheck` (where you will also find more examples).

-}
compositionFromCanBeCombinedCheck :
    { fromFn : ( ModuleName, String ), combinedFn : ( ModuleName, String ) }
    -> CompositionIntoCheckInfo
    -> Maybe ErrorInfoAndFix
compositionFromCanBeCombinedCheck config checkInfo =
    if checkInfo.earlier.fn == config.fromFn then
        Just
            { info =
                { message = qualifiedToString config.fromFn ++ ", then " ++ qualifiedToString checkInfo.later.fn ++ " can be combined into " ++ qualifiedToString config.combinedFn
                , details = [ "You can replace this composition by " ++ qualifiedToString config.combinedFn ++ " with the same arguments given to " ++ qualifiedToString config.fromFn ++ " which is meant for this exact purpose." ]
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


listRangeChecks : CheckInfo -> Maybe (Error {})
listRangeChecks checkInfo =
    case secondArg checkInfo of
        Just rangeEndArg ->
            case ( Evaluate.getInt checkInfo checkInfo.firstArg, Evaluate.getInt checkInfo rangeEndArg ) of
                ( Just rangeStartValue, Just rangeEndValue ) ->
                    if rangeStartValue > rangeEndValue then
                        Just
                            (resultsInConstantError
                                (qualifiedToString checkInfo.fn ++ " with a start index greater than the end index")
                                (\_ -> "[]")
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


listRepeatChecks : CheckInfo -> Maybe (Error {})
listRepeatChecks =
    firstThatConstructsJust
        [ emptiableRepeatChecks listCollection
        , wrapperRepeatChecks listCollection
        ]


arrayToListChecks : CheckInfo -> Maybe (Error {})
arrayToListChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = listCollection.empty.asString } arrayCollection
        , onCallToInverseReturnsItsArgumentCheck ( [ "Array" ], "fromList" )
        , callFromCanBeCombinedCheck
            { fromFn = ( [ "Array" ], "repeat" ), combinedFn = ( [ "List" ], "repeat" ) }
        ]


arrayToListCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
arrayToListCompositionChecks =
    firstThatConstructsJust
        [ inversesCompositionCheck ( [ "Array" ], "fromList" )
        , compositionFromCanBeCombinedCheck
            { fromFn = ( [ "Array" ], "repeat" ), combinedFn = ( [ "List" ], "repeat" ) }
        ]


arrayToIndexedListChecks : CheckInfo -> Maybe (Error {})
arrayToIndexedListChecks =
    callOnEmptyReturnsCheck { resultAsString = listCollection.empty.asString } arrayCollection


arrayFromListChecks : CheckInfo -> Maybe (Error {})
arrayFromListChecks =
    firstThatConstructsJust
        [ collectionFromListChecks arrayCollection
        , onCallToInverseReturnsItsArgumentCheck ( [ "Array" ], "toList" )
        ]


arrayFromListCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
arrayFromListCompositionChecks =
    inversesCompositionCheck ( [ "Array" ], "toList" )


arrayRepeatChecks : CheckInfo -> Maybe (Error {})
arrayRepeatChecks =
    emptiableRepeatChecks arrayCollection


arrayInitializeChecks : CheckInfo -> Maybe (Error {})
arrayInitializeChecks =
    emptiableRepeatChecks arrayCollection


arrayIndexedMapChecks : CheckInfo -> Maybe (Error {})
arrayIndexedMapChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck arrayCollection
        , operationWithExtraArgChecks { operationWithoutExtraArg = ( [ "Array" ], "map" ) }
        ]


emptiableRepeatChecks : CollectionProperties (EmptiableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
emptiableRepeatChecks collection checkInfo =
    case Evaluate.getInt checkInfo checkInfo.firstArg of
        Just intValue ->
            callWithNonPositiveIntCanBeReplacedByCheck
                { int = intValue
                , intDescription = collection.size.description
                , replacement = collection.empty.asString
                }
                checkInfo

        Nothing ->
            Nothing


wrapperRepeatChecks : CollectionProperties (WrapperProperties otherProperties) -> CheckInfo -> Maybe (Error {})
wrapperRepeatChecks wrapper checkInfo =
    case Evaluate.getInt checkInfo checkInfo.firstArg of
        Just 1 ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " with " ++ wrapper.size.description ++ " 1 will result in " ++ qualifiedToString wrapper.wrap.fn
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


arrayLengthChecks : CheckInfo -> Maybe (Error {})
arrayLengthChecks =
    firstThatConstructsJust
        [ collectionSizeChecks arrayCollection
        , arrayLengthOnArrayRepeatOrInitializeChecks
        ]


arrayLengthOnArrayRepeatOrInitializeChecks : CheckInfo -> Maybe (Error {})
arrayLengthOnArrayRepeatOrInitializeChecks checkInfo =
    let
        maybeCall : Maybe ( String, { nodeRange : Range, fnRange : Range, firstArg : Node Expression, argsAfterFirst : List (Node Expression) } )
        maybeCall =
            firstThatConstructsJust
                [ \() ->
                    AstHelpers.getSpecificFnCall ( [ "Array" ], "repeat" ) checkInfo.lookupTable checkInfo.firstArg
                        |> Maybe.map (Tuple.pair "repeat")
                , \() ->
                    AstHelpers.getSpecificFnCall ( [ "Array" ], "initialize" ) checkInfo.lookupTable checkInfo.firstArg
                        |> Maybe.map (Tuple.pair "initialize")
                ]
                ()
    in
    case maybeCall of
        Just ( fnName, call ) ->
            let
                maxFn : String
                maxFn =
                    qualifiedToString (qualify ( [ "Basics" ], "max" ) defaultQualifyResources)
            in
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString (qualify checkInfo.fn checkInfo) ++ " on an array created by " ++ qualifiedToString (qualify ( [ "Array" ], fnName ) defaultQualifyResources) ++ " with a given length will result in that length"
                    , details = [ "You can replace this call by " ++ maxFn ++ " 0 with the given length. " ++ maxFn ++ " 0 makes sure that negative given lengths return 0." ]
                    }
                    checkInfo.fnRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range call.firstArg }
                        ++ [ Fix.insertAt checkInfo.parentRange.start (qualifiedToString (qualify ( [ "Basics" ], "max" ) checkInfo) ++ " 0 ") ]
                    )
                )

        Nothing ->
            Nothing


arrayFoldlChecks : CheckInfo -> Maybe (Error {})
arrayFoldlChecks =
    emptiableFoldChecks arrayCollection


arrayFoldrChecks : CheckInfo -> Maybe (Error {})
arrayFoldrChecks =
    emptiableFoldChecks arrayCollection


getChecks : EmptiableProperties (IndexableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
getChecks collection =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = maybeWithJustAsWrap.empty.asString } collection
        , \checkInfo ->
            Evaluate.getInt checkInfo checkInfo.firstArg
                |> Maybe.andThen (indexAccessChecks collection checkInfo)
        ]


indexAccessChecks : IndexableProperties otherProperties -> CheckInfo -> Int -> Maybe (Error {})
indexAccessChecks collection checkInfo n =
    if n < 0 then
        Just
            (alwaysResultsInUnparenthesizedConstantError (qualifiedToString checkInfo.fn ++ " with negative index")
                { replacement = maybeWithJustAsWrap.empty.asString }
                checkInfo
            )

    else
        case secondArg checkInfo of
            Just arg ->
                case collection.literalElements checkInfo.lookupTable arg of
                    Just literalElements ->
                        case List.drop n literalElements |> List.head of
                            Just element ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "The element returned by " ++ qualifiedToString checkInfo.fn ++ " is known"
                                        , details = [ "You can replace this call by Just the targeted element." ]
                                        }
                                        checkInfo.fnRange
                                        (replaceBySubExpressionFix (Node.range arg) element
                                            ++ [ Fix.replaceRangeBy (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                                    (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                                               ]
                                        )
                                    )

                            Nothing ->
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return " ++ qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo)
                                        , details = [ "You can replace this call by Nothing." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy checkInfo.parentRange (qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo)) ]
                                    )

                    Nothing ->
                        Nothing

            Nothing ->
                Nothing


setChecks : CollectionProperties (EmptiableProperties (FromListProperties (IndexableProperties otherProperties))) -> CheckInfo -> Maybe (Error {})
setChecks collection =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck collection
        , \checkInfo ->
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
        ]


setOnKnownElementChecks :
    CollectionProperties (FromListProperties (IndexableProperties otherProperties))
    -> CheckInfo
    -> Int
    -> Range
    -> Maybe (Error {})
setOnKnownElementChecks collection checkInfo n replacementArgRange =
    case thirdArg checkInfo of
        Just collectionArg ->
            case collection.literalElements checkInfo.lookupTable collectionArg of
                Just literalElements ->
                    case List.drop n literalElements |> List.head of
                        Just element ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " will replace a known element in the " ++ collection.fromListLiteral.description
                                    , details = [ "You can move the replacement argument directly into the " ++ collection.fromListLiteral.description ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range collectionArg }
                                        ++ [ Fix.replaceRangeBy (Node.range element) (checkInfo.extractSourceCode replacementArgRange) ]
                                    )
                                )

                        Nothing ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " with an index out of bounds of the given " ++ collection.represents ++ " will always return the same given " ++ collection.represents
                                    , details = [ "You can replace this call by the given " ++ collection.represents ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range collectionArg })
                                )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


emptiableReverseChecks : EmptiableProperties otherProperties -> CheckInfo -> Maybe (Error {})
emptiableReverseChecks emptiable =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck emptiable
        , toggleCallChecks
        ]


listReverseChecks : CheckInfo -> Maybe (Error {})
listReverseChecks =
    firstThatConstructsJust
        [ emptiableReverseChecks listCollection
        , callOnWrappedDoesNotChangeItCheck listCollection
        ]


listReverseCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listReverseCompositionChecks =
    firstThatConstructsJust
        [ compositionAfterWrapIsUnnecessaryCheck listCollection
        , toggleCompositionChecks
        ]


listSortChecks : CheckInfo -> Maybe (Error {})
listSortChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck listCollection
        , callOnWrappedDoesNotChangeItCheck listCollection
        , operationDoesNotChangeResultOfOperationCheck
        ]


listSortCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listSortCompositionChecks =
    operationDoesNotChangeResultOfOperationCompositionCheck


{-| Condense applying the same function with equal arguments (except the last one) twice in sequence into one.
This applies to functions that are equivalent to identity when operating on the result another such function.

Examples of such functions:

  - one argument: `Simplify.expectNaN`, `Review.Rule.providesFixesForModuleRule`, `List.sort`, `List.Extra.unique`, [`AVL.Set.clear`](https://package.elm-lang.org/packages/owanturist/elm-avl-dict/2.1.0/AVL-Set#clear)
  - two arguments: `List.filter f`, `List.Extra.filterNot f`, `List.Extra.takeWhile/dropWhile(Right) f`, `List.sortBy f`, `List.Extra.uniqueBy f`
  - three arguments: `Array.set i new`, `Array.Extra.resizelRepeat l pad`, `List.Extra.setAt i new`

Note that `update` or `setWhere` operations for example _can_ have an effect even after the same operation has already been applied.

For operations that toggle between 2 states, like `reverse` or `List.Extra.swapAt i j`, use `toggleCallChecks`

-}
operationDoesNotChangeResultOfOperationCheck : CheckInfo -> Maybe (Error {})
operationDoesNotChangeResultOfOperationCheck checkInfo =
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


operationDoesNotChangeResultOfOperationCompositionCheck : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
operationDoesNotChangeResultOfOperationCompositionCheck checkInfo =
    let
        areAllArgsEqual : () -> Bool
        areAllArgsEqual () =
            List.all
                (\( arg, earlierArg ) ->
                    Normalize.compare checkInfo arg earlierArg == Normalize.ConfirmedEquality
                )
                (List.map2 Tuple.pair checkInfo.later.args checkInfo.earlier.args)
    in
    if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == checkInfo.later.fn) == areAllArgsEqual () then
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


{-| The last argument of a fully applied function (the given `argCount` specifies what is considered "fully applied").

For example, `fullyAppliedLastArg` on `Array.set 3 "Hitagi"` would return `Nothing`
while `fullyAppliedLastArg` on `Array.set 3 "Hitagi" arr` would return `Just arr`.

-}
fullyAppliedLastArg : { callInfo | firstArg : Node Expression, argsAfterFirst : List (Node Expression), argCount : Int } -> Maybe (Node Expression)
fullyAppliedLastArg callInfo =
    List.drop (callInfo.argCount - 1) (callInfo.firstArg :: callInfo.argsAfterFirst) |> List.head


listSortByChecks : CheckInfo -> Maybe (Error {})
listSortByChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck listCollection
        , callOnWrappedDoesNotChangeItCheck listCollection
        , \checkInfo ->
            case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just _ ->
                    Just
                        (alwaysReturnsLastArgError
                            (qualifiedToString checkInfo.fn ++ " (always a)")
                            { represents = "list" }
                            checkInfo
                        )

                Nothing ->
                    Nothing
        , operationWithIdentityCanBeReplacedChecks { replacementFn = ( [ "List" ], "sort" ) }
        , operationDoesNotChangeResultOfOperationCheck
        ]


listSortByCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
listSortByCompositionChecks =
    operationDoesNotChangeResultOfOperationCompositionCheck


listSortWithChecks : CheckInfo -> Maybe (Error {})
listSortWithChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck listCollection
        , callOnWrappedDoesNotChangeItCheck listCollection
        , \checkInfo ->
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
                                (qualifiedToString checkInfo.fn ++ " (\\_ _ -> " ++ AstHelpers.orderToString order ++ ")")
                                { represents = "list" }
                                checkInfo
                    in
                    case order of
                        LT ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " (\\_ _ -> LT) is the same as " ++ qualifiedToString ( [ "List" ], "reverse" )
                                    , details = [ "You can replace this call by " ++ qualifiedToString ( [ "List" ], "reverse" ) ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy
                                        { start = checkInfo.fnRange.start
                                        , end = (Node.range checkInfo.firstArg).end
                                        }
                                        (qualifiedToString (qualify ( [ "List" ], "reverse" ) checkInfo))
                                    ]
                                )

                        EQ ->
                            Just fixToIdentity

                        GT ->
                            Just fixToIdentity

                Nothing ->
                    Nothing
        ]


listTakeChecks : CheckInfo -> Maybe (Error {})
listTakeChecks =
    firstThatConstructsJust
        [ \checkInfo ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just length ->
                    callWithNonPositiveIntCanBeReplacedByCheck
                        { int = length
                        , intDescription = "length"
                        , replacement = \_ -> "[]"
                        }
                        checkInfo

                Nothing ->
                    Nothing
        , callOnEmptyReturnsEmptyCheck listCollection
        ]


listDropChecks : CheckInfo -> Maybe (Error {})
listDropChecks =
    firstThatConstructsJust
        [ \checkInfo ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just 0 ->
                    Just
                        (alwaysReturnsLastArgError
                            (qualifiedToString checkInfo.fn ++ " 0")
                            { represents = "list" }
                            checkInfo
                        )

                _ ->
                    Nothing
        , callOnEmptyReturnsEmptyCheck listCollection
        ]


emptiableMapNChecks : EmptiableProperties otherProperties -> CheckInfo -> Maybe (Error {})
emptiableMapNChecks emptiable checkInfo =
    if List.any (emptiable.empty.is checkInfo.lookupTable) checkInfo.argsAfterFirst then
        Just
            (alwaysResultsInUnparenthesizedConstantError
                (qualifiedToString checkInfo.fn ++ " with any " ++ emptiable.represents ++ " being " ++ emptiable.empty.asString defaultQualifyResources)
                { replacement = emptiable.empty.asString }
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

This is pretty similar to `wrapperSequenceChecks` where we look at arguments instead of list elements.

-}
wrapperMapNChecks : WrapperProperties otherProperties -> CheckInfo -> Maybe (Error {})
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
                        { message = qualifiedToString checkInfo.fn ++ " where each " ++ wrapper.represents ++ " is " ++ descriptionForIndefinite wrapper.wrap.description ++ " will result in " ++ wrapFnDescription ++ " on the values inside"
                        , details = [ "You can replace this call by " ++ wrapFnDescription ++ " with the function applied to the values inside each " ++ descriptionWithoutArticle wrapper.wrap.description ++ "." ]
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

This is pretty similar to `sequenceOrFirstEmptyChecks` where we look at arguments instead of list elements.

-}
mapNOrFirstEmptyConstructionChecks :
    WrapperProperties
        { otherProperties
            | empty :
                { empty
                    | is : ModuleNameLookupTable -> Node Expression -> Bool
                    , description : Description
                }
        }
    -> CheckInfo
    -> Maybe (Error {})
mapNOrFirstEmptyConstructionChecks emptiable checkInfo =
    case findMapAndAllBefore (getEmptyExpressionNode checkInfo.lookupTable emptiable) checkInfo.argsAfterFirst of
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
                                    { description = descriptionForDefinite "the first" emptiable.empty.description
                                    , fix = replaceBySubExpressionFix checkInfo.parentRange emptyAndBefore.found
                                    }

                                -- one arg curried
                                1 ->
                                    { description =
                                        "always with " ++ descriptionForDefinite "the first" emptiable.empty.description
                                    , fix =
                                        replaceBySubExpressionFix checkInfo.parentRange emptyAndBefore.found
                                            ++ [ Fix.insertAt checkInfo.parentRange.start (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo) ++ " ") ]
                                    }

                                -- multiple args curried
                                atLeast2 ->
                                    let
                                        lambdaStart : String
                                        lambdaStart =
                                            "\\" ++ String.repeat atLeast2 "_ " ++ "-> "
                                    in
                                    { description =
                                        lambdaStart ++ "with " ++ descriptionForDefinite "the first" emptiable.empty.description
                                    , fix =
                                        replaceBySubExpressionFix checkInfo.parentRange emptyAndBefore.found
                                            ++ [ Fix.insertAt checkInfo.parentRange.start ("(" ++ lambdaStart)
                                               , Fix.insertAt checkInfo.parentRange.end ")"
                                               ]
                                    }
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " where we know " ++ descriptionForDefinite "the first" emptiable.empty.description ++ " will result in " ++ descriptionForDefinite "that" emptiable.empty.description
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
                                            qualifiedToString replacementMap ++ " with the same arguments until " ++ descriptionForDefinite "the first" emptiable.empty.description
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
                                                (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo) ++ " (")
                                            ]
                                        , description =
                                            "always with " ++ qualifiedToString replacementMap ++ " with the same arguments until " ++ descriptionForDefinite "the first" emptiable.empty.description
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
                                            lambdaStart ++ "with " ++ qualifiedToString replacementMap ++ " with the same arguments until " ++ descriptionForDefinite "the first" emptiable.empty.description
                                        }
                        in
                        Just
                            (Rule.errorWithFix
                                { message = qualifiedToString checkInfo.fn ++ " with " ++ descriptionForIndefinite emptiable.empty.description ++ " early will ignore later arguments"
                                , details = [ "You can replace this call by " ++ replacement.description ++ "." ]
                                }
                                checkInfo.fnRange
                                replacement.fix
                            )


listUnzipChecks : CheckInfo -> Maybe (Error {})
listUnzipChecks =
    callOnEmptyReturnsCheck { resultAsString = \_ -> "( [], [] )" } listCollection


setFromListChecks : CheckInfo -> Maybe (Error {})
setFromListChecks =
    firstThatConstructsJust
        [ collectionFromListChecks setCollection
        , wrapperFromListSingletonChecks setCollection
        , onCallToInverseReturnsItsArgumentCheck ( [ "Set" ], "toList" )
        ]


setFromListCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
setFromListCompositionChecks =
    firstThatConstructsJust
        [ wrapperFromListSingletonCompositionChecks setCollection
        , inversesCompositionCheck ( [ "Set" ], "toList" )
        ]


setFoldlChecks : CheckInfo -> Maybe (Error {})
setFoldlChecks =
    emptiableFoldChecks setCollection


setFoldrChecks : CheckInfo -> Maybe (Error {})
setFoldrChecks =
    emptiableFoldChecks setCollection


{-| The folding/reducing check

    fold f initial empty --> initial

so for example

    List.Extra.indexedFoldl f initial empty --> initial

    Graph.Tree.levelOrder f initial Graph.Tree.empty --> initial

    Graph.fold f initial empty --> initial

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

-}
emptiableFoldChecks :
    TypeProperties
        { otherProperties
            | empty :
                { empty
                    | is : ModuleNameLookupTable -> Node Expression -> Bool
                    , description : Description
                }
        }
    -> CheckInfo
    -> Maybe (Error {})
emptiableFoldChecks emptiable =
    firstThatConstructsJust
        [ foldToUnchangedAccumulatorCheck emptiable
        , \checkInfo ->
            case checkInfo.argsAfterFirst of
                initialArg :: emptiableArg :: [] ->
                    if emptiable.empty.is checkInfo.lookupTable emptiableArg then
                        Just
                            (returnsArgError
                                (qualifiedToString checkInfo.fn ++ " on " ++ descriptionForIndefinite emptiable.empty.description)
                                { argRepresents = "initial accumulator"
                                , arg = initialArg
                                }
                                checkInfo
                            )

                    else
                        Nothing

                _ ->
                    Nothing
        ]


foldToUnchangedAccumulatorCheck : TypeProperties otherProperties -> CheckInfo -> Maybe (Error {})
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
                                        (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo))
                                    ]
                                }

                            -- fold (\_ -> identity) initial
                            _ :: [] ->
                                { description = "`always` with the given initial accumulator"
                                , fix =
                                    [ Fix.replaceRangeBy
                                        (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                        (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo))
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


dictFromListChecks : CheckInfo -> Maybe (Error {})
dictFromListChecks =
    firstThatConstructsJust
        [ collectionFromListChecks dictCollection
        , onCallToInverseReturnsItsArgumentCheck ( [ "Dict" ], "toList" )
        ]


dictFromListCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
dictFromListCompositionChecks =
    inversesCompositionCheck ( [ "Dict" ], "toList" )


subAndCmdBatchChecks :
    EmptiableProperties otherProperties
    -> CheckInfo
    -> Maybe (Error {})
subAndCmdBatchChecks batchable =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck { resultAsString = batchable.empty.asString } listCollection
        , callOnWrapReturnsItsValue listCollection
        , callOnListWithIrrelevantEmptyElement batchable
        ]


batchCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
batchCompositionChecks =
    onWrapAlwaysReturnsIncomingCompositionCheck listCollection



-- TASK


taskMapChecks : CheckInfo -> Maybe (Error {})
taskMapChecks =
    firstThatConstructsJust
        [ emptiableMapChecks taskWithSucceedAsWrap
        , mapWrapChecks taskWithSucceedAsWrap
        ]


taskMapCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
taskMapCompositionChecks =
    wrapToMapCompositionChecks taskWithSucceedAsWrap


taskMapNChecks : CheckInfo -> Maybe (Error {})
taskMapNChecks =
    firstThatConstructsJust
        [ wrapperMapNChecks taskWithSucceedAsWrap
        , mapNOrFirstEmptyConstructionChecks taskWithSucceedAsWrap
        ]


taskAndThenChecks : CheckInfo -> Maybe (Error {})
taskAndThenChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck taskWithSucceedAsWrap
        , wrapperAndThenChecks taskWithSucceedAsWrap
        ]


taskMapErrorChecks : CheckInfo -> Maybe (Error {})
taskMapErrorChecks =
    firstThatConstructsJust
        [ emptiableMapChecks taskWithFailAsWrap
        , mapWrapChecks taskWithFailAsWrap
        ]


taskMapErrorCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
taskMapErrorCompositionChecks =
    wrapToMapCompositionChecks taskWithFailAsWrap


taskOnErrorChecks : CheckInfo -> Maybe (Error {})
taskOnErrorChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck taskWithFailAsWrap
        , wrapperAndThenChecks taskWithFailAsWrap
        ]


taskSequenceChecks : CheckInfo -> Maybe (Error {})
taskSequenceChecks =
    firstThatConstructsJust
        [ wrapperSequenceChecks taskWithSucceedAsWrap
        , sequenceOrFirstEmptyChecks taskWithSucceedAsWrap
        ]


taskSequenceCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
taskSequenceCompositionChecks =
    mappableSequenceCompositionChecks taskWithSucceedAsWrap


sequenceOrFirstEmptyChecks :
    WrapperProperties
        { otherProperties
            | empty :
                { empty
                    | is : ModuleNameLookupTable -> Node Expression -> Bool
                    , description : Description
                }
        }
    -> CheckInfo
    -> Maybe (Error {})
sequenceOrFirstEmptyChecks emptiable checkInfo =
    case AstHelpers.getListLiteral checkInfo.firstArg of
        Just list ->
            firstThatConstructsJust
                [ \() ->
                    case List.filter (\el -> isNothing (emptiable.wrap.getValue checkInfo.lookupTable el)) list of
                        firstNonWrappedElement :: _ ->
                            if emptiable.empty.is checkInfo.lookupTable firstNonWrappedElement then
                                Just
                                    (Rule.errorWithFix
                                        { message = qualifiedToString checkInfo.fn ++ " on a list containing " ++ descriptionForIndefinite emptiable.empty.description ++ " will result in " ++ descriptionForDefinite "the first" emptiable.empty.description
                                        , details = [ "You can replace this call by " ++ descriptionForDefinite "the first" emptiable.empty.description ++ " in the list." ]
                                        }
                                        checkInfo.fnRange
                                        (replaceBySubExpressionFix checkInfo.parentRange firstNonWrappedElement)
                                    )

                            else
                                Nothing

                        [] ->
                            Nothing
                , \() ->
                    case findMapNeighboring (\el -> getEmpty checkInfo.lookupTable emptiable el) list of
                        Just emptyAndNeighbors ->
                            case emptyAndNeighbors.after of
                                Just _ ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = qualifiedToString checkInfo.fn ++ " on a list containing " ++ descriptionForIndefinite emptiable.empty.description ++ " early will ignore later elements"
                                            , details = [ "You can remove all list elements after " ++ descriptionForDefinite "the first" emptiable.empty.description ++ "." ]
                                            }
                                            checkInfo.fnRange
                                            [ Fix.removeRange
                                                { start = emptyAndNeighbors.found.range.end
                                                , end = endWithoutBoundary (Node.range checkInfo.firstArg)
                                                }
                                            ]
                                        )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
                ]
                ()

        Nothing ->
            Nothing


wrapperSequenceChecks : WrapperProperties { otherProperties | mapFn : ( ModuleName, String ) } -> CheckInfo -> Maybe (Error {})
wrapperSequenceChecks wrapper =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck
            { resultAsString =
                \res -> qualifiedToString (qualify wrapper.wrap.fn res) ++ " []"
            }
            listCollection
        , \checkInfo ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just singletonList ->
                    let
                        replacement : QualifyResources a -> String
                        replacement qualifyResources =
                            qualifiedToString (qualify wrapper.mapFn qualifyResources)
                                ++ " "
                                ++ qualifiedToString (qualify ( [ "List" ], "singleton" ) qualifyResources)
                    in
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on a singleton list is the same as " ++ replacement defaultQualifyResources ++ " on the value inside"
                            , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ " on the value inside the singleton list." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange
                                (replacement checkInfo)
                                :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) singletonList.element
                            )
                        )

                Nothing ->
                    Nothing
        , \checkInfo ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just list ->
                    case traverse (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) list of
                        Just wraps ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on a list where each element is " ++ descriptionForIndefinite wrapper.wrap.description ++ " will result in " ++ qualifiedToString wrapper.wrap.fn ++ " on the values inside"
                                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " on a list where each element is replaced by its value inside " ++ descriptionForDefinite "the" wrapper.wrap.description ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy
                                        checkInfo.fnRange
                                        (qualifiedToString (qualify wrapper.wrap.fn checkInfo))
                                        :: List.concatMap
                                            (\wrap -> keepOnlyFix { parentRange = wrap.nodeRange, keep = Node.range wrap.value })
                                            wraps
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        ]


mappableSequenceCompositionChecks : TypeProperties { otherProperties | mapFn : ( ModuleName, String ) } -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
mappableSequenceCompositionChecks mappable checkInfo =
    case checkInfo.earlier.fn of
        ( [ "List" ], "singleton" ) ->
            let
                replacement : QualifyResources a -> String
                replacement qualifyResources =
                    qualifiedToString (qualify mappable.mapFn qualifyResources)
                        ++ " "
                        ++ qualifiedToString (qualify ( [ "List" ], "singleton" ) qualifyResources)
            in
            Just
                { info =
                    { message = qualifiedToString checkInfo.later.fn ++ " on a singleton list is the same as " ++ replacement defaultQualifyResources ++ " on the value inside"
                    , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
                    }
                , fix = compositionReplaceByFix (replacement checkInfo) checkInfo
                }

        _ ->
            Nothing



-- HTML.ATTRIBUTES


getTupleWithSpecificSecond : Bool -> Node Expression -> ModuleNameLookupTable -> Maybe { range : Range, first : Node Expression }
getTupleWithSpecificSecond specificBool expressionNode lookupTable =
    case AstHelpers.getTuple2Literal expressionNode of
        Just tuple ->
            case AstHelpers.getSpecificBool specificBool lookupTable tuple.second of
                Just _ ->
                    Just { range = tuple.range, first = tuple.first }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


htmlAttributesClassListFalseElementError : CheckInfo -> { message : String, details : List String }
htmlAttributesClassListFalseElementError checkInfo =
    { message = "In a " ++ qualifiedToString checkInfo.fn ++ ", a tuple paired with False can be removed"
    , details = [ "You can remove the tuple list element where the second part is False." ]
    }


htmlAttributesClassListChecks : CheckInfo -> Maybe (Error {})
htmlAttributesClassListChecks =
    firstThatConstructsJust
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
                                                ( [ "Html", "Attributes" ], "class" )
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
                    case findMapNeighboring (\el -> getTupleWithSpecificSecond False el checkInfo.lookupTable) (tuple0 :: tuple1 :: tuple2Up) of
                        Just classPart ->
                            Just
                                (Rule.errorWithFix (htmlAttributesClassListFalseElementError checkInfo)
                                    checkInfo.fnRange
                                    (listLiteralElementRemoveFix classPart)
                                )

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        , \checkInfo ->
            case AstHelpers.getCollapsedCons checkInfo.firstArg of
                Just classParts ->
                    case findMapNeighboring (\el -> getTupleWithSpecificSecond False el checkInfo.lookupTable) classParts.consed of
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



-- JSON.DECODE


jsonDecodeMapChecks : CheckInfo -> Maybe (Error {})
jsonDecodeMapChecks =
    firstThatConstructsJust
        [ emptiableMapChecks jsonDecoderWithSucceedAsWrap
        , mapWrapChecks jsonDecoderWithSucceedAsWrap
        ]


jsonDecodeMapCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
jsonDecodeMapCompositionChecks =
    wrapToMapCompositionChecks jsonDecoderWithSucceedAsWrap


jsonDecodeMapNChecks : CheckInfo -> Maybe (Error {})
jsonDecodeMapNChecks =
    firstThatConstructsJust
        [ wrapperMapNChecks jsonDecoderWithSucceedAsWrap
        , mapNOrFirstEmptyConstructionChecks jsonDecoderWithSucceedAsWrap
        ]


jsonDecodeAndThenChecks : CheckInfo -> Maybe (Error {})
jsonDecodeAndThenChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck jsonDecoderWithSucceedAsWrap
        , wrapperAndThenChecks jsonDecoderWithSucceedAsWrap
        ]



-- PARSER


oneOfChecks : CheckInfo -> Maybe (Error {})
oneOfChecks checkInfo =
    case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
        Just listSingletonArg ->
            Just
                (Rule.errorWithFix
                    { message = "Unnecessary oneOf"
                    , details = [ "There is only a single element in the list of elements to try out." ]
                    }
                    checkInfo.fnRange
                    (replaceBySubExpressionFix checkInfo.parentRange listSingletonArg.element)
                )

        Nothing ->
            Nothing



-- RANDOM


randomUniformChecks : CheckInfo -> Maybe (Error {})
randomUniformChecks checkInfo =
    case secondArg checkInfo of
        Just otherOptionsArg ->
            case AstHelpers.getListLiteral otherOptionsArg of
                Just [] ->
                    let
                        onlyValueRange : Range
                        onlyValueRange =
                            Node.range checkInfo.firstArg
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "Random.uniform with only one possible value can be replaced by Random.constant"
                            , details = [ "Only a single value can be produced by this Random.uniform call. You can replace the call with Random.constant with the value." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = onlyValueRange.start }
                                (qualifiedToString (qualify ( [ "Random" ], "constant" ) checkInfo) ++ " ")
                            , Fix.removeRange { start = onlyValueRange.end, end = checkInfo.parentRange.end }
                            ]
                        )

                _ ->
                    Nothing

        Nothing ->
            Nothing


randomWeightedChecks : CheckInfo -> Maybe (Error {})
randomWeightedChecks checkInfo =
    case secondArg checkInfo of
        Just otherOptionsArg ->
            case AstHelpers.getListLiteral otherOptionsArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Random.weighted with only one possible value can be replaced by Random.constant"
                            , details = [ "Only a single value can be produced by this Random.weighted call. You can replace the call with Random.constant with the value." ]
                            }
                            checkInfo.fnRange
                            (case Node.value checkInfo.firstArg of
                                Expression.TupledExpression (_ :: (Node valuePartRange _) :: []) ->
                                    [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = valuePartRange.start }
                                        (qualifiedToString (qualify ( [ "Random" ], "constant" ) checkInfo) ++ " ")
                                    , Fix.removeRange { start = valuePartRange.end, end = checkInfo.parentRange.end }
                                    ]

                                _ ->
                                    let
                                        tupleRange : Range
                                        tupleRange =
                                            Node.range checkInfo.firstArg
                                    in
                                    [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = tupleRange.start }
                                        (qualifiedToString (qualify ( [ "Random" ], "constant" ) checkInfo) ++ " (Tuple.first ")
                                    , Fix.replaceRangeBy { start = tupleRange.end, end = checkInfo.parentRange.end }
                                        ")"
                                    ]
                            )
                        )

                _ ->
                    Nothing

        Nothing ->
            Nothing


randomListChecks : CheckInfo -> Maybe (Error {})
randomListChecks =
    firstThatConstructsJust
        [ \checkInfo ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just 1 ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " 1 can be replaced by " ++ qualifiedToString ( [ "Random" ], "map" ) ++ " " ++ qualifiedToString ( [ "List" ], "singleton" )
                            , details = [ "This " ++ qualifiedToString checkInfo.fn ++ " call always produces a list with one generated element. This means you can replace the call with " ++ qualifiedToString ( [ "Random" ], "map" ) ++ " " ++ qualifiedToString ( [ "List" ], "singleton" ) ++ "." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy
                                (Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ])
                                (qualifiedToString (qualify ( [ "Random" ], "map" ) checkInfo)
                                    ++ " "
                                    ++ qualifiedToString (qualify ( [ "List" ], "singleton" ) checkInfo)
                                )
                            ]
                        )

                Just non1Length ->
                    if non1Length <= 0 then
                        Just
                            (alwaysResultsInConstantError
                                (case non1Length of
                                    0 ->
                                        "Random.list with length 0"

                                    _ ->
                                        "Random.list with a negative length"
                                )
                                { replacement =
                                    \res -> qualifiedToString (qualify ( [ "Random" ], "constant" ) res) ++ " []"
                                , replacementNeedsParens = True
                                }
                                checkInfo
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
        , \checkInfo ->
            case secondArg checkInfo of
                Just elementGeneratorArg ->
                    case AstHelpers.getSpecificFnCall ( [ "Random" ], "constant" ) checkInfo.lookupTable elementGeneratorArg of
                        Just constantCall ->
                            let
                                currentAsString : String
                                currentAsString =
                                    qualifiedToString checkInfo.fn ++ " n (" ++ qualifiedToString ( [ "Random" ], "constant" ) ++ " el)"

                                replacementAsString : String
                                replacementAsString =
                                    qualifiedToString ( [ "Random" ], "constant" ) ++ " (" ++ qualifiedToString ( [ "List" ], "repeat" ) ++ " n el)"
                            in
                            Just
                                (Rule.errorWithFix
                                    { message = currentAsString ++ " can be replaced by " ++ replacementAsString
                                    , details = [ currentAsString ++ " generates the same value for each of the n elements. This means you can replace the call with " ++ replacementAsString ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (replaceBySubExpressionFix constantCall.nodeRange constantCall.firstArg
                                        ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                                (qualifiedToString (qualify ( [ "List" ], "repeat" ) checkInfo))
                                           , Fix.insertAt checkInfo.parentRange.start
                                                (qualifiedToString (qualify ( [ "Random" ], "constant" ) checkInfo)
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


randomMapChecks : CheckInfo -> Maybe (Error {})
randomMapChecks =
    firstThatConstructsJust
        [ mapIdentityChecks randomGeneratorWrapper
        , mapWrapChecks randomGeneratorWrapper
        , nonEmptiableWrapperMapAlwaysChecks randomGeneratorWrapper
        ]


randomMapCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
randomMapCompositionChecks =
    wrapperMapCompositionChecks randomGeneratorWrapper


randomAndThenChecks : CheckInfo -> Maybe (Error {})
randomAndThenChecks =
    firstThatConstructsJust
        [ wrapperAndThenChecks randomGeneratorWrapper
        , nonEmptiableWrapperAndThenAlwaysChecks randomGeneratorWrapper
        ]


nonEmptiableWrapperAndThenAlwaysChecks :
    NonEmptiableProperties (WrapperProperties otherProperties)
    -> CheckInfo
    -> Maybe (Error {})
nonEmptiableWrapperAndThenAlwaysChecks wrapper checkInfo =
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



--


type alias TypeProperties properties =
    { properties
        | represents : String
    }


{-| Properties of a type that can hold some data or none.
-}
type alias EmptiableProperties otherProperties =
    TypeProperties
        { otherProperties
            | empty : ConstantProperties
        }


type alias ConstantProperties =
    { asString : QualifyResources {} -> String
    , description : Description
    , is : ModuleNameLookupTable -> Node Expression -> Bool
    }


{-| `TypeProperties` of a structure that will always have data inside, for example a non-empty list, a `Test`, a `Benchmark` or a tree (but not a forest).

This can be really valuable, for example when you want to know whether the function of a map or andThen will always be called.

The way this type is defined,
it is impossible to have one type that has both `EmptiableProperties` and `NonEmptiableProperties`

-}
type alias NonEmptiableProperties otherProperties =
    TypeProperties
        { otherProperties
            | empty : { invalid : () }
        }


{-| Properties of a type that has a construction function that takes one value.

Example "wrap" construction functions: `Just`, `Err`, `List.singleton` and `[ a ]`
Note that for example `Cmd.batch [ a ]` is not a "wrap" because it keeps the type of the inner value `a`

-}
type alias WrapperProperties otherProperties =
    TypeProperties
        { otherProperties
            | wrap : ConstructWithOneArgProperties
        }


type alias FromListProperties otherProperties =
    TypeProperties
        { otherProperties
            | fromListLiteral :
                { description : String
                , getListRange : ModuleNameLookupTable -> Node Expression -> Maybe Range
                }
            , unionLeftElementsStayOnTheLeft : Bool
        }


type alias IndexableProperties otherProperties =
    TypeProperties
        { otherProperties
            | literalElements : ModuleNameLookupTable -> Node Expression -> Maybe (List (Node Expression))
        }


type alias ConstructWithOneArgProperties =
    { description : Description
    , fn : ( ModuleName, String )
    , getValue : ModuleNameLookupTable -> Node Expression -> Maybe (Node Expression)
    }


{-| Properties of a type with with multiple elements.
-}
type alias CollectionProperties otherProperties =
    TypeProperties
        { otherProperties
            | size :
                { description : String
                , determine : Infer.Resources {} -> Node Expression -> Maybe CollectionSize
                }
        }


getEmpty :
    ModuleNameLookupTable
    -> { otherProperties | empty : { empty | is : ModuleNameLookupTable -> Node Expression -> Bool } }
    -> Node Expression
    -> Maybe { range : Range }
getEmpty lookupTable emptiable expressionNode =
    if emptiable.empty.is lookupTable expressionNode then
        Just { range = Node.range expressionNode }

    else
        Nothing


getEmptyExpressionNode :
    ModuleNameLookupTable
    -> { otherProperties | empty : { empty | is : ModuleNameLookupTable -> Node Expression -> Bool } }
    -> Node Expression
    -> Maybe (Node Expression)
getEmptyExpressionNode lookupTable emptiable expressionNode =
    if emptiable.empty.is lookupTable expressionNode then
        Just expressionNode

    else
        Nothing


{-| Description of a set of values.

  - Only one value is possible, like Cmd.none or [] → Constant
  - Multiple values are possible, like `Ok anyValue` or `[ onlyElementAnyValue ]`? → `A`/`An` depending on the indefinite article in front of the description

-}
type Description
    = A String
    | An String
    | Constant String


descriptionForIndefinite : Description -> String
descriptionForIndefinite incomingArgDescription =
    case incomingArgDescription of
        A description ->
            "a " ++ description

        An description ->
            "an " ++ description

        Constant description ->
            description


descriptionForDefinite : String -> Description -> String
descriptionForDefinite startWithDefiniteArticle referenceArgDescription =
    case referenceArgDescription of
        A description ->
            startWithDefiniteArticle ++ " " ++ description

        An description ->
            startWithDefiniteArticle ++ " " ++ description

        Constant description ->
            description


descriptionWithoutArticle : Description -> String
descriptionWithoutArticle referenceArgDescription =
    case referenceArgDescription of
        A description ->
            description

        An description ->
            description

        Constant description ->
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


emptyAsString : QualifyResources a -> { emptiable | empty : { empty | asString : QualifyResources {} -> String } } -> String
emptyAsString qualifyResources emptiable =
    emptiable.empty.asString (extractQualifyResources qualifyResources)


randomGeneratorWrapper : NonEmptiableProperties (WrapperProperties { mapFn : ( ModuleName, String ) })
randomGeneratorWrapper =
    { represents = "random generator"
    , wrap =
        { description = A "constant generator"
        , fn = ( [ "Random" ], "constant" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "Random" ], "constant" ) lookupTable expr)
        }
    , empty = { invalid = () }
    , mapFn = ( [ "Random" ], "map" )
    }


maybeWithJustAsWrap :
    EmptiableProperties
        (WrapperProperties { mapFn : ( ModuleName, String ) })
maybeWithJustAsWrap =
    { represents = "maybe"
    , empty =
        { description = Constant "Nothing"
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificValueOrFn ( [ "Maybe" ], "Nothing" ) lookupTable expr)
        , asString =
            \resources ->
                qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) resources)
        }
    , wrap =
        { description = A "just maybe"
        , fn = ( [ "Maybe" ], "Just" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "Maybe" ], "Just" ) lookupTable expr)
        }
    , mapFn = ( [ "Maybe" ], "map" )
    }


resultWithOkAsWrap :
    WrapperProperties
        { empty :
            { description : Description
            , is : ModuleNameLookupTable -> Node Expression -> Bool
            , fn : ( ModuleName, String )
            }
        , mapFn : ( ModuleName, String )
        }
resultWithOkAsWrap =
    { represents = "result"
    , wrap =
        { description = An "okay result"
        , fn = ( [ "Result" ], "Ok" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "Result" ], "Ok" ) lookupTable expr)
        }
    , empty =
        { description = An "error"
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificFnCall ( [ "Result" ], "Err" ) lookupTable expr)
        , fn = ( [ "Result" ], "Err" )
        }
    , mapFn = ( [ "Result" ], "map" )
    }


resultWithErrAsWrap :
    WrapperProperties
        { empty :
            { description : Description
            , is : ModuleNameLookupTable -> Node Expression -> Bool
            , fn : ( ModuleName, String )
            }
        , mapFn : ( ModuleName, String )
        }
resultWithErrAsWrap =
    { represents = "result"
    , wrap =
        { description = An "error"
        , fn = ( [ "Result" ], "Err" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "Result" ], "Err" ) lookupTable expr)
        }
    , empty =
        { description = An "okay result"
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificFnCall ( [ "Result" ], "Ok" ) lookupTable expr)
        , fn = ( [ "Result" ], "Ok" )
        }
    , mapFn = ( [ "Result" ], "mapError" )
    }


taskWithSucceedAsWrap :
    WrapperProperties
        { empty :
            { description : Description
            , is : ModuleNameLookupTable -> Node Expression -> Bool
            , fn : ( ModuleName, String )
            }
        , mapFn : ( ModuleName, String )
        }
taskWithSucceedAsWrap =
    { represents = "task"
    , wrap =
        { description = A "succeeding task"
        , fn = ( [ "Task" ], "succeed" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "Task" ], "succeed" ) lookupTable expr)
        }
    , empty =
        { description = A "failing task"
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificFnCall ( [ "Task" ], "fail" ) lookupTable expr)
        , fn = ( [ "Task" ], "fail" )
        }
    , mapFn = ( [ "Task" ], "map" )
    }


taskWithFailAsWrap :
    WrapperProperties
        { empty :
            { description : Description
            , is : ModuleNameLookupTable -> Node Expression -> Bool
            , fn : ( ModuleName, String )
            }
        , mapFn : ( ModuleName, String )
        }
taskWithFailAsWrap =
    { represents = "task"
    , wrap =
        { description = A "failing task"
        , fn = ( [ "Task" ], "fail" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "Task" ], "fail" ) lookupTable expr)
        }
    , empty =
        { description = A "succeeding task"
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificFnCall ( [ "Task" ], "succeed" ) lookupTable expr)
        , fn = ( [ "Task" ], "succeed" )
        }
    , mapFn = ( [ "Task" ], "mapError" )
    }


jsonDecoderWithSucceedAsWrap :
    WrapperProperties
        { empty :
            { description : Description
            , is : ModuleNameLookupTable -> Node Expression -> Bool
            }
        , mapFn : ( ModuleName, String )
        }
jsonDecoderWithSucceedAsWrap =
    { represents = "json decoder"
    , wrap =
        { description = A "succeeding decoder"
        , fn = ( [ "Json", "Decode" ], "succeed" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "Json", "Decode" ], "succeed" ) lookupTable expr)
        }
    , empty =
        { description = A "failing decoder"
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificFnCall ( [ "Json", "Decode" ], "fail" ) lookupTable expr)
        }
    , mapFn = ( [ "Json", "Decode" ], "map" )
    }


listCollection : CollectionProperties (EmptiableProperties (WrapperProperties (FromListProperties { mapFn : ( ModuleName, String ) })))
listCollection =
    { represents = "list"
    , empty =
        { description = Constant "[]"
        , is = \_ expr -> AstHelpers.getListLiteral expr == Just []
        , asString = \_ -> "[]"
        }
    , size =
        { description = "length"
        , determine = listDetermineLength
        }
    , wrap =
        { description = A "singleton list"
        , fn = ( [ "List" ], "singleton" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .element (AstHelpers.getListSingleton lookupTable expr)
        }
    , mapFn = ( [ "List" ], "map" )
    , fromListLiteral =
        { description = "list literal"
        , getListRange = \_ expr -> AstHelpers.getListLiteralRange expr
        }
    , unionLeftElementsStayOnTheLeft = True
    }


listDetermineLength : Infer.Resources a -> Node Expression -> Maybe CollectionSize
listDetermineLength resources expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.ListExpr list ->
            Just (Exactly (List.length list))

        Expression.OperatorApplication "::" _ _ right ->
            case listDetermineLength resources right of
                Just (Exactly n) ->
                    Just (Exactly (n + 1))

                _ ->
                    Just NotEmpty

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "singleton")) :: _ :: []) ->
            if ModuleNameLookupTable.moduleNameAt resources.lookupTable fnRange == Just [ "List" ] then
                Just (Exactly 1)

            else
                Nothing

        _ ->
            Nothing


stringCollection : CollectionProperties (WrapperProperties (EmptiableProperties (FromListProperties {})))
stringCollection =
    { represents = "string"
    , empty =
        { description = Constant emptyStringAsString
        , asString = \_ -> emptyStringAsString
        , is = \_ (Node _ expr) -> expr == Expression.Literal ""
        }
    , size =
        { description = "length"
        , determine = \_ (Node _ expr) -> stringDetermineLength expr
        }
    , wrap =
        { description = A "single-char string"
        , fn = ( [ "String" ], "fromChar" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "String" ], "fromChar" ) lookupTable expr)
        }
    , fromListLiteral =
        { description = "String.fromList call"
        , getListRange =
            \lookupTable expr ->
                AstHelpers.getSpecificFnCall ( [ "String" ], "fromList" ) lookupTable expr
                    |> Maybe.andThen (\stringFromListCall -> AstHelpers.getListLiteralRange stringFromListCall.firstArg)
        }
    , unionLeftElementsStayOnTheLeft = True
    }


stringDetermineLength : Expression -> Maybe CollectionSize
stringDetermineLength expression =
    case expression of
        Expression.Literal string ->
            Just (Exactly (String.length string))

        _ ->
            Nothing


arrayCollection : CollectionProperties (EmptiableProperties (FromListProperties (IndexableProperties {})))
arrayCollection =
    { represents = "array"
    , empty =
        { description = Constant (qualifiedToString ( [ "Array" ], "empty" ))
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificValueOrFn ( [ "Array" ], "empty" ) lookupTable expr)
        , asString =
            \resources ->
                qualifiedToString (qualify ( [ "Array" ], "empty" ) resources)
        }
    , size = { description = "length", determine = arrayDetermineSize }
    , literalElements =
        \lookupTable expr ->
            AstHelpers.getSpecificFnCall ( [ "Array" ], "fromList" ) lookupTable expr
                |> Maybe.andThen (\arrayFromListCall -> AstHelpers.getListLiteral arrayFromListCall.firstArg)
    , fromListLiteral =
        { description = "Array.fromList call"
        , getListRange =
            \lookupTable expr ->
                AstHelpers.getSpecificFnCall ( [ "Array" ], "fromList" ) lookupTable expr
                    |> Maybe.andThen (\call -> AstHelpers.getListLiteralRange call.firstArg)
        }
    , unionLeftElementsStayOnTheLeft = True
    }


arrayDetermineSize :
    Infer.Resources a
    -> Node Expression
    -> Maybe CollectionSize
arrayDetermineSize resources =
    firstThatConstructsJust
        [ \expressionNode ->
            case AstHelpers.getSpecificValueOrFn ( [ "Array" ], "empty" ) resources.lookupTable expressionNode of
                Just _ ->
                    Just (Exactly 0)

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall ( [ "Array" ], "fromList" ) resources.lookupTable expressionNode of
                Just fromListCall ->
                    listDetermineLength resources fromListCall.firstArg

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall ( [ "Array" ], "repeat" ) resources.lookupTable expressionNode of
                Just repeatCall ->
                    Evaluate.getInt resources repeatCall.firstArg
                        |> Maybe.map (\n -> Exactly (max 0 n))

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall ( [ "Array" ], "initialize" ) resources.lookupTable expressionNode of
                Just repeatCall ->
                    Evaluate.getInt resources repeatCall.firstArg
                        |> Maybe.map (\n -> Exactly (max 0 n))

                Nothing ->
                    Nothing
        ]


setCollection : CollectionProperties (EmptiableProperties (WrapperProperties (FromListProperties {})))
setCollection =
    { represents = "set"
    , empty =
        { description = Constant (qualifiedToString ( [ "Set" ], "empty" ))
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificValueOrFn ( [ "Set" ], "empty" ) lookupTable expr)
        , asString =
            \resources ->
                qualifiedToString (qualify ( [ "Set" ], "empty" ) resources)
        }
    , size = { description = "size", determine = setDetermineSize }
    , wrap =
        { description = A "singleton set"
        , fn = ( [ "Set" ], "singleton" )
        , getValue =
            \lookupTable expr ->
                Maybe.map .firstArg (AstHelpers.getSpecificFnCall ( [ "Set" ], "singleton" ) lookupTable expr)
        }
    , fromListLiteral =
        { description = "Set.fromList call"
        , getListRange =
            \lookupTable expr ->
                AstHelpers.getSpecificFnCall ( [ "Set" ], "fromList" ) lookupTable expr
                    |> Maybe.andThen (\setFromListCall -> AstHelpers.getListLiteralRange setFromListCall.firstArg)
        }
    , unionLeftElementsStayOnTheLeft = True
    }


setDetermineSize :
    Infer.Resources a
    -> Node Expression
    -> Maybe CollectionSize
setDetermineSize resources =
    firstThatConstructsJust
        [ \expressionNode ->
            case AstHelpers.getSpecificValueOrFn ( [ "Set" ], "empty" ) resources.lookupTable expressionNode of
                Just _ ->
                    Just (Exactly 0)

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall ( [ "Set" ], "singleton" ) resources.lookupTable expressionNode of
                Just _ ->
                    Just (Exactly 1)

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall ( [ "Set" ], "fromList" ) resources.lookupTable expressionNode of
                Just fromListCall ->
                    case AstHelpers.getListLiteral fromListCall.firstArg of
                        Just [] ->
                            Just (Exactly 0)

                        Just (_ :: []) ->
                            Just (Exactly 1)

                        Just (el0 :: el1 :: el2Up) ->
                            case traverse getComparableExpression (el0 :: el1 :: el2Up) of
                                Nothing ->
                                    Just NotEmpty

                                Just comparableExpressions ->
                                    comparableExpressions |> unique |> List.length |> Exactly |> Just

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        ]


dictCollection : CollectionProperties (EmptiableProperties (FromListProperties {}))
dictCollection =
    { represents = "dict"
    , empty =
        { description = Constant (qualifiedToString ( [ "Dict" ], "empty" ))
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificValueOrFn ( [ "Dict" ], "empty" ) lookupTable expr)
        , asString =
            \resources ->
                qualifiedToString (qualify ( [ "Dict" ], "empty" ) resources)
        }
    , size = { description = "size", determine = dictDetermineSize }
    , fromListLiteral =
        { description = "Dict.fromList call"
        , getListRange =
            \lookupTable expr ->
                AstHelpers.getSpecificFnCall ( [ "Dict" ], "fromList" ) lookupTable expr
                    |> Maybe.andThen (\dictFromListCall -> AstHelpers.getListLiteralRange dictFromListCall.firstArg)
        }
    , unionLeftElementsStayOnTheLeft = False
    }


dictDetermineSize :
    Infer.Resources a
    -> Node Expression
    -> Maybe CollectionSize
dictDetermineSize resources =
    firstThatConstructsJust
        [ \expressionNode ->
            case AstHelpers.getSpecificValueOrFn ( [ "Dict" ], "empty" ) resources.lookupTable expressionNode of
                Just _ ->
                    Just (Exactly 0)

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall ( [ "Dict" ], "singleton" ) resources.lookupTable expressionNode of
                Just singletonCall ->
                    case singletonCall.argsAfterFirst of
                        _ :: [] ->
                            Just (Exactly 1)

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        , \expressionNode ->
            case AstHelpers.getSpecificFnCall ( [ "Dict" ], "fromList" ) resources.lookupTable expressionNode of
                Just fromListCall ->
                    case AstHelpers.getListLiteral fromListCall.firstArg of
                        Just [] ->
                            Just (Exactly 0)

                        Just (_ :: []) ->
                            Just (Exactly 1)

                        Just (el0 :: el1 :: el2Up) ->
                            case traverse getComparableExpressionInTupleFirst (el0 :: el1 :: el2Up) of
                                Nothing ->
                                    Just NotEmpty

                                Just comparableExpressions ->
                                    comparableExpressions |> unique |> List.length |> Exactly |> Just

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        ]


cmdCollection : EmptiableProperties {}
cmdCollection =
    { represents = "command"
    , empty =
        { description =
            Constant "Cmd.none"
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificValueOrFn ( [ "Platform", "Cmd" ], "none" ) lookupTable expr)
        , asString =
            \resources ->
                qualifiedToString (qualify ( [ "Platform", "Cmd" ], "none" ) resources)
        }
    }


subCollection : EmptiableProperties {}
subCollection =
    { represents = "subscription"
    , empty =
        { description =
            Constant "Sub.none"
        , is =
            \lookupTable expr ->
                isJust (AstHelpers.getSpecificValueOrFn ( [ "Platform", "Sub" ], "none" ) lookupTable expr)
        , asString =
            \resources ->
                qualifiedToString (qualify ( [ "Platform", "Sub" ], "none" ) resources)
        }
    }


emptiableMapChecks :
    TypeProperties
        { otherProperties
            | empty :
                { empty
                    | description : Description
                    , is : ModuleNameLookupTable -> Node Expression -> Bool
                }
        }
    -> CheckInfo
    -> Maybe (Error {})
emptiableMapChecks emptiable =
    firstThatConstructsJust
        [ mapIdentityChecks emptiable
        , callOnEmptyReturnsEmptyCheck emptiable
        ]


mapIdentityChecks :
    TypeProperties properties
    -> CheckInfo
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


wrapperMapCompositionChecks : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
wrapperMapCompositionChecks wrapper =
    firstThatConstructsJust
        [ wrapToMapCompositionChecks wrapper
        , mapAlwaysCompositionChecks wrapper
        ]


mapWrapChecks :
    WrapperProperties otherProperties
    -> CheckInfo
    -> Maybe (Error {})
mapWrapChecks wrapper checkInfo =
    case secondArg checkInfo of
        Just wrapperArg ->
            case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) wrapperArg of
                Determined wraps ->
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

                Undetermined ->
                    Nothing

        Nothing ->
            Nothing


wrapToMapCompositionChecks :
    WrapperProperties otherProperties
    -> CompositionIntoCheckInfo
    -> Maybe ErrorInfoAndFix
wrapToMapCompositionChecks wrapper checkInfo =
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


nonEmptiableWrapperMapAlwaysChecks :
    NonEmptiableProperties (WrapperProperties otherProperties)
    -> CheckInfo
    -> Maybe (Error {})
nonEmptiableWrapperMapAlwaysChecks wrapper checkInfo =
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
                                (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo)
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


mapAlwaysCompositionChecks :
    WrapperProperties otherProperties
    -> CompositionIntoCheckInfo
    -> Maybe ErrorInfoAndFix
mapAlwaysCompositionChecks wrapper checkInfo =
    case ( ( checkInfo.earlier.fn, checkInfo.earlier.args ), checkInfo.later.args ) of
        ( ( ( [ "Basics" ], "always" ), [] ), [] ) ->
            Just
                { info =
                    { message = qualifiedToString checkInfo.later.fn ++ " with a function that always maps to the same value is equivalent to " ++ qualifiedToString wrapper.wrap.fn
                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ "." ]
                    }
                , fix =
                    compositionReplaceByFnFix wrapper.wrap.fn checkInfo
                }

        _ ->
            Nothing


emptiableAndThenChecks :
    { otherProperties
        | empty : ConstantProperties
    }
    -> CheckInfo
    -> Maybe (Error {})
emptiableAndThenChecks emptiable =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck emptiable
        , \checkInfo ->
            case constructs (sameInAllBranches (getEmpty checkInfo.lookupTable emptiable)) checkInfo.lookupTable checkInfo.firstArg of
                Determined _ ->
                    Just
                        (alwaysResultsInUnparenthesizedConstantError
                            (qualifiedToString checkInfo.fn ++ " with a function that will always return " ++ emptiable.empty.asString defaultQualifyResources)
                            { replacement = emptiable.empty.asString }
                            checkInfo
                        )

                Undetermined ->
                    Nothing
        ]


getValueWithNodeRange :
    (Node Expression -> Maybe (Node Expression))
    -> Node Expression
    -> Maybe { value : Node Expression, nodeRange : Range }
getValueWithNodeRange getValue expressionNode =
    Maybe.map (\value -> { value = value, nodeRange = Node.range expressionNode })
        (getValue expressionNode)


wrapperAndThenChecks :
    WrapperProperties { otherProperties | mapFn : ( ModuleName, String ) }
    -> CheckInfo
    -> Maybe (Error {})
wrapperAndThenChecks wrapper =
    firstThatConstructsJust
        [ \checkInfo ->
            case secondArg checkInfo of
                Just maybeArg ->
                    case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) maybeArg of
                        Determined wrapCalls ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on " ++ descriptionForIndefinite wrapper.wrap.description ++ " is the same as applying the function to the value from " ++ descriptionForDefinite "the" wrapper.wrap.description
                                    , details = [ "You can replace this call by the function directly applied to the value inside " ++ descriptionForDefinite "the" wrapper.wrap.description ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                                        :: List.concatMap (\justCall -> replaceBySubExpressionFix justCall.nodeRange justCall.value) wrapCalls
                                    )
                                )

                        Undetermined ->
                            Nothing

                Nothing ->
                    Nothing
        , \checkInfo ->
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
        , \checkInfo ->
            case
                constructs
                    (sameInAllBranches (\expr -> getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable) expr))
                    checkInfo.lookupTable
                    checkInfo.firstArg
            of
                Determined wrapCalls ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " with a function that always returns " ++ descriptionForIndefinite wrapper.wrap.description ++ " is the same as " ++ qualifiedToString wrapper.mapFn ++ " with the function returning the value inside"
                            , details = [ "You can replace this call by " ++ qualifiedToString wrapper.mapFn ++ " with the function returning the value inside " ++ descriptionForDefinite "the" wrapper.wrap.description ++ "." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange
                                (qualifiedToString (qualify wrapper.mapFn checkInfo))
                                :: List.concatMap (\call -> replaceBySubExpressionFix call.nodeRange call.value) wrapCalls
                            )
                        )

                Undetermined ->
                    Nothing
        ]


maybeAndThenChecks : CheckInfo -> Maybe (Error {})
maybeAndThenChecks =
    firstThatConstructsJust
        [ wrapperAndThenChecks maybeWithJustAsWrap
        , emptiableAndThenChecks maybeWithJustAsWrap
        ]


resultAndThenChecks : CheckInfo -> Maybe (Error {})
resultAndThenChecks =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck resultWithOkAsWrap
        , wrapperAndThenChecks resultWithOkAsWrap
        ]


withDefaultChecks :
    WrapperProperties
        { otherProperties
            | empty :
                { empty
                    | description : Description
                    , is : ModuleNameLookupTable -> Node Expression -> Bool
                }
        }
    -> CheckInfo
    -> Maybe (Error {})
withDefaultChecks emptiable =
    firstThatConstructsJust
        [ emptiableWithDefaultChecks emptiable
        , callOnWrapReturnsItsValue emptiable
        ]


wrapperWithDefaultChecks : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
wrapperWithDefaultChecks wrapper =
    onWrapAlwaysReturnsIncomingCompositionCheck wrapper


emptiableWithDefaultChecks :
    { otherProperties
        | empty :
            { empty
                | description : Description
                , is : ModuleNameLookupTable -> Node Expression -> Bool
            }
    }
    -> CheckInfo
    -> Maybe (Error {})
emptiableWithDefaultChecks emptiable checkInfo =
    case secondArg checkInfo of
        Just emptiableArg ->
            case sameInAllBranches (getEmpty checkInfo.lookupTable emptiable) emptiableArg of
                Determined _ ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on " ++ descriptionForIndefinite emptiable.empty.description ++ " will result in the default value"
                            , details = [ "You can replace this call by the default value." ]
                            }
                            checkInfo.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                        )

                Undetermined ->
                    Nothing

        Nothing ->
            Nothing


unwrapToMaybeChecks :
    WrapperProperties
        { otherProperties
            | empty :
                { empty
                    | is : ModuleNameLookupTable -> Node Expression -> Bool
                    , description : Description
                }
        }
    -> CheckInfo
    -> Maybe (Error {})
unwrapToMaybeChecks emptiableWrapper =
    firstThatConstructsJust
        [ callOnWrapReturnsJustItsValue emptiableWrapper
        , callOnEmptyReturnsCheck
            { resultAsString = \res -> qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) res) }
            emptiableWrapper
        ]


resultToMaybeCompositionChecks : CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
resultToMaybeCompositionChecks =
    firstThatConstructsJust
        [ onWrapAlwaysReturnsJustIncomingCompositionCheck resultWithOkAsWrap
        , \checkInfo ->
            case checkInfo.earlier.fn of
                ( [ "Result" ], "Err" ) ->
                    Just
                        { info =
                            { message = qualifiedToString ( [ "Result" ], "toMaybe" ) ++ " on an error will result in Nothing"
                            , details = [ "You can replace this call by always Nothing." ]
                            }
                        , fix =
                            compositionReplaceByFix
                                (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo)
                                    ++ " "
                                    ++ qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo)
                                )
                                checkInfo
                        }

                _ ->
                    Nothing
        ]


resultFromMaybeChecks : CheckInfo -> Maybe (Error {})
resultFromMaybeChecks =
    fromMaybeChecks
        { onNothingFn = ( [ "Result" ], "Err" ), onJustFn = ( [ "Result" ], "Ok" ) }


fromMaybeChecks : { onNothingFn : ( ModuleName, String ), onJustFn : ( ModuleName, String ) } -> CheckInfo -> Maybe (Error {})
fromMaybeChecks config checkInfo =
    case secondArg checkInfo of
        Just maybeArg ->
            firstThatConstructsJust
                [ \() ->
                    case sameInAllBranches (AstHelpers.getSpecificValueOrFn ( [ "Maybe" ], "Nothing" ) checkInfo.lookupTable) maybeArg of
                        Determined _ ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on Nothing will result in " ++ qualifiedToString (qualify config.onNothingFn checkInfo) ++ " with the given first value"
                                    , details = [ "You can replace this call by " ++ qualifiedToString (qualify config.onNothingFn checkInfo) ++ " with the given first value." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify config.onNothingFn checkInfo))
                                        :: keepOnlyFix
                                            { parentRange = checkInfo.parentRange
                                            , keep = Range.combine [ checkInfo.fnRange, Node.range checkInfo.firstArg ]
                                            }
                                    )
                                )

                        Undetermined ->
                            Nothing
                , \() ->
                    case sameInAllBranches (AstHelpers.getSpecificFnCall ( [ "Maybe" ], "Just" ) checkInfo.lookupTable) maybeArg of
                        Determined justCalls ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString checkInfo.fn ++ " on a just maybe will result in " ++ qualifiedToString (qualify config.onJustFn checkInfo) ++ " with the value inside"
                                    , details = [ "You can replace this call by " ++ qualifiedToString (qualify config.onJustFn checkInfo) ++ " with the value inside the given just maybe." ]
                                    }
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range maybeArg }
                                        ++ List.map
                                            (\justCall ->
                                                Fix.replaceRangeBy justCall.fnRange
                                                    (qualifiedToString (qualify config.onJustFn checkInfo))
                                            )
                                            justCalls
                                    )
                                )

                        Undetermined ->
                            Nothing
                ]
                ()

        Nothing ->
            Nothing


wrapperFromMaybeCompositionChecks : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
wrapperFromMaybeCompositionChecks wrapper checkInfo =
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


pipelineChecks :
    { commentRanges : List Range
    , extractSourceCode : Range -> String
    , nodeRange : Range
    , pipedInto : Node Expression
    , arg : Node Expression
    , direction : LeftOrRightDirection
    }
    -> Maybe (Error {})
pipelineChecks =
    firstThatConstructsJust
        [ \checkInfo -> pipingIntoCompositionChecks { commentRanges = checkInfo.commentRanges, extractSourceCode = checkInfo.extractSourceCode } checkInfo.direction checkInfo.pipedInto
        , \checkInfo -> fullyAppliedLambdaInPipelineChecks { nodeRange = checkInfo.nodeRange, function = checkInfo.pipedInto, firstArgument = checkInfo.arg }
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
                        , "Please use " ++ replacement ++ " instead as that is more idiomatic in Elm and generally easier to read."
                        ]
                    }
                    error.opToReplaceRange
                    error.fixes
                )


compositionAfterWrapIsUnnecessaryCheck : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
compositionAfterWrapIsUnnecessaryCheck wrapper =
    unnecessaryCompositionAfterCheck wrapper.wrap


callOnWrappedDoesNotChangeItCheck : WrapperProperties otherProperties -> CheckInfo -> Maybe (Error {})
callOnWrappedDoesNotChangeItCheck wrapper =
    callOnDoesNotChangeItCheck
        { description = wrapper.wrap.description
        , is = \lookupTable expr -> isJust (wrapper.wrap.getValue lookupTable expr)
        }


callOnDoesNotChangeItCheck :
    { a
        | description : Description
        , is : ModuleNameLookupTable -> Node Expression -> Bool
    }
    -> CheckInfo
    -> Maybe (Error {})
callOnDoesNotChangeItCheck constructable checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just constructableArg ->
            let
                getConstructable : Node Expression -> Maybe ()
                getConstructable expressionNode =
                    if constructable.is checkInfo.lookupTable expressionNode then
                        Just ()

                    else
                        Nothing
            in
            case sameInAllBranches getConstructable constructableArg of
                Determined _ ->
                    Just
                        (Rule.errorWithFix
                            (operationDoesNotChangeSpecificLastArgErrorInfo { fn = checkInfo.fn, specific = constructable.description })
                            checkInfo.fnRange
                            (keepOnlyFix
                                { parentRange = checkInfo.parentRange
                                , keep = Node.range constructableArg
                                }
                            )
                        )

                Undetermined ->
                    Nothing

        Nothing ->
            Nothing


callOnEmptyReturnsEmptyCheck :
    { a
        | empty :
            { empty
                | is : ModuleNameLookupTable -> Node Expression -> Bool
                , description : Description
            }
    }
    -> CheckInfo
    -> Maybe (Error {})
callOnEmptyReturnsEmptyCheck emptiable =
    callOnDoesNotChangeItCheck emptiable.empty


callOnEmptyReturnsCheck :
    { resultAsString : QualifyResources {} -> String }
    ->
        { a
            | empty :
                { empty
                    | is : ModuleNameLookupTable -> Node Expression -> Bool
                    , description : Description
                }
        }
    -> CheckInfo
    -> Maybe (Error {})
callOnEmptyReturnsCheck config collection checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just lastArg ->
            if collection.empty.is checkInfo.lookupTable lastArg then
                let
                    resultDescription : String
                    resultDescription =
                        config.resultAsString defaultQualifyResources
                in
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on " ++ descriptionForIndefinite collection.empty.description ++ " will result in " ++ resultDescription
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


{-| The operation is equivalent to identity when applied on an empty value:

    operation << empty --> empty

Examples

    Json.Decode.map f << Json.Decode.fail
    --> Json.Decode.fail

    Task.mapError << Task.succeed
    --> Task.succeed

Use together with `callOnEmptyReturnsEmptyCheck`

-}
unnecessaryCompositionAfterEmptyCheck :
    { a
        | empty :
            { empty
                | description : Description
                , fn : ( ModuleName, String )
            }
    }
    -> CompositionIntoCheckInfo
    -> Maybe ErrorInfoAndFix
unnecessaryCompositionAfterEmptyCheck emptiable =
    unnecessaryCompositionAfterCheck emptiable.empty


unnecessaryCompositionAfterCheck :
    { construct
        | description : Description
        , fn : ( ModuleName, String )
    }
    -> CompositionIntoCheckInfo
    -> Maybe ErrorInfoAndFix
unnecessaryCompositionAfterCheck construct checkInfo =
    if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == construct.fn) then
        Just
            { info =
                { message = qualifiedToString checkInfo.later.fn ++ " on " ++ descriptionForIndefinite construct.description ++ " will result in " ++ descriptionForDefinite "the unchanged" construct.description
                , details = [ "You can replace this composition by " ++ qualifiedToString (qualify construct.fn checkInfo) ++ "." ]
                }
            , fix =
                [ Fix.removeRange checkInfo.later.removeRange ]
            }

    else
        Nothing


onlyLastArgIsCurried : { function | args : List (Node Expression), argCount : Int } -> Bool
onlyLastArgIsCurried functionInfo =
    List.length functionInfo.args == (functionInfo.argCount - 1)


{-| This operation is equivalent to identity when called on a wrapped value.

    operation (wrap a) --> a

For example

    List.sum [ a ] --> a

    Cmd.batch [ a ] --> a

Use together with `onWrapAlwaysReturnsIncomingCompositionCheck`

-}
callOnWrapReturnsItsValue :
    { otherProperties
        | wrap : ConstructWithOneArgProperties
    }
    -> CheckInfo
    -> Maybe (Error {})
callOnWrapReturnsItsValue wrapper checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just wrapperArg ->
            case sameInAllBranches (getValueWithNodeRange (wrapper.wrap.getValue checkInfo.lookupTable)) wrapperArg of
                Undetermined ->
                    Nothing

                Determined wraps ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " on " ++ descriptionForIndefinite wrapper.wrap.description ++ " will result in the value inside"
                            , details = [ "You can replace this call by the value inside " ++ descriptionForDefinite "the" wrapper.wrap.description ++ "." ]
                            }
                            checkInfo.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range wrapperArg }
                                ++ List.concatMap (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value) wraps
                            )
                        )

        Nothing ->
            Nothing


{-| This operation is equivalent to identity when called on a wrapped value.

    operation << wrap --> identity

For example

    List.sum << List.singleton --> identity

    Cmd.batch << List.singleton --> identity

Use together with `callOnWrapReturnsItsValue`.

-}
onWrapAlwaysReturnsIncomingCompositionCheck : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
onWrapAlwaysReturnsIncomingCompositionCheck wrapper checkInfo =
    if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == wrapper.wrap.fn) then
        Just
            (compositionAlwaysReturnsIncomingError
                (qualifiedToString (qualify checkInfo.later.fn defaultQualifyResources) ++ " on " ++ descriptionForIndefinite wrapper.wrap.description ++ " will always result in the value inside")
                checkInfo
            )

    else
        Nothing


{-| This operation is equivalent to Just when called on a wrapped value.

    operation (wrap a) --> Just a

For example

    List.minimum [ a ] --> Just a

    Result.toMaybe (Ok a) --> Just a

Use together with `callOnWrapReturnsJustItsValue`.

-}
callOnWrapReturnsJustItsValue :
    { otherProperties
        | wrap : ConstructWithOneArgProperties
    }
    -> CheckInfo
    -> Maybe (Error {})
callOnWrapReturnsJustItsValue withWrap checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just withWrapArg ->
            case sameInAllBranches (getValueWithNodeRange (withWrap.wrap.getValue checkInfo.lookupTable)) withWrapArg of
                Determined wraps ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString checkInfo.fn ++ " on " ++ descriptionForIndefinite withWrap.wrap.description ++ " will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside " ++ descriptionForDefinite "the" withWrap.wrap.description ++ "." ]
                            }
                            checkInfo.fnRange
                            (Fix.removeRange { start = (Node.range withWrapArg).end, end = checkInfo.parentRange.end }
                                :: List.concatMap (\wrap -> replaceBySubExpressionFix wrap.nodeRange wrap.value) wraps
                                ++ [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = (Node.range withWrapArg).start }
                                        (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo) ++ " ")
                                   ]
                            )
                        )

                Undetermined ->
                    Nothing

        Nothing ->
            Nothing


{-| This operation is equivalent to Just when called on a wrapped value.

    operation << wrap --> Just

For example

    List.minimum << List.singleton --> Just

    Result.toMaybe << Ok --> Just

Use together with `callOnWrapReturnsJustItsValue`.

-}
onWrapAlwaysReturnsJustIncomingCompositionCheck : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
onWrapAlwaysReturnsJustIncomingCompositionCheck wrapper checkInfo =
    if onlyLastArgIsCurried checkInfo.later && (checkInfo.earlier.fn == wrapper.wrap.fn) then
        Just
            { info =
                { message = qualifiedToString checkInfo.later.fn ++ " on " ++ descriptionForIndefinite wrapper.wrap.description ++ " will always result in Just the value inside"
                , details = [ "You can replace this call by Just." ]
                }
            , fix = compositionReplaceByFnFix ( [ "Maybe" ], "Just" ) checkInfo
            }

    else
        Nothing


emptiableFilterChecks : EmptiableProperties otherProperties -> CheckInfo -> Maybe (Error {})
emptiableFilterChecks emptiable =
    firstThatConstructsJust
        [ callOnEmptyReturnsEmptyCheck emptiable
        , \checkInfo ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
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
                            { replacement = emptiable.empty.asString }
                            checkInfo
                        )

                Undetermined ->
                    Nothing
        ]


collectionRemoveChecks : CollectionProperties (EmptiableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
collectionRemoveChecks collection =
    callOnEmptyReturnsEmptyCheck collection


collectionIntersectChecks : CollectionProperties (EmptiableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
collectionIntersectChecks collection =
    firstThatConstructsJust
        [ \checkInfo ->
            if collection.empty.is checkInfo.lookupTable checkInfo.firstArg then
                Just
                    (alwaysResultsInUnparenthesizedConstantError
                        (qualifiedToString checkInfo.fn ++ " on " ++ collection.empty.asString defaultQualifyResources)
                        { replacement = collection.empty.asString }
                        checkInfo
                    )

            else
                Nothing
        , callOnEmptyReturnsEmptyCheck collection
        ]


collectionDiffChecks : CollectionProperties (EmptiableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
collectionDiffChecks collection =
    firstThatConstructsJust
        [ \checkInfo ->
            if collection.empty.is checkInfo.lookupTable checkInfo.firstArg then
                Just
                    (alwaysResultsInUnparenthesizedConstantError
                        (qualifiedToString checkInfo.fn ++ " on " ++ emptyAsString checkInfo collection)
                        { replacement = collection.empty.asString }
                        checkInfo
                    )

            else
                Nothing
        , \checkInfo ->
            case secondArg checkInfo of
                Just collectionArg ->
                    if collection.empty.is checkInfo.lookupTable collectionArg then
                        Just
                            (Rule.errorWithFix
                                { message = "Diffing a " ++ collection.represents ++ " with " ++ emptyAsString checkInfo collection ++ " will result in the " ++ collection.represents ++ " itself"
                                , details = [ "You can replace this call by the " ++ collection.represents ++ " itself." ]
                                }
                                checkInfo.fnRange
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
        ]


collectionUnionChecks : CollectionProperties (EmptiableProperties (FromListProperties otherProperties)) -> CheckInfo -> Maybe (Error {})
collectionUnionChecks collection =
    firstThatConstructsJust
        [ \checkInfo ->
            if collection.empty.is checkInfo.lookupTable checkInfo.firstArg then
                Just
                    (alwaysReturnsLastArgError
                        (qualifiedToString checkInfo.fn ++ " " ++ descriptionForIndefinite collection.empty.description)
                        collection
                        checkInfo
                    )

            else
                Nothing
        , \checkInfo ->
            case secondArg checkInfo of
                Just secondArg_ ->
                    if collection.empty.is checkInfo.lookupTable secondArg_ then
                        Just
                            (Rule.errorWithFix
                                { message = "Unnecessary " ++ qualifiedToString (qualify checkInfo.fn defaultQualifyResources) ++ " with " ++ descriptionForIndefinite collection.empty.description
                                , details = [ "You can replace this call by the " ++ collection.represents ++ " itself." ]
                                }
                                checkInfo.fnRange
                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg })
                            )

                    else
                        collectionUnionWithLiteralsChecks
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


collectionUnionWithLiteralsChecks :
    { first : Node Expression
    , second : Node Expression
    , operationRange : Range
    , operation : String
    }
    -> CollectionProperties (FromListProperties otherProperties)
    ->
        { checkInfo
            | lookupTable : ModuleNameLookupTable
            , extractSourceCode : Range -> String
            , parentRange : Range
        }
    -> Maybe (Error {})
collectionUnionWithLiteralsChecks operationInfo collection checkInfo =
    case collection.fromListLiteral.getListRange checkInfo.lookupTable operationInfo.second of
        Just literalListRangeSecond ->
            case collection.fromListLiteral.getListRange checkInfo.lookupTable operationInfo.first of
                Just literalListRangeFirst ->
                    Just
                        (Rule.errorWithFix
                            { message = operationInfo.operation ++ " on " ++ collection.fromListLiteral.description ++ "s can be turned into a single " ++ collection.fromListLiteral.description
                            , details = [ "Try moving all the elements into a single " ++ collection.fromListLiteral.description ++ "." ]
                            }
                            operationInfo.operationRange
                            (if collection.unionLeftElementsStayOnTheLeft then
                                keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range operationInfo.second }
                                    ++ [ Fix.insertAt
                                            (rangeWithoutBoundaries literalListRangeSecond).start
                                            (checkInfo.extractSourceCode (rangeWithoutBoundaries literalListRangeFirst) ++ ",")
                                       ]

                             else
                                keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range operationInfo.first }
                                    ++ [ Fix.insertAt
                                            (rangeWithoutBoundaries literalListRangeFirst).start
                                            (checkInfo.extractSourceCode (rangeWithoutBoundaries literalListRangeSecond) ++ ",")
                                       ]
                            )
                        )

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


collectionInsertChecks : CollectionProperties (EmptiableProperties (WrapperProperties otherProperties)) -> CheckInfo -> Maybe (Error {})
collectionInsertChecks collection checkInfo =
    case secondArg checkInfo of
        Just collectionArg ->
            if collection.empty.is checkInfo.lookupTable collectionArg then
                Just
                    (Rule.errorWithFix
                        { message = "Use " ++ qualifiedToString collection.wrap.fn ++ " instead of inserting in " ++ descriptionForIndefinite collection.empty.description
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


collectionMemberChecks : CollectionProperties (EmptiableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
collectionMemberChecks collection =
    callOnEmptyReturnsCheck
        { resultAsString = \res -> qualifiedToString (qualify ( [ "Basics" ], "False" ) res) }
        collection


collectionIsEmptyChecks : CollectionProperties (EmptiableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
collectionIsEmptyChecks collection checkInfo =
    case collection.size.determine (extractInferResources checkInfo) checkInfo.firstArg of
        Just (Exactly 0) ->
            Just
                (resultsInConstantError
                    (qualifiedToString checkInfo.fn ++ " on " ++ descriptionForIndefinite collection.empty.description)
                    (\res -> qualifiedToString (qualify ( [ "Basics" ], "True" ) res))
                    checkInfo
                )

        Just _ ->
            Just
                (resultsInConstantError
                    (qualifiedToString checkInfo.fn ++ " on this " ++ collection.represents)
                    (\res -> qualifiedToString (qualify ( [ "Basics" ], "False" ) res))
                    checkInfo
                )

        Nothing ->
            Nothing


collectionSizeChecks : CollectionProperties otherProperties -> CheckInfo -> Maybe (Error {})
collectionSizeChecks collection checkInfo =
    case collection.size.determine (extractInferResources checkInfo) checkInfo.firstArg of
        Just (Exactly size) ->
            Just
                (Rule.errorWithFix
                    { message = "The " ++ collection.size.description ++ " of the " ++ collection.represents ++ " is " ++ String.fromInt size
                    , details = [ "The " ++ collection.size.description ++ " of the " ++ collection.represents ++ " can be determined by looking at the code." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange (String.fromInt size) ]
                )

        _ ->
            Nothing


collectionFromListChecks : CollectionProperties (EmptiableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
collectionFromListChecks collection =
    callOnEmptyReturnsCheck { resultAsString = collection.empty.asString } listCollection


wrapperFromListSingletonChecks : WrapperProperties otherProperties -> CheckInfo -> Maybe (Error {})
wrapperFromListSingletonChecks wrapper checkInfo =
    case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
        Nothing ->
            Nothing

        Just listSingleton ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString checkInfo.fn ++ " on a singleton list will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside"
                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside the singleton list." ]
                    }
                    checkInfo.fnRange
                    (replaceBySubExpressionFix (Node.range checkInfo.firstArg) listSingleton.element
                        ++ [ Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify wrapper.wrap.fn checkInfo)) ]
                    )
                )


wrapperFromListSingletonCompositionChecks : WrapperProperties otherProperties -> CompositionIntoCheckInfo -> Maybe ErrorInfoAndFix
wrapperFromListSingletonCompositionChecks wrapper checkInfo =
    case checkInfo.earlier.fn of
        ( [ "List" ], "singleton" ) ->
            Just
                { info =
                    { message = qualifiedToString checkInfo.later.fn ++ " on a singleton list will result in " ++ qualifiedToString wrapper.wrap.fn ++ " with the value inside"
                    , details = [ "You can replace this call by " ++ qualifiedToString wrapper.wrap.fn ++ "." ]
                    }
                , fix =
                    compositionReplaceByFnFix wrapper.wrap.fn checkInfo
                }

        _ ->
            Nothing


emptiableToListChecks :
    { otherProperties
        | empty :
            { empty
                | is : ModuleNameLookupTable -> Node Expression -> Bool
                , description : Description
            }
    }
    -> CheckInfo
    -> Maybe (Error {})
emptiableToListChecks collection =
    callOnEmptyReturnsCheck { resultAsString = listCollection.empty.asString } collection


collectionPartitionChecks : CollectionProperties (EmptiableProperties otherProperties) -> CheckInfo -> Maybe (Error {})
collectionPartitionChecks collection =
    firstThatConstructsJust
        [ callOnEmptyReturnsCheck
            { resultAsString = \res -> "( " ++ collection.empty.asString res ++ ", " ++ collection.empty.asString res ++ " )" }
            collection
        , \checkInfo ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined True ->
                    case secondArg checkInfo of
                        Just (Node listArgRange _) ->
                            Just
                                (Rule.errorWithFix
                                    { message = "All elements will go to the first " ++ collection.represents
                                    , details = [ "Since the predicate function always returns True, the second " ++ collection.represents ++ " will always be " ++ collection.empty.asString defaultQualifyResources ++ "." ]
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
                            , details = [ "Since the predicate function always returns False, the first " ++ collection.represents ++ " will always be " ++ collection.empty.asString defaultQualifyResources ++ "." ]
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
                                            ++ qualifiedToString (qualify ( [ "Tuple" ], "pair" ) checkInfo)
                                            ++ " "
                                            ++ emptyAsString checkInfo collection
                                            ++ ")"
                                        )
                                    ]
                            )
                        )

                Undetermined ->
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
    case AstHelpers.removeParens valueNode of
        Node valueAccessRange (Expression.RecordAccess (Node _ (Expression.FunctionOrValue [] valueHolder)) (Node _ fieldName)) ->
            if field == fieldName && recordVariableName == valueHolder then
                Just { setterRange = setterRange, valueAccessRange = valueAccessRange }

            else
                Nothing

        _ ->
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
                                            (qualifiedToString (qualify ( [ "Basics" ], "not" ) checkInfo) ++ " ")
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
    ]


type alias CaseOfCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , customTypesToReportInCases : Set ( ModuleName, ConstructorName )
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


recordAccessChecks :
    { nodeRange : Range
    , maybeRecordNameRange : Maybe Range
    , fieldName : String
    , setters : List (Node Expression.RecordSetter)
    }
    -> Maybe ErrorInfoAndFix
recordAccessChecks checkInfo =
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
                checkInfo.setters
    in
    case maybeMatchingSetterValue of
        Just setter ->
            Just
                { info =
                    { message = "Field access can be simplified"
                    , details = [ "Accessing the field of a record or record update can be simplified to just that field's value" ]
                    }
                , fix = replaceBySubExpressionFix checkInfo.nodeRange setter
                }

        Nothing ->
            case checkInfo.maybeRecordNameRange of
                Just recordNameRange ->
                    Just
                        { info =
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of an unrelated record update can be simplified to just the original field's value" ]
                            }
                        , fix =
                            [ Fix.replaceRangeBy { start = checkInfo.nodeRange.start, end = recordNameRange.start } ""
                            , Fix.replaceRangeBy { start = recordNameRange.end, end = checkInfo.nodeRange.end } ("." ++ checkInfo.fieldName)
                            ]
                        }

                Nothing ->
                    Nothing


distributeFieldAccess : String -> Range -> List (Node Expression) -> String -> Maybe ErrorInfoAndFix
distributeFieldAccess kind dotFieldRange branches fieldName =
    case returnsRecordInAllBranches branches of
        Just records ->
            Just
                { info =
                    { message = "Field access can be simplified"
                    , details = [ "Accessing the field outside " ++ kind ++ " expression can be simplified to access the field inside it" ]
                    }
                , fix =
                    Fix.removeRange dotFieldRange
                        :: List.concatMap (\leaf -> replaceSubExpressionByRecordAccessFix fieldName leaf) records
                }

        Nothing ->
            Nothing


injectRecordAccessIntoLetExpression : Range -> Node Expression -> String -> ErrorInfoAndFix
injectRecordAccessIntoLetExpression dotFieldRange letBody fieldName =
    { info =
        { message = "Field access can be simplified"
        , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it" ]
        }
    , fix =
        Fix.removeRange dotFieldRange
            :: replaceSubExpressionByRecordAccessFix fieldName letBody
    }


returnsRecordInAllBranches : List (Node Expression) -> Maybe (List (Node Expression))
returnsRecordInAllBranches nodes =
    case Match.traverse (sameInAllBranches getRecordLeafExpression) nodes of
        Match.Determined leaves ->
            Just (List.concat leaves)

        Match.Undetermined ->
            Nothing


getRecordLeafExpression : Node Expression -> Maybe (Node Expression)
getRecordLeafExpression expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.RecordExpr _ ->
            Just expressionNode

        Expression.RecordUpdateExpression _ _ ->
            Just expressionNode

        _ ->
            Nothing



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
    -> CheckInfo
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
    -> CheckInfo
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
                        qualifiedToString (qualify ( [ "Basics" ], "always" ) res) ++ " " ++ addNecessaryParens (config.replacement res)

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
resultsInConstantError : String -> (QualifyResources {} -> String) -> CheckInfo -> Error {}
resultsInConstantError usingSituation replacement checkInfo =
    Rule.errorWithFix
        { message = usingSituation ++ " will result in " ++ replacement defaultQualifyResources
        , details = [ "You can replace this call by " ++ replacement defaultQualifyResources ++ "." ]
        }
        checkInfo.fnRange
        [ Fix.replaceRangeBy checkInfo.parentRange (replacement (extractQualifyResources checkInfo)) ]


operationDoesNotChangeSpecificLastArgErrorInfo : { fn : ( ModuleName, String ), specific : Description } -> { message : String, details : List String }
operationDoesNotChangeSpecificLastArgErrorInfo config =
    let
        specificLastArgReference : String
        specificLastArgReference =
            descriptionForDefinite "the given" config.specific
    in
    { message = qualifiedToString config.fn ++ " on " ++ descriptionForIndefinite config.specific ++ " will result in " ++ specificLastArgReference
    , details = [ "You can replace this call by " ++ specificLastArgReference ++ "." ]
    }


{-| In your specific situation, the last incoming argument will always be returned unchanged.

For example, `List.map identity` will not change whatever list comes next. It is equivalent to `identity`

Use `returnsArgError` with the given last arg as `arg` when the last arg is already present.

-}
alwaysReturnsLastArgError :
    String
    -> { b | represents : String }
    -> QualifyResources { a | fnRange : Range, parentRange : Range, argCount : Int, firstArg : Node Expression, argsAfterFirst : List (Node Expression) }
    -> Error {}
alwaysReturnsLastArgError usingSpecificSituation config checkInfo =
    case fullyAppliedLastArg checkInfo of
        Just lastArg ->
            returnsArgError usingSpecificSituation { arg = lastArg, argRepresents = config.represents } checkInfo

        Nothing ->
            -- Not enough arguments
            let
                replacement : { description : String, fix : List Fix }
                replacement =
                    case checkInfo.argCount - List.length checkInfo.argsAfterFirst - 1 of
                        1 ->
                            { description = "identity"
                            , fix =
                                [ Fix.replaceRangeBy checkInfo.parentRange
                                    (qualifiedToString (qualify ( [ "Basics" ], "identity" ) checkInfo))
                                ]
                            }

                        2 ->
                            { description = "always identity"
                            , fix =
                                [ Fix.replaceRangeBy checkInfo.parentRange
                                    (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo) ++ " " ++ qualifiedToString (qualify ( [ "Basics" ], "identity" ) checkInfo))
                                ]
                            }

                        _ ->
                            -- Use-case is absent for now
                            { description = "the " ++ config.represents ++ " argument"
                            , fix = []
                            }
            in
            Rule.errorWithFix
                { message = usingSpecificSituation ++ " will always return the same given " ++ config.represents
                , details =
                    [ "You can replace this call by " ++ replacement.description ++ "." ]
                }
                checkInfo.fnRange
                replacement.fix


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


{-| Use in combination with
`findMapNeighboring` where finding returns a record containing the element's Range
Works for patterns and expressions.
-}
listLiteralElementRemoveFix : { before : Maybe (Node element), found : { found | range : Range }, after : Maybe (Node element) } -> List Fix
listLiteralElementRemoveFix toRemove =
    case ( toRemove.before, toRemove.after ) of
        -- found the only element
        ( Nothing, Nothing ) ->
            [ Fix.removeRange toRemove.found.range ]

        -- found first element
        ( Nothing, Just (Node afterRange _) ) ->
            [ Fix.removeRange
                { start = toRemove.found.range.start
                , end = afterRange.start
                }
            ]

        -- found after first element
        ( Just (Node beforeRange _), _ ) ->
            [ Fix.removeRange
                { start = beforeRange.end
                , end = toRemove.found.range.end
                }
            ]


{-| Use in combination with
`findMapNeighboring` where finding returns a record containing the element's Range
Works for patterns and expressions.
-}
collapsedConsRemoveElementFix :
    { toRemove : { before : Maybe (Node element), after : Maybe (Node element), found : { found | range : Range } }
    , tailRange : Range
    }
    -> List Fix
collapsedConsRemoveElementFix config =
    case ( config.toRemove.before, config.toRemove.after ) of
        -- found the only consed element
        ( Nothing, Nothing ) ->
            [ Fix.removeRange
                { start = config.toRemove.found.range.start, end = config.tailRange.start }
            ]

        -- found first consed element
        ( Nothing, Just (Node afterRange _) ) ->
            [ Fix.removeRange
                { start = config.toRemove.found.range.start
                , end = afterRange.start
                }
            ]

        -- found after first consed element
        ( Just (Node beforeRange _), _ ) ->
            [ Fix.removeRange
                { start = beforeRange.end
                , end = config.toRemove.found.range.end
                }
            ]



-- STRING


wrapInBackticks : String -> String
wrapInBackticks s =
    "`" ++ s ++ "`"



-- MATCHERS AND PARSERS


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
    (Node Expression -> Match specific)
    -> ModuleNameLookupTable
    -> Node Expression
    -> Match specific
constructs getSpecific lookupTable expressionNode =
    case AstHelpers.getSpecificFnCall ( [ "Basics" ], "always" ) lookupTable expressionNode of
        Just alwaysCall ->
            getSpecific alwaysCall.firstArg

        Nothing ->
            case Node.value (AstHelpers.removeParens expressionNode) of
                Expression.LambdaExpression lambda ->
                    case lambda.args of
                        _ :: [] ->
                            getSpecific lambda.expression

                        _ ->
                            Undetermined

                _ ->
                    Undetermined


sameInAllBranches :
    (Node Expression -> Maybe info)
    -> Node Expression
    -> Match (List info)
sameInAllBranches getSpecific baseExpressionNode =
    case getSpecific baseExpressionNode of
        Just specific ->
            Determined [ specific ]

        Nothing ->
            case Node.value (AstHelpers.removeParens baseExpressionNode) of
                Expression.LetExpression letIn ->
                    sameInAllBranches getSpecific letIn.expression

                Expression.IfBlock _ thenBranch elseBranch ->
                    Match.traverse
                        (\branchExpression -> sameInAllBranches getSpecific branchExpression)
                        [ thenBranch, elseBranch ]
                        |> Match.map List.concat

                Expression.CaseExpression caseOf ->
                    Match.traverse
                        (\( _, caseExpression ) -> sameInAllBranches getSpecific caseExpression)
                        caseOf.cases
                        |> Match.map List.concat

                _ ->
                    Undetermined


getComparableExpressionInTupleFirst : Node Expression -> Maybe (List Expression)
getComparableExpressionInTupleFirst expressionNode =
    case AstHelpers.getTuple2Literal expressionNode of
        Just tuple ->
            getComparableExpression tuple.first

        Nothing ->
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
    uniqueHelp [] list []


uniqueHelp : List a -> List a -> List a -> List a
uniqueHelp existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            if List.member first existing then
                uniqueHelp existing rest accumulator

            else
                uniqueHelp (first :: existing) rest (first :: accumulator)



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
