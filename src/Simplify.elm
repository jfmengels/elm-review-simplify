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

    (\x y -> x + y) n m
    -- Reported because simplifiable but not autofixed


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


### Strings

    "a" ++ ""
    --> "a"

    String.fromList []
    --> ""

    String.fromList [ a ]
    --> String.fromChar a

    String.isEmpty ""
    --> True

    String.isEmpty "a"
    --> False

    String.concat []
    --> ""

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


### Maybes

    Maybe.map identity x
    --> x

    Maybe.map f Nothing
    --> Nothing

    Maybe.map f (Just x)
    --> Just (f x)

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

    List.reverse (List.reverse list)
    --> list

    List.sortBy (always a) list
    --> list

    List.sortBy identity list
    --> List.sort list

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


### Sets

    Set.map f Set.empty -- same for Set.filter, Set.remove...
    --> Set.empty

    Set.map identity set
    --> set

    Set.isEmpty Set.empty
    --> True

    Set.member x Set.empty
    --> False

    Set.fromList []
    --> Set.empty

    Set.fromList [ a ]
    --> Set.singleton a

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

    Set.insert x Set.empty
    --> Set.singleton x

    -- same for foldr
    List.foldl f x (Set.toList set)
    --> Set.foldl f x set

    Set.partition f Set.empty
    --> ( Set.empty, Set.empty )

    Set.partition (always True) set
    --> ( set, Set.empty )


### Dict

    Dict.isEmpty Dict.empty
    --> True

    Dict.fromList []
    --> Dict.empty

    Dict.toList Dict.empty
    --> []

    Dict.size Dict.empty
    --> 0

    Dict.member x Dict.empty
    --> False

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


### Html.Attributes

    Html.Attributes.classList [ x, y, ( z, False ) ]
    --> Html.Attributes.classList [ x, y ]

    Html.Attributes.classList [ ( onlyOneThing, True ) ]
    --> Html.Attributes.class onlyOneThing


### Json.Decode

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


firstThatReportsError : List (a -> Maybe (Error {})) -> a -> Maybe (Error {})
firstThatReportsError remainingChecks data =
    findMap (\checkFn -> checkFn data) remainingChecks


expressionVisitorHelp : Node Expression -> { config | expectNaN : Bool } -> ModuleContext -> { error : Maybe (Error {}), rangesToIgnore : RangeDict (), rightSidesOfPlusPlus : RangeDict (), inferredConstants : List ( Range, Infer.Inferred ) }
expressionVisitorHelp (Node expressionRange expression) config context =
    let
        toCheckInfo :
            { fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            , usingRightPizza : Bool
            }
            -> CheckInfo
        toCheckInfo checkInfo =
            { lookupTable = context.lookupTable
            , expectNaN = config.expectNaN
            , extractSourceCode = context.extractSourceCode
            , importLookup = context.importLookup
            , commentRanges = context.commentRanges
            , moduleBindings = context.moduleBindings
            , localBindings = context.localBindings
            , inferredConstants = context.inferredConstants
            , parentRange = expressionRange
            , fnRange = checkInfo.fnRange
            , firstArg = checkInfo.firstArg
            , argsAfterFirst = checkInfo.argsAfterFirst
            , secondArg = List.head checkInfo.argsAfterFirst
            , thirdArg = List.head (List.drop 1 checkInfo.argsAfterFirst)
            , usingRightPizza = checkInfo.usingRightPizza
            }

        toCompositionCheckInfo :
            { direction : LeftOrRightDirection
            , earlier : Node Expression
            , later : Node Expression
            , parentRange : Range
            }
            -> CompositionCheckInfo
        toCompositionCheckInfo compositionSpecific =
            { lookupTable = context.lookupTable
            , importLookup = context.importLookup
            , moduleBindings = context.moduleBindings
            , localBindings = context.localBindings
            , direction = compositionSpecific.direction
            , parentRange = compositionSpecific.parentRange
            , earlier = compositionSpecific.earlier
            , later = compositionSpecific.later
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
                                    Just checkFn ->
                                        checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsAfterFirst
                                                , usingRightPizza = False
                                                }
                                            )

                                    Nothing ->
                                        Nothing

                            Nothing ->
                                Nothing

                    Node _ (Expression.ParenthesizedExpression (Node lambdaRange (Expression.LambdaExpression lambda))) ->
                        Just
                            (appliedLambdaError
                                { nodeRange = expressionRange
                                , lambdaRange = lambdaRange
                                , lambda = lambda
                                }
                            )

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
                                    Just checkFn ->
                                        checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , firstArg = lastArg
                                                , argsAfterFirst = []
                                                , usingRightPizza = False
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
                                Just checkFn ->
                                    maybeErrorAndRangesToIgnore
                                        (checkFn
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                                , usingRightPizza = False
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
                                    Just checks ->
                                        checks
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , firstArg = lastArg
                                                , argsAfterFirst = []
                                                , usingRightPizza = True
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
                                Just checks ->
                                    maybeErrorAndRangesToIgnore
                                        (checks
                                            (toCheckInfo
                                                { fnRange = fnRange
                                                , firstArg = firstArg
                                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                                , usingRightPizza = True
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
            let
                ( later, parentRange ) =
                    case composedLater of
                        Node _ (Expression.OperatorApplication ">>" _ later_ _) ->
                            ( later_, { start = (Node.range earlier).start, end = (Node.range later_).end } )

                        endLater ->
                            ( endLater, expressionRange )
            in
            onlyMaybeError
                (firstThatReportsError compositionChecks
                    (toCompositionCheckInfo
                        { direction = LeftToRight
                        , parentRange = parentRange
                        , earlier = earlier
                        , later = later
                        }
                    )
                )

        ----------
        -- (<<) --
        ----------
        Expression.OperatorApplication "<<" _ composedLater earlier ->
            let
                ( later, parentRange ) =
                    case composedLater of
                        Node _ (Expression.OperatorApplication "<<" _ _ later_) ->
                            ( later_, { start = (Node.range later_).start, end = (Node.range earlier).end } )

                        endLater ->
                            ( endLater, expressionRange )
            in
            onlyMaybeError
                (firstThatReportsError compositionChecks
                    (toCompositionCheckInfo
                        { direction = RightToLeft
                        , parentRange = parentRange
                        , earlier = earlier
                        , later = later
                        }
                    )
                )

        ---------------------
        -- OTHER OPERATION --
        ---------------------
        Expression.OperatorApplication operator _ left right ->
            case Dict.get operator operatorChecks of
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
        Expression.RecordAccess record field ->
            onlyMaybeError
                (case Node.value (AstHelpers.removeParens record) of
                    Expression.RecordExpr setters ->
                        recordAccessChecks
                            { nodeRange = expressionRange
                            , maybeRecordNameRange = Nothing
                            , fieldName = Node.value field
                            , setters = setters
                            }

                    Expression.RecordUpdateExpression (Node recordNameRange _) setters ->
                        recordAccessChecks
                            { nodeRange = expressionRange
                            , maybeRecordNameRange = Just recordNameRange
                            , fieldName = Node.value field
                            , setters = setters
                            }

                    Expression.LetExpression letIn ->
                        Just (injectRecordAccessIntoLetExpression (Node.range record) letIn.expression field)

                    Expression.IfBlock _ thenBranch elseBranch ->
                        distributeFieldAccess "an if/then/else" (Node.range record) [ thenBranch, elseBranch ] field

                    Expression.CaseExpression caseOf ->
                        distributeFieldAccess "a case/of" (Node.range record) (List.map Tuple.second caseOf.cases) field

                    _ ->
                        Nothing
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
                (firstThatReportsError caseOfChecks
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
            onlyMaybeError (removeRecordFields expressionRange variable fields)

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
    , usingRightPizza : Bool
    , firstArg : Node Expression
    , argsAfterFirst : List (Node Expression)

    -- stored for quick access since usage is very common
    -- prefer using secondArg and thirdArg functions
    -- because the optimization could change in the future
    , secondArg : Maybe (Node Expression)
    , thirdArg : Maybe (Node Expression)
    }


secondArg : CheckInfo -> Maybe (Node Expression)
secondArg checkInfo =
    checkInfo.secondArg


thirdArg : CheckInfo -> Maybe (Node Expression)
thirdArg checkInfo =
    checkInfo.thirdArg


functionCallChecks : Dict ( ModuleName, String ) (CheckInfo -> Maybe (Error {}))
functionCallChecks =
    Dict.fromList
        [ ( ( [ "Basics" ], "identity" ), basicsIdentityChecks )
        , ( ( [ "Basics" ], "always" ), basicsAlwaysChecks )
        , ( ( [ "Basics" ], "not" ), basicsNotChecks )
        , ( ( [ "Basics" ], "negate" ), basicsNegateChecks )
        , ( ( [ "Maybe" ], "map" ), maybeMapChecks )
        , ( ( [ "Maybe" ], "andThen" ), maybeAndThenChecks )
        , ( ( [ "Maybe" ], "withDefault" ), maybeWithDefaultChecks )
        , ( ( [ "Result" ], "map" ), resultMapChecks )
        , ( ( [ "Result" ], "mapError" ), resultMapErrorChecks )
        , ( ( [ "Result" ], "andThen" ), resultAndThenChecks )
        , ( ( [ "Result" ], "withDefault" ), resultWithDefaultChecks )
        , ( ( [ "Result" ], "toMaybe" ), resultToMaybeChecks )
        , ( ( [ "List" ], "append" ), listAppendChecks )
        , ( ( [ "List" ], "head" ), listHeadChecks )
        , ( ( [ "List" ], "tail" ), listTailChecks )
        , ( ( [ "List" ], "member" ), listMemberChecks )
        , ( ( [ "List" ], "map" ), listMapChecks )
        , ( ( [ "List" ], "filter" ), collectionFilterChecks listCollection )
        , ( ( [ "List" ], "filterMap" ), listFilterMapChecks )
        , ( ( [ "List" ], "concat" ), listConcatChecks )
        , ( ( [ "List" ], "concatMap" ), listConcatMapChecks )
        , ( ( [ "List" ], "indexedMap" ), listIndexedMapChecks )
        , ( ( [ "List" ], "intersperse" ), listIntersperseChecks )
        , ( ( [ "List" ], "sum" ), listSumChecks )
        , ( ( [ "List" ], "product" ), listProductChecks )
        , ( ( [ "List" ], "minimum" ), listMinimumChecks )
        , ( ( [ "List" ], "maximum" ), listMaximumChecks )
        , ( ( [ "List" ], "foldl" ), listFoldlChecks )
        , ( ( [ "List" ], "foldr" ), listFoldrChecks )
        , ( ( [ "List" ], "all" ), listAllChecks )
        , ( ( [ "List" ], "any" ), listAnyChecks )
        , ( ( [ "List" ], "range" ), listRangeChecks )
        , ( ( [ "List" ], "length" ), collectionSizeChecks listCollection )
        , ( ( [ "List" ], "repeat" ), listRepeatChecks )
        , ( ( [ "List" ], "isEmpty" ), collectionIsEmptyChecks listCollection )
        , ( ( [ "List" ], "partition" ), collectionPartitionChecks listCollection )
        , ( ( [ "List" ], "reverse" ), listReverseChecks )
        , ( ( [ "List" ], "sort" ), listSortChecks )
        , ( ( [ "List" ], "sortBy" ), listSortByChecks )
        , ( ( [ "List" ], "sortWith" ), listSortWithChecks )
        , ( ( [ "List" ], "take" ), listTakeChecks )
        , ( ( [ "List" ], "drop" ), listDropChecks )
        , ( ( [ "List" ], "map2" ), listMapNChecks { n = 2 } )
        , ( ( [ "List" ], "map3" ), listMapNChecks { n = 3 } )
        , ( ( [ "List" ], "map4" ), listMapNChecks { n = 4 } )
        , ( ( [ "List" ], "map5" ), listMapNChecks { n = 5 } )
        , ( ( [ "List" ], "unzip" ), listUnzipChecks )
        , ( ( [ "Set" ], "map" ), containerMapChecks setCollection )
        , ( ( [ "Set" ], "filter" ), collectionFilterChecks setCollection )
        , ( ( [ "Set" ], "remove" ), collectionRemoveChecks setCollection )
        , ( ( [ "Set" ], "isEmpty" ), collectionIsEmptyChecks setCollection )
        , ( ( [ "Set" ], "size" ), collectionSizeChecks setCollection )
        , ( ( [ "Set" ], "member" ), collectionMemberChecks setCollection )
        , ( ( [ "Set" ], "fromList" ), setFromListChecks )
        , ( ( [ "Set" ], "toList" ), collectionToListChecks setCollection )
        , ( ( [ "Set" ], "partition" ), collectionPartitionChecks setCollection )
        , ( ( [ "Set" ], "intersect" ), collectionIntersectChecks setCollection )
        , ( ( [ "Set" ], "diff" ), collectionDiffChecks setCollection )
        , ( ( [ "Set" ], "union" ), collectionUnionChecks setCollection )
        , ( ( [ "Set" ], "insert" ), collectionInsertChecks setCollection )
        , ( ( [ "Dict" ], "isEmpty" ), collectionIsEmptyChecks dictCollection )
        , ( ( [ "Dict" ], "fromList" ), collectionFromListChecks dictCollection )
        , ( ( [ "Dict" ], "toList" ), collectionToListChecks dictCollection )
        , ( ( [ "Dict" ], "size" ), collectionSizeChecks dictCollection )
        , ( ( [ "Dict" ], "member" ), collectionMemberChecks dictCollection )
        , ( ( [ "Dict" ], "partition" ), collectionPartitionChecks dictCollection )
        , ( ( [ "String" ], "fromList" ), stringFromListChecks )
        , ( ( [ "String" ], "isEmpty" ), stringIsEmptyChecks )
        , ( ( [ "String" ], "concat" ), stringConcatChecks )
        , ( ( [ "String" ], "join" ), stringJoinChecks )
        , ( ( [ "String" ], "length" ), stringLengthChecks )
        , ( ( [ "String" ], "repeat" ), stringRepeatChecks )
        , ( ( [ "String" ], "replace" ), stringReplaceChecks )
        , ( ( [ "String" ], "words" ), stringWordsChecks )
        , ( ( [ "String" ], "lines" ), stringLinesChecks )
        , ( ( [ "String" ], "reverse" ), stringReverseChecks )
        , ( ( [ "String" ], "slice" ), stringSliceChecks )
        , ( ( [ "String" ], "left" ), stringLeftChecks )
        , ( ( [ "String" ], "right" ), stringRightChecks )
        , ( ( [ "Platform", "Cmd" ], "batch" ), subAndCmdBatchChecks "Cmd" )
        , ( ( [ "Platform", "Cmd" ], "map" ), containerMapChecks cmdCollection )
        , ( ( [ "Platform", "Sub" ], "batch" ), subAndCmdBatchChecks "Sub" )
        , ( ( [ "Platform", "Sub" ], "map" ), containerMapChecks subCollection )
        , ( ( [ "Json", "Decode" ], "oneOf" ), oneOfChecks )
        , ( ( [ "Html", "Attributes" ], "classList" ), htmlAttributesClassListChecks )
        , ( ( [ "Parser" ], "oneOf" ), oneOfChecks )
        , ( ( [ "Parser", "Advanced" ], "oneOf" ), oneOfChecks )
        , ( ( [ "Random" ], "uniform" ), randomUniformChecks )
        , ( ( [ "Random" ], "weighted" ), randomWeightedChecks )
        , ( ( [ "Random" ], "list" ), randomListChecks )
        , ( ( [ "Random" ], "map" ), randomMapChecks )
        ]


type alias OperatorCheckInfo =
    { lookupTable : ModuleNameLookupTable
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


operatorChecks : Dict String (OperatorCheckInfo -> Maybe (Error {}))
operatorChecks =
    Dict.fromList
        [ ( "+", plusChecks )
        , ( "-", minusChecks )
        , ( "*", multiplyChecks )
        , ( "/", divisionChecks )
        , ( "++", plusplusChecks )
        , ( "::", consChecks )
        , ( "||", orChecks )
        , ( "&&", andChecks )
        , ( "==", equalityChecks True )
        , ( "/=", equalityChecks False )
        , ( "<", comparisonChecks (<) )
        , ( ">", comparisonChecks (>) )
        , ( "<=", comparisonChecks (<=) )
        , ( ">=", comparisonChecks (>=) )
        ]


type alias CompositionCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , importLookup : ImportLookup
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , direction : LeftOrRightDirection
    , parentRange : Range
    , earlier : Node Expression
    , later : Node Expression
    }


compositionChecks : List (CompositionCheckInfo -> Maybe (Error {}))
compositionChecks =
    [ basicsIdentityCompositionChecks
    , basicsNotCompositionChecks
    , basicsNegateCompositionChecks
    , \checkInfo ->
        case
            ( AstHelpers.getValueOrFunctionOrFunctionCall checkInfo.earlier
            , AstHelpers.getValueOrFunctionOrFunctionCall checkInfo.later
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
                            Just compositionIntoChecksForSpecificLater ->
                                compositionIntoChecksForSpecificLater
                                    { lookupTable = checkInfo.lookupTable
                                    , importLookup = checkInfo.importLookup
                                    , moduleBindings = checkInfo.moduleBindings
                                    , localBindings = checkInfo.localBindings
                                    , direction = checkInfo.direction
                                    , parentRange = checkInfo.parentRange
                                    , later =
                                        { range = laterFnOrCall.nodeRange
                                        , fnRange = laterFnOrCall.fnRange
                                        , args = laterFnOrCall.args
                                        }
                                    , earlier =
                                        { range = earlierFnOrCall.nodeRange
                                        , fn = ( earlierFnModuleName, earlierFnOrCall.fnName )
                                        , fnRange = earlierFnOrCall.fnRange
                                        , args = earlierFnOrCall.args
                                        }
                                    }

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
    , moduleBindings : Set String
    , localBindings : RangeDict (Set String)
    , direction : LeftOrRightDirection
    , parentRange : Range
    , later :
        { range : Range
        , fnRange : Range
        , args : List (Node Expression)
        }
    , earlier :
        { range : Range
        , fn : ( ModuleName, String )
        , fnRange : Range
        , args : List (Node Expression)
        }
    }


compositionIntoChecks : Dict ( ModuleName, String ) (CompositionIntoCheckInfo -> Maybe (Error {}))
compositionIntoChecks =
    Dict.fromList
        [ ( ( [ "Basics" ], "always" ), basicsAlwaysCompositionChecks )
        , ( ( [ "Maybe" ], "map" ), maybeMapCompositionChecks )
        , ( ( [ "Result" ], "map" ), resultMapCompositionChecks )
        , ( ( [ "Result" ], "mapError" ), resultMapErrorCompositionChecks )
        , ( ( [ "Result" ], "toMaybe" ), resultToMaybeCompositionChecks )
        , ( ( [ "List" ], "map" ), listMapCompositionChecks )
        , ( ( [ "List" ], "filterMap" ), listFilterMapCompositionChecks )
        , ( ( [ "List" ], "concat" ), listConcatCompositionChecks )
        , ( ( [ "List" ], "foldl" ), listFoldlCompositionChecks )
        , ( ( [ "List" ], "foldr" ), listFoldrCompositionChecks )
        , ( ( [ "Set" ], "fromList" ), setFromListCompositionChecks )
        , ( ( [ "Random" ], "map" ), randomMapCompositionChecks )
        ]


removeAlongWithOtherFunctionCheck :
    { message : String, details : List String }
    -> (ModuleNameLookupTable -> Node Expression -> Maybe Range)
    -> CheckInfo
    -> Maybe (Error {})
removeAlongWithOtherFunctionCheck errorMessage secondFunctionCheck checkInfo =
    case Node.value (AstHelpers.removeParens checkInfo.firstArg) of
        Expression.Application (secondFn :: firstArgOfSecondCall :: _) ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    Just
                        (Rule.errorWithFix
                            errorMessage
                            (Range.combine [ checkInfo.fnRange, secondRange ])
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg }
                                ++ replaceBySubExpressionFix (Node.range checkInfo.firstArg)
                                    firstArgOfSecondCall
                            )
                        )

                Nothing ->
                    Nothing

        Expression.OperatorApplication "|>" _ firstArgOfSecondCall secondFn ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    Just
                        (Rule.errorWithFix
                            errorMessage
                            (Range.combine [ checkInfo.fnRange, secondRange ])
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg }
                                ++ replaceBySubExpressionFix (Node.range checkInfo.firstArg)
                                    firstArgOfSecondCall
                            )
                        )

                Nothing ->
                    Nothing

        Expression.OperatorApplication "<|" _ secondFn firstArgOfSecondCall ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    Just
                        (Rule.errorWithFix
                            errorMessage
                            (Range.combine [ checkInfo.fnRange, secondRange ])
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg }
                                ++ replaceBySubExpressionFix (Node.range checkInfo.firstArg)
                                    firstArgOfSecondCall
                            )
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


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
plusChecks checkInfo =
    firstThatReportsError
        [ addingZeroCheck
        , addingOppositesCheck
        ]
        checkInfo


addingZeroCheck : OperatorCheckInfo -> Maybe (Error {})
addingZeroCheck checkInfo =
    findMap
        (\side ->
            if AstHelpers.getUncomputedNumberValue side.node == Just 0 then
                Just
                    (Rule.errorWithFix
                        { message = "Unnecessary addition with 0"
                        , details = [ "Adding 0 does not change the value of the number." ]
                        }
                        side.errorRange
                        [ Fix.removeRange side.removeRange ]
                    )

            else
                Nothing
        )
        (operationToSides checkInfo)


addingOppositesCheck : OperatorCheckInfo -> Maybe (Error {})
addingOppositesCheck checkInfo =
    if checkInfo.expectNaN then
        Nothing

    else
        case Normalize.compare checkInfo checkInfo.left (Node Range.emptyRange (Expression.Negation checkInfo.right)) of
            Normalize.ConfirmedEquality ->
                Just
                    (Rule.errorWithFix
                        { message = "Addition always results in 0"
                        , details = [ "These two expressions have an equal absolute value but an opposite sign. This means adding them they will cancel out to 0." ]
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
                { message = "Unnecessary subtraction with 0"
                , details = [ "Subtracting 0 does not change the value of the number." ]
                }
                (errorToRightRange checkInfo)
                [ Fix.removeRange (fixToRightRange checkInfo) ]
            )

    else if AstHelpers.getUncomputedNumberValue checkInfo.left == Just 0 then
        let
            replacedRange : Range
            replacedRange =
                fixToLeftRange checkInfo
        in
        Just
            (Rule.errorWithFix
                { message = "Unnecessary subtracting from 0"
                , details = [ "You can negate the expression on the right like `-n`." ]
                }
                (errorToLeftRange checkInfo)
                (if needsParens (Node.value checkInfo.right) then
                    [ Fix.replaceRangeBy replacedRange "-(", Fix.insertAt checkInfo.rightRange.end ")" ]

                 else
                    [ Fix.replaceRangeBy replacedRange "-" ]
                )
            )

    else if checkInfo.expectNaN then
        Nothing

    else
        checkIfMinusResultsInZero checkInfo


checkIfMinusResultsInZero : OperatorCheckInfo -> Maybe (Error {})
checkIfMinusResultsInZero checkInfo =
    case Normalize.compare checkInfo checkInfo.left checkInfo.right of
        Normalize.ConfirmedEquality ->
            Just
                (Rule.errorWithFix
                    { message = "Subtraction always results in 0"
                    , details = [ "These two expressions have the same value, which means they will cancel add when subtracting one by the other." ]
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
                                { message = "Unnecessary multiplication by 1"
                                , details = [ "Multiplying by 1 does not change the value of the number." ]
                                }
                                side.errorRange
                                [ Fix.removeRange side.removeRange ]
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
                                side.errorRange
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
        (operationToSides checkInfo)


operationToSides : OperatorCheckInfo -> List { node : Node Expression, removeRange : Range, errorRange : Range }
operationToSides checkInfo =
    [ { node = checkInfo.right
      , removeRange = fixToRightRange checkInfo
      , errorRange = errorToRightRange checkInfo
      }
    , { node = checkInfo.left
      , removeRange = fixToLeftRange checkInfo
      , errorRange = errorToLeftRange checkInfo
      }
    ]


fixToLeftRange : { checkInfo | leftRange : Range, rightRange : Range } -> Range
fixToLeftRange checkInfo =
    { start = checkInfo.leftRange.start, end = checkInfo.rightRange.start }


errorToLeftRange : { checkInfo | leftRange : Range, operatorRange : Range } -> Range
errorToLeftRange checkInfo =
    { start = checkInfo.leftRange.start, end = checkInfo.operatorRange.end }


fixToRightRange : { checkInfo | leftRange : Range, rightRange : Range } -> Range
fixToRightRange checkInfo =
    { start = checkInfo.leftRange.end, end = checkInfo.rightRange.end }


errorToRightRange : { checkInfo | rightRange : Range, operatorRange : Range } -> Range
errorToRightRange checkInfo =
    { start = checkInfo.operatorRange.start, end = checkInfo.rightRange.end }


divisionChecks : OperatorCheckInfo -> Maybe (Error {})
divisionChecks checkInfo =
    if AstHelpers.getUncomputedNumberValue checkInfo.right == Just 1 then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary division by 1"
                , details = [ "Dividing by 1 does not change the value of the number." ]
                }
                (errorToRightRange checkInfo)
                [ Fix.removeRange (fixToRightRange checkInfo) ]
            )

    else if not checkInfo.expectNaN && (AstHelpers.getUncomputedNumberValue checkInfo.left == Just 0) then
        Just
            (Rule.errorWithFix
                { message = "Dividing 0 always returns 0"
                , details =
                    [ "Dividing 0 by anything, even infinite numbers, gives 0 which means you can replace the whole division operation by 0."
                    , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                    ]
                }
                (errorToLeftRange checkInfo)
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.leftRange })
            )

    else
        Nothing


plusplusChecks : OperatorCheckInfo -> Maybe (Error {})
plusplusChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case ( Node.value checkInfo.left, Node.value checkInfo.right ) of
                ( Expression.Literal "", Expression.Literal _ ) ->
                    Just
                        (Rule.errorWithFix
                            (concatenateEmptyErrorInfo { represents = "string" })
                            checkInfo.operatorRange
                            (keepOnlyFix
                                { keep = checkInfo.rightRange
                                , parentRange = checkInfo.parentRange
                                }
                            )
                        )

                ( Expression.Literal _, Expression.Literal "" ) ->
                    Just
                        (Rule.errorWithFix
                            (concatenateEmptyErrorInfo { represents = "string" })
                            checkInfo.operatorRange
                            (keepOnlyFix
                                { keep = checkInfo.leftRange
                                , parentRange = checkInfo.parentRange
                                }
                            )
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListLiteral checkInfo.left of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            (concatenateEmptyErrorInfo { represents = "list" })
                            checkInfo.operatorRange
                            (keepOnlyFix
                                { keep = checkInfo.rightRange
                                , parentRange = checkInfo.parentRange
                                }
                            )
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListLiteral checkInfo.right of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            (concatenateEmptyErrorInfo { represents = "list" })
                            checkInfo.operatorRange
                            (keepOnlyFix
                                { keep = checkInfo.leftRange
                                , parentRange = checkInfo.parentRange
                                }
                            )
                        )

                _ ->
                    Nothing
        , \() ->
            case ( AstHelpers.getListLiteral checkInfo.left, AstHelpers.getListLiteral checkInfo.right ) of
                ( Just _, Just _ ) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            }
                            checkInfo.operatorRange
                            [ Fix.replaceRangeBy
                                { start = endWithoutBoundary checkInfo.leftRange
                                , end = startWithoutBoundary checkInfo.rightRange
                                }
                                ","
                            ]
                        )

                ( Nothing, _ ) ->
                    Nothing

                ( _, Nothing ) ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.left of
                Just leftListSingleton ->
                    if checkInfo.isOnTheRightSideOfPlusPlus then
                        Nothing

                    else
                        Just
                            (Rule.errorWithFix
                                { message = "Should use (::) instead of (++)"
                                , details = [ "Concatenating a list with a single value is the same as using (::) on the list with the value." ]
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
        ()


concatenateEmptyErrorInfo : { represents : String } -> { message : String, details : List String }
concatenateEmptyErrorInfo config =
    { message = "Unnecessary concatenation with an empty " ++ config.represents
    , details = [ "You should remove the concatenation with the empty " ++ config.represents ++ "." ]
    }


consChecks : OperatorCheckInfo -> Maybe (Error {})
consChecks checkInfo =
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


toggleCompositionChecks : ( ModuleName, String ) -> CompositionCheckInfo -> Maybe (Error {})
toggleCompositionChecks toggle checkInfo =
    let
        getToggleFn : Node Expression -> Maybe Range
        getToggleFn =
            AstHelpers.getSpecificValueOrFunction toggle checkInfo.lookupTable

        maybeEarlierToggleFn : Maybe Range
        maybeEarlierToggleFn =
            getToggleFn checkInfo.earlier

        maybeLaterToggleFn : Maybe Range
        maybeLaterToggleFn =
            getToggleFn checkInfo.later

        getToggleComposition : { earlierToLater : Bool } -> Node Expression -> Maybe { removeFix : List Fix, range : Range }
        getToggleComposition takeFirstFunction expressionNode =
            case AstHelpers.getComposition expressionNode of
                Just composition ->
                    if takeFirstFunction.earlierToLater then
                        getToggleFn composition.earlier
                            |> Maybe.map
                                (\toggleFn ->
                                    { range = toggleFn
                                    , removeFix = keepOnlyFix { parentRange = composition.parentRange, keep = Node.range composition.later }
                                    }
                                )

                    else
                        getToggleFn composition.later
                            |> Maybe.map
                                (\toggleFn ->
                                    { range = toggleFn
                                    , removeFix = keepOnlyFix { parentRange = composition.parentRange, keep = Node.range composition.earlier }
                                    }
                                )

                Nothing ->
                    Nothing
    in
    firstThatReportsError
        [ \() ->
            case ( maybeEarlierToggleFn, maybeLaterToggleFn ) of
                ( Just _, Just _ ) ->
                    Just
                        (Rule.errorWithFix
                            (doubleToggleErrorInfo toggle)
                            checkInfo.parentRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Basics" ], "identity" ) checkInfo))
                            ]
                        )

                ( Nothing, _ ) ->
                    Nothing

                ( _, Nothing ) ->
                    Nothing
        , \() ->
            case maybeEarlierToggleFn of
                Just earlierToggleFn ->
                    case getToggleComposition { earlierToLater = True } checkInfo.later of
                        Just laterToggle ->
                            Just
                                (Rule.errorWithFix
                                    (doubleToggleErrorInfo toggle)
                                    (Range.combine [ earlierToggleFn, laterToggle.range ])
                                    (laterToggle.removeFix
                                        ++ keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.later }
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        , \() ->
            case maybeLaterToggleFn of
                Just laterToggleFn ->
                    case getToggleComposition { earlierToLater = False } checkInfo.earlier of
                        Just earlierToggle ->
                            Just
                                (Rule.errorWithFix
                                    (doubleToggleErrorInfo toggle)
                                    (Range.combine [ earlierToggle.range, laterToggleFn ])
                                    (earlierToggle.removeFix
                                        ++ keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.earlier }
                                    )
                                )

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        ]
        ()


doubleToggleErrorInfo : ( ModuleName, String ) -> { message : String, details : List String }
doubleToggleErrorInfo toggle =
    let
        toggleFullyQualifiedAsString : String
        toggleFullyQualifiedAsString =
            qualifiedToString toggle
    in
    { message = "Unnecessary double " ++ toggleFullyQualifiedAsString
    , details = [ "Chaining " ++ toggleFullyQualifiedAsString ++ " with " ++ toggleFullyQualifiedAsString ++ " makes both functions cancel each other out." ]
    }



-- NEGATE


basicsNegateCompositionChecks : CompositionCheckInfo -> Maybe (Error {})
basicsNegateCompositionChecks checkInfo =
    toggleCompositionChecks ( [ "Basics" ], "negate" ) checkInfo


basicsNegateChecks : CheckInfo -> Maybe (Error {})
basicsNegateChecks checkInfo =
    removeAlongWithOtherFunctionCheck
        (doubleToggleErrorInfo ( [ "Basics" ], "negate" ))
        (AstHelpers.getSpecificValueOrFunction ( [ "Basics" ], "negate" ))
        checkInfo



-- BOOLEAN


basicsNotChecks : CheckInfo -> Maybe (Error {})
basicsNotChecks checkInfo =
    firstThatReportsError
        [ notOnKnownBoolCheck
        , removeAlongWithOtherFunctionCheck
            (doubleToggleErrorInfo ( [ "Basics" ], "not" ))
            (AstHelpers.getSpecificValueOrFunction ( [ "Basics" ], "not" ))
        , isNotOnBooleanOperatorCheck
        ]
        checkInfo


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
                    { message = "Expression is equal to " ++ notBoolAsString
                    , details = [ "You can replace the call to `not` by the boolean value directly." ]
                    }
                    checkInfo.parentRange
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


basicsNotCompositionChecks : CompositionCheckInfo -> Maybe (Error {})
basicsNotCompositionChecks checkInfo =
    toggleCompositionChecks ( [ "Basics" ], "not" ) checkInfo


orChecks : OperatorCheckInfo -> Maybe (Error {})
orChecks operatorCheckInfo =
    firstThatReportsError
        [ \() -> or_isLeftSimplifiableError operatorCheckInfo
        , \() -> or_isRightSimplifiableError operatorCheckInfo
        , \() -> findSimilarConditionsError operatorCheckInfo
        ]
        ()


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


or_isLeftSimplifiableError : OperatorCheckInfo -> Maybe (Error {})
or_isLeftSimplifiableError checkInfo =
    case Evaluate.getBoolean checkInfo checkInfo.left of
        Determined True ->
            Just
                (Rule.errorWithFix
                    { message = "Comparison is always True"
                    , details = alwaysSameDetails
                    }
                    checkInfo.parentRange
                    [ Fix.removeRange
                        { start = checkInfo.leftRange.end
                        , end = checkInfo.rightRange.end
                        }
                    ]
                )

        Determined False ->
            Just
                (Rule.errorWithFix
                    { message = unnecessaryMessage
                    , details = unnecessaryDetails
                    }
                    checkInfo.parentRange
                    [ Fix.removeRange
                        { start = checkInfo.leftRange.start
                        , end = checkInfo.rightRange.start
                        }
                    ]
                )

        Undetermined ->
            Nothing


or_isRightSimplifiableError : OperatorCheckInfo -> Maybe (Error {})
or_isRightSimplifiableError checkInfo =
    case Evaluate.getBoolean checkInfo checkInfo.right of
        Determined True ->
            Just
                (Rule.errorWithFix
                    { message = unnecessaryMessage
                    , details = unnecessaryDetails
                    }
                    checkInfo.parentRange
                    [ Fix.removeRange
                        { start = checkInfo.leftRange.start
                        , end = checkInfo.rightRange.start
                        }
                    ]
                )

        Determined False ->
            Just
                (Rule.errorWithFix
                    { message = unnecessaryMessage
                    , details = unnecessaryDetails
                    }
                    checkInfo.parentRange
                    [ Fix.removeRange
                        { start = checkInfo.leftRange.end
                        , end = checkInfo.rightRange.end
                        }
                    ]
                )

        Undetermined ->
            Nothing


andChecks : OperatorCheckInfo -> Maybe (Error {})
andChecks operatorCheckInfo =
    firstThatReportsError
        [ \() -> and_isLeftSimplifiableError operatorCheckInfo
        , \() -> and_isRightSimplifiableError operatorCheckInfo
        , \() -> findSimilarConditionsError operatorCheckInfo
        ]
        ()


and_isLeftSimplifiableError : OperatorCheckInfo -> Maybe (Error {})
and_isLeftSimplifiableError checkInfo =
    case Evaluate.getBoolean checkInfo checkInfo.left of
        Determined True ->
            Just
                (Rule.errorWithFix
                    { message = unnecessaryMessage
                    , details = unnecessaryDetails
                    }
                    checkInfo.parentRange
                    [ Fix.removeRange
                        { start = checkInfo.leftRange.start
                        , end = checkInfo.rightRange.start
                        }
                    ]
                )

        Determined False ->
            Just
                (Rule.errorWithFix
                    { message = "Comparison is always False"
                    , details = alwaysSameDetails
                    }
                    checkInfo.parentRange
                    [ Fix.removeRange
                        { start = checkInfo.leftRange.end
                        , end = checkInfo.rightRange.end
                        }
                    ]
                )

        Undetermined ->
            Nothing


and_isRightSimplifiableError : OperatorCheckInfo -> Maybe (Error {})
and_isRightSimplifiableError checkInfo =
    case Evaluate.getBoolean checkInfo checkInfo.right of
        Determined True ->
            Just
                (Rule.errorWithFix
                    { message = unnecessaryMessage
                    , details = unnecessaryDetails
                    }
                    checkInfo.parentRange
                    [ Fix.removeRange
                        { start = checkInfo.leftRange.end
                        , end = checkInfo.rightRange.end
                        }
                    ]
                )

        Determined False ->
            Just
                (Rule.errorWithFix
                    { message = "Comparison is always False"
                    , details = alwaysSameDetails
                    }
                    checkInfo.parentRange
                    [ Fix.removeRange
                        { start = checkInfo.leftRange.start
                        , end = checkInfo.rightRange.start
                        }
                    ]
                )

        Undetermined ->
            Nothing



-- EQUALITY


equalityChecks : Bool -> OperatorCheckInfo -> Maybe (Error {})
equalityChecks isEqual checkInfo =
    if Evaluate.getBoolean checkInfo checkInfo.right == Determined isEqual then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary comparison with boolean"
                , details = [ "The result of the expression will be the same with or without the comparison." ]
                }
                (errorToRightRange checkInfo)
                [ Fix.removeRange (fixToRightRange checkInfo) ]
            )

    else if Evaluate.getBoolean checkInfo checkInfo.left == Determined isEqual then
        Just
            (Rule.errorWithFix
                { message = "Unnecessary comparison with boolean"
                , details = [ "The result of the expression will be the same with or without the comparison." ]
                }
                (errorToLeftRange checkInfo)
                [ Fix.removeRange (fixToLeftRange checkInfo) ]
            )

    else
        case
            Maybe.map2 Tuple.pair
                (AstHelpers.getSpecificFunctionCall ( [ "Basics" ], "not" ) checkInfo.lookupTable checkInfo.left)
                (AstHelpers.getSpecificFunctionCall ( [ "Basics" ], "not" ) checkInfo.lookupTable checkInfo.right)
        of
            Just ( leftNot, rightNot ) ->
                Just
                    (Rule.errorWithFix
                        { message = "Unnecessary negation on both sides"
                        , details = [ "Since both sides are negated using `not`, they are redundant and can be removed." ]
                        }
                        checkInfo.parentRange
                        [ Fix.removeRange leftNot.fnRange, Fix.removeRange rightNot.fnRange ]
                    )

            _ ->
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


alwaysSameDetails : List String
alwaysSameDetails =
    [ "This condition will always result in the same value. You may have hardcoded a value or mistyped a condition."
    ]


unnecessaryMessage : String
unnecessaryMessage =
    "Part of the expression is unnecessary"


unnecessaryDetails : List String
unnecessaryDetails =
    [ "A part of this condition is unnecessary. You can remove it and it would not impact the behavior of the program."
    ]



-- COMPARISONS


comparisonChecks : (Float -> Float -> Bool) -> OperatorCheckInfo -> Maybe (Error {})
comparisonChecks operatorFunction operatorCheckInfo =
    case
        Maybe.map2 operatorFunction
            (Normalize.getNumberValue operatorCheckInfo.left)
            (Normalize.getNumberValue operatorCheckInfo.right)
    of
        Just bool ->
            Just (comparisonError bool operatorCheckInfo)

        Nothing ->
            Nothing


comparisonError : Bool -> QualifyResources { a | parentRange : Range } -> Error {}
comparisonError bool checkInfo =
    let
        boolAsString : String
        boolAsString =
            AstHelpers.boolToString bool
    in
    Rule.errorWithFix
        { message = "Comparison is always " ++ boolAsString
        , details =
            [ "Based on the values and/or the context, we can determine that the value of this operation will always be " ++ boolAsString ++ "."
            ]
        }
        checkInfo.parentRange
        [ Fix.replaceRangeBy checkInfo.parentRange
            (qualifiedToString (qualify ( [ "Basics" ], boolAsString ) checkInfo))
        ]



-- IF EXPRESSIONS


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
    if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.later then
        Just
            (Rule.errorWithFix
                basicsIdentityCompositionErrorMessage
                (Node.range checkInfo.later)
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.earlier })
            )

    else if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.earlier then
        Just
            (Rule.errorWithFix
                basicsIdentityCompositionErrorMessage
                (Node.range checkInfo.earlier)
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.later })
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


basicsAlwaysCompositionErrorMessage : { message : String, details : List String }
basicsAlwaysCompositionErrorMessage =
    { message = "Function composed with always will be ignored"
    , details = [ "`always` will swallow the function composed into it." ]
    }


basicsAlwaysCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
basicsAlwaysCompositionChecks checkInfo =
    case checkInfo.later.args of
        _ :: [] ->
            Just
                (Rule.errorWithFix
                    basicsAlwaysCompositionErrorMessage
                    checkInfo.later.range
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.later.range })
                )

        _ ->
            Nothing


callWithEmptyListArgReturnsEmptyListCheck : { checkedArg : Node Expression, fn : ( ModuleName, String ) } -> CheckInfo -> Maybe (Error {})
callWithEmptyListArgReturnsEmptyListCheck config checkInfo =
    case AstHelpers.getListLiteral config.checkedArg of
        Just [] ->
            Just
                (Rule.errorWithFix
                    (operationDoesNotChangeSpecificLastArgErrorInfo
                        { fn = config.fn, replacementDescription = "an empty list" }
                    )
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                )

        _ ->
            Nothing



-- STRING


stringFromListChecks : CheckInfo -> Maybe (Error {})
stringFromListChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Calling " ++ qualifiedToString ( [ "String" ], "fromList" ) ++ " [] will result in " ++ emptyStringAsString
                            , details = [ "You can replace this call by " ++ emptyStringAsString ++ "." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just listSingletonArg ->
                    Just
                        (Rule.errorWithFix
                            { message = "Calling " ++ qualifiedToString ( [ "String" ], "fromList" ) ++ " with a list with a single char is the same as String.fromChar with the contained char"
                            , details = [ "You can replace this call by " ++ qualifiedToString ( [ "String" ], "fromChar" ) ++ " with the contained char." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange listSingletonArg.element
                                ++ [ Fix.insertAt checkInfo.parentRange.start
                                        (qualifiedToString (qualify ( [ "String" ], "fromChar" ) checkInfo) ++ " ")
                                   ]
                            )
                        )

                Nothing ->
                    Nothing
        ]
        ()


stringIsEmptyChecks : CheckInfo -> Maybe (Error {})
stringIsEmptyChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.Literal str ->
            let
                replacementValueAsString : String
                replacementValueAsString =
                    AstHelpers.boolToString (str == "")
            in
            Just
                (Rule.errorWithFix
                    { message = "The call to String.isEmpty will result in " ++ replacementValueAsString
                    , details = [ "You can replace this call by " ++ replacementValueAsString ++ "." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Basics" ], replacementValueAsString ) checkInfo))
                    ]
                )

        _ ->
            Nothing


stringConcatChecks : CheckInfo -> Maybe (Error {})
stringConcatChecks checkInfo =
    case AstHelpers.getListLiteral checkInfo.firstArg of
        Just [] ->
            Just
                (Rule.errorWithFix
                    { message = "Using String.concat on an empty list will result in an empty string"
                    , details = [ "You can replace this call by an empty string." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
                )

        _ ->
            Nothing


stringWordsChecks : CheckInfo -> Maybe (Error {})
stringWordsChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.Literal "" ->
            Just
                (Rule.errorWithFix
                    { message = "Using String.words on an empty string will result in an empty list"
                    , details = [ "You can replace this call by an empty list." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                )

        _ ->
            Nothing


stringLinesChecks : CheckInfo -> Maybe (Error {})
stringLinesChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.Literal "" ->
            Just
                (Rule.errorWithFix
                    { message = "Using String.lines on an empty string will result in an empty list"
                    , details = [ "You can replace this call by an empty list." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                )

        _ ->
            Nothing


stringReverseChecks : CheckInfo -> Maybe (Error {})
stringReverseChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.Literal "" ->
            Just
                (Rule.errorWithFix
                    (operationDoesNotChangeSpecificLastArgErrorInfo
                        { fn = ( [ "String" ], "reverse" ), replacementDescription = "an empty string" }
                    )
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
                )

        _ ->
            removeAlongWithOtherFunctionCheck
                reverseReverseCompositionErrorMessage
                (AstHelpers.getSpecificValueOrFunction ( [ "String" ], "reverse" ))
                checkInfo


stringSliceChecks : CheckInfo -> Maybe (Error {})
stringSliceChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case thirdArg checkInfo of
                Just (Node _ (Expression.Literal "")) ->
                    Just
                        (Rule.errorWithFix
                            (operationDoesNotChangeSpecificLastArgErrorInfo
                                { fn = ( [ "String" ], "slice" ), replacementDescription = "an empty string" }
                            )
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
                        )

                _ ->
                    Nothing
        , \() ->
            case secondArg checkInfo of
                Just endArg ->
                    firstThatReportsError
                        [ \() ->
                            case Evaluate.getInt checkInfo endArg of
                                Just endInt ->
                                    firstThatReportsError
                                        [ \() ->
                                            case endInt of
                                                0 ->
                                                    Just
                                                        (Rule.errorWithFix
                                                            { message = "Using String.slice with end index 0 will result in an empty string"
                                                            , details = [ "You can replace this call by an empty string." ]
                                                            }
                                                            checkInfo.fnRange
                                                            (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (thirdArg checkInfo) checkInfo)
                                                        )

                                                _ ->
                                                    Nothing
                                        , \() ->
                                            case Evaluate.getInt checkInfo checkInfo.firstArg of
                                                Just startInt ->
                                                    if
                                                        (startInt >= endInt)
                                                            && -- have the same sign
                                                               ((startInt <= -1 && endInt <= -1)
                                                                    || (startInt >= 0 && endInt >= 0)
                                                               )
                                                    then
                                                        Rule.errorWithFix
                                                            { message = "The call to String.slice will result in " ++ emptyStringAsString
                                                            , details = [ "You can replace this slice operation by " ++ emptyStringAsString ++ "." ]
                                                            }
                                                            checkInfo.fnRange
                                                            (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (thirdArg checkInfo) checkInfo)
                                                            |> Just

                                                    else
                                                        -- either is negative or startInt < endIntNon0
                                                        Nothing

                                                Nothing ->
                                                    Nothing
                                        ]
                                        ()

                                Nothing ->
                                    Nothing
                        , \() ->
                            if Normalize.areAllTheSame checkInfo checkInfo.firstArg [ endArg ] then
                                Rule.errorWithFix
                                    { message = "Using String.slice with equal start and end index will result in an empty string"
                                    , details = [ "You can replace this call by an empty string." ]
                                    }
                                    checkInfo.fnRange
                                    (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (thirdArg checkInfo) checkInfo)
                                    |> Just

                            else
                                Nothing
                        ]
                        ()

                Nothing ->
                    Nothing
        ]
        ()


stringLeftChecks : CheckInfo -> Maybe (Error {})
stringLeftChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just length ->
                    callWithNonPositiveIntCanBeReplacedByCheck
                        { fn = ( [ "String" ], "left" )
                        , int = length
                        , intDescription = "length"
                        , replacementDescription = "an empty string"
                        , replacement = emptyStringAsString
                        , lastArg = secondArg checkInfo
                        }
                        checkInfo

                Nothing ->
                    Nothing
        , \() ->
            case secondArg checkInfo of
                Just (Node _ (Expression.Literal "")) ->
                    Just
                        (Rule.errorWithFix
                            (operationDoesNotChangeSpecificLastArgErrorInfo
                                { fn = ( [ "String" ], "left" ), replacementDescription = "an empty string" }
                            )
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
                        )

                _ ->
                    Nothing
        ]
        ()


callWithNonPositiveIntCanBeReplacedByCheck :
    { fn : ( ModuleName, String )
    , int : number
    , intDescription : String
    , replacement : String
    , replacementDescription : String
    , lastArg : Maybe a
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
            (Rule.errorWithFix
                { message = "Using " ++ qualifiedToString config.fn ++ " with " ++ lengthDescription ++ " will result in " ++ config.replacementDescription
                , details = [ "You can replace this call by " ++ config.replacementDescription ++ "." ]
                }
                checkInfo.fnRange
                (replaceByEmptyFix config.replacement checkInfo.parentRange config.lastArg checkInfo)
            )

    else
        Nothing


stringRightChecks : CheckInfo -> Maybe (Error {})
stringRightChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just length ->
                    callWithNonPositiveIntCanBeReplacedByCheck
                        { fn = ( [ "String" ], "right" )
                        , int = length
                        , intDescription = "length"
                        , replacementDescription = "an empty string"
                        , replacement = emptyStringAsString
                        , lastArg = secondArg checkInfo
                        }
                        checkInfo

                Nothing ->
                    Nothing
        , \() ->
            case secondArg checkInfo of
                Just (Node _ (Expression.Literal "")) ->
                    Just
                        (Rule.errorWithFix
                            (operationDoesNotChangeSpecificLastArgErrorInfo
                                { fn = ( [ "String" ], "right" ), replacementDescription = "an empty string" }
                            )
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
                        )

                _ ->
                    Nothing
        ]
        ()


reverseReverseCompositionErrorMessage : { message : String, details : List String }
reverseReverseCompositionErrorMessage =
    { message = "Unnecessary double reversal"
    , details = [ "Composing `reverse` with `reverse` cancel each other out." ]
    }


stringJoinChecks : CheckInfo -> Maybe (Error {})
stringJoinChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case secondArg checkInfo of
                Just (Node _ (Expression.ListExpr [])) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using String.join on an empty list will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
                        )

                _ ->
                    Nothing
        , \() ->
            case Node.value checkInfo.firstArg of
                Expression.Literal "" ->
                    Just
                        (Rule.errorWithFix
                            { message = "Use String.concat instead"
                            , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end }
                                (qualifiedToString (qualify ( [ "String" ], "concat" ) checkInfo))
                            ]
                        )

                _ ->
                    Nothing
        ]
        ()


stringLengthChecks : CheckInfo -> Maybe (Error {})
stringLengthChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.Literal str ->
            Just
                (Rule.errorWithFix
                    { message = "The length of the string is " ++ String.fromInt (String.length str)
                    , details = [ "The length of the string can be determined by looking at the code." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange (String.fromInt (String.length str)) ]
                )

        _ ->
            Nothing


stringRepeatChecks : CheckInfo -> Maybe (Error {})
stringRepeatChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case secondArg checkInfo of
                Just (Node _ (Expression.Literal "")) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using String.repeat with an empty string will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
                        )

                _ ->
                    Nothing
        , \() ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just intValue ->
                    firstThatReportsError
                        [ \() ->
                            case intValue of
                                1 ->
                                    Just
                                        (identityError
                                            { toFix = "String.repeat 1"
                                            , lastArg = secondArg checkInfo
                                            , lastArgName = "string to repeat"
                                            , resources = checkInfo
                                            }
                                        )

                                _ ->
                                    Nothing
                        , \() ->
                            callWithNonPositiveIntCanBeReplacedByCheck
                                { fn = ( [ "String" ], "repeat" )
                                , int = intValue
                                , intDescription = "length"
                                , replacementDescription = "an empty string"
                                , replacement = emptyStringAsString
                                , lastArg = secondArg checkInfo
                                }
                                checkInfo
                        ]
                        ()

                _ ->
                    Nothing
        ]
        ()


stringReplaceChecks : CheckInfo -> Maybe (Error {})
stringReplaceChecks checkInfo =
    case secondArg checkInfo of
        Just replacementArg ->
            firstThatReportsError
                [ \() ->
                    case Normalize.compare checkInfo checkInfo.firstArg replacementArg of
                        Normalize.ConfirmedEquality ->
                            Just
                                (Rule.errorWithFix
                                    { message = "The result of String.replace will be the original string"
                                    , details = [ "The pattern to replace and the replacement are equal, therefore the result of the String.replace call will be the original string." ]
                                    }
                                    checkInfo.fnRange
                                    (toIdentityFix { lastArg = thirdArg checkInfo, resources = checkInfo })
                                )

                        _ ->
                            Nothing
                , \() ->
                    case thirdArg checkInfo of
                        Just (Node thirdRange (Expression.Literal "")) ->
                            Just
                                (Rule.errorWithFix
                                    { message = "The result of String.replace will be the empty string"
                                    , details = [ "Replacing anything on an empty string results in an empty string." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.removeRange
                                        { start = checkInfo.fnRange.start
                                        , end = thirdRange.start
                                        }
                                    ]
                                )

                        _ ->
                            Nothing
                , \() ->
                    case ( Node.value checkInfo.firstArg, Node.value replacementArg, thirdArg checkInfo ) of
                        ( Expression.Literal first, Expression.Literal second, Just (Node thirdRange (Expression.Literal third)) ) ->
                            if not (String.contains "\u{000D}" first) && String.replace first second third == third then
                                Just
                                    (Rule.errorWithFix
                                        { message = "The result of String.replace will be the original string"
                                        , details = [ "The replacement doesn't haven't any noticeable impact. You can remove the call to String.replace." ]
                                        }
                                        checkInfo.fnRange
                                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = thirdRange })
                                    )

                            else
                                Nothing

                        _ ->
                            Nothing
                ]
                ()

        Nothing ->
            Nothing



-- MAYBE FUNCTIONS


maybeMapChecks : CheckInfo -> Maybe (Error {})
maybeMapChecks checkInfo =
    firstThatReportsError
        [ \() -> containerMapChecks maybeCollection checkInfo
        , \() -> mapPureChecks { moduleName = [ "Maybe" ], pure = "Just", map = "map" } checkInfo
        ]
        ()


maybeMapCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
maybeMapCompositionChecks checkInfo =
    pureToMapCompositionChecks { moduleName = [ "Maybe" ], pure = "Just", map = "map" } checkInfo



-- RESULT FUNCTIONS


resultMapChecks : CheckInfo -> Maybe (Error {})
resultMapChecks checkInfo =
    firstThatReportsError
        [ \() -> containerMapChecks resultCollection checkInfo
        , \() -> mapPureChecks { moduleName = [ "Result" ], pure = "Ok", map = "map" } checkInfo
        ]
        ()


resultMapCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
resultMapCompositionChecks checkInfo =
    pureToMapCompositionChecks { moduleName = [ "Result" ], pure = "Ok", map = "map" } checkInfo


resultMapErrorOnErrErrorInfo : { message : String, details : List String }
resultMapErrorOnErrErrorInfo =
    { message = "Using " ++ qualifiedToString ( [ "Result" ], "mapError" ) ++ " on Err will result in Err with the function applied to the error"
    , details = [ "You can replace this call by Err with the function directly applied to the error itself." ]
    }


resultMapErrorOnOkErrorInfo : { message : String, details : List String }
resultMapErrorOnOkErrorInfo =
    { message = "Calling " ++ qualifiedToString ( [ "Result" ], "mapError" ) ++ " on a value that is Ok will always return the Ok result value"
    , details = [ "You can remove the " ++ qualifiedToString ( [ "Result" ], "mapError" ) ++ " call." ]
    }


resultMapErrorChecks : CheckInfo -> Maybe (Error {})
resultMapErrorChecks checkInfo =
    let
        maybeResultArg : Maybe (Node Expression)
        maybeResultArg =
            secondArg checkInfo
    in
    firstThatReportsError
        -- TODO use containerMapChecks
        [ \() ->
            mapIdentityChecks "mapError"
                { moduleName = [ "Result" ], represents = "result" }
                checkInfo
        , \() ->
            mapPureChecks { moduleName = [ "Result" ], pure = "Err", map = "mapError" } checkInfo
        , \() ->
            case maybeResultArg of
                Just resultArg ->
                    case sameCallInAllBranches ( [ "Result" ], "Ok" ) checkInfo.lookupTable resultArg of
                        Determined _ ->
                            Just
                                (Rule.errorWithFix
                                    resultMapErrorOnOkErrorInfo
                                    checkInfo.fnRange
                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range resultArg })
                                )

                        Undetermined ->
                            Nothing

                Nothing ->
                    Nothing
        ]
        ()


resultMapErrorCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
resultMapErrorCompositionChecks checkInfo =
    case checkInfo.later.args of
        (Node errorMappingArgRange _) :: _ ->
            case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                ( ( [ "Result" ], "Err" ), [] ) ->
                    Just
                        (Rule.errorWithFix
                            resultMapErrorOnErrErrorInfo
                            checkInfo.later.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = errorMappingArgRange }
                                ++ [ case checkInfo.direction of
                                        LeftToRight ->
                                            Fix.insertAt checkInfo.parentRange.end
                                                (" >> " ++ qualifiedToString (qualify ( [ "Result" ], "Err" ) checkInfo))

                                        RightToLeft ->
                                            Fix.insertAt checkInfo.parentRange.start
                                                (qualifiedToString (qualify ( [ "Result" ], "Err" ) checkInfo) ++ " << ")
                                   ]
                            )
                        )

                ( ( [ "Result" ], "Ok" ), [] ) ->
                    Just
                        (Rule.errorWithFix
                            resultMapErrorOnOkErrorInfo
                            checkInfo.later.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.earlier.fnRange })
                        )

                _ ->
                    Nothing

        [] ->
            Nothing



-- LIST FUNCTIONS


listConcatChecks : CheckInfo -> Maybe (Error {})
listConcatChecks checkInfo =
    firstThatReportsError
        [ \() ->
            callWithEmptyListArgReturnsEmptyListCheck
                { fn = ( [ "List" ], "concat" ), checkedArg = checkInfo.firstArg }
                checkInfo
        , \() ->
            case Node.value checkInfo.firstArg of
                Expression.ListExpr list ->
                    case list of
                        (Node elementRange _) :: [] ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Unnecessary use of " ++ qualifiedToString ( [ "List" ], "concat" ) ++ " on a list with 1 element"
                                    , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                                    }
                                    checkInfo.parentRange
                                    [ Fix.removeRange { start = checkInfo.parentRange.start, end = elementRange.start }
                                    , Fix.removeRange { start = elementRange.end, end = checkInfo.parentRange.end }
                                    ]
                                )

                        firstListElement :: restOfListElements ->
                            firstThatReportsError
                                [ \() ->
                                    case findEmptyLiteral list of
                                        Just emptyLiteral ->
                                            Just
                                                (Rule.errorWithFix
                                                    { message = "Found empty list in the list given " ++ qualifiedToString ( [ "List" ], "concat" )
                                                    , details = [ "This element is unnecessary and can be removed." ]
                                                    }
                                                    emptyLiteral.element
                                                    [ Fix.removeRange emptyLiteral.removalRange ]
                                                )

                                        Nothing ->
                                            Nothing
                                , \() ->
                                    case traverse AstHelpers.getListLiteral list of
                                        Just _ ->
                                            Just
                                                (Rule.errorWithFix
                                                    { message = "Expression could be simplified to be a single List"
                                                    , details = [ "Try moving all the elements into a single list." ]
                                                    }
                                                    checkInfo.parentRange
                                                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range checkInfo.firstArg }
                                                        ++ List.concatMap removeBoundariesFix (firstListElement :: restOfListElements)
                                                    )
                                                )

                                        Nothing ->
                                            Nothing
                                , \() ->
                                    case findConsecutiveListLiterals firstListElement restOfListElements of
                                        firstFix :: fixesAFterFirst ->
                                            Just
                                                (Rule.errorWithFix
                                                    { message = "Consecutive literal lists should be merged"
                                                    , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                                                    }
                                                    checkInfo.fnRange
                                                    (firstFix :: fixesAFterFirst)
                                                )

                                        [] ->
                                            Nothing
                                ]
                                ()

                        _ ->
                            Nothing

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getSpecificFunctionCall ( [ "List" ], "map" ) checkInfo.lookupTable checkInfo.firstArg of
                Just listMapArg ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString ( [ "List" ], "map" ) ++ " and " ++ qualifiedToString ( [ "List" ], "concat" ) ++ " can be combined using " ++ qualifiedToString ( [ "List" ], "concatMap" )
                            , details = [ qualifiedToString ( [ "List" ], "concatMap" ) ++ " is meant for this exact purpose and will also be faster." ]
                            }
                            checkInfo.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = listMapArg.nodeRange }
                                ++ [ Fix.replaceRangeBy listMapArg.fnRange
                                        (qualifiedToString (qualify ( [ "List" ], "concatMap" ) checkInfo))
                                   ]
                            )
                        )

                Nothing ->
                    Nothing
        ]
        ()


findEmptyLiteral : List (Node Expression) -> Maybe { element : Range, removalRange : Range }
findEmptyLiteral elements =
    case elements of
        [] ->
            Nothing

        head :: rest ->
            let
                headRange : Range
                headRange =
                    Node.range head
            in
            case AstHelpers.getListLiteral head of
                Just [] ->
                    let
                        end : Location
                        end =
                            case rest of
                                [] ->
                                    headRange.end

                                (Node nextItem _) :: _ ->
                                    nextItem.start
                    in
                    Just
                        { element = headRange
                        , removalRange = { start = headRange.start, end = end }
                        }

                _ ->
                    findEmptyLiteralHelp rest headRange.end


findEmptyLiteralHelp : List (Node Expression) -> Location -> Maybe { element : Range, removalRange : Range }
findEmptyLiteralHelp elements previousItemEnd =
    case elements of
        [] ->
            Nothing

        head :: rest ->
            let
                headRange : Range
                headRange =
                    Node.range head
            in
            case AstHelpers.getListLiteral head of
                Just [] ->
                    Just
                        { element = headRange
                        , removalRange = { start = previousItemEnd, end = headRange.end }
                        }

                _ ->
                    findEmptyLiteralHelp rest headRange.end


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
listConcatMapChecks checkInfo =
    firstThatReportsError
        [ \() ->
            if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
                Just
                    (Rule.errorWithFix
                        { message = "Using " ++ qualifiedToString ( [ "List" ], "concatMap" ) ++ " with an identity function is the same as using " ++ qualifiedToString ( [ "List" ], "concat" ) ++ ""
                        , details = [ "You can replace this call by " ++ qualifiedToString ( [ "List" ], "concat" ) ++ "." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy
                            { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end }
                            (qualifiedToString (qualify ( [ "List" ], "concat" ) checkInfo))
                        ]
                    )

            else
                Nothing
        , \() ->
            case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just alwaysResult ->
                    case AstHelpers.getListLiteral alwaysResult of
                        Just [] ->
                            Just
                                (Rule.errorWithFix
                                    { message = qualifiedToString ( [ "List" ], "concatMap" ) ++ " will result in on an empty list"
                                    , details = [ "You can replace this call by an empty list." ]
                                    }
                                    checkInfo.fnRange
                                    (replaceByEmptyFix "[]" checkInfo.parentRange (secondArg checkInfo) checkInfo)
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        , \() ->
            case Node.value (AstHelpers.removeParens checkInfo.firstArg) of
                Expression.LambdaExpression lambda ->
                    case replaceSingleElementListBySingleValue checkInfo.lookupTable lambda.expression of
                        Just fixes ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Use " ++ qualifiedToString ( [ "List" ], "map" ) ++ " instead"
                                    , details = [ "The function passed to " ++ qualifiedToString ( [ "List" ], "concatMap" ) ++ " always returns a list with a single element." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify ( [ "List" ], "map" ) checkInfo))
                                        :: fixes
                                    )
                                )

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        , \() ->
            case secondArg checkInfo of
                Just listArg ->
                    firstThatReportsError
                        [ \() ->
                            case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                                Just listSingleton ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = "Using " ++ qualifiedToString ( [ "List" ], "concatMap" ) ++ " on an element with a single item is the same as calling the function directly on that lone element."
                                            , details = [ "You can replace this call by a call to the function directly." ]
                                            }
                                            checkInfo.fnRange
                                            (Fix.removeRange checkInfo.fnRange
                                                :: replaceBySubExpressionFix (Node.range listArg) listSingleton.element
                                            )
                                        )

                                Nothing ->
                                    Nothing
                        , \() ->
                            callWithEmptyListArgReturnsEmptyListCheck
                                { fn = ( [ "List" ], "concatMap" ), checkedArg = listArg }
                                checkInfo
                        ]
                        ()

                Nothing ->
                    Nothing
        ]
        ()


listConcatCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
listConcatCompositionChecks checkInfo =
    case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
        ( ( [ "List" ], "map" ), _ :: [] ) ->
            Just
                (Rule.errorWithFix
                    { message = qualifiedToString ( [ "List" ], "map" ) ++ " and " ++ qualifiedToString ( [ "List" ], "concat" ) ++ " can be combined using " ++ qualifiedToString ( [ "List" ], "concatMap" ) ++ ""
                    , details = [ qualifiedToString ( [ "List" ], "concatMap" ) ++ " is meant for this exact purpose and will also be faster." ]
                    }
                    checkInfo.later.fnRange
                    (Fix.replaceRangeBy checkInfo.earlier.fnRange
                        (qualifiedToString (qualify ( [ "List" ], "concatMap" ) checkInfo))
                        :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.earlier.range }
                    )
                )

        _ ->
            Nothing


listIndexedMapChecks : CheckInfo -> Maybe (Error {})
listIndexedMapChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case secondArg checkInfo of
                Just listArg ->
                    callWithEmptyListArgReturnsEmptyListCheck
                        { fn = ( [ "List" ], "indexedMap" ), checkedArg = listArg }
                        checkInfo

                Nothing ->
                    Nothing
        , \() ->
            case AstHelpers.removeParens checkInfo.firstArg of
                Node lambdaRange (Expression.LambdaExpression lambda) ->
                    case Maybe.map AstHelpers.removeParensFromPattern (List.head lambda.args) of
                        Just (Node _ Pattern.AllPattern) ->
                            let
                                rangeToRemove : Range
                                rangeToRemove =
                                    case lambda.args of
                                        [] ->
                                            Range.emptyRange

                                        _ :: [] ->
                                            -- Only one argument, remove the entire lambda except the expression
                                            { start = lambdaRange.start, end = (Node.range lambda.expression).start }

                                        (Node firstRange _) :: (Node secondRange _) :: _ ->
                                            { start = firstRange.start, end = secondRange.start }
                            in
                            Just
                                (Rule.errorWithFix
                                    { message = "Use " ++ qualifiedToString ( [ "List" ], "map" ) ++ " instead"
                                    , details = [ "Using " ++ qualifiedToString ( [ "List" ], "indexedMap" ) ++ " while ignoring the first argument is the same thing as calling " ++ qualifiedToString ( [ "List" ], "map" ) ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify ( [ "List" ], "map" ) checkInfo))
                                    , Fix.removeRange rangeToRemove
                                    ]
                                )

                        _ ->
                            Nothing

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getSpecificFunctionCall ( [ "Basics" ], "always" ) checkInfo.lookupTable checkInfo.firstArg of
                Just alwaysCall ->
                    Just
                        (Rule.errorWithFix
                            { message = "Use " ++ qualifiedToString ( [ "List" ], "map" ) ++ " instead"
                            , details = [ "Using " ++ qualifiedToString ( [ "List" ], "indexedMap" ) ++ " while ignoring the first argument is the same thing as calling " ++ qualifiedToString ( [ "List" ], "map" ) ++ "." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange
                                (qualifiedToString (qualify ( [ "List" ], "map" ) checkInfo))
                                :: replaceBySubExpressionFix alwaysCall.nodeRange alwaysCall.firstArg
                            )
                        )

                Nothing ->
                    Nothing
        ]
        ()


listIntersperseChecks : CheckInfo -> Maybe (Error {})
listIntersperseChecks checkInfo =
    case secondArg checkInfo of
        Just listArg ->
            callWithEmptyListArgReturnsEmptyListCheck
                { fn = ( [ "List" ], "intersperse" ), checkedArg = listArg }
                checkInfo

        Nothing ->
            Nothing


listAppendEmptyErrorInfo : { message : String, details : List String }
listAppendEmptyErrorInfo =
    { message = "Appending [] doesn't have any effect"
    , details = [ "You can remove the " ++ qualifiedToString ( [ "List" ], "append" ) ++ " function and the []." ]
    }


listAppendChecks : CheckInfo -> Maybe (Error {})
listAppendChecks checkInfo =
    case ( checkInfo.firstArg, secondArg checkInfo ) of
        ( Node _ (Expression.ListExpr []), maybeSecondListArg ) ->
            case maybeSecondListArg of
                Nothing ->
                    Just
                        (Rule.errorWithFix
                            { listAppendEmptyErrorInfo
                                | details = [ "You can replace this call by identity." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Basics" ], "identity" ) checkInfo))
                            ]
                        )

                Just secondListArg ->
                    Just
                        (Rule.errorWithFix
                            listAppendEmptyErrorInfo
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange secondListArg)
                        )

        ( firstList, Just (Node _ (Expression.ListExpr [])) ) ->
            Just
                (Rule.errorWithFix
                    listAppendEmptyErrorInfo
                    checkInfo.fnRange
                    (replaceBySubExpressionFix checkInfo.parentRange firstList)
                )

        ( Node firstListRange (Expression.ListExpr (_ :: _)), Just (Node secondListRange (Expression.ListExpr (_ :: _))) ) ->
            Just
                (Rule.errorWithFix
                    { message = "Appending literal lists could be simplified to be a single List"
                    , details = [ "Try moving all the elements into a single list." ]
                    }
                    checkInfo.fnRange
                    [ Fix.removeRange { start = secondListRange.end, end = checkInfo.parentRange.end }
                    , Fix.replaceRangeBy
                        { start = checkInfo.parentRange.start, end = startWithoutBoundary secondListRange }
                        ("[" ++ checkInfo.extractSourceCode (rangeWithoutBoundaries firstListRange) ++ ",")
                    ]
                )

        _ ->
            Nothing


listHeadExistsError : { message : String, details : List String }
listHeadExistsError =
    { message = "Using " ++ qualifiedToString ( [ "List" ], "head" ) ++ " on a list with a first element will result in Just that element"
    , details = [ "You can replace this call by Just the first list element." ]
    }


listHeadChecks : CheckInfo -> Maybe (Error {})
listHeadChecks checkInfo =
    let
        justFirstElementError : Node Expression -> Error {}
        justFirstElementError keep =
            Rule.errorWithFix
                listHeadExistsError
                checkInfo.fnRange
                (replaceBySubExpressionFix (Node.range listArg) keep
                    ++ [ Fix.replaceRangeBy checkInfo.fnRange
                            (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                       ]
                )

        listArg : Node Expression
        listArg =
            AstHelpers.removeParens checkInfo.firstArg
    in
    firstThatReportsError
        [ \() ->
            case Node.value listArg of
                Expression.ListExpr [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "head" ) ++ " on an empty list will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo))
                            ]
                        )

                Expression.ListExpr (head :: _) ->
                    Just (justFirstElementError head)

                Expression.OperatorApplication "::" _ head _ ->
                    Just (justFirstElementError head)

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                Just single ->
                    Just (justFirstElementError single.element)

                Nothing ->
                    Nothing
        ]
        ()


listTailExistsError : { message : String, details : List String }
listTailExistsError =
    { message = "Using " ++ qualifiedToString ( [ "List" ], "tail" ) ++ " on a list with some elements will result in Just the elements after the first"
    , details = [ "You can replace this call by Just the list elements after the first." ]
    }


listEmptyTailExistsError : { message : String, details : List String }
listEmptyTailExistsError =
    { message = "Using " ++ qualifiedToString ( [ "List" ], "tail" ) ++ " on a list with a single element will result in Just the empty list"
    , details = [ "You can replace this call by Just the empty list." ]
    }


listTailChecks : CheckInfo -> Maybe (Error {})
listTailChecks checkInfo =
    let
        listArg : Node Expression
        listArg =
            AstHelpers.removeParens checkInfo.firstArg
    in
    firstThatReportsError
        [ \() ->
            case Node.value listArg of
                Expression.ListExpr [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "tail" ) ++ " on an empty list will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo))
                            ]
                        )

                Expression.ListExpr ((Node headRange _) :: (Node tailFirstRange _) :: _) ->
                    Just
                        (Rule.errorWithFix
                            listTailExistsError
                            checkInfo.fnRange
                            [ Fix.removeRange { start = headRange.start, end = tailFirstRange.start }
                            , Fix.replaceRangeBy checkInfo.fnRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                            ]
                        )

                Expression.OperatorApplication "::" _ _ tail ->
                    Just
                        (Rule.errorWithFix
                            listTailExistsError
                            checkInfo.fnRange
                            (replaceBySubExpressionFix (Node.range listArg) tail
                                ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                                   ]
                            )
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                Just _ ->
                    Just
                        (Rule.errorWithFix
                            listEmptyTailExistsError
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy (Node.range checkInfo.firstArg) "[]"
                            , Fix.replaceRangeBy checkInfo.fnRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                            ]
                        )

                Nothing ->
                    Nothing
        ]
        ()


listMapChecks : CheckInfo -> Maybe (Error {})
listMapChecks checkInfo =
    firstThatReportsError
        [ \() -> containerMapChecks listCollection checkInfo
        , \() -> dictToListMapChecks checkInfo
        ]
        ()


dictToListMapErrorInfo : { toEntryAspectList : String, tuplePart : String } -> { message : String, details : List String }
dictToListMapErrorInfo info =
    let
        toEntryAspectListAsQualifiedString : String
        toEntryAspectListAsQualifiedString =
            qualifiedToString ( [ "Dict" ], info.toEntryAspectList )
    in
    { message = "Using " ++ qualifiedToString ( [ "Dict" ], "toList" ) ++ ", then " ++ qualifiedToString ( [ "List" ], "map" ) ++ " " ++ qualifiedToString ( [ "Tuple" ], info.tuplePart ) ++ " is the same as using " ++ toEntryAspectListAsQualifiedString
    , details = [ "Using " ++ toEntryAspectListAsQualifiedString ++ " directly is meant for this exact purpose and will also be faster." ]
    }


dictToListMapChecks : CheckInfo -> Maybe (Error {})
dictToListMapChecks listMapCheckInfo =
    case secondArg listMapCheckInfo of
        Just listArgument ->
            case AstHelpers.getSpecificFunctionCall ( [ "Dict" ], "toList" ) listMapCheckInfo.lookupTable listArgument of
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


listMapCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
listMapCompositionChecks checkInfo =
    case
        ( ( checkInfo.earlier.fn, checkInfo.earlier.args )
        , checkInfo.later.args
        )
    of
        ( ( ( [ "Dict" ], "toList" ), [] ), elementMappingArg :: [] ) ->
            let
                error : { toEntryAspectList : String, tuplePart : String } -> Error {}
                error info =
                    Rule.errorWithFix
                        (dictToListMapErrorInfo info)
                        checkInfo.later.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange (qualifiedToString (qualify ( [ "Dict" ], info.toEntryAspectList ) checkInfo)) ]
            in
            if AstHelpers.isTupleFirstAccess checkInfo.lookupTable elementMappingArg then
                Just (error { tuplePart = "first", toEntryAspectList = "keys" })

            else if AstHelpers.isTupleSecondAccess checkInfo.lookupTable elementMappingArg then
                Just (error { tuplePart = "second", toEntryAspectList = "values" })

            else
                Nothing

        _ ->
            Nothing


listMemberChecks : CheckInfo -> Maybe (Error {})
listMemberChecks checkInfo =
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
    case secondArg checkInfo of
        Just listArg ->
            let
                needleRange : Range
                needleRange =
                    Node.range needleArg

                listMemberExistsError : Maybe (Error {})
                listMemberExistsError =
                    if checkInfo.expectNaN then
                        Nothing

                    else
                        Just
                            (Rule.errorWithFix
                                { message = "Using " ++ qualifiedToString ( [ "List" ], "member" ) ++ " on a list which contains the given element will result in True"
                                , details = [ "You can replace this call by True." ]
                                }
                                checkInfo.fnRange
                                [ Fix.replaceRangeBy checkInfo.parentRange
                                    (qualifiedToString (qualify ( [ "Basics" ], "True" ) checkInfo))
                                ]
                            )

                singleNonNormalizedEqualElementError : Node Expression -> Error {}
                singleNonNormalizedEqualElementError element =
                    let
                        elementRange : Range
                        elementRange =
                            Node.range element
                    in
                    Rule.errorWithFix
                        { message = "Using " ++ qualifiedToString ( [ "List" ], "member" ) ++ " on an list with a single element is equivalent to directly checking for equality"
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
                            , parenthesizeIfNeededFix element
                            ]
                        )
            in
            firstThatReportsError
                [ \() ->
                    case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                        Just single ->
                            if isNeedle single.element then
                                listMemberExistsError

                            else
                                Just (singleNonNormalizedEqualElementError single.element)

                        Nothing ->
                            Nothing
                , \() ->
                    case Node.value (AstHelpers.removeParens listArg) of
                        Expression.ListExpr [] ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "List" ], "member" ) ++ " on an empty list will result in False"
                                    , details = [ "You can replace this call by False." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        (qualifiedToString (qualify ( [ "Basics" ], "False" ) checkInfo))
                                    ]
                                )

                        Expression.ListExpr (el0 :: el1 :: el2Up) ->
                            if List.any isNeedle (el0 :: el1 :: el2Up) then
                                listMemberExistsError

                            else
                                Nothing

                        Expression.OperatorApplication "::" _ head tail ->
                            if List.any isNeedle (head :: getBeforeLastCons tail) then
                                listMemberExistsError

                            else
                                Nothing

                        _ ->
                            Nothing
                ]
                ()

        Nothing ->
            Nothing


getBeforeLastCons : Node Expression -> List (Node Expression)
getBeforeLastCons (Node _ expression) =
    case expression of
        Expression.OperatorApplication "::" _ beforeCons afterCons ->
            beforeCons :: getBeforeLastCons afterCons

        _ ->
            []


listSumChecks : CheckInfo -> Maybe (Error {})
listSumChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "sum" ) ++ " on [] will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange "0" ]
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just listSingletonArg ->
                    Just
                        (Rule.errorWithFix
                            { message = "Summing a list with a single element will result in the element itself"
                            , details = [ "You can replace this call by the single element itself." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange listSingletonArg.element)
                        )

                Nothing ->
                    Nothing
        ]
        ()


listProductChecks : CheckInfo -> Maybe (Error {})
listProductChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "product" ) ++ " on [] will result in 1"
                            , details = [ "You can replace this call by 1." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange "1" ]
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just listSingletonArg ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString ( [ "List" ], "product" ) ++ " on a list with a single element will result in the element itself"
                            , details = [ "You can replace this call by the single element itself." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange listSingletonArg.element)
                        )

                Nothing ->
                    Nothing
        ]
        ()


listMinimumChecks : CheckInfo -> Maybe (Error {})
listMinimumChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "minimum" ) ++ " on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo))
                            ]
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just listSingletonArg ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString ( [ "List" ], "minimum" ) ++ " on a list with a single element will result in Just the element itself"
                            , details = [ "You can replace this call by Just the single element itself." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                                :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) listSingletonArg.element
                            )
                        )

                _ ->
                    Nothing
        ]
        ()


listMaximumChecks : CheckInfo -> Maybe (Error {})
listMaximumChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "maximum" ) ++ " on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo))
                            ]
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just listSingletonArg ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString ( [ "List" ], "maximum" ) ++ " on a list with a single element will result in Just the element itself"
                            , details = [ "You can replace this call by Just the single element itself." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                                :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) listSingletonArg.element
                            )
                        )

                Nothing ->
                    Nothing
        ]
        ()


listFoldlChecks : CheckInfo -> Maybe (Error {})
listFoldlChecks checkInfo =
    listFoldAnyDirectionChecks "foldl" checkInfo


listFoldrChecks : CheckInfo -> Maybe (Error {})
listFoldrChecks checkInfo =
    listFoldAnyDirectionChecks "foldr" checkInfo


listFoldAnyDirectionChecks : String -> CheckInfo -> Maybe (Error {})
listFoldAnyDirectionChecks foldOperationName checkInfo =
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
                                { message = "Use " ++ replacementOperationAsString ++ " instead"
                                , details =
                                    [ "Using " ++ qualifiedToString ( [ "List" ], foldOperationName ) ++ " (" ++ operation.two ++ ") " ++ String.fromInt operation.identity ++ " is the same as using " ++ replacementOperationAsString ++ "." ]
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
                                if checkInfo.usingRightPizza then
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

                                else
                                    -- <| or application
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
                        Rule.errorWithFix
                            { message = "The call to " ++ qualifiedToString ( [ "List" ], foldOperationName ) ++ " will result in " ++ determiningAsString
                            , details = [ "You can replace this call by " ++ determiningAsString ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix (qualifiedToString (qualify ( [ "Basics" ], determiningAsString ) checkInfo))
                                checkInfo.parentRange
                                (thirdArg checkInfo)
                                checkInfo
                            )

                    else
                        -- initialIsTrue /= operation.determining
                        let
                            replacementOperationAsString : String
                            replacementOperationAsString =
                                qualifiedToString ( [ "List" ], operation.list ) ++ " identity"
                        in
                        Rule.errorWithFix
                            { message = "Use " ++ replacementOperationAsString ++ " instead"
                            , details = [ "Using " ++ qualifiedToString ( [ "List" ], foldOperationName ) ++ " (" ++ operation.two ++ ") " ++ AstHelpers.boolToString (not operation.determining) ++ " is the same as using " ++ replacementOperationAsString ++ "." ]
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
            firstThatReportsError
                [ \() ->
                    case maybeListArg of
                        Just listArg ->
                            case AstHelpers.getSpecificFunctionCall ( [ "Set" ], "toList" ) checkInfo.lookupTable listArg of
                                Just setToListCall ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = "To fold a set, you don't need to convert to a List"
                                            , details = [ "Using " ++ qualifiedToString ( [ "Set" ], foldOperationName ) ++ " directly is meant for this exact purpose and will also be faster." ]
                                            }
                                            checkInfo.fnRange
                                            (replaceBySubExpressionFix setToListCall.nodeRange setToListCall.firstArg
                                                ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                                        (qualifiedToString (qualify ( [ "Set" ], foldOperationName ) checkInfo))
                                                   ]
                                            )
                                        )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing
                , \() ->
                    case maybeListArg of
                        Just listArg ->
                            case AstHelpers.getListLiteral listArg of
                                Just [] ->
                                    Just
                                        (Rule.errorWithFix
                                            { message = "The call to " ++ qualifiedToString ( [ "List" ], foldOperationName ) ++ " will result in the initial accumulator"
                                            , details = [ "You can replace this call by the initial accumulator." ]
                                            }
                                            checkInfo.fnRange
                                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range initialArg })
                                        )

                                _ ->
                                    Nothing

                        Nothing ->
                            Nothing
                , \() ->
                    case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                        Just reduceAlwaysResult ->
                            if AstHelpers.isIdentity checkInfo.lookupTable reduceAlwaysResult then
                                Just
                                    (Rule.errorWithFix
                                        { message = "The call to " ++ qualifiedToString ( [ "List" ], foldOperationName ) ++ " will result in the initial accumulator"
                                        , details = [ "You can replace this call by the initial accumulator." ]
                                        }
                                        checkInfo.fnRange
                                        (case maybeListArg of
                                            Nothing ->
                                                [ Fix.replaceRangeBy
                                                    { start = checkInfo.fnRange.start
                                                    , end = (Node.range checkInfo.firstArg).end
                                                    }
                                                    (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo))
                                                ]

                                            Just _ ->
                                                keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range initialArg }
                                        )
                                    )

                            else
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


listFoldlCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
listFoldlCompositionChecks checkInfo =
    foldAndSetToListCompositionChecks "foldl" checkInfo


listFoldrCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
listFoldrCompositionChecks checkInfo =
    foldAndSetToListCompositionChecks "foldr" checkInfo


foldAndSetToListCompositionChecks : String -> CompositionIntoCheckInfo -> Maybe (Error {})
foldAndSetToListCompositionChecks foldOperationName checkInfo =
    case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
        ( ( [ "Set" ], "toList" ), [] ) ->
            Just
                (Rule.errorWithFix
                    { message = "To fold a set, you don't need to convert to a List"
                    , details = [ "Using " ++ qualifiedToString ( [ "Set" ], foldOperationName ) ++ " directly is meant for this exact purpose and will also be faster." ]
                    }
                    checkInfo.later.fnRange
                    (keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.later.range }
                        ++ [ Fix.replaceRangeBy checkInfo.later.fnRange
                                (qualifiedToString (qualify ( [ "Set" ], foldOperationName ) checkInfo))
                           ]
                    )
                )

        _ ->
            Nothing


listAllChecks : CheckInfo -> Maybe (Error {})
listAllChecks checkInfo =
    let
        maybeListArg : Maybe (Node Expression)
        maybeListArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case maybeListArg of
                Just (Node _ (Expression.ListExpr [])) ->
                    Just
                        (Rule.errorWithFix
                            { message = "The call to " ++ qualifiedToString ( [ "List" ], "all" ) ++ " will result in True"
                            , details = [ "You can replace this call by True." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Basics" ], "True" ) checkInfo))
                            ]
                        )

                _ ->
                    Nothing
        , \() ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined True ->
                    Just
                        (Rule.errorWithFix
                            { message = "The call to " ++ qualifiedToString ( [ "List" ], "all" ) ++ " will result in True"
                            , details = [ "You can replace this call by True." ]
                            }
                            checkInfo.fnRange
                            (replaceByBoolWithIrrelevantLastArgFix { lastArg = maybeListArg, replacement = True, checkInfo = checkInfo })
                        )

                _ ->
                    Nothing
        ]
        ()


listAnyChecks : CheckInfo -> Maybe (Error {})
listAnyChecks checkInfo =
    let
        maybeListArg : Maybe (Node Expression)
        maybeListArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case maybeListArg of
                Just (Node _ (Expression.ListExpr [])) ->
                    Just
                        (Rule.errorWithFix
                            { message = "The call to " ++ qualifiedToString ( [ "List" ], "any" ) ++ " will result in False"
                            , details = [ "You can replace this call by False." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Basics" ], "False" ) checkInfo))
                            ]
                        )

                _ ->
                    Nothing
        , \() ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined False ->
                    Just
                        (Rule.errorWithFix
                            { message = "The call to " ++ qualifiedToString ( [ "List" ], "any" ) ++ " will result in False"
                            , details = [ "You can replace this call by False." ]
                            }
                            checkInfo.fnRange
                            (replaceByBoolWithIrrelevantLastArgFix { lastArg = maybeListArg, replacement = False, checkInfo = checkInfo })
                        )

                _ ->
                    Nothing
        , \() ->
            case Evaluate.isEqualToSomethingFunction checkInfo.firstArg of
                Nothing ->
                    Nothing

                Just equatedTo ->
                    Just
                        (Rule.errorWithFix
                            { message = "Use " ++ qualifiedToString ( [ "List" ], "member" ) ++ " instead"
                            , details = [ "This call to " ++ qualifiedToString ( [ "List" ], "any" ) ++ " checks for the presence of a value. " ++ qualifiedToString ( [ "List" ], "member" ) ++ " is meant for this exact purpose." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify ( [ "List" ], "member" ) checkInfo))
                                :: replaceBySubExpressionFix (Node.range checkInfo.firstArg) equatedTo.something
                            )
                        )
        ]
        ()


listFilterMapChecks : CheckInfo -> Maybe (Error {})
listFilterMapChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case constructsSpecificInAllBranches ( [ "Maybe" ], "Just" ) checkInfo.lookupTable checkInfo.firstArg of
                Determined justConstruction ->
                    case justConstruction of
                        NonDirectConstruction fix ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "List" ], "filterMap" ) ++ " with a function that will always return Just is the same as using " ++ qualifiedToString ( [ "List" ], "map" )
                                    , details = [ "You can remove the `Just`s and replace the call by " ++ qualifiedToString ( [ "List" ], "map" ) ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify ( [ "List" ], "map" ) checkInfo))
                                        :: fix
                                    )
                                )

                        DirectConstruction ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "List" ], "filterMap" ) ++ " with a function that will always return Just is the same as not using " ++ qualifiedToString ( [ "List" ], "filterMap" )
                                    , details = [ "You can remove this call and replace it by the list itself." ]
                                    }
                                    checkInfo.fnRange
                                    (toIdentityFix
                                        { lastArg = secondArg checkInfo, resources = checkInfo }
                                    )
                                )

                Undetermined ->
                    Nothing
        , \() ->
            case returnsSpecificValueOrFunctionInAllBranches ( [ "Maybe" ], "Nothing" ) checkInfo.lookupTable checkInfo.firstArg of
                Determined _ ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "filterMap" ) ++ " with a function that will always return Nothing will result in an empty list"
                            , details = [ "You can remove this call and replace it by an empty list." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix "[]" checkInfo.parentRange (secondArg checkInfo) checkInfo)
                        )

                Undetermined ->
                    Nothing
        , \() ->
            case secondArg checkInfo of
                Just listArg ->
                    firstThatReportsError
                        [ \() ->
                            callWithEmptyListArgReturnsEmptyListCheck
                                { fn = ( [ "List" ], "filterMap" ), checkedArg = listArg }
                                checkInfo
                        , \() ->
                            if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
                                firstThatReportsError
                                    [ \() ->
                                        case AstHelpers.getSpecificFunctionCall ( [ "List" ], "map" ) checkInfo.lookupTable listArg of
                                            Just listMapCall ->
                                                Just
                                                    (Rule.errorWithFix
                                                        -- TODO rework error info
                                                        { message = qualifiedToString ( [ "List" ], "map" ) ++ " and " ++ qualifiedToString ( [ "List" ], "filterMap" ) ++ " identity can be combined using " ++ qualifiedToString ( [ "List" ], "filterMap" )
                                                        , details = [ qualifiedToString ( [ "List" ], "filterMap" ) ++ " is meant for this exact purpose and will also be faster." ]
                                                        }
                                                        { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end }
                                                        (replaceBySubExpressionFix checkInfo.parentRange listArg
                                                            ++ [ Fix.replaceRangeBy listMapCall.fnRange
                                                                    (qualifiedToString (qualify ( [ "List" ], "filterMap" ) checkInfo))
                                                               ]
                                                        )
                                                    )

                                            Nothing ->
                                                Nothing
                                    , \() ->
                                        case listArg of
                                            Node listRange (Expression.ListExpr list) ->
                                                case collectJusts checkInfo.lookupTable list [] of
                                                    Just justRanges ->
                                                        Just
                                                            (Rule.errorWithFix
                                                                { message = "Unnecessary use of " ++ qualifiedToString ( [ "List" ], "filterMap" ) ++ " identity"
                                                                , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                                                                }
                                                                { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end }
                                                                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = listRange }
                                                                    ++ List.map Fix.removeRange justRanges
                                                                )
                                                            )

                                                    Nothing ->
                                                        Nothing

                                            _ ->
                                                Nothing
                                    ]
                                    ()

                            else
                                Nothing
                        ]
                        ()

                Nothing ->
                    Nothing
        ]
        ()


collectJusts : ModuleNameLookupTable -> List (Node Expression) -> List Range -> Maybe (List Range)
collectJusts lookupTable list acc =
    case list of
        [] ->
            Just acc

        (Node _ element) :: restOfList ->
            case element of
                Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Just")) :: (Node justArgRange _) :: []) ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                        Just [ "Maybe" ] ->
                            collectJusts lookupTable restOfList ({ start = justRange.start, end = justArgRange.start } :: acc)

                        _ ->
                            Nothing

                _ ->
                    Nothing


listFilterMapCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
listFilterMapCompositionChecks checkInfo =
    case checkInfo.later.args of
        elementToMaybeMappingArg :: [] ->
            if AstHelpers.isIdentity checkInfo.lookupTable elementToMaybeMappingArg then
                case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
                    ( ( [ "List" ], "map" ), _ :: [] ) ->
                        Just
                            (Rule.errorWithFix
                                -- TODO rework error info
                                { message = qualifiedToString ( [ "List" ], "map" ) ++ " and " ++ qualifiedToString ( [ "List" ], "filterMap" ) ++ " identity can be combined using " ++ qualifiedToString ( [ "List" ], "filterMap" )
                                , details = [ qualifiedToString ( [ "List" ], "filterMap" ) ++ " is meant for this exact purpose and will also be faster." ]
                                }
                                -- TODO use laterFnRange
                                checkInfo.later.range
                                (Fix.replaceRangeBy checkInfo.earlier.fnRange
                                    (qualifiedToString (qualify ( [ "List" ], "filterMap" ) checkInfo))
                                    :: keepOnlyFix { parentRange = checkInfo.parentRange, keep = checkInfo.earlier.range }
                                )
                            )

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing


listRangeChecks : CheckInfo -> Maybe (Error {})
listRangeChecks checkInfo =
    case secondArg checkInfo of
        Just rangeEndArg ->
            case ( Evaluate.getInt checkInfo checkInfo.firstArg, Evaluate.getInt checkInfo rangeEndArg ) of
                ( Just rangeStartValue, Just rangeEndValue ) ->
                    if rangeStartValue > rangeEndValue then
                        Just
                            (Rule.errorWithFix
                                { message = "The call to " ++ qualifiedToString ( [ "List" ], "range" ) ++ " will result in []"
                                , details = [ "The second argument to " ++ qualifiedToString ( [ "List" ], "range" ) ++ " is bigger than the first one, therefore you can replace this list by an empty list." ]
                                }
                                checkInfo.fnRange
                                (replaceByEmptyFix "[]" checkInfo.parentRange (Just rangeEndValue) checkInfo)
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
listRepeatChecks checkInfo =
    case Evaluate.getInt checkInfo checkInfo.firstArg of
        Just intValue ->
            if intValue < 1 then
                Just
                    (Rule.errorWithFix
                        { message = qualifiedToString ( [ "List" ], "repeat" ) ++ " will result in an empty list"
                        , details = [ "Using " ++ qualifiedToString ( [ "List" ], "repeat" ) ++ " with a number less than 1 will result in an empty list. You can replace this call by an empty list." ]
                        }
                        checkInfo.fnRange
                        (replaceByEmptyFix "[]" checkInfo.parentRange (secondArg checkInfo) checkInfo)
                    )

            else
                Nothing

        Nothing ->
            Nothing


listReverseChecks : CheckInfo -> Maybe (Error {})
listReverseChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "reverse" ) ++ " on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                        )

                _ ->
                    Nothing
        , \() ->
            removeAlongWithOtherFunctionCheck
                reverseReverseCompositionErrorMessage
                (AstHelpers.getSpecificValueOrFunction ( [ "List" ], "reverse" ))
                checkInfo
        ]
        ()


listSortChecks : CheckInfo -> Maybe (Error {})
listSortChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "List" ], "sort" ) ++ " on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                        )

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just _ ->
                    Just
                        (Rule.errorWithFix
                            { message = "Sorting a list with a single element will result in the list itself"
                            , details = [ "You can replace this call by the list itself." ]
                            }
                            checkInfo.fnRange
                            (keepOnlyFix
                                { parentRange = checkInfo.parentRange
                                , keep = Node.range checkInfo.firstArg
                                }
                            )
                        )

                Nothing ->
                    Nothing
        ]
        ()


listSortByChecks : CheckInfo -> Maybe (Error {})
listSortByChecks checkInfo =
    firstThatReportsError
        [ case secondArg checkInfo of
            Just listArg ->
                firstThatReportsError
                    [ \() ->
                        case AstHelpers.getListLiteral listArg of
                            Just [] ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Using " ++ qualifiedToString ( [ "List" ], "sortBy" ) ++ " on [] will result in []"
                                        , details = [ "You can replace this call by []." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                                    )

                            _ ->
                                Nothing
                    , \() ->
                        case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                            Just _ ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Sorting a list with a single element will result in the list itself"
                                        , details = [ "You can replace this call by the list itself." ]
                                        }
                                        checkInfo.fnRange
                                        (keepOnlyFix
                                            { parentRange = checkInfo.parentRange
                                            , keep = Node.range listArg
                                            }
                                        )
                                    )

                            Nothing ->
                                Nothing
                    ]

            Nothing ->
                \() -> Nothing
        , \() ->
            case AstHelpers.getAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just _ ->
                    Just
                        (identityError
                            { toFix = qualifiedToString ( [ "List" ], "sortBy" ) ++ " (always a)"
                            , lastArgName = "list"
                            , lastArg = secondArg checkInfo
                            , resources = checkInfo
                            }
                        )

                Nothing ->
                    Nothing
        , \() ->
            if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
                Just
                    (Rule.errorWithFix
                        { message = "Using " ++ qualifiedToString ( [ "List" ], "sortBy" ) ++ " identity is the same as using " ++ qualifiedToString ( [ "List" ], "sort" )
                        , details = [ "You can replace this call by " ++ qualifiedToString ( [ "List" ], "sort" ) ++ "." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy
                            { start = checkInfo.fnRange.start
                            , end = (Node.range checkInfo.firstArg).end
                            }
                            (qualifiedToString (qualify ( [ "List" ], "sort" ) checkInfo))
                        ]
                    )

            else
                Nothing
        ]
        ()


listSortWithChecks : CheckInfo -> Maybe (Error {})
listSortWithChecks checkInfo =
    firstThatReportsError
        [ case secondArg checkInfo of
            Just listArg ->
                firstThatReportsError
                    [ \() ->
                        case AstHelpers.getListLiteral listArg of
                            Just [] ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Using " ++ qualifiedToString ( [ "List" ], "sortWith" ) ++ " on [] will result in []"
                                        , details = [ "You can replace this call by []." ]
                                        }
                                        checkInfo.fnRange
                                        [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                                    )

                            _ ->
                                Nothing
                    , \() ->
                        case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                            Just _ ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Sorting a list with a single element will result in the list itself"
                                        , details = [ "You can replace this call by the list itself." ]
                                        }
                                        checkInfo.fnRange
                                        (keepOnlyFix
                                            { parentRange = checkInfo.parentRange
                                            , keep = Node.range listArg
                                            }
                                        )
                                    )

                            _ ->
                                Nothing
                    ]

            Nothing ->
                \() -> Nothing
        , \() ->
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
                            identityError
                                { toFix = qualifiedToString ( [ "List" ], "sortWith" ) ++ " (\\_ _ -> " ++ AstHelpers.orderToString order ++ ")"
                                , lastArgName = "list"
                                , lastArg = secondArg checkInfo
                                , resources = checkInfo
                                }
                    in
                    case order of
                        LT ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "List" ], "sortWith" ) ++ " (\\_ _ -> LT) is the same as using " ++ qualifiedToString ( [ "List" ], "reverse" )
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
        ()


listTakeChecks : CheckInfo -> Maybe (Error {})
listTakeChecks checkInfo =
    let
        maybeListArg : Maybe (Node Expression)
        maybeListArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just length ->
                    callWithNonPositiveIntCanBeReplacedByCheck
                        { fn = ( [ "List" ], "take" )
                        , int = length
                        , intDescription = "length"
                        , replacement = "[]"
                        , replacementDescription = "an empty list"
                        , lastArg = maybeListArg
                        }
                        checkInfo

                Nothing ->
                    Nothing
        , \() ->
            case maybeListArg of
                Just listArg ->
                    case determineListLength checkInfo.lookupTable listArg of
                        Just (Exactly 0) ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "List" ], "take" ) ++ " on [] will result in []"
                                    , details = [ "You can replace this call by []." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        ]
        ()


listDropChecks : CheckInfo -> Maybe (Error {})
listDropChecks checkInfo =
    let
        maybeListArg : Maybe (Node Expression)
        maybeListArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just 0 ->
                    Just
                        -- TODO use identityError
                        (Rule.errorWithFix
                            (case maybeListArg of
                                Just _ ->
                                    { message = "Dropping 0 items from a list will result in the list itself"
                                    , details = [ "You can replace this call by the list itself." ]
                                    }

                                Nothing ->
                                    { message = "Dropping 0 items from a list will result in the list itself"
                                    , details = [ "You can replace this function by identity." ]
                                    }
                            )
                            checkInfo.fnRange
                            (toIdentityFix { lastArg = maybeListArg, resources = checkInfo })
                        )

                _ ->
                    Nothing
        , \() ->
            case maybeListArg of
                Just listArg ->
                    case determineListLength checkInfo.lookupTable listArg of
                        Just (Exactly 0) ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "List" ], "drop" ) ++ " on [] will result in []"
                                    , details = [ "You can replace this call by []." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        ]
        ()


listMapNChecks : { n : Int } -> CheckInfo -> Maybe (Error {})
listMapNChecks { n } checkInfo =
    if List.any (\list -> AstHelpers.getListLiteral list == Just []) checkInfo.argsAfterFirst then
        let
            callReplacement : String
            callReplacement =
                multiAlways (n - List.length checkInfo.argsAfterFirst) "[]" checkInfo
        in
        Just
            (Rule.errorWithFix
                { message = "Using " ++ qualifiedToString ( [ "List" ], "map" ++ String.fromInt n ) ++ " with any list being [] will result in []"
                , details = [ "You can replace this call by " ++ callReplacement ++ "." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange callReplacement ]
            )

    else
        Nothing


listUnzipChecks : CheckInfo -> Maybe (Error {})
listUnzipChecks checkInfo =
    case AstHelpers.getListLiteral checkInfo.firstArg of
        Just [] ->
            Just
                (Rule.errorWithFix
                    { message = "Using " ++ qualifiedToString ( [ "List" ], "unzip" ) ++ " on [] will result in ( [], [] )"
                    , details = [ "You can replace this call by ( [], [] )." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "( [], [] )" ]
                )

        _ ->
            Nothing


setFromListChecks : CheckInfo -> Maybe (Error {})
setFromListChecks checkInfo =
    firstThatReportsError
        [ \() -> collectionFromListChecks setCollection checkInfo
        , \() -> setFromListSingletonChecks checkInfo
        ]
        ()


setFromListSingletonChecks : CheckInfo -> Maybe (Error {})
setFromListSingletonChecks checkInfo =
    case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
        Nothing ->
            Nothing

        Just listSingleton ->
            Just
                (Rule.errorWithFix
                    setFromListSingletonError
                    checkInfo.fnRange
                    (replaceBySubExpressionFix (Node.range checkInfo.firstArg) listSingleton.element
                        ++ [ Fix.replaceRangeBy checkInfo.fnRange (qualifiedToString (qualify ( [ "Set" ], "singleton" ) checkInfo)) ]
                    )
                )


setFromListSingletonError : { message : String, details : List String }
setFromListSingletonError =
    { message = qualifiedToString ( [ "Set" ], "fromList" ) ++ " with a single element can be replaced using " ++ qualifiedToString ( [ "Set" ], "singleton" )
    , details = [ "You can replace this call by " ++ qualifiedToString ( [ "Set" ], "singleton" ) ++ " with the list element itself." ]
    }


setFromListCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
setFromListCompositionChecks checkInfo =
    case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
        ( ( [ "List" ], "singleton" ), [] ) ->
            Just
                (Rule.errorWithFix
                    setFromListSingletonError
                    checkInfo.later.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Set" ], "singleton" ) checkInfo))
                    ]
                )

        _ ->
            Nothing


subAndCmdBatchChecks : String -> CheckInfo -> Maybe (Error {})
subAndCmdBatchChecks moduleName checkInfo =
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListLiteral checkInfo.firstArg of
                Just [] ->
                    Just
                        (Rule.errorWithFix
                            { message = "Replace by " ++ moduleName ++ ".batch"
                            , details = [ moduleName ++ ".batch [] and " ++ moduleName ++ ".none are equivalent but the latter is more idiomatic in Elm code" ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Platform", moduleName ], "none" ) checkInfo))
                            ]
                        )

                Just (arg0 :: arg1 :: arg2Up) ->
                    neighboringMap
                        (\arg ->
                            case AstHelpers.removeParens arg.current of
                                Node batchRange (Expression.FunctionOrValue _ "none") ->
                                    if ModuleNameLookupTable.moduleNameAt checkInfo.lookupTable batchRange == Just [ "Platform", moduleName ] then
                                        let
                                            argRange : Range
                                            argRange =
                                                Node.range arg.current
                                        in
                                        Just
                                            (Rule.errorWithFix
                                                { message = "Unnecessary " ++ moduleName ++ ".none"
                                                , details = [ moduleName ++ ".none will be ignored by " ++ moduleName ++ ".batch." ]
                                                }
                                                argRange
                                                (case arg.before of
                                                    Just (Node prevRange _) ->
                                                        [ Fix.removeRange { start = prevRange.end, end = argRange.end } ]

                                                    Nothing ->
                                                        case arg.after of
                                                            Just (Node nextRange _) ->
                                                                [ Fix.removeRange { start = argRange.start, end = nextRange.start } ]

                                                            Nothing ->
                                                                [ Fix.replaceRangeBy checkInfo.parentRange
                                                                    (qualifiedToString (qualify ( [ "Platform", moduleName ], "none" ) checkInfo))
                                                                ]
                                                )
                                            )

                                    else
                                        Nothing

                                _ ->
                                    Nothing
                        )
                        (arg0 :: arg1 :: arg2Up)
                        |> findMap identity

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable checkInfo.firstArg of
                Just listSingletonArg ->
                    Just
                        (Rule.errorWithFix
                            { message = "Unnecessary " ++ moduleName ++ ".batch"
                            , details = [ moduleName ++ ".batch with a single element is equal to that element." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange listSingletonArg.element)
                        )

                Nothing ->
                    Nothing
        ]
        ()



-- HTML.ATTRIBUTES


htmlAttributesClassListFalseElementError : { message : String, details : List String }
htmlAttributesClassListFalseElementError =
    { message = "In a " ++ qualifiedToString ( [ "Html", "Attributes" ], "classList" ) ++ ", a tuple paired with False can be removed"
    , details = [ "You can remove the tuple list element where the second part is False." ]
    }


htmlAttributesClassListChecks : CheckInfo -> Maybe (Error {})
htmlAttributesClassListChecks checkInfo =
    let
        listArg : Node Expression
        listArg =
            checkInfo.firstArg

        getTupleWithSpecificSecond : Bool -> Node Expression -> Maybe { range : Range, first : Node Expression }
        getTupleWithSpecificSecond specificBool expressionNode =
            case AstHelpers.getTuple expressionNode of
                Just tuple ->
                    case AstHelpers.getSpecificBool specificBool checkInfo.lookupTable tuple.second of
                        Just _ ->
                            Just { range = tuple.range, first = tuple.first }

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
    in
    firstThatReportsError
        [ \() ->
            case AstHelpers.getListSingleton checkInfo.lookupTable listArg of
                Just single ->
                    case AstHelpers.getTuple single.element of
                        Just tuple ->
                            case AstHelpers.getBool checkInfo.lookupTable tuple.second of
                                Just bool ->
                                    if bool then
                                        Just
                                            (Rule.errorWithFix
                                                { message = qualifiedToString ( [ "Html", "Attributes" ], "classList" ) ++ " with a single tuple paired with True can be replaced with " ++ qualifiedToString ( [ "Html", "Attributes" ], "class" )
                                                , details = [ "You can replace this call by " ++ qualifiedToString ( [ "Html", "Attributes" ], "class" ) ++ " with the String from the single tuple list element." ]
                                                }
                                                checkInfo.fnRange
                                                (replaceBySubExpressionFix (Node.range listArg) tuple.first
                                                    ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                                            (qualifiedToString (qualify ( [ "Html", "Attributes" ], "class" ) checkInfo))
                                                       ]
                                                )
                                            )

                                    else
                                        Just
                                            (Rule.errorWithFix htmlAttributesClassListFalseElementError
                                                checkInfo.fnRange
                                                [ Fix.replaceRangeBy (Node.range listArg) "[]" ]
                                            )

                                Nothing ->
                                    Nothing

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing
        , \() ->
            case AstHelpers.getListLiteral listArg of
                Just (tuple0 :: tuple1 :: tuple2Up) ->
                    case findMapNeighboring (getTupleWithSpecificSecond False) (tuple0 :: tuple1 :: tuple2Up) of
                        Just classPart ->
                            Just
                                (Rule.errorWithFix htmlAttributesClassListFalseElementError
                                    checkInfo.fnRange
                                    (listLiteralElementRemoveFix classPart)
                                )

                        Nothing ->
                            Nothing

                _ ->
                    Nothing
        , \() ->
            case AstHelpers.getCollapsedCons listArg of
                Just classParts ->
                    case findMapNeighboring (getTupleWithSpecificSecond False) classParts.consed of
                        Just classPart ->
                            Just
                                (Rule.errorWithFix htmlAttributesClassListFalseElementError
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
        ()



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
randomListChecks checkInfo =
    let
        maybeElementGeneratorArg : Maybe (Node Expression)
        maybeElementGeneratorArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just 1 ->
                    Just
                        (Rule.errorWithFix
                            { message = qualifiedToString ( [ "Random" ], "list" ) ++ " 1 can be replaced by " ++ qualifiedToString ( [ "Random" ], "map" ) ++ " " ++ qualifiedToString ( [ "List" ], "singleton" )
                            , details = [ "This " ++ qualifiedToString ( [ "Random" ], "list" ) ++ " call always produces a list with one generated element. This means you can replace the call with " ++ qualifiedToString ( [ "Random" ], "map" ) ++ " " ++ qualifiedToString ( [ "List" ], "singleton" ) ++ "." ]
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
                        let
                            replacement : String
                            replacement =
                                replacementWithIrrelevantLastArg
                                    { forNoLastArg =
                                        qualifiedToString (qualify ( [ "Random" ], "constant" ) checkInfo)
                                            ++ " []"
                                    , lastArg = maybeElementGeneratorArg
                                    , resources = checkInfo
                                    }

                            callDescription : String
                            callDescription =
                                case non1Length of
                                    0 ->
                                        "Random.list 0"

                                    _ ->
                                        "Random.list with a negative length"
                        in
                        Just
                            (Rule.errorWithFix
                                { message = callDescription ++ " can be replaced by Random.constant []"
                                , details = [ callDescription ++ " always generates an empty list. This means you can replace the call with " ++ replacement ++ "." ]
                                }
                                checkInfo.fnRange
                                [ Fix.replaceRangeBy checkInfo.parentRange replacement ]
                            )

                    else
                        Nothing

                Nothing ->
                    Nothing
        , \() ->
            case maybeElementGeneratorArg of
                Just elementGeneratorArg ->
                    case AstHelpers.getSpecificFunctionCall ( [ "Random" ], "constant" ) checkInfo.lookupTable elementGeneratorArg of
                        Just constantCall ->
                            let
                                currentAsString : String
                                currentAsString =
                                    qualifiedToString ( [ "Random" ], "list" ) ++ " n (" ++ qualifiedToString ( [ "Random" ], "constant" ) ++ " el)"

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
        ()


randomMapChecks : CheckInfo -> Maybe (Error {})
randomMapChecks checkInfo =
    firstThatReportsError
        [ \() -> containerMapIdentityChecks { moduleName = [ "Random" ], represents = "random generator" } checkInfo
        , \() -> mapPureChecks { moduleName = [ "Random" ], pure = "constant", map = "map" } checkInfo
        , \() -> randomMapAlwaysChecks checkInfo
        ]
        ()


randomMapCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
randomMapCompositionChecks checkInfo =
    firstThatReportsError
        [ \() -> pureToMapCompositionChecks { moduleName = [ "Random" ], pure = "constant", map = "map" } checkInfo
        , \() -> randomMapAlwaysCompositionChecks checkInfo
        ]
        ()


randomMapAlwaysErrorInfo : { message : String, details : List String }
randomMapAlwaysErrorInfo =
    { message = "Always mapping to the same value is equivalent to Random.constant"
    , details = [ "Since your Random.map call always produces the same value, you can replace the whole call by Random.constant that value." ]
    }


randomMapAlwaysChecks : CheckInfo -> Maybe (Error {})
randomMapAlwaysChecks checkInfo =
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
                (Rule.errorWithFix
                    randomMapAlwaysErrorInfo
                    checkInfo.fnRange
                    (case secondArg checkInfo of
                        Nothing ->
                            [ Fix.replaceRangeBy
                                { start = checkInfo.parentRange.start, end = alwaysMapResultRange.start }
                                (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo)
                                    ++ " ("
                                    ++ qualifiedToString (qualify ( [ "Random" ], "constant" ) checkInfo)
                                    ++ " "
                                    ++ leftParenIfRequired
                                )
                            , Fix.replaceRangeBy
                                { start = alwaysMapResultRange.end, end = checkInfo.parentRange.end }
                                (rightParenIfRequired ++ ")")
                            ]

                        Just _ ->
                            [ Fix.replaceRangeBy
                                { start = checkInfo.parentRange.start, end = alwaysMapResultRange.start }
                                (qualifiedToString (qualify ( [ "Random" ], "constant" ) checkInfo)
                                    ++ " "
                                    ++ leftParenIfRequired
                                )
                            , Fix.replaceRangeBy
                                { start = alwaysMapResultRange.end, end = checkInfo.parentRange.end }
                                rightParenIfRequired
                            ]
                    )
                )

        Nothing ->
            Nothing


randomMapAlwaysCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
randomMapAlwaysCompositionChecks checkInfo =
    case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
        ( ( [ "Basics" ], "always" ), [] ) ->
            Just
                (Rule.errorWithFix
                    randomMapAlwaysErrorInfo
                    checkInfo.later.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Random" ], "constant" ) checkInfo))
                    ]
                )

        _ ->
            Nothing



--


extractQualifyResources : QualifyResources a -> QualifyResources {}
extractQualifyResources resources =
    { importLookup = resources.importLookup
    , moduleBindings = resources.moduleBindings
    , localBindings = resources.localBindings
    }


emptyAsString : QualifyResources a -> { emptiable | emptyAsString : QualifyResources {} -> String } -> String
emptyAsString qualifyResources emptiable =
    emptiable.emptyAsString (extractQualifyResources qualifyResources)


listCollection : Collection {}
listCollection =
    { moduleName = [ "List" ]
    , represents = "list"
    , emptyAsString = \_ -> "[]"
    , emptyDescription = "[]"
    , isEmpty = \_ expr -> AstHelpers.getListLiteral expr == Just []
    , nameForSize = "length"
    , determineSize = determineListLength
    }


setCollection : Collection {}
setCollection =
    { moduleName = [ "Set" ]
    , represents = "set"
    , emptyAsString =
        \resources ->
            qualifiedToString (qualify ( [ "Set" ], "empty" ) resources)
    , emptyDescription = qualifiedToString ( [ "Set" ], "empty" )
    , isEmpty =
        \lookupTable expr ->
            isJust (AstHelpers.getSpecificValueOrFunction ( [ "Set" ], "empty" ) lookupTable expr)
    , nameForSize = "size"
    , determineSize = setDetermineSize
    }


setDetermineSize :
    ModuleNameLookupTable
    -> Node Expression
    -> Maybe CollectionSize
setDetermineSize lookupTable expressionNode =
    findMap (\f -> f ())
        [ \() ->
            case AstHelpers.getSpecificValueOrFunction ( [ "Set" ], "empty" ) lookupTable expressionNode of
                Just _ ->
                    Just (Exactly 0)

                Nothing ->
                    Nothing
        , \() ->
            case AstHelpers.getSpecificFunctionCall ( [ "Set" ], "singleton" ) lookupTable expressionNode of
                Just _ ->
                    Just (Exactly 1)

                Nothing ->
                    Nothing
        , \() ->
            case AstHelpers.getSpecificFunctionCall ( [ "Set" ], "fromList" ) lookupTable expressionNode of
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


dictCollection : Collection {}
dictCollection =
    { moduleName = [ "Dict" ]
    , represents = "Dict"
    , emptyAsString =
        \resources ->
            qualifiedToString (qualify ( [ "Dict" ], "empty" ) resources)
    , emptyDescription = qualifiedToString ( [ "Dict" ], "empty" )
    , isEmpty =
        \lookupTable expr ->
            isJust (AstHelpers.getSpecificValueOrFunction ( [ "Dict" ], "empty" ) lookupTable expr)
    , nameForSize = "size"
    , determineSize = dictDetermineSize
    }


dictDetermineSize :
    ModuleNameLookupTable
    -> Node Expression
    -> Maybe CollectionSize
dictDetermineSize lookupTable expressionNode =
    findMap (\f -> f ())
        [ \() ->
            case AstHelpers.getSpecificValueOrFunction ( [ "Dict" ], "empty" ) lookupTable expressionNode of
                Just _ ->
                    Just (Exactly 0)

                Nothing ->
                    Nothing
        , \() ->
            case AstHelpers.getSpecificFunctionCall ( [ "Dict" ], "singleton" ) lookupTable expressionNode of
                Just singletonCall ->
                    case singletonCall.argsAfterFirst of
                        _ :: [] ->
                            Just (Exactly 1)

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        , \() ->
            case AstHelpers.getSpecificFunctionCall ( [ "Dict" ], "fromList" ) lookupTable expressionNode of
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


{-| Properties of a type that can hold some data or none.
-}
type alias Container otherProperties =
    { otherProperties
        | moduleName : ModuleName
        , represents : String
        , emptyAsString : QualifyResources {} -> String
        , emptyDescription : String
        , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    }


{-| Properties of a `Container` type with an explicit constructor for the "there is some data there" case.
-}
type alias Defaultable otherProperties =
    Container
        { otherProperties
            | isSomethingConstructor : QualifyResources {} -> String
        }


{-| Properties of a `Container` type that can be
transformed (e.g. added to, removed from)
and observed (e.g. checked for emptiness, size, member).
-}
type alias Collection otherProperties =
    Container
        { otherProperties
            | nameForSize : String
            , determineSize : ModuleNameLookupTable -> Node Expression -> Maybe CollectionSize
        }


maybeCollection : Defaultable {}
maybeCollection =
    { moduleName = [ "Maybe" ]
    , represents = "maybe"
    , emptyAsString =
        \resources ->
            qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) resources)
    , emptyDescription = "Nothing"
    , isEmpty =
        \lookupTable expr ->
            isJust (AstHelpers.getSpecificValueOrFunction ( [ "Maybe" ], "Nothing" ) lookupTable expr)
    , isSomethingConstructor =
        \resources ->
            qualifiedToString (qualify ( [ "Maybe" ], "Just" ) resources)
    }


resultCollection : Defaultable {}
resultCollection =
    { moduleName = [ "Result" ]
    , represents = "result"
    , emptyAsString =
        \resources ->
            qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) resources)
    , emptyDescription = "an error"
    , isEmpty =
        \lookupTable expr ->
            isJust (AstHelpers.getSpecificFunctionCall ( [ "Result" ], "Err" ) lookupTable expr)
    , isSomethingConstructor =
        \resources ->
            qualifiedToString (qualify ( [ "Result" ], "Ok" ) resources)
    }


cmdCollection : Container {}
cmdCollection =
    { moduleName = [ "Platform", "Cmd" ]
    , represents = "command"
    , emptyAsString =
        \resources ->
            qualifiedToString (qualify ( [ "Platform", "Cmd" ], "none" ) resources)
    , emptyDescription =
        -- TODO change to qualifiedToString ( [ "Platform", "Cmd" ], "none" )
        "Cmd.none"
    , isEmpty =
        \lookupTable expr ->
            isJust (AstHelpers.getSpecificValueOrFunction ( [ "Platform", "Cmd" ], "none" ) lookupTable expr)
    }


subCollection : Container {}
subCollection =
    { moduleName = [ "Platform", "Sub" ]
    , represents = "subscription"
    , emptyAsString =
        \resources ->
            qualifiedToString (qualify ( [ "Platform", "Sub" ], "none" ) resources)
    , emptyDescription =
        -- TODO change to qualifiedToString ( [ "Platform", "Sub" ], "none" )
        "Sub.none"
    , isEmpty =
        \lookupTable expr ->
            isJust (AstHelpers.getSpecificValueOrFunction ( [ "Platform", "Sub" ], "none" ) lookupTable expr)
    }


containerMapChecks :
    { a
        | moduleName : ModuleName
        , represents : String
        , emptyDescription : String
        , emptyAsString : QualifyResources {} -> String
        , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    }
    -> CheckInfo
    -> Maybe (Error {})
containerMapChecks collection checkInfo =
    firstThatReportsError
        [ \() -> containerMapIdentityChecks collection checkInfo
        , \() ->
            case secondArg checkInfo of
                Just collectionArg ->
                    callOnEmptyReturnsEmptyCheck "map" collectionArg collection checkInfo

                Nothing ->
                    Nothing
        ]
        ()


operationDoesNotChangeSpecificLastArgErrorInfo : { fn : ( ModuleName, String ), replacementDescription : String } -> { message : String, details : List String }
operationDoesNotChangeSpecificLastArgErrorInfo config =
    { message = "Using " ++ qualifiedToString config.fn ++ " on " ++ config.replacementDescription ++ " will result in " ++ config.replacementDescription
    , details = [ "You can replace this call by " ++ config.replacementDescription ++ "." ]
    }


mapIdentityChecks :
    String
    ->
        { a
            | moduleName : ModuleName
            , represents : String
        }
    -> CheckInfo
    -> Maybe (Error {})
mapIdentityChecks mapFnName mappable checkInfo =
    if AstHelpers.isIdentity checkInfo.lookupTable checkInfo.firstArg then
        Just
            (identityError
                { toFix = qualifiedToString ( mappable.moduleName, mapFnName ) ++ " with an identity function"
                , lastArg = secondArg checkInfo
                , lastArgName = mappable.represents
                , resources = checkInfo
                }
            )

    else
        Nothing


containerMapIdentityChecks :
    { a
        | moduleName : ModuleName
        , represents : String
    }
    -> CheckInfo
    -> Maybe (Error {})
containerMapIdentityChecks mappable checkInfo =
    mapIdentityChecks "map" mappable checkInfo


mapPureChecks :
    { a | moduleName : List String, pure : String, map : String }
    -> CheckInfo
    -> Maybe (Error {})
mapPureChecks mappable checkInfo =
    case secondArg checkInfo of
        Just mappableArg ->
            case sameCallInAllBranches ( mappable.moduleName, mappable.pure ) checkInfo.lookupTable mappableArg of
                Determined pureCalls ->
                    let
                        mappingArgRange : Range
                        mappingArgRange =
                            Node.range checkInfo.firstArg

                        removePureCalls : List Fix
                        removePureCalls =
                            List.concatMap
                                (\pureCall ->
                                    keepOnlyFix
                                        { parentRange = pureCall.nodeRange
                                        , keep = Node.range pureCall.firstArg
                                        }
                                )
                                pureCalls
                    in
                    Just
                        (Rule.errorWithFix
                            -- TODO reword error info to something more like resultMapErrorOnErrErrorInfo
                            { message = "Calling " ++ qualifiedToString ( mappable.moduleName, mappable.map ) ++ " on a value that is " ++ mappable.pure
                            , details = [ "The function can be called without " ++ qualifiedToString ( mappable.moduleName, mappable.map ) ++ "." ]
                            }
                            checkInfo.fnRange
                            (if checkInfo.usingRightPizza then
                                [ Fix.removeRange { start = checkInfo.fnRange.start, end = mappingArgRange.start }
                                , Fix.insertAt mappingArgRange.end
                                    (" |> " ++ qualifiedToString (qualify ( mappable.moduleName, mappable.pure ) checkInfo))
                                ]
                                    ++ removePureCalls

                             else
                                [ Fix.replaceRangeBy
                                    { start = checkInfo.parentRange.start, end = mappingArgRange.start }
                                    (qualifiedToString (qualify ( mappable.moduleName, mappable.pure ) checkInfo) ++ " (")
                                , Fix.insertAt checkInfo.parentRange.end ")"
                                ]
                                    ++ removePureCalls
                            )
                        )

                Undetermined ->
                    Nothing

        Nothing ->
            Nothing


pureToMapCompositionChecks :
    { a | moduleName : ModuleName, pure : String, map : String }
    -> CompositionIntoCheckInfo
    -> Maybe (Error {})
pureToMapCompositionChecks mappable checkInfo =
    case
        ( checkInfo.earlier.fn == ( mappable.moduleName, mappable.pure )
        , checkInfo.later.args
        )
    of
        ( True, (Node mapperFunctionRange _) :: _ ) ->
            let
                fixes : List Fix
                fixes =
                    case checkInfo.direction of
                        LeftToRight ->
                            [ Fix.removeRange
                                { start = checkInfo.parentRange.start, end = mapperFunctionRange.start }
                            , Fix.insertAt mapperFunctionRange.end
                                (" >> " ++ qualifiedToString (qualify ( mappable.moduleName, mappable.pure ) checkInfo))
                            ]

                        RightToLeft ->
                            [ Fix.replaceRangeBy
                                { start = checkInfo.parentRange.start, end = mapperFunctionRange.start }
                                (qualifiedToString (qualify ( mappable.moduleName, mappable.pure ) checkInfo) ++ " << ")
                            , Fix.removeRange { start = mapperFunctionRange.end, end = checkInfo.parentRange.end }
                            ]
            in
            Just
                (Rule.errorWithFix
                    -- TODO reword error info
                    { message = "Calling " ++ qualifiedToString ( mappable.moduleName, mappable.map ) ++ " on a value that is " ++ mappable.pure
                    , details = [ "The function can be called without " ++ qualifiedToString ( mappable.moduleName, mappable.map ) ++ "." ]
                    }
                    checkInfo.later.fnRange
                    fixes
                )

        _ ->
            Nothing


maybeAndThenChecks : CheckInfo -> Maybe (Error {})
maybeAndThenChecks checkInfo =
    let
        maybeEmptyAsString : String
        maybeEmptyAsString =
            emptyAsString checkInfo maybeCollection

        maybeMaybeArg : Maybe (Node Expression)
        maybeMaybeArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ case maybeMaybeArg of
            Just maybeArg ->
                firstThatReportsError
                    [ \() ->
                        case sameCallInAllBranches ( [ "Maybe" ], "Just" ) checkInfo.lookupTable maybeArg of
                            Determined justCalls ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Calling " ++ qualifiedToString ( maybeCollection.moduleName, "andThen" ) ++ " on a value that is known to be Just"
                                        , details = [ "You can remove the Just and just call the function directly." ]
                                        }
                                        checkInfo.fnRange
                                        (Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                                            :: List.concatMap (\justCall -> replaceBySubExpressionFix justCall.nodeRange justCall.firstArg) justCalls
                                        )
                                    )

                            Undetermined ->
                                Nothing
                    , \() ->
                        case sameValueOrFunctionInAllBranches ( [ "Maybe" ], "Nothing" ) checkInfo.lookupTable maybeArg of
                            Determined _ ->
                                Just
                                    (Rule.errorWithFix
                                        (operationDoesNotChangeSpecificLastArgErrorInfo
                                            { fn = ( maybeCollection.moduleName, "andThen" ), replacementDescription = maybeEmptyAsString }
                                        )
                                        checkInfo.fnRange
                                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range maybeArg })
                                    )

                            Undetermined ->
                                Nothing
                    ]

            Nothing ->
                \() -> Nothing
        , \() ->
            case constructsSpecificInAllBranches ( [ "Maybe" ], "Just" ) checkInfo.lookupTable checkInfo.firstArg of
                Determined justConstruction ->
                    case justConstruction of
                        NonDirectConstruction fix ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Use " ++ qualifiedToString ( maybeCollection.moduleName, "map" ) ++ " instead"
                                    , details = [ "Using " ++ qualifiedToString ( maybeCollection.moduleName, "andThen" ) ++ " with a function that always returns Just is the same thing as using " ++ qualifiedToString ( maybeCollection.moduleName, "map" ) ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify ( maybeCollection.moduleName, "map" ) checkInfo))
                                        :: fix
                                    )
                                )

                        DirectConstruction ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( maybeCollection.moduleName, "andThen" ) ++ " with a function that will always return Just is the same as not using " ++ qualifiedToString ( maybeCollection.moduleName, "andThen" )
                                    , details = [ "You can remove this call and replace it by the value itself." ]
                                    }
                                    checkInfo.fnRange
                                    (toIdentityFix
                                        { lastArg = maybeMaybeArg, resources = checkInfo }
                                    )
                                )

                Undetermined ->
                    Nothing
        , \() ->
            case returnsSpecificValueOrFunctionInAllBranches ( [ "Maybe" ], "Nothing" ) checkInfo.lookupTable checkInfo.firstArg of
                Determined _ ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( maybeCollection.moduleName, "andThen" ) ++ " with a function that will always return Nothing will result in Nothing"
                            , details = [ "You can remove this call and replace it by Nothing." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix maybeEmptyAsString checkInfo.parentRange (secondArg checkInfo) checkInfo)
                        )

                Undetermined ->
                    Nothing
        ]
        ()


resultAndThenChecks : CheckInfo -> Maybe (Error {})
resultAndThenChecks checkInfo =
    let
        maybeResultArg : Maybe (Node Expression)
        maybeResultArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ case maybeResultArg of
            Just resultArg ->
                firstThatReportsError
                    [ \() ->
                        case sameCallInAllBranches ( [ "Result" ], "Ok" ) checkInfo.lookupTable resultArg of
                            Determined okCalls ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Calling " ++ qualifiedToString ( resultCollection.moduleName, "andThen" ) ++ " on a value that is known to be Ok"
                                        , details = [ "You can remove the Ok and just call the function directly." ]
                                        }
                                        checkInfo.fnRange
                                        (Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                                            :: List.concatMap (\okCall -> replaceBySubExpressionFix okCall.nodeRange okCall.firstArg) okCalls
                                        )
                                    )

                            Undetermined ->
                                Nothing
                    , \() ->
                        case sameCallInAllBranches ( [ "Result" ], "Err" ) checkInfo.lookupTable resultArg of
                            Determined _ ->
                                Just
                                    (Rule.errorWithFix
                                        { message = "Using " ++ qualifiedToString ( resultCollection.moduleName, "andThen" ) ++ " on an error will result in the error"
                                        , details = [ "You can replace this call by the error itself." ]
                                        }
                                        checkInfo.fnRange
                                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range resultArg })
                                    )

                            Undetermined ->
                                Nothing
                    ]

            Nothing ->
                \() -> Nothing
        , \() ->
            case constructsSpecificInAllBranches ( [ "Result" ], "Ok" ) checkInfo.lookupTable checkInfo.firstArg of
                Determined okConstruction ->
                    case okConstruction of
                        NonDirectConstruction fix ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Use " ++ qualifiedToString ( [ "Result" ], "map" ) ++ " instead"
                                    , details = [ "Using " ++ qualifiedToString ( [ "Result" ], "andThen" ) ++ " with a function that always returns Ok is the same thing as using " ++ qualifiedToString ( [ "Result" ], "map" ) ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify ( resultCollection.moduleName, "map" ) checkInfo))
                                        :: fix
                                    )
                                )

                        DirectConstruction ->
                            Just
                                (identityError
                                    { toFix = qualifiedToString ( [ "Result" ], "andThen" ) ++ " with a function equivalent to Ok"
                                    , lastArgName = "result"
                                    , lastArg = maybeResultArg
                                    , resources = checkInfo
                                    }
                                )

                Undetermined ->
                    Nothing
        ]
        ()


resultWithDefaultChecks : CheckInfo -> Maybe (Error {})
resultWithDefaultChecks checkInfo =
    case secondArg checkInfo of
        Just resultArg ->
            firstThatReportsError
                [ \() ->
                    case sameCallInAllBranches ( [ "Result" ], "Ok" ) checkInfo.lookupTable resultArg of
                        Determined okCalls ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "Result" ], "withDefault" ) ++ " on a value that is Ok will result in that value"
                                    , details = [ "You can replace this call by the value wrapped in Ok." ]
                                    }
                                    checkInfo.fnRange
                                    (List.concatMap (\okCall -> replaceBySubExpressionFix okCall.nodeRange okCall.firstArg) okCalls
                                        ++ keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range resultArg }
                                    )
                                )

                        Undetermined ->
                            Nothing
                , \() ->
                    case sameCallInAllBranches ( [ "Result" ], "Err" ) checkInfo.lookupTable resultArg of
                        Determined _ ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "Result" ], "withDefault" ) ++ " on an error will result in the default value"
                                    , details = [ "You can replace this call by the default value." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                                    , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                                    ]
                                )

                        Undetermined ->
                            Nothing
                ]
                ()

        Nothing ->
            Nothing


resultToMaybeChecks : CheckInfo -> Maybe (Error {})
resultToMaybeChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case sameCallInAllBranches ( [ "Result" ], "Ok" ) checkInfo.lookupTable checkInfo.firstArg of
                Determined okCalls ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "Result" ], "toMaybe" ) ++ " on a value that is Ok will result in Just that value itself"
                            , details = [ "You can replace this call by the value itself wrapped in Just." ]
                            }
                            checkInfo.fnRange
                            (List.concatMap (\okCall -> replaceBySubExpressionFix okCall.nodeRange okCall.firstArg) okCalls
                                ++ [ Fix.replaceRangeBy checkInfo.fnRange
                                        (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                                   ]
                            )
                        )

                Undetermined ->
                    Nothing
        , \() ->
            case sameCallInAllBranches ( [ "Result" ], "Err" ) checkInfo.lookupTable checkInfo.firstArg of
                Determined _ ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( [ "Result" ], "toMaybe" ) ++ " on an error will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo))
                            ]
                        )

                Undetermined ->
                    Nothing
        ]
        ()


resultToMaybeCompositionChecks : CompositionIntoCheckInfo -> Maybe (Error {})
resultToMaybeCompositionChecks checkInfo =
    let
        resultToMaybeFunctionRange : Range
        resultToMaybeFunctionRange =
            checkInfo.later.fnRange
    in
    case ( checkInfo.earlier.fn, checkInfo.earlier.args ) of
        ( ( [ "Result" ], "Err" ), [] ) ->
            Just
                (Rule.errorWithFix
                    { message = "Using " ++ qualifiedToString ( [ "Result" ], "toMaybe" ) ++ " on an error will result in Nothing"
                    , details = [ "You can replace this call by always Nothing." ]
                    }
                    resultToMaybeFunctionRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Basics" ], "always" ) checkInfo)
                            ++ " "
                            ++ qualifiedToString (qualify ( [ "Maybe" ], "Nothing" ) checkInfo)
                        )
                    ]
                )

        ( ( [ "Result" ], "Ok" ), [] ) ->
            Just
                (Rule.errorWithFix
                    { message = "Using " ++ qualifiedToString ( [ "Result" ], "toMaybe" ) ++ " on a value that is Ok will result in Just that value itself"
                    , details = [ "You can replace this call by Just." ]
                    }
                    resultToMaybeFunctionRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Maybe" ], "Just" ) checkInfo))
                    ]
                )

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
pipelineChecks checkInfo =
    firstThatReportsError
        [ \() -> pipingIntoCompositionChecks { commentRanges = checkInfo.commentRanges, extractSourceCode = checkInfo.extractSourceCode } checkInfo.direction checkInfo.pipedInto
        , \() -> fullyAppliedLambdaInPipelineChecks { nodeRange = checkInfo.nodeRange, function = checkInfo.pipedInto, firstArgument = checkInfo.arg }
        ]
        ()


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
                    Just
                        (appliedLambdaError
                            { nodeRange = checkInfo.nodeRange
                            , lambdaRange = lambdaRange
                            , lambda = lambda
                            }
                        )

        _ ->
            Nothing


type LeftOrRightDirection
    = RightToLeft
    | LeftToRight


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


callOnEmptyReturnsEmptyCheck :
    String
    -> Node Expression
    ->
        { a
            | moduleName : ModuleName
            , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
            , emptyDescription : String
        }
    -> CheckInfo
    -> Maybe (Error {})
callOnEmptyReturnsEmptyCheck fnName collectionArg collection checkInfo =
    if collection.isEmpty checkInfo.lookupTable collectionArg then
        Just
            (Rule.errorWithFix
                (operationDoesNotChangeSpecificLastArgErrorInfo
                    { fn = ( collection.moduleName, fnName ), replacementDescription = collection.emptyDescription }
                )
                checkInfo.fnRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range collectionArg })
            )

    else
        Nothing


collectionFilterChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionFilterChecks collection checkInfo =
    let
        maybeCollectionArg : Maybe (Node Expression)
        maybeCollectionArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case maybeCollectionArg of
                Just collectionArg ->
                    callOnEmptyReturnsEmptyCheck "filter" collectionArg collection checkInfo

                Nothing ->
                    Nothing
        , \() ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined True ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( collection.moduleName, "filter" ) ++ " with a function that will always return True is the same as not using " ++ qualifiedToString ( collection.moduleName, "filter" )
                            , details = [ "You can remove this call and replace it by the " ++ collection.represents ++ " itself." ]
                            }
                            checkInfo.fnRange
                            (toIdentityFix
                                { lastArg = maybeCollectionArg, resources = checkInfo }
                            )
                        )

                Determined False ->
                    let
                        collectionEmptyAsString : String
                        collectionEmptyAsString =
                            emptyAsString checkInfo collection
                    in
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( collection.moduleName, "filter" ) ++ " with a function that will always return False will result in " ++ collectionEmptyAsString
                            , details = [ "You can remove this call and replace it by " ++ collectionEmptyAsString ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix collectionEmptyAsString checkInfo.parentRange maybeCollectionArg checkInfo)
                        )

                Undetermined ->
                    Nothing
        ]
        ()


collectionRemoveChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionRemoveChecks collection checkInfo =
    case secondArg checkInfo of
        Just collectionArg ->
            callOnEmptyReturnsEmptyCheck "remove" collectionArg collection checkInfo

        Nothing ->
            Nothing


collectionIntersectChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionIntersectChecks collection checkInfo =
    let
        maybeCollectionArg : Maybe (Node Expression)
        maybeCollectionArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            callOnEmptyReturnsEmptyCheck "intersect" checkInfo.firstArg collection checkInfo
        , \() ->
            case maybeCollectionArg of
                Just collectionArg ->
                    callOnEmptyReturnsEmptyCheck "intersect" collectionArg collection checkInfo

                Nothing ->
                    Nothing
        ]
        ()


collectionDiffChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionDiffChecks collection checkInfo =
    let
        maybeCollectionArg : Maybe (Node Expression)
        maybeCollectionArg =
            secondArg checkInfo

        collectionEmptyAsString : String
        collectionEmptyAsString =
            emptyAsString checkInfo collection
    in
    firstThatReportsError
        [ \() ->
            case collection.determineSize checkInfo.lookupTable checkInfo.firstArg of
                Just (Exactly 0) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Diffing " ++ collectionEmptyAsString ++ " will result in " ++ collectionEmptyAsString
                            , details = [ "You can replace this call by " ++ collectionEmptyAsString ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix collectionEmptyAsString checkInfo.parentRange maybeCollectionArg checkInfo)
                        )

                _ ->
                    Nothing
        , \() ->
            case maybeCollectionArg of
                Just collectionArg ->
                    case collection.determineSize checkInfo.lookupTable collectionArg of
                        Just (Exactly 0) ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Diffing a " ++ collection.represents ++ " with " ++ collectionEmptyAsString ++ " will result in the " ++ collection.represents ++ " itself"
                                    , details = [ "You can replace this call by the " ++ collection.represents ++ " itself." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                                    , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                                    ]
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        ]
        ()


collectionUnionChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionUnionChecks collection checkInfo =
    let
        maybeCollectionArg : Maybe (Node Expression)
        maybeCollectionArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case collection.determineSize checkInfo.lookupTable checkInfo.firstArg of
                Just (Exactly 0) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Unnecessary union with " ++ collection.emptyAsString (extractQualifyResources checkInfo)
                            , details = [ "You can replace this call by the " ++ collection.represents ++ " itself." ]
                            }
                            checkInfo.fnRange
                            (toIdentityFix
                                { lastArg = maybeCollectionArg, resources = checkInfo }
                            )
                        )

                _ ->
                    Nothing
        , \() ->
            case maybeCollectionArg of
                Just collectionArg ->
                    case collection.determineSize checkInfo.lookupTable collectionArg of
                        Just (Exactly 0) ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Unnecessary union with " ++ collection.emptyAsString (extractQualifyResources checkInfo)
                                    , details = [ "You can replace this call by the " ++ collection.represents ++ " itself." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                                    , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                                    ]
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        ]
        ()


collectionInsertChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionInsertChecks collection checkInfo =
    case secondArg checkInfo of
        Just collectionArg ->
            case collection.determineSize checkInfo.lookupTable collectionArg of
                Just (Exactly 0) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Use " ++ qualifiedToString ( collection.moduleName, "singleton" ) ++ " instead of inserting in " ++ emptyAsString checkInfo collection
                            , details = [ "You can replace this call by " ++ qualifiedToString ( collection.moduleName, "singleton" ) ++ "." ]
                            }
                            checkInfo.fnRange
                            (replaceBySubExpressionFix checkInfo.parentRange checkInfo.firstArg
                                ++ [ Fix.insertAt checkInfo.parentRange.start
                                        (qualifiedToString (qualify ( collection.moduleName, "singleton" ) checkInfo) ++ " ")
                                   ]
                            )
                        )

                _ ->
                    Nothing

        Nothing ->
            Nothing


collectionMemberChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionMemberChecks collection checkInfo =
    case secondArg checkInfo of
        Just collectionArg ->
            case collection.determineSize checkInfo.lookupTable collectionArg of
                Just (Exactly 0) ->
                    Just
                        (Rule.errorWithFix
                            { message = "Using " ++ qualifiedToString ( collection.moduleName, "member" ) ++ " on " ++ collection.emptyDescription ++ " will result in False"
                            , details = [ "You can replace this call by False." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy checkInfo.parentRange
                                (qualifiedToString (qualify ( [ "Basics" ], "False" ) checkInfo))
                            ]
                        )

                _ ->
                    Nothing

        Nothing ->
            Nothing


collectionIsEmptyChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionIsEmptyChecks collection checkInfo =
    case collection.determineSize checkInfo.lookupTable checkInfo.firstArg of
        Just (Exactly 0) ->
            Just
                (Rule.errorWithFix
                    { message = "The call to " ++ qualifiedToString ( collection.moduleName, "isEmpty" ) ++ " will result in True"
                    , details = [ "You can replace this call by True." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Basics" ], "True" ) checkInfo))
                    ]
                )

        Just _ ->
            Just
                (Rule.errorWithFix
                    { message = "The call to " ++ qualifiedToString ( collection.moduleName, "isEmpty" ) ++ " will result in False"
                    , details = [ "You can replace this call by False." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange
                        (qualifiedToString (qualify ( [ "Basics" ], "False" ) checkInfo))
                    ]
                )

        Nothing ->
            Nothing


collectionSizeChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionSizeChecks collection checkInfo =
    case collection.determineSize checkInfo.lookupTable checkInfo.firstArg of
        Just (Exactly size) ->
            Just
                (Rule.errorWithFix
                    { message = "The " ++ collection.nameForSize ++ " of the " ++ collection.represents ++ " is " ++ String.fromInt size
                    , details = [ "The " ++ collection.nameForSize ++ " of the " ++ collection.represents ++ " can be determined by looking at the code." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange (String.fromInt size) ]
                )

        _ ->
            Nothing


collectionFromListChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionFromListChecks collection checkInfo =
    case AstHelpers.getListLiteral checkInfo.firstArg of
        Just [] ->
            let
                collectionEmptyAsString : String
                collectionEmptyAsString =
                    emptyAsString checkInfo collection
            in
            Just
                (Rule.errorWithFix
                    { message = "The call to " ++ qualifiedToString ( collection.moduleName, "fromList" ) ++ " will result in " ++ collectionEmptyAsString
                    , details = [ "You can replace this call by " ++ collectionEmptyAsString ++ "." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange collectionEmptyAsString ]
                )

        _ ->
            Nothing


collectionToListChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionToListChecks collection checkInfo =
    case collection.determineSize checkInfo.lookupTable checkInfo.firstArg of
        Just (Exactly 0) ->
            Just
                (Rule.errorWithFix
                    { message = "The call to " ++ qualifiedToString ( collection.moduleName, "toList" ) ++ " will result in []"
                    , details = [ "You can replace this call by []." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                )

        _ ->
            Nothing


collectionPartitionChecks : Collection otherProperties -> CheckInfo -> Maybe (Error {})
collectionPartitionChecks collection checkInfo =
    let
        collectionEmptyAsString : String
        collectionEmptyAsString =
            emptyAsString checkInfo collection
    in
    firstThatReportsError
        [ \() ->
            case secondArg checkInfo of
                Just collectionArg ->
                    case collection.determineSize checkInfo.lookupTable collectionArg of
                        Just (Exactly 0) ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( collection.moduleName, "partition" ) ++ " on " ++ collection.emptyDescription ++ " will result in ( " ++ collectionEmptyAsString ++ ", " ++ collectionEmptyAsString ++ " )"
                                    , details = [ "You can replace this call by ( " ++ collectionEmptyAsString ++ ", " ++ collectionEmptyAsString ++ " )." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy checkInfo.parentRange ("( " ++ collectionEmptyAsString ++ ", " ++ collectionEmptyAsString ++ " )") ]
                                )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing
        , \() ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined True ->
                    case secondArg checkInfo of
                        Just (Node listArgRange _) ->
                            Just
                                (Rule.errorWithFix
                                    { message = "All elements will go to the first " ++ collection.represents
                                    , details = [ "Since the predicate function always returns True, the second " ++ collection.represents ++ " will always be " ++ collection.emptyDescription ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = listArgRange.start } "( "
                                    , Fix.insertAt listArgRange.end (", " ++ collectionEmptyAsString ++ " )")
                                    ]
                                )

                        Nothing ->
                            Nothing

                Determined False ->
                    Just
                        (Rule.errorWithFix
                            { message = "All elements will go to the second " ++ collection.represents
                            , details = [ "Since the predicate function always returns False, the first " ++ collection.represents ++ " will always be " ++ collection.emptyDescription ++ "." ]
                            }
                            checkInfo.fnRange
                            (case secondArg checkInfo of
                                Just listArg ->
                                    [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range listArg).start } ("( " ++ collectionEmptyAsString ++ ", ")
                                    , Fix.insertAt (Node.range listArg).end " )"
                                    ]

                                Nothing ->
                                    [ Fix.replaceRangeBy checkInfo.parentRange
                                        ("("
                                            ++ qualifiedToString (qualify ( [ "Tuple" ], "pair" ) checkInfo)
                                            ++ " "
                                            ++ collectionEmptyAsString
                                            ++ ")"
                                        )
                                    ]
                            )
                        )

                Undetermined ->
                    Nothing
        ]
        ()


maybeWithDefaultChecks : CheckInfo -> Maybe (Error {})
maybeWithDefaultChecks checkInfo =
    case secondArg checkInfo of
        Just maybeArg ->
            firstThatReportsError
                [ \() ->
                    case sameCallInAllBranches ( [ "Maybe" ], "Just" ) checkInfo.lookupTable maybeArg of
                        Determined justCalls ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "Maybe" ], "withDefault" ) ++ " on a value that is Just will result in that value"
                                    , details = [ "You can replace this call by the value wrapped in Just." ]
                                    }
                                    checkInfo.fnRange
                                    (List.concatMap (\justCall -> replaceBySubExpressionFix justCall.nodeRange justCall.firstArg) justCalls
                                        ++ keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range maybeArg }
                                    )
                                )

                        Undetermined ->
                            Nothing
                , \() ->
                    case sameValueOrFunctionInAllBranches ( [ "Maybe" ], "Nothing" ) checkInfo.lookupTable maybeArg of
                        Determined _ ->
                            Just
                                (Rule.errorWithFix
                                    { message = "Using " ++ qualifiedToString ( [ "Maybe" ], "withDefault" ) ++ " on Nothing will result in the default value"
                                    , details = [ "You can replace this call by the default value." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                                    , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                                    ]
                                )

                        Undetermined ->
                            Nothing
                ]
                ()

        Nothing ->
            Nothing


type CollectionSize
    = Exactly Int
    | NotEmpty


determineListLength : ModuleNameLookupTable -> Node Expression -> Maybe CollectionSize
determineListLength lookupTable expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.ListExpr list ->
            Just (Exactly (List.length list))

        Expression.OperatorApplication "::" _ _ right ->
            case determineListLength lookupTable right of
                Just (Exactly n) ->
                    Just (Exactly (n + 1))

                _ ->
                    Just NotEmpty

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "singleton")) :: _ :: []) ->
            if ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just [ "List" ] then
                Just (Exactly 1)

            else
                Nothing

        _ ->
            Nothing


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


removeRecordFields : Range -> Node String -> List (Node Expression.RecordSetter) -> Maybe (Error {})
removeRecordFields recordUpdateRange variable fields =
    case fields of
        [] ->
            -- Not possible
            Nothing

        (Node _ ( field, valueWithParens )) :: [] ->
            let
                value : Node Expression
                value =
                    AstHelpers.removeParens valueWithParens
            in
            if isUnnecessaryRecordUpdateSetter variable field value then
                Just
                    (Rule.errorWithFix
                        { message = "Unnecessary field assignment"
                        , details = [ "The field is being set to its own value." ]
                        }
                        (Node.range value)
                        (keepOnlyFix { parentRange = recordUpdateRange, keep = Node.range variable })
                    )

            else
                Nothing

        (Node firstRange _) :: (Node secondRange _) :: _ ->
            withBeforeMap
                (\field ->
                    let
                        (Node currentFieldRange ( currentFieldName, valueWithParens )) =
                            field.current

                        value : Node Expression
                        value =
                            AstHelpers.removeParens valueWithParens
                    in
                    if isUnnecessaryRecordUpdateSetter variable currentFieldName value then
                        Just
                            (Rule.errorWithFix
                                { message = "Unnecessary field assignment"
                                , details = [ "The field is being set to its own value." ]
                                }
                                (Node.range value)
                                (case field.before of
                                    Just (Node prevRange _) ->
                                        [ Fix.removeRange { start = prevRange.end, end = currentFieldRange.end } ]

                                    Nothing ->
                                        -- It's the first element, so we can remove until the second element
                                        [ Fix.removeRange { start = firstRange.start, end = secondRange.start } ]
                                )
                            )

                    else
                        Nothing
                )
                fields
                |> findMap identity


isUnnecessaryRecordUpdateSetter : Node String -> Node String -> Node Expression -> Bool
isUnnecessaryRecordUpdateSetter (Node _ variable) (Node _ field) (Node _ value) =
    case value of
        Expression.RecordAccess (Node _ (Expression.FunctionOrValue [] valueHolder)) (Node _ fieldName) ->
            field == fieldName && variable == valueHolder

        _ ->
            False



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


ifChecks :
    IfCheckInfo
    -> Maybe { errors : Error {}, rangesToIgnore : RangeDict () }
ifChecks checkInfo =
    findMap (\f -> f ())
        [ \() ->
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
        , \() ->
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
        , \() ->
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


appliedLambdaError : { nodeRange : Range, lambdaRange : Range, lambda : Expression.Lambda } -> Error {}
appliedLambdaError checkInfo =
    case checkInfo.lambda.args of
        (Node unitRange Pattern.UnitPattern) :: otherPatterns ->
            Rule.errorWithFix
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

        (Node allRange Pattern.AllPattern) :: otherPatterns ->
            Rule.errorWithFix
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

        _ ->
            Rule.error
                { message = "Anonymous function is immediately invoked"
                , details =
                    [ "This expression defines a function which then gets called directly afterwards, which overly complexifies the intended computation."
                    , "While there are reasonable uses for this in languages like JavaScript, the same benefits aren't there in Elm because of not allowing name shadowing."
                    , "Here are a few ways you can simplify this:"
                    , """- Remove the lambda and reference the arguments directly instead of giving them new names
- Remove the lambda and use let variables to give names to the current arguments
- Extract the lambda to a named function (at the top-level or defined in a let expression)"""
                    ]
                }
                checkInfo.lambdaRange



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
    -> Maybe (Error {})
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
                (Rule.errorWithFix
                    { message = "Field access can be simplified"
                    , details = [ "Accessing the field of a record or record update can be simplified to just that field's value" ]
                    }
                    checkInfo.nodeRange
                    (replaceBySubExpressionFix checkInfo.nodeRange setter)
                )

        Nothing ->
            case checkInfo.maybeRecordNameRange of
                Just recordNameRange ->
                    Just
                        (Rule.errorWithFix
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of an unrelated record update can be simplified to just the original field's value" ]
                            }
                            checkInfo.nodeRange
                            [ Fix.replaceRangeBy { start = checkInfo.nodeRange.start, end = recordNameRange.start } ""
                            , Fix.replaceRangeBy { start = recordNameRange.end, end = checkInfo.nodeRange.end } ("." ++ checkInfo.fieldName)
                            ]
                        )

                Nothing ->
                    Nothing


distributeFieldAccess : String -> Range -> List (Node Expression) -> Node String -> Maybe (Error {})
distributeFieldAccess kind recordRange branches (Node fieldRange fieldName) =
    case recordLeavesRanges branches of
        Just records ->
            Just
                (let
                    fieldAccessRange : Range
                    fieldAccessRange =
                        { start = recordRange.end, end = fieldRange.end }
                 in
                 Rule.errorWithFix
                    { message = "Field access can be simplified"
                    , details = [ "Accessing the field outside " ++ kind ++ " expression can be simplified to access the field inside it" ]
                    }
                    fieldAccessRange
                    (Fix.removeRange fieldAccessRange
                        :: List.map (\leafRange -> Fix.insertAt leafRange.end ("." ++ fieldName)) records
                    )
                )

        Nothing ->
            Nothing


injectRecordAccessIntoLetExpression : Range -> Node Expression -> Node String -> Error {}
injectRecordAccessIntoLetExpression recordRange letBody (Node fieldRange fieldName) =
    let
        removalRange : Range
        removalRange =
            { start = recordRange.end, end = fieldRange.end }
    in
    Rule.errorWithFix
        { message = "Field access can be simplified"
        , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it" ]
        }
        removalRange
        (Fix.removeRange removalRange
            :: replaceSubExpressionByRecordAccessFix fieldName letBody
        )


recordLeavesRanges : List (Node Expression) -> Maybe (List Range)
recordLeavesRanges nodes =
    recordLeavesRangesHelp nodes []


recordLeavesRangesHelp : List (Node Expression) -> List Range -> Maybe (List Range)
recordLeavesRangesHelp nodes foundRanges =
    case nodes of
        [] ->
            Just foundRanges

        (Node range expr) :: rest ->
            case expr of
                Expression.IfBlock _ thenBranch elseBranch ->
                    recordLeavesRangesHelp (thenBranch :: elseBranch :: rest) foundRanges

                Expression.LetExpression letIn ->
                    recordLeavesRangesHelp (letIn.expression :: rest) foundRanges

                Expression.ParenthesizedExpression child ->
                    recordLeavesRangesHelp (child :: rest) foundRanges

                Expression.CaseExpression caseOf ->
                    recordLeavesRangesHelp (List.map Tuple.second caseOf.cases ++ rest) foundRanges

                Expression.RecordExpr _ ->
                    recordLeavesRangesHelp rest (range :: foundRanges)

                Expression.RecordUpdateExpression _ _ ->
                    recordLeavesRangesHelp rest (range :: foundRanges)

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
    [ Fix.removeRange (leftBoundaryRange nodeRange)
    , Fix.removeRange (rightBoundaryRange nodeRange)
    ]


leftBoundaryRange : Range -> Range
leftBoundaryRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 1 }
    }


rightBoundaryRange : Range -> Range
rightBoundaryRange range =
    { start = { row = range.end.row, column = range.end.column - 1 }
    , end = range.end
    }


replaceByEmptyFix : String -> Range -> Maybe a -> QualifyResources b -> List Fix
replaceByEmptyFix empty parentRange lastArg qualifyResources =
    [ case lastArg of
        Just _ ->
            Fix.replaceRangeBy parentRange empty

        Nothing ->
            Fix.replaceRangeBy parentRange
                (qualifiedToString (qualify ( [ "Basics" ], "always" ) qualifyResources)
                    ++ " "
                    ++ empty
                )
    ]


replaceByBoolWithIrrelevantLastArgFix :
    { replacement : Bool, lastArg : Maybe a, checkInfo : QualifyResources { b | parentRange : Range } }
    -> List Fix
replaceByBoolWithIrrelevantLastArgFix config =
    let
        replacementAsString : String
        replacementAsString =
            qualifiedToString (qualify ( [ "Basics" ], AstHelpers.boolToString config.replacement ) config.checkInfo)
    in
    case config.lastArg of
        Just _ ->
            [ Fix.replaceRangeBy config.checkInfo.parentRange replacementAsString ]

        Nothing ->
            [ Fix.replaceRangeBy config.checkInfo.parentRange
                ("("
                    ++ qualifiedToString (qualify ( [ "Basics" ], "always" ) config.checkInfo)
                    ++ " "
                    ++ replacementAsString
                    ++ ")"
                )
            ]


replacementWithIrrelevantLastArg : { resources : QualifyResources a, lastArg : Maybe arg, forNoLastArg : String } -> String
replacementWithIrrelevantLastArg config =
    case config.lastArg of
        Just _ ->
            config.forNoLastArg

        Nothing ->
            qualifiedToString (qualify ( [ "Basics" ], "always" ) config.resources)
                ++ (" (" ++ config.forNoLastArg ++ ")")


identityError :
    { toFix : String
    , lastArgName : String
    , lastArg : Maybe (Node lastArgument)
    , resources : QualifyResources { a | fnRange : Range, parentRange : Range }
    }
    -> Error {}
identityError config =
    Rule.errorWithFix
        { message = "Using " ++ config.toFix ++ " will always return the same given " ++ config.lastArgName
        , details =
            case config.lastArg of
                Nothing ->
                    [ "You can replace this call by identity." ]

                Just _ ->
                    [ "You can replace this call by the " ++ config.lastArgName ++ " itself." ]
        }
        config.resources.fnRange
        (toIdentityFix { lastArg = config.lastArg, resources = config.resources })


toIdentityFix :
    { lastArg : Maybe (Node lastArgument)
    , resources : QualifyResources { a | parentRange : Range }
    }
    -> List Fix
toIdentityFix config =
    case config.lastArg of
        Nothing ->
            [ Fix.replaceRangeBy config.resources.parentRange
                (qualifiedToString (qualify ( [ "Basics" ], "identity" ) config.resources))
            ]

        Just (Node lastArgRange _) ->
            keepOnlyFix { parentRange = config.resources.parentRange, keep = lastArgRange }


multiAlways : Int -> String -> QualifyResources a -> String
multiAlways alwaysCount alwaysResultExpressionAsString qualifyResources =
    case alwaysCount of
        0 ->
            alwaysResultExpressionAsString

        1 ->
            qualifiedToString (qualify ( [ "Basics" ], "always" ) qualifyResources)
                ++ " "
                ++ alwaysResultExpressionAsString

        alwaysCountPositive ->
            "(\\" ++ String.repeat alwaysCountPositive "_ " ++ "-> " ++ alwaysResultExpressionAsString ++ ")"


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

        _ ->
            False


returnsSpecificValueOrFunctionInAllBranches : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Match (List Range)
returnsSpecificValueOrFunctionInAllBranches specificQualified lookupTable expressionNode =
    constructs (sameValueOrFunctionInAllBranches specificQualified) lookupTable expressionNode


constructsSpecificInAllBranches : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Match ConstructionKind
constructsSpecificInAllBranches specificFullyQualifiedFn lookupTable expressionNode =
    case AstHelpers.getSpecificValueOrFunction specificFullyQualifiedFn lookupTable expressionNode of
        Just _ ->
            Determined DirectConstruction

        Nothing ->
            constructs (sameCallInAllBranches specificFullyQualifiedFn) lookupTable expressionNode
                |> Match.map
                    (\calls ->
                        NonDirectConstruction
                            (List.concatMap (\call -> replaceBySubExpressionFix call.nodeRange call.firstArg) calls)
                    )


type ConstructionKind
    = -- either
      --   - `always specific`
      --   - `\a -> ... (specific a)`
      --   - `... specific`
      DirectConstruction
    | -- `a` argument not directly used,
      -- e.g. `\a -> ... (specific (f a))` or `\a -> if a then specific b`
      NonDirectConstruction (List Fix)


constructs :
    (ModuleNameLookupTable -> Node Expression -> Match specific)
    -> ModuleNameLookupTable
    -> Node Expression
    -> Match specific
constructs getSpecific lookupTable expressionNode =
    case AstHelpers.getSpecificFunctionCall ( [ "Basics" ], "always" ) lookupTable expressionNode of
        Just alwaysCall ->
            getSpecific lookupTable alwaysCall.firstArg

        Nothing ->
            case Node.value (AstHelpers.removeParens expressionNode) of
                Expression.LambdaExpression lambda ->
                    getSpecific lookupTable lambda.expression

                _ ->
                    Undetermined


sameCallInAllBranches :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    -> Node Expression
    ->
        Match
            (List
                { argsAfterFirst : List (Node Expression)
                , firstArg : Node Expression
                , fnRange : Range
                , nodeRange : Range
                }
            )
sameCallInAllBranches pureFullyQualified lookupTable baseExpressionNode =
    sameInAllBranches (AstHelpers.getSpecificFunctionCall pureFullyQualified lookupTable) baseExpressionNode


sameValueOrFunctionInAllBranches :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    -> Node Expression
    -> Match (List Range)
sameValueOrFunctionInAllBranches pureFullyQualified lookupTable baseExpressionNode =
    sameInAllBranches (AstHelpers.getSpecificValueOrFunction pureFullyQualified lookupTable) baseExpressionNode


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
    case AstHelpers.getTuple expressionNode of
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


neighboringMap : ({ before : Maybe a, current : a, after : Maybe a } -> b) -> List a -> List b
neighboringMap changeWithNeighboring list =
    List.map3
        (\before current after ->
            changeWithNeighboring { before = before, current = current, after = after }
        )
        (Nothing :: List.map Just list)
        list
        (List.map Just (List.drop 1 list) ++ [ Nothing ])


withBeforeMap : ({ before : Maybe a, current : a } -> b) -> List a -> List b
withBeforeMap changeWithBefore list =
    List.map2
        (\before current ->
            changeWithBefore { before = before, current = current }
        )
        (Nothing :: List.map Just list)
        list


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


maybeToList : Maybe a -> List a
maybeToList maybe =
    case maybe of
        Nothing ->
            []

        Just content ->
            [ content ]
