module Simplify exposing
    ( rule
    , Configuration, defaults, ignoreCaseOfForTypes
    )

{-| Reports when an expression can be simplified.

🔧 Running with `--fix` will automatically remove all the reported errors.

    config =
        [ Simplify.rule Simplify.defaults
        ]

@docs rule
@docs Configuration, defaults, ignoreCaseOfForTypes


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

    -(-n)
    --> n

    negate (negate n)
    --> n


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

    String.slice 0 n str
    --> String.left n str

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

    List.map fn [] -- same for most List functions like List.filter, List.filterMap, ...
    --> []

    List.map identity list
    --> list

    List.filter (\a -> True) list
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

    List.concatMap identity list
    --> List.concat list

    List.concatMap (\a -> [ b ]) list
    --> List.map (\a -> b) list

    List.concatMap fn [ x ]
    --> fn x

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
    List.foldl fn x []
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

    List.all fn []
    --> True

    List.all (always True) list
    --> True

    List.any fn []
    --> True

    List.any (always False) list
    --> False

    List.range 6 3
    --> []

    List.length [ a, b, c ]
    --> 3

    List.repeat 0 x
    --> []

    List.partition fn []
    --> ( [], [] )

    List.partition (always True) list
    --> ( list, [] )

    List.take 0 list
    --> []

    List.drop 0 list
    --> list

    List.reverse (List.reverse list)
    --> list

    List.sortBy (\_ -> a) list
    --> list

    List.sortBy identity list
    --> List.sort list

    List.sortWith (\_ _ -> LT) list
    --> List.reverse list

    List.sortWith (\_ _ -> EQ) list
    --> list

    List.sortWith (\_ _ -> GT) list
    --> list

    -- The following simplifications for List.sort also work for List.sortBy fn and List.sortWith fn
    List.sort []
    --> []

    List.sort [ a ]
    --> [ a ]

    -- same for up to List.map5 when any list is empty
    List.map2 fn xs []
    --> []

    List.map2 fn [] ys
    --> []

    List.unzip []
    --> ( [], [] )


### Sets

    Set.map fn Set.empty -- same for Set.filter, Set.remove...
    --> Set.empty

    Set.map identity set
    --> set

    Set.isEmpty Set.empty
    --> True

    Set.member x Set.empty
    --> False

    Set.fromList []
    --> Set.empty

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

    Set.partition fn Set.empty
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

    Cmd.map fn Cmd.none
    --> Cmd.none


### Json.Decode

    Json.Decode.oneOf [ a ]
    --> a


### Html.Attributes

    Html.Attributes.classList [ x, y, ( z, False ) ]
    --> Html.Attributes.classList [ x, y ]

    Html.Attributes.classList [ ( onlyOneThing, True ) ]
    --> Html.Attributes.class onlyOneThing


### Parser

    Parser.oneOf [ a ]
    --> a

-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Syntax.Expression as Expression exposing (Expression, RecordSetter)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range exposing (Location, Range)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Project.Dependency as Dependency exposing (Dependency)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)
import Simplify.AstHelpers as AstHelpers
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
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = \_ previous -> previous
            }
        |> Rule.providesFixesForProjectRule
        |> Rule.fromProjectRuleSchema


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationEnterVisitor (\node context -> ( [], declarationVisitor node context ))
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withExpressionExitVisitor (\node context -> ( [], expressionExitVisitor node context ))



-- CONFIGURATION


{-| Configuration for this rule. Create a new one with [`defaults`](#defaults) and use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) to alter it.
-}
type Configuration
    = Configuration
        { ignoreConstructors : List String
        }


{-| Default configuration for this rule. Use [`ignoreCaseOfForTypes`](#ignoreCaseOfForTypes) if you want to change the configuration.

    config =
        [ Simplify.defaults
            |> Simplify.ignoreCaseOfForTypes [ "Module.Name.Type" ]
            |> Simplify.rule
        ]

-}
defaults : Configuration
defaults =
    Configuration { ignoreConstructors = [] }


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
    Configuration { config | ignoreConstructors = ignoreConstructors ++ config.ignoreConstructors }



-- CONTEXT


type alias ProjectContext =
    { customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleName : ModuleName
    , rangesToIgnore : List Range
    , rightSidesOfPlusPlus : List Range
    , customTypesToReportInCases : Set ( ModuleName, ConstructorName )
    , localIgnoredCustomTypes : List Constructor
    , constructorsToIgnore : Set ( ModuleName, String )
    , inferredConstantsDict : RangeDict Infer.Inferred
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , extractSourceCode : Range -> String
    }


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
    }


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator (\_ -> initialContext)


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable metadata extractSourceCode projectContext ->
            { lookupTable = lookupTable
            , moduleName = Rule.moduleNameFromMetadata metadata
            , rangesToIgnore = []
            , rightSidesOfPlusPlus = []
            , localIgnoredCustomTypes = []
            , customTypesToReportInCases = projectContext.customTypesToReportInCases
            , constructorsToIgnore = Set.empty
            , inferredConstantsDict = RangeDict.empty
            , inferredConstants = ( Infer.empty, [] )
            , extractSourceCode = extractSourceCode
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withMetadata
        |> Rule.withSourceCodeExtractor



-- DEPENDENCIES VISITOR


dependenciesVisitor : Set String -> Dict String Dependency -> ProjectContext -> ( List (Error scope), ProjectContext )
dependenciesVisitor typeNamesAsStrings dict _ =
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
                                String.split "." mod.name
                        in
                        mod.unions
                            |> List.filter (\union -> not (Set.member (mod.name ++ "." ++ union.name) typeNamesAsStrings))
                            |> List.concatMap (\union -> union.tags)
                            |> List.map (\( tagName, _ ) -> ( moduleName, tagName ))
                    )
                |> Set.fromList
    in
    ( if List.isEmpty unknownTypesToIgnore then
        []

      else
        [ errorForUnknownIgnoredConstructor unknownTypesToIgnore ]
    , { customTypesToReportInCases = customTypesToReportInCases }
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


wrapInBackticks : String -> String
wrapInBackticks s =
    "`" ++ s ++ "`"



-- DECLARATION VISITOR


declarationVisitor : Node a -> ModuleContext -> ModuleContext
declarationVisitor _ context =
    { context
        | rangesToIgnore = []
        , rightSidesOfPlusPlus = []
        , inferredConstantsDict = RangeDict.empty
    }



-- EXPRESSION VISITOR


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor node context =
    let
        newContext : ModuleContext
        newContext =
            case RangeDict.get (Node.range node) context.inferredConstantsDict of
                Just inferredConstants ->
                    let
                        ( previous, previousStack ) =
                            context.inferredConstants
                    in
                    { context | inferredConstants = ( inferredConstants, previous :: previousStack ) }

                Nothing ->
                    context
    in
    if List.member (Node.range node) newContext.rangesToIgnore then
        ( [], newContext )

    else
        let
            { errors, rangesToIgnore, rightSidesOfPlusPlus, inferredConstants } =
                expressionVisitorHelp node newContext
        in
        ( errors
        , { newContext
            | rangesToIgnore = rangesToIgnore ++ newContext.rangesToIgnore
            , rightSidesOfPlusPlus = rightSidesOfPlusPlus ++ newContext.rightSidesOfPlusPlus
            , inferredConstantsDict = List.foldl (\( range, constants ) acc -> RangeDict.insert range constants acc) newContext.inferredConstantsDict inferredConstants
          }
        )


expressionExitVisitor : Node Expression -> ModuleContext -> ModuleContext
expressionExitVisitor node context =
    if RangeDict.member (Node.range node) context.inferredConstantsDict then
        case Tuple.second context.inferredConstants of
            topOfStack :: restOfStack ->
                { context | inferredConstants = ( topOfStack, restOfStack ) }

            [] ->
                -- should never be empty
                context

    else
        context


errorsAndRangesToIgnore : List (Error {}) -> List Range -> { errors : List (Error {}), rangesToIgnore : List Range, rightSidesOfPlusPlus : List Range, inferredConstants : List ( Range, Infer.Inferred ) }
errorsAndRangesToIgnore errors rangesToIgnore =
    { errors = errors
    , rangesToIgnore = rangesToIgnore
    , rightSidesOfPlusPlus = []
    , inferredConstants = []
    }


onlyErrors : List (Error {}) -> { errors : List (Error {}), rangesToIgnore : List Range, rightSidesOfPlusPlus : List Range, inferredConstants : List ( Range, Infer.Inferred ) }
onlyErrors errors =
    { errors = errors
    , rangesToIgnore = []
    , rightSidesOfPlusPlus = []
    , inferredConstants = []
    }


expressionVisitorHelp : Node Expression -> ModuleContext -> { errors : List (Error {}), rangesToIgnore : List Range, rightSidesOfPlusPlus : List Range, inferredConstants : List ( Range, Infer.Inferred ) }
expressionVisitorHelp node context =
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
            , inferredConstants = context.inferredConstants
            , parentRange = Node.range node
            , fnRange = checkInfo.fnRange
            , firstArg = checkInfo.firstArg
            , argsAfterFirst = checkInfo.argsAfterFirst
            , secondArg = List.head checkInfo.argsAfterFirst
            , thirdArg = List.head (List.drop 1 checkInfo.argsAfterFirst)
            , usingRightPizza = checkInfo.usingRightPizza
            }
    in
    case Node.value node of
        --------------------
        -- FUNCTION CALLS --
        --------------------
        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: restOfArguments) ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    onlyErrors
                        (checkFn
                            (toCheckInfo
                                { fnRange = fnRange
                                , firstArg = firstArg
                                , argsAfterFirst = restOfArguments
                                , usingRightPizza = False
                                }
                            )
                        )

                _ ->
                    onlyErrors []

        -------------------
        -- IF EXPRESSION --
        -------------------
        Expression.IfBlock condition trueBranch falseBranch ->
            ifChecks
                context
                (Node.range node)
                { condition = condition
                , trueBranch = trueBranch
                , falseBranch = falseBranch
                }

        -------------------------------
        --  APPLIED LAMBDA FUNCTIONS --
        -------------------------------
        Expression.Application ((Node _ (Expression.ParenthesizedExpression (Node lambdaRange (Expression.LambdaExpression lambda)))) :: firstArgument :: _) ->
            case lambda.args of
                (Node unitRange Pattern.UnitPattern) :: otherPatterns ->
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            }
                            unitRange
                            (case otherPatterns of
                                [] ->
                                    [ Fix.removeRange { start = lambdaRange.start, end = (Node.range lambda.expression).start }
                                    , Fix.removeRange (Node.range firstArgument)
                                    ]

                                secondPattern :: _ ->
                                    [ Fix.removeRange { start = unitRange.start, end = (Node.range secondPattern).start }
                                    , Fix.removeRange (Node.range firstArgument)
                                    ]
                            )
                        ]

                (Node allRange Pattern.AllPattern) :: otherPatterns ->
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "Unnecessary wildcard argument argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            }
                            allRange
                            (case otherPatterns of
                                [] ->
                                    [ Fix.removeRange { start = lambdaRange.start, end = (Node.range lambda.expression).start }
                                    , Fix.removeRange (Node.range firstArgument)
                                    ]

                                secondPattern :: _ ->
                                    [ Fix.removeRange { start = allRange.start, end = (Node.range secondPattern).start }
                                    , Fix.removeRange (Node.range firstArgument)
                                    ]
                            )
                        ]

                _ ->
                    onlyErrors []

        -----------------------------------
        -- FULLY APPLIED PREFIX OPERATOR --
        -----------------------------------
        Expression.Application [ Node.Node operatorRange (Expression.PrefixOperator operator), left, right ] ->
            onlyErrors
                [ Rule.errorWithFix
                    { message = "Use the infix form (a + b) over the prefix form ((+) a b)"
                    , details = [ "The prefix form is generally more unfamiliar to Elm developers, and therefore it is nicer when the infix form is used." ]
                    }
                    operatorRange
                    [ Fix.removeRange { start = operatorRange.start, end = (Node.range left).start }
                    , Fix.insertAt (Node.range right).start (operator ++ " ")
                    ]
                ]

        -------------------
        -- RECORD UPDATE --
        -------------------
        Expression.RecordUpdateExpression variable fields ->
            onlyErrors (removeRecordFields (Node.range node) variable fields)

        -------------
        -- CASE OF --
        -------------
        Expression.CaseExpression caseBlock ->
            onlyErrors (caseOfChecks context (Node.range node) caseBlock)

        ------------
        -- LET IN --
        ------------
        Expression.LetExpression caseBlock ->
            onlyErrors (letInChecks caseBlock)

        ----------
        -- (<|) --
        ----------
        Expression.OperatorApplication "<|" _ (Node fnRange (Expression.FunctionOrValue _ fnName)) firstArg ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    onlyErrors
                        (checkFn
                            (toCheckInfo
                                { fnRange = fnRange
                                , firstArg = firstArg
                                , argsAfterFirst = []
                                , usingRightPizza = False
                                }
                            )
                        )

                _ ->
                    onlyErrors []

        Expression.OperatorApplication "<|" _ (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsBetweenFirstAndLast))) lastArg ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    errorsAndRangesToIgnore
                        (checkFn
                            (toCheckInfo
                                { fnRange = fnRange
                                , firstArg = firstArg
                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                , usingRightPizza = False
                                }
                            )
                        )
                        [ applicationRange ]

                Nothing ->
                    onlyErrors []

        ----------
        -- (|>) --
        ----------
        Expression.OperatorApplication "|>" _ firstArg (Node fnRange (Expression.FunctionOrValue _ fnName)) ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    onlyErrors
                        (checkFn
                            (toCheckInfo
                                { fnRange = fnRange
                                , firstArg = firstArg
                                , argsAfterFirst = []
                                , usingRightPizza = True
                                }
                            )
                        )

                _ ->
                    onlyErrors []

        Expression.OperatorApplication "|>" _ lastArg (Node applicationRange (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsBetweenFirstAndLast))) ->
            case
                ModuleNameLookupTable.moduleNameAt context.lookupTable fnRange
                    |> Maybe.andThen (\moduleName -> Dict.get ( moduleName, fnName ) functionCallChecks)
            of
                Just checkFn ->
                    errorsAndRangesToIgnore
                        (checkFn
                            (toCheckInfo
                                { fnRange = fnRange
                                , firstArg = firstArg
                                , argsAfterFirst = argsBetweenFirstAndLast ++ [ lastArg ]
                                , usingRightPizza = True
                                }
                            )
                        )
                        [ applicationRange ]

                _ ->
                    onlyErrors []

        Expression.OperatorApplication ">>" _ left (Node _ (Expression.OperatorApplication ">>" _ right _)) ->
            onlyErrors
                (firstThatReportsError compositionChecks
                    { lookupTable = context.lookupTable
                    , fromLeftToRight = True
                    , parentRange = { start = (Node.range left).start, end = (Node.range right).end }
                    , left = left
                    , leftRange = Node.range left
                    , right = right
                    , rightRange = Node.range right
                    }
                )

        Expression.OperatorApplication ">>" _ left right ->
            onlyErrors
                (firstThatReportsError compositionChecks
                    { lookupTable = context.lookupTable
                    , fromLeftToRight = True
                    , parentRange = Node.range node
                    , left = left
                    , leftRange = Node.range left
                    , right = right
                    , rightRange = Node.range right
                    }
                )

        Expression.OperatorApplication "<<" _ (Node _ (Expression.OperatorApplication "<<" _ _ left)) right ->
            onlyErrors
                (firstThatReportsError compositionChecks
                    { lookupTable = context.lookupTable
                    , fromLeftToRight = False
                    , parentRange = { start = (Node.range left).start, end = (Node.range right).end }
                    , left = left
                    , leftRange = Node.range left
                    , right = right
                    , rightRange = Node.range right
                    }
                )

        Expression.OperatorApplication "<<" _ left right ->
            onlyErrors
                (firstThatReportsError compositionChecks
                    { lookupTable = context.lookupTable
                    , fromLeftToRight = False
                    , parentRange = Node.range node
                    , left = left
                    , leftRange = Node.range left
                    , right = right
                    , rightRange = Node.range right
                    }
                )

        Expression.OperatorApplication operator _ left right ->
            case Dict.get operator operatorChecks of
                Just checkFn ->
                    { errors =
                        checkFn
                            { lookupTable = context.lookupTable
                            , inferredConstants = context.inferredConstants
                            , parentRange = Node.range node
                            , operator = operator
                            , left = left
                            , leftRange = Node.range left
                            , right = right
                            , rightRange = Node.range right
                            , isOnTheRightSideOfPlusPlus = List.member (Node.range node) context.rightSidesOfPlusPlus
                            }
                    , rangesToIgnore = []
                    , rightSidesOfPlusPlus =
                        if operator == "++" then
                            [ Node.range <| AstHelpers.removeParens right ]

                        else
                            []
                    , inferredConstants = []
                    }

                Nothing ->
                    onlyErrors []

        Expression.Negation baseExpr ->
            case AstHelpers.removeParens baseExpr of
                Node range (Expression.Negation negatedValue) ->
                    let
                        doubleNegationRange : Range
                        doubleNegationRange =
                            { start = (Node.range node).start
                            , end = { row = range.start.row, column = range.start.column + 1 }
                            }
                    in
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            }
                            doubleNegationRange
                            (replaceBySubExpressionFix (Node.range node) negatedValue)
                        ]

                _ ->
                    onlyErrors []

        Expression.RecordAccess record field ->
            case Node.value (AstHelpers.removeParens record) of
                Expression.RecordExpr setters ->
                    onlyErrors (recordAccessChecks (Node.range node) Nothing (Node.value field) setters)

                Expression.RecordUpdateExpression (Node recordNameRange _) setters ->
                    onlyErrors (recordAccessChecks (Node.range node) (Just recordNameRange) (Node.value field) setters)

                Expression.LetExpression { expression } ->
                    onlyErrors [ injectRecordAccessIntoLetExpression (Node.range record) expression field ]

                Expression.IfBlock _ thenBranch elseBranch ->
                    onlyErrors (distributeFieldAccess "an if/then/else" (Node.range record) [ thenBranch, elseBranch ] field)

                Expression.CaseExpression { cases } ->
                    onlyErrors (distributeFieldAccess "a case/of" (Node.range record) (List.map Tuple.second cases) field)

                _ ->
                    onlyErrors []

        _ ->
            onlyErrors []


distributeFieldAccess : String -> Range -> List (Node Expression) -> Node String -> List (Error {})
distributeFieldAccess kind recordRange branches (Node fieldRange fieldName) =
    case recordLeavesRanges branches of
        Just records ->
            [ let
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
            ]

        Nothing ->
            []


injectRecordAccessIntoLetExpression : Range -> Node Expression -> Node String -> Rule.Error {}
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

                Expression.LetExpression { expression } ->
                    recordLeavesRangesHelp (expression :: rest) foundRanges

                Expression.ParenthesizedExpression child ->
                    recordLeavesRangesHelp (child :: rest) foundRanges

                Expression.CaseExpression { cases } ->
                    recordLeavesRangesHelp (List.map Tuple.second cases ++ rest) foundRanges

                Expression.RecordExpr _ ->
                    recordLeavesRangesHelp rest (range :: foundRanges)

                Expression.RecordUpdateExpression _ _ ->
                    recordLeavesRangesHelp rest (range :: foundRanges)

                _ ->
                    Nothing


recordAccessChecks : Range -> Maybe Range -> String -> List (Node RecordSetter) -> List (Error {})
recordAccessChecks nodeRange recordNameRange fieldName setters =
    case
        findMap
            (\(Node _ ( setterField, setterValue )) ->
                if Node.value setterField == fieldName then
                    Just setterValue

                else
                    Nothing
            )
            setters
    of
        Just setter ->
            [ Rule.errorWithFix
                { message = "Field access can be simplified"
                , details = [ "Accessing the field of a record or record update can be simplified to just that field's value" ]
                }
                nodeRange
                (replaceBySubExpressionFix nodeRange setter)
            ]

        Nothing ->
            case recordNameRange of
                Just rnr ->
                    [ Rule.errorWithFix
                        { message = "Field access can be simplified"
                        , details = [ "Accessing the field of an unrelated record update can be simplified to just the original field's value" ]
                        }
                        nodeRange
                        [ Fix.replaceRangeBy { start = nodeRange.start, end = rnr.start } ""
                        , Fix.replaceRangeBy { start = rnr.end, end = nodeRange.end } ("." ++ fieldName)
                        ]
                    ]

                Nothing ->
                    []


replaceBySubExpressionFix : Range -> Node Expression -> List Fix
replaceBySubExpressionFix outerRange (Node exprRange exprValue) =
    if needsParens exprValue then
        [ Fix.replaceRangeBy { start = outerRange.start, end = exprRange.start } "("
        , Fix.replaceRangeBy { start = exprRange.end, end = outerRange.end } ")"
        ]

    else
        [ Fix.removeRange { start = outerRange.start, end = exprRange.start }
        , Fix.removeRange { start = exprRange.end, end = outerRange.end }
        ]


replaceSubExpressionByRecordAccessFix : String -> Node Expression -> List Fix
replaceSubExpressionByRecordAccessFix fieldName (Node exprRange exprValue) =
    if needsParens exprValue then
        [ Fix.insertAt exprRange.start "("
        , Fix.insertAt exprRange.end (")." ++ fieldName)
        ]

    else
        [ Fix.insertAt exprRange.end ("." ++ fieldName) ]


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


type alias CheckInfo =
    { lookupTable : ModuleNameLookupTable
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


functionCallChecks : Dict ( ModuleName, String ) (CheckInfo -> List (Error {}))
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
        , ( ( [ "Result" ], "andThen" ), resultAndThenChecks )
        , ( ( [ "Result" ], "withDefault" ), resultWithDefaultChecks )
        , ( ( [ "List" ], "append" ), listAppendChecks )
        , ( ( [ "List" ], "head" ), listHeadChecks )
        , ( ( [ "List" ], "tail" ), listTailChecks )
        , ( ( [ "List" ], "member" ), listMemberChecks )
        , ( ( [ "List" ], "map" ), collectionMapChecks listCollection )
        , ( ( [ "List" ], "filter" ), collectionFilterChecks listCollection )
        , reportEmptyListSecondArgument ( ( [ "List" ], "filterMap" ), listFilterMapChecks )
        , reportEmptyListFirstArgument ( ( [ "List" ], "concat" ), listConcatChecks )
        , reportEmptyListSecondArgument ( ( [ "List" ], "concatMap" ), listConcatMapChecks )
        , reportEmptyListSecondArgument ( ( [ "List" ], "indexedMap" ), listIndexedMapChecks )
        , reportEmptyListSecondArgument ( ( [ "List" ], "intersperse" ), \_ -> [] )
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
        , ( ( [ "Set" ], "map" ), collectionMapChecks setCollection )
        , ( ( [ "Set" ], "filter" ), collectionFilterChecks setCollection )
        , ( ( [ "Set" ], "remove" ), collectionRemoveChecks setCollection )
        , ( ( [ "Set" ], "isEmpty" ), collectionIsEmptyChecks setCollection )
        , ( ( [ "Set" ], "size" ), collectionSizeChecks setCollection )
        , ( ( [ "Set" ], "member" ), collectionMemberChecks setCollection )
        , ( ( [ "Set" ], "fromList" ), collectionFromListChecks setCollection )
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
        , ( ( [ "Platform", "Cmd" ], "map" ), collectionMapChecks cmdCollection )
        , ( ( [ "Platform", "Sub" ], "batch" ), subAndCmdBatchChecks "Sub" )
        , ( ( [ "Platform", "Sub" ], "map" ), collectionMapChecks subCollection )
        , ( ( [ "Json", "Decode" ], "oneOf" ), oneOfChecks )
        , ( ( [ "Html", "Attributes" ], "classList" ), htmlAttributesClassListChecks )
        , ( ( [ "Parser" ], "oneOf" ), oneOfChecks )
        , ( ( [ "Parser", "Advanced" ], "oneOf" ), oneOfChecks )
        ]


type alias OperatorCheckInfo =
    { lookupTable : ModuleNameLookupTable
    , inferredConstants : ( Infer.Inferred, List Infer.Inferred )
    , parentRange : Range
    , operator : String
    , left : Node Expression
    , leftRange : Range
    , right : Node Expression
    , rightRange : Range
    , isOnTheRightSideOfPlusPlus : Bool
    }


operatorChecks : Dict String (OperatorCheckInfo -> List (Error {}))
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
    , fromLeftToRight : Bool
    , parentRange : Range
    , left : Node Expression
    , leftRange : Range
    , right : Node Expression
    , rightRange : Range
    }


firstThatReportsError : List (a -> List (Error {})) -> a -> List (Error {})
firstThatReportsError remainingChecks data =
    case remainingChecks of
        [] ->
            []

        checkFn :: restOfFns ->
            case checkFn data of
                [] ->
                    firstThatReportsError restOfFns data

                errors ->
                    errors


compositionChecks : List (CompositionCheckInfo -> List (Error {}))
compositionChecks =
    [ identityCompositionCheck
    , notNotCompositionCheck
    , negateCompositionCheck
    , alwaysCompositionCheck
    , maybeMapCompositionChecks
    , resultMapCompositionChecks
    , filterAndMapCompositionCheck
    , concatAndMapCompositionCheck
    , foldAndSetToListCompositionChecks "foldl"
    , foldAndSetToListCompositionChecks "foldr"
    ]


removeAlongWithOtherFunctionCheck :
    { message : String, details : List String }
    -> (ModuleNameLookupTable -> Node Expression -> Maybe Range)
    -> CheckInfo
    -> List (Error {})
removeAlongWithOtherFunctionCheck errorMessage secondFunctionCheck checkInfo =
    case Node.value (AstHelpers.removeParens checkInfo.firstArg) of
        Expression.Application (secondFn :: firstArgOfSecondCall :: _) ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    [ Rule.errorWithFix
                        errorMessage
                        (Range.combine [ checkInfo.fnRange, secondRange ])
                        [ removeFunctionFromFunctionCall checkInfo
                        , removeFunctionFromFunctionCall
                            { fnRange = Node.range secondFn
                            , firstArg = firstArgOfSecondCall
                            , usingRightPizza = False
                            }
                        ]
                    ]

                Nothing ->
                    []

        Expression.OperatorApplication "|>" _ firstArgOfSecondCall secondFn ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    [ Rule.errorWithFix
                        errorMessage
                        (Range.combine [ checkInfo.fnRange, secondRange ])
                        [ removeFunctionFromFunctionCall checkInfo
                        , removeFunctionFromFunctionCall
                            { fnRange = Node.range secondFn
                            , firstArg = firstArgOfSecondCall
                            , usingRightPizza = True
                            }
                        ]
                    ]

                Nothing ->
                    []

        Expression.OperatorApplication "<|" _ secondFn firstArgOfSecondCall ->
            case secondFunctionCheck checkInfo.lookupTable secondFn of
                Just secondRange ->
                    [ Rule.errorWithFix
                        errorMessage
                        (Range.combine [ checkInfo.fnRange, secondRange ])
                        [ removeFunctionFromFunctionCall checkInfo
                        , removeFunctionFromFunctionCall
                            { fnRange = Node.range secondFn
                            , firstArg = firstArgOfSecondCall
                            , usingRightPizza = False
                            }
                        ]
                    ]

                Nothing ->
                    []

        _ ->
            []


plusChecks : OperatorCheckInfo -> List (Error {})
plusChecks { leftRange, rightRange, left, right } =
    findMap
        (\( node, getRange ) ->
            if getUncomputedNumberValue node == Just 0 then
                Just
                    [ Rule.errorWithFix
                        { message = "Unnecessary addition with 0"
                        , details = [ "Adding 0 does not change the value of the number." ]
                        }
                        (getRange ())
                        [ Fix.removeRange (getRange ()) ]
                    ]

            else
                Nothing
        )
        [ ( right, \() -> { start = leftRange.end, end = rightRange.end } )
        , ( left, \() -> { start = leftRange.start, end = rightRange.start } )
        ]
        |> Maybe.withDefault []


minusChecks : OperatorCheckInfo -> List (Error {})
minusChecks { leftRange, rightRange, left, right } =
    if getUncomputedNumberValue right == Just 0 then
        let
            range : Range
            range =
                { start = leftRange.end, end = rightRange.end }
        in
        [ Rule.errorWithFix
            { message = "Unnecessary subtraction with 0"
            , details = [ "Subtracting 0 does not change the value of the number." ]
            }
            range
            [ Fix.removeRange range ]
        ]

    else if getUncomputedNumberValue left == Just 0 then
        let
            range : Range
            range =
                { start = leftRange.start, end = rightRange.start }
        in
        [ Rule.errorWithFix
            { message = "Unnecessary subtracting from 0"
            , details = [ "You can negate the expression on the right like `-n`." ]
            }
            range
            (if needsParens (Node.value right) then
                [ Fix.replaceRangeBy range "-(", Fix.insertAt rightRange.end ")" ]

             else
                [ Fix.replaceRangeBy range "-" ]
            )
        ]

    else
        []


multiplyChecks : OperatorCheckInfo -> List (Error {})
multiplyChecks { leftRange, rightRange, left, right } =
    findMap
        (\( node, getRange ) ->
            case getUncomputedNumberValue node of
                Just number ->
                    if number == 1 then
                        Just
                            [ Rule.errorWithFix
                                { message = "Unnecessary multiplication by 1"
                                , details = [ "Multiplying by 1 does not change the value of the number." ]
                                }
                                (getRange ())
                                [ Fix.removeRange (getRange ()) ]
                            ]

                    else if number == 0 then
                        Just
                            [ Rule.error
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
                                (getRange ())
                            ]

                    else
                        Nothing

                Nothing ->
                    Nothing
        )
        [ ( right, \() -> { start = leftRange.end, end = rightRange.end } )
        , ( left, \() -> { start = leftRange.start, end = rightRange.start } )
        ]
        |> Maybe.withDefault []


divisionChecks : OperatorCheckInfo -> List (Error {})
divisionChecks { leftRange, rightRange, right } =
    if getUncomputedNumberValue right == Just 1 then
        let
            range : Range
            range =
                { start = leftRange.end, end = rightRange.end }
        in
        [ Rule.errorWithFix
            { message = "Unnecessary division by 1"
            , details = [ "Dividing by 1 does not change the value of the number." ]
            }
            range
            [ Fix.removeRange range ]
        ]

    else
        []


plusplusChecks : OperatorCheckInfo -> List (Error {})
plusplusChecks { parentRange, leftRange, rightRange, left, right, isOnTheRightSideOfPlusPlus } =
    case ( Node.value left, Node.value right ) of
        ( Expression.Literal "", Expression.Literal _ ) ->
            [ errorForAddingEmptyStrings leftRange
                { start = leftRange.start
                , end = rightRange.start
                }
            ]

        ( Expression.Literal _, Expression.Literal "" ) ->
            [ errorForAddingEmptyStrings rightRange
                { start = leftRange.end
                , end = rightRange.end
                }
            ]

        ( Expression.ListExpr [], _ ) ->
            [ errorForAddingEmptyLists leftRange
                { start = leftRange.start
                , end = rightRange.start
                }
            ]

        ( _, Expression.ListExpr [] ) ->
            [ errorForAddingEmptyLists rightRange
                { start = leftRange.end
                , end = rightRange.end
                }
            ]

        ( Expression.ListExpr _, Expression.ListExpr _ ) ->
            [ Rule.errorWithFix
                { message = "Expression could be simplified to be a single List"
                , details = [ "Try moving all the elements into a single list." ]
                }
                parentRange
                [ Fix.replaceRangeBy
                    { start = { row = leftRange.end.row, column = leftRange.end.column - 1 }
                    , end = { row = rightRange.start.row, column = rightRange.start.column + 1 }
                    }
                    ","
                ]
            ]

        ( Expression.ListExpr [ listElement ], _ ) ->
            if isOnTheRightSideOfPlusPlus then
                []

            else
                [ Rule.errorWithFix
                    { message = "Should use (::) instead of (++)"
                    , details = [ "Concatenating a list with a single value is the same as using (::) on the list with the value." ]
                    }
                    parentRange
                    (Fix.replaceRangeBy
                        { start = leftRange.end
                        , end = rightRange.start
                        }
                        " :: "
                        :: replaceBySubExpressionFix leftRange listElement
                    )
                ]

        _ ->
            []


errorForAddingEmptyStrings : Range -> Range -> Error {}
errorForAddingEmptyStrings range rangeToRemove =
    Rule.errorWithFix
        { message = "Unnecessary concatenation with an empty string"
        , details = [ "You should remove the concatenation with the empty string." ]
        }
        range
        [ Fix.removeRange rangeToRemove ]


errorForAddingEmptyLists : Range -> Range -> Error {}
errorForAddingEmptyLists range rangeToRemove =
    Rule.errorWithFix
        { message = "Concatenating with a single list doesn't have any effect"
        , details = [ "You should remove the concatenation with the empty list." ]
        }
        range
        [ Fix.removeRange rangeToRemove ]


consChecks : OperatorCheckInfo -> List (Error {})
consChecks { right, leftRange, rightRange } =
    case Node.value right of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Element added to the beginning of the list could be included in the list"
                , details = [ "Try moving the element inside the list it is being added to." ]
                }
                leftRange
                [ Fix.insertAt leftRange.start "[ "
                , Fix.replaceRangeBy
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                    " ]"
                ]
            ]

        Expression.ListExpr _ ->
            [ Rule.errorWithFix
                { message = "Element added to the beginning of the list could be included in the list"
                , details = [ "Try moving the element inside the list it is being added to." ]
                }
                leftRange
                [ Fix.insertAt leftRange.start "[ "
                , Fix.replaceRangeBy
                    { start = leftRange.end
                    , end = { row = rightRange.start.row, column = rightRange.start.column + 1 }
                    }
                    ","
                ]
            ]

        _ ->
            []



-- NUMBERS


negateNegateCompositionErrorMessage : { message : String, details : List String }
negateNegateCompositionErrorMessage =
    { message = "Unnecessary double negation"
    , details = [ "Composing `negate` with `negate` cancel each other out." ]
    }


negateCompositionCheck : CompositionCheckInfo -> List (Error {})
negateCompositionCheck { lookupTable, fromLeftToRight, parentRange, left, right, leftRange, rightRange } =
    case Maybe.map2 Tuple.pair (getNegateFunction lookupTable left) (getNegateFunction lookupTable right) of
        Just _ ->
            [ Rule.errorWithFix
                negateNegateCompositionErrorMessage
                parentRange
                [ Fix.replaceRangeBy parentRange "identity" ]
            ]

        _ ->
            case getNegateFunction lookupTable left of
                Just leftNotRange ->
                    case getNegateComposition lookupTable fromLeftToRight right of
                        Just rightNotRange ->
                            [ Rule.errorWithFix
                                negateNegateCompositionErrorMessage
                                { start = leftNotRange.start, end = rightNotRange.end }
                                [ Fix.removeRange { start = leftNotRange.start, end = rightRange.start }
                                , Fix.removeRange rightNotRange
                                ]
                            ]

                        Nothing ->
                            []

                Nothing ->
                    case getNegateFunction lookupTable right of
                        Just rightNotRange ->
                            case getNegateComposition lookupTable (not fromLeftToRight) left of
                                Just leftNotRange ->
                                    [ Rule.errorWithFix
                                        negateNegateCompositionErrorMessage
                                        { start = leftNotRange.start, end = rightNotRange.end }
                                        [ Fix.removeRange leftNotRange
                                        , Fix.removeRange { start = leftRange.end, end = rightNotRange.end }
                                        ]
                                    ]

                                Nothing ->
                                    []

                        Nothing ->
                            []


getNegateComposition : ModuleNameLookupTable -> Bool -> Node Expression -> Maybe Range
getNegateComposition lookupTable takeFirstFunction node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.OperatorApplication "<<" _ left right ->
            if takeFirstFunction then
                getNegateFunction lookupTable right
                    |> Maybe.map (\_ -> { start = (Node.range left).end, end = (Node.range right).end })

            else
                getNegateFunction lookupTable left
                    |> Maybe.map (\_ -> { start = (Node.range left).start, end = (Node.range right).start })

        Expression.OperatorApplication ">>" _ left right ->
            if takeFirstFunction then
                getNegateFunction lookupTable left
                    |> Maybe.map (\_ -> { start = (Node.range left).start, end = (Node.range right).start })

            else
                getNegateFunction lookupTable right
                    |> Maybe.map (\_ -> { start = (Node.range left).end, end = (Node.range right).end })

        _ ->
            Nothing


basicsNegateChecks : CheckInfo -> List (Error {})
basicsNegateChecks checkInfo =
    removeAlongWithOtherFunctionCheck negateNegateCompositionErrorMessage getNegateFunction checkInfo


getNegateFunction : ModuleNameLookupTable -> Node Expression -> Maybe Range
getNegateFunction lookupTable baseNode =
    case AstHelpers.removeParens baseNode of
        Node range (Expression.FunctionOrValue _ "negate") ->
            case ModuleNameLookupTable.moduleNameAt lookupTable range of
                Just [ "Basics" ] ->
                    Just range

                _ ->
                    Nothing

        _ ->
            Nothing



-- BOOLEAN


basicsNotChecks : CheckInfo -> List (Error {})
basicsNotChecks checkInfo =
    case Evaluate.getBoolean checkInfo checkInfo.firstArg of
        Determined bool ->
            [ Rule.errorWithFix
                { message = "Expression is equal to " ++ boolToString (not bool)
                , details = [ "You can replace the call to `not` by the boolean value directly." ]
                }
                checkInfo.parentRange
                [ Fix.replaceRangeBy checkInfo.parentRange (boolToString (not bool)) ]
            ]

        Undetermined ->
            removeAlongWithOtherFunctionCheck notNotCompositionErrorMessage getNotFunction checkInfo


notNotCompositionCheck : CompositionCheckInfo -> List (Error {})
notNotCompositionCheck { lookupTable, fromLeftToRight, parentRange, left, right, leftRange, rightRange } =
    let
        notOnLeft : Maybe Range
        notOnLeft =
            getNotFunction lookupTable left

        notOnRight : Maybe Range
        notOnRight =
            getNotFunction lookupTable right
    in
    case ( notOnLeft, notOnRight ) of
        ( Just _, Just _ ) ->
            [ Rule.errorWithFix
                notNotCompositionErrorMessage
                parentRange
                [ Fix.replaceRangeBy parentRange "identity" ]
            ]

        ( Just leftNotRange, _ ) ->
            case getNotComposition lookupTable fromLeftToRight right of
                Just rightNotRange ->
                    [ Rule.errorWithFix
                        notNotCompositionErrorMessage
                        { start = leftNotRange.start, end = rightNotRange.end }
                        [ Fix.removeRange { start = leftNotRange.start, end = rightRange.start }
                        , Fix.removeRange rightNotRange
                        ]
                    ]

                Nothing ->
                    []

        ( _, Just rightNotRange ) ->
            case getNotComposition lookupTable (not fromLeftToRight) left of
                Just leftNotRange ->
                    [ Rule.errorWithFix
                        notNotCompositionErrorMessage
                        { start = leftNotRange.start, end = rightNotRange.end }
                        [ Fix.removeRange leftNotRange
                        , Fix.removeRange { start = leftRange.end, end = rightNotRange.end }
                        ]
                    ]

                Nothing ->
                    []

        _ ->
            []


notNotCompositionErrorMessage : { message : String, details : List String }
notNotCompositionErrorMessage =
    { message = "Unnecessary double negation"
    , details = [ "Composing `not` with `not` cancel each other out." ]
    }


getNotComposition : ModuleNameLookupTable -> Bool -> Node Expression -> Maybe Range
getNotComposition lookupTable takeFirstFunction node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.OperatorApplication "<<" _ left right ->
            if takeFirstFunction then
                getNotFunction lookupTable right
                    |> Maybe.map (\_ -> { start = (Node.range left).end, end = (Node.range right).end })

            else
                getNotFunction lookupTable left
                    |> Maybe.map (\_ -> { start = (Node.range left).start, end = (Node.range right).start })

        Expression.OperatorApplication ">>" _ left right ->
            if takeFirstFunction then
                getNotFunction lookupTable left
                    |> Maybe.map (\_ -> { start = (Node.range left).start, end = (Node.range right).start })

            else
                getNotFunction lookupTable right
                    |> Maybe.map (\_ -> { start = (Node.range left).end, end = (Node.range right).end })

        _ ->
            Nothing


orChecks : OperatorCheckInfo -> List (Error {})
orChecks operatorCheckInfo =
    firstThatReportsError
        [ \() ->
            List.concat
                [ or_isLeftSimplifiableError operatorCheckInfo
                , or_isRightSimplifiableError operatorCheckInfo
                ]
        , \() -> findSimilarConditionsError operatorCheckInfo
        ]
        ()


type RedundantConditionResolution
    = RemoveFrom Location
    | ReplaceByNoop Bool


findSimilarConditionsError : OperatorCheckInfo -> List (Error {})
findSimilarConditionsError operatorCheckInfo =
    let
        conditionsOnTheRight : List ( RedundantConditionResolution, Node Expression )
        conditionsOnTheRight =
            listConditions
                operatorCheckInfo.operator
                (RemoveFrom operatorCheckInfo.leftRange.end)
                operatorCheckInfo.right

        errorsForNode : Node Expression -> List (Error {})
        errorsForNode nodeToCompareTo =
            List.concatMap
                (areSimilarConditionsError
                    operatorCheckInfo
                    operatorCheckInfo.operator
                    nodeToCompareTo
                )
                conditionsOnTheRight
    in
    operatorCheckInfo.left
        |> listConditions operatorCheckInfo.operator (RemoveFrom operatorCheckInfo.leftRange.end)
        |> List.concatMap (Tuple.second >> errorsForNode)


areSimilarConditionsError : Infer.Resources a -> String -> Node Expression -> ( RedundantConditionResolution, Node Expression ) -> List (Error {})
areSimilarConditionsError resources operator nodeToCompareTo ( redundantConditionResolution, nodeToLookAt ) =
    case Normalize.compare resources nodeToCompareTo nodeToLookAt of
        Normalize.ConfirmedEquality ->
            errorForRedundantCondition operator redundantConditionResolution nodeToLookAt

        Normalize.ConfirmedInequality ->
            []

        Normalize.Unconfirmed ->
            []


errorForRedundantCondition : String -> RedundantConditionResolution -> Node a -> List (Error {})
errorForRedundantCondition operator redundantConditionResolution node =
    let
        ( range, fix ) =
            rangeAndFixForRedundantCondition redundantConditionResolution node
    in
    [ Rule.errorWithFix
        { message = "Condition is redundant"
        , details =
            [ "This condition is the same as another one found on the left side of the (" ++ operator ++ ") operator, therefore one of them can be removed."
            ]
        }
        range
        fix
    ]


rangeAndFixForRedundantCondition : RedundantConditionResolution -> Node a -> ( Range, List Fix )
rangeAndFixForRedundantCondition redundantConditionResolution node =
    case redundantConditionResolution of
        RemoveFrom locationOfPrevElement ->
            let
                range : Range
                range =
                    { start = locationOfPrevElement
                    , end = (Node.range node).end
                    }
            in
            ( range
            , [ Fix.removeRange range ]
            )

        ReplaceByNoop noopValue ->
            let
                range : Range
                range =
                    Node.range node
            in
            ( range
            , [ Fix.replaceRangeBy range (boolToString noopValue) ]
            )


listConditions : String -> RedundantConditionResolution -> Node Expression -> List ( RedundantConditionResolution, Node Expression )
listConditions operatorToLookFor redundantConditionResolution node =
    case Node.value node of
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
                [ ( redundantConditionResolution, node ) ]

        _ ->
            [ ( redundantConditionResolution, node ) ]


or_isLeftSimplifiableError : OperatorCheckInfo -> List (Error {})
or_isLeftSimplifiableError ({ parentRange, left, leftRange, rightRange } as checkInfo) =
    case Evaluate.getBoolean checkInfo left of
        Determined True ->
            [ Rule.errorWithFix
                { message = "Comparison is always True"
                , details = alwaysSameDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                ]
            ]

        Determined False ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.start
                    , end = rightRange.start
                    }
                ]
            ]

        Undetermined ->
            []


or_isRightSimplifiableError : OperatorCheckInfo -> List (Error {})
or_isRightSimplifiableError ({ parentRange, right, leftRange, rightRange } as checkInfo) =
    case Evaluate.getBoolean checkInfo right of
        Determined True ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.start
                    , end = rightRange.start
                    }
                ]
            ]

        Determined False ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                ]
            ]

        Undetermined ->
            []


andChecks : OperatorCheckInfo -> List (Error {})
andChecks operatorCheckInfo =
    firstThatReportsError
        [ \() ->
            List.concat
                [ and_isLeftSimplifiableError operatorCheckInfo
                , and_isRightSimplifiableError operatorCheckInfo
                ]
        , \() -> findSimilarConditionsError operatorCheckInfo
        ]
        ()


and_isLeftSimplifiableError : OperatorCheckInfo -> List (Rule.Error {})
and_isLeftSimplifiableError ({ parentRange, left, leftRange, rightRange } as checkInfo) =
    case Evaluate.getBoolean checkInfo left of
        Determined True ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.start
                    , end = rightRange.start
                    }
                ]
            ]

        Determined False ->
            [ Rule.errorWithFix
                { message = "Comparison is always False"
                , details = alwaysSameDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                ]
            ]

        Undetermined ->
            []


and_isRightSimplifiableError : OperatorCheckInfo -> List (Rule.Error {})
and_isRightSimplifiableError ({ parentRange, leftRange, right, rightRange } as checkInfo) =
    case Evaluate.getBoolean checkInfo right of
        Determined True ->
            [ Rule.errorWithFix
                { message = unnecessaryMessage
                , details = unnecessaryDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.end
                    , end = rightRange.end
                    }
                ]
            ]

        Determined False ->
            [ Rule.errorWithFix
                { message = "Comparison is always False"
                , details = alwaysSameDetails
                }
                parentRange
                [ Fix.removeRange
                    { start = leftRange.start
                    , end = rightRange.start
                    }
                ]
            ]

        Undetermined ->
            []



-- EQUALITY


equalityChecks : Bool -> OperatorCheckInfo -> List (Error {})
equalityChecks isEqual ({ lookupTable, parentRange, left, right, leftRange, rightRange } as checkInfo) =
    if Evaluate.getBoolean checkInfo right == Determined isEqual then
        [ Rule.errorWithFix
            { message = "Unnecessary comparison with boolean"
            , details = [ "The result of the expression will be the same with or without the comparison." ]
            }
            parentRange
            [ Fix.removeRange { start = leftRange.end, end = rightRange.end } ]
        ]

    else if Evaluate.getBoolean checkInfo left == Determined isEqual then
        [ Rule.errorWithFix
            { message = "Unnecessary comparison with boolean"
            , details = [ "The result of the expression will be the same with or without the comparison." ]
            }
            parentRange
            [ Fix.removeRange { start = leftRange.start, end = rightRange.start } ]
        ]

    else
        case Maybe.map2 Tuple.pair (getNotCall lookupTable left) (getNotCall lookupTable right) of
            Just ( notRangeLeft, notRangeRight ) ->
                [ Rule.errorWithFix
                    { message = "Unnecessary negation on both sides"
                    , details = [ "Since both sides are negated using `not`, they are redundant and can be removed." ]
                    }
                    parentRange
                    [ Fix.removeRange notRangeLeft, Fix.removeRange notRangeRight ]
                ]

            _ ->
                let
                    inferred : Infer.Inferred
                    inferred =
                        Tuple.first checkInfo.inferredConstants

                    normalizeAndInfer : Node Expression -> Node Expression
                    normalizeAndInfer node =
                        let
                            newNode : Node Expression
                            newNode =
                                Normalize.normalize checkInfo node
                        in
                        case Infer.get (Node.value newNode) inferred of
                            Just expr ->
                                Node Range.emptyRange expr

                            Nothing ->
                                newNode

                    normalizedLeft : Node Expression
                    normalizedLeft =
                        normalizeAndInfer left

                    normalizedRight : Node Expression
                    normalizedRight =
                        normalizeAndInfer right
                in
                case Normalize.compareWithoutNormalization normalizedLeft normalizedRight of
                    Normalize.ConfirmedEquality ->
                        [ comparisonError isEqual parentRange ]

                    Normalize.ConfirmedInequality ->
                        [ comparisonError (not isEqual) parentRange ]

                    Normalize.Unconfirmed ->
                        []


getNotCall : ModuleNameLookupTable -> Node Expression -> Maybe Range
getNotCall lookupTable baseNode =
    case Node.value (AstHelpers.removeParens baseNode) of
        Expression.Application ((Node notRange (Expression.FunctionOrValue _ "not")) :: _) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable notRange of
                Just [ "Basics" ] ->
                    Just notRange

                _ ->
                    Nothing

        _ ->
            Nothing


getNotFunction : ModuleNameLookupTable -> Node Expression -> Maybe Range
getNotFunction lookupTable baseNode =
    getSpecificFunction ( [ "Basics" ], "not" ) lookupTable baseNode


getListSingletonCall : ModuleNameLookupTable -> Node Expression -> Maybe { element : Node Expression }
getListSingletonCall lookupTable expressionNode =
    case getSpecificFunctionCall ( [ "List" ], "singleton" ) lookupTable expressionNode of
        Just singletonCall ->
            case singletonCall.argsAfterFirst of
                [] ->
                    Just { element = singletonCall.firstArg }

                _ :: _ ->
                    Nothing

        Nothing ->
            Nothing


getSpecificFunction : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificFunction ( moduleName, name ) lookupTable baseNode =
    case AstHelpers.removeParens baseNode of
        Node fnRange (Expression.FunctionOrValue _ foundName) ->
            if
                (foundName == name)
                    && (ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just moduleName)
            then
                Just fnRange

            else
                Nothing

        _ ->
            Nothing


getSpecificFunctionCall :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            }
getSpecificFunctionCall ( moduleName, name ) lookupTable baseNode =
    getFunctionCall baseNode
        |> Maybe.andThen
            (\call ->
                if
                    (call.fnName /= name)
                        || (ModuleNameLookupTable.moduleNameAt lookupTable call.fnRange /= Just moduleName)
                then
                    Nothing

                else
                    Just
                        { nodeRange = call.nodeRange
                        , fnRange = call.fnRange
                        , firstArg = call.firstArg
                        , argsAfterFirst = call.argsAfterFirst
                        }
            )


getFunctionCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            }
getFunctionCall baseNode =
    case Node.value (AstHelpers.removeParens baseNode) of
        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsAfterFirst) ->
            Just
                { nodeRange = Node.range baseNode
                , fnRange = fnRange
                , fnName = fnName
                , firstArg = firstArg
                , argsAfterFirst = argsAfterFirst
                }

        Expression.OperatorApplication "|>" _ firstArg fedFunction ->
            case fedFunction of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    Just
                        { nodeRange = Node.range baseNode
                        , fnRange = fnRange
                        , fnName = fnName
                        , firstArg = firstArg
                        , argsAfterFirst = []
                        }

                Node _ (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: argsAfterFirst)) ->
                    Just
                        { nodeRange = Node.range baseNode
                        , fnRange = fnRange
                        , fnName = fnName
                        , firstArg = firstArg
                        , argsAfterFirst = argsAfterFirst
                        }

                _ ->
                    Nothing

        Expression.OperatorApplication "<|" _ fedFunction firstArg ->
            case fedFunction of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    Just
                        { nodeRange = Node.range baseNode
                        , fnRange = fnRange
                        , fnName = fnName
                        , firstArg = firstArg
                        , argsAfterFirst = []
                        }

                Node _ (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: argsAfterFirst)) ->
                    Just
                        { nodeRange = Node.range baseNode
                        , fnRange = fnRange
                        , fnName = fnName
                        , firstArg = firstArg
                        , argsAfterFirst = argsAfterFirst
                        }

                _ ->
                    Nothing

        _ ->
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


comparisonChecks : (Float -> Float -> Bool) -> OperatorCheckInfo -> List (Error {})
comparisonChecks operatorFunction operatorCheckInfo =
    case
        Maybe.map2 operatorFunction
            (Normalize.getNumberValue operatorCheckInfo.left)
            (Normalize.getNumberValue operatorCheckInfo.right)
    of
        Just bool ->
            [ comparisonError bool operatorCheckInfo.parentRange ]

        Nothing ->
            []


comparisonError : Bool -> Range -> Error {}
comparisonError bool range =
    let
        boolAsString : String
        boolAsString =
            boolToString bool
    in
    Rule.errorWithFix
        { message = "Comparison is always " ++ boolAsString
        , details =
            [ "Based on the values and/or the context, we can determine that the value of this operation will always be " ++ boolAsString ++ "."
            ]
        }
        range
        [ Fix.replaceRangeBy range boolAsString ]



-- IF EXPRESSIONS


targetIfKeyword : Range -> Range
targetIfKeyword { start } =
    { start = start
    , end = { start | column = start.column + 2 }
    }



-- BASICS


basicsIdentityChecks : CheckInfo -> List (Error {})
basicsIdentityChecks checkInfo =
    [ Rule.errorWithFix
        { message = "`identity` should be removed"
        , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
        }
        checkInfo.fnRange
        [ removeFunctionFromFunctionCall checkInfo
        ]
    ]


identityCompositionErrorMessage : { message : String, details : List String }
identityCompositionErrorMessage =
    { message = "`identity` should be removed"
    , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
    }


identityCompositionCheck : CompositionCheckInfo -> List (Error {})
identityCompositionCheck { lookupTable, left, right } =
    if isIdentity lookupTable right then
        [ Rule.errorWithFix
            identityCompositionErrorMessage
            (Node.range right)
            [ Fix.removeRange { start = (Node.range left).end, end = (Node.range right).end }
            ]
        ]

    else if isIdentity lookupTable left then
        [ Rule.errorWithFix
            identityCompositionErrorMessage
            (Node.range left)
            [ Fix.removeRange { start = (Node.range left).start, end = (Node.range right).start }
            ]
        ]

    else
        []


basicsAlwaysChecks : CheckInfo -> List (Error {})
basicsAlwaysChecks checkInfo =
    case secondArg checkInfo of
        Just (Node secondArgRange _) ->
            [ Rule.errorWithFix
                { message = "Expression can be replaced by the first argument given to `always`"
                , details = [ "The second argument will be ignored because of the `always` call." ]
                }
                checkInfo.fnRange
                (if checkInfo.usingRightPizza then
                    [ Fix.removeRange { start = secondArgRange.start, end = (Node.range checkInfo.firstArg).start }
                    ]

                 else
                    [ removeFunctionFromFunctionCall checkInfo
                    , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = secondArgRange.end }
                    ]
                )
            ]

        Nothing ->
            []


alwaysCompositionErrorMessage : { message : String, details : List String }
alwaysCompositionErrorMessage =
    { message = "Function composed with always will be ignored"
    , details = [ "`always` will swallow the function composed into it." ]
    }


alwaysCompositionCheck : CompositionCheckInfo -> List (Error {})
alwaysCompositionCheck { lookupTable, fromLeftToRight, left, right, leftRange, rightRange } =
    if fromLeftToRight then
        if isAlwaysCall lookupTable right then
            [ Rule.errorWithFix
                alwaysCompositionErrorMessage
                rightRange
                [ Fix.removeRange { start = leftRange.start, end = rightRange.start } ]
            ]

        else
            []

    else if isAlwaysCall lookupTable left then
        [ Rule.errorWithFix
            alwaysCompositionErrorMessage
            leftRange
            [ Fix.removeRange { start = leftRange.end, end = rightRange.end } ]
        ]

    else
        []


isAlwaysCall : ModuleNameLookupTable -> Node Expression -> Bool
isAlwaysCall lookupTable node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: _ :: []) ->
            ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange == Just [ "Basics" ]

        _ ->
            False


getAlwaysArgument : ModuleNameLookupTable -> Node Expression -> Maybe { alwaysRange : Range, rangeToRemove : Range }
getAlwaysArgument lookupTable node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: arg :: []) ->
            if ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange == Just [ "Basics" ] then
                Just
                    { alwaysRange = alwaysRange
                    , rangeToRemove = { start = alwaysRange.start, end = (Node.range arg).start }
                    }

            else
                Nothing

        Expression.OperatorApplication "<|" _ (Node alwaysRange (Expression.FunctionOrValue _ "always")) arg ->
            if ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange == Just [ "Basics" ] then
                Just
                    { alwaysRange = alwaysRange
                    , rangeToRemove = { start = alwaysRange.start, end = (Node.range arg).start }
                    }

            else
                Nothing

        Expression.OperatorApplication "|>" _ arg (Node alwaysRange (Expression.FunctionOrValue _ "always")) ->
            if ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange == Just [ "Basics" ] then
                Just
                    { alwaysRange = alwaysRange
                    , rangeToRemove = { start = (Node.range arg).end, end = alwaysRange.end }
                    }

            else
                Nothing

        _ ->
            Nothing


reportEmptyListSecondArgument : ( ( ModuleName, String ), CheckInfo -> List (Error {}) ) -> ( ( ModuleName, String ), CheckInfo -> List (Error {}) )
reportEmptyListSecondArgument ( ( moduleName, name ), function ) =
    ( ( moduleName, name )
    , \checkInfo ->
        case secondArg checkInfo of
            Just (Node _ (Expression.ListExpr [])) ->
                [ Rule.errorWithFix
                    { message = "Using " ++ String.join "." moduleName ++ "." ++ name ++ " on an empty list will result in an empty list"
                    , details = [ "You can replace this call by an empty list." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                function checkInfo
    )


reportEmptyListFirstArgument : ( ( ModuleName, String ), CheckInfo -> List (Error {}) ) -> ( ( ModuleName, String ), CheckInfo -> List (Error {}) )
reportEmptyListFirstArgument ( ( moduleName, name ), function ) =
    ( ( moduleName, name )
    , \checkInfo ->
        case checkInfo.firstArg of
            Node _ (Expression.ListExpr []) ->
                [ Rule.errorWithFix
                    { message = "Using " ++ String.join "." moduleName ++ "." ++ name ++ " on an empty list will result in an empty list"
                    , details = [ "You can replace this call by an empty list." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                function checkInfo
    )



-- STRING


stringFromListChecks : CheckInfo -> List (Error {})
stringFromListChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Calling String.fromList [] will result in " ++ emptyStringAsString
                , details = [ "You can replace this call by " ++ emptyStringAsString ++ "." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange emptyStringAsString ]
            ]

        Expression.ListExpr ((Node keep _) :: []) ->
            [ Rule.errorWithFix
                { message = "Calling String.fromList with a list with a single char is the same as String.fromChar with the contained char"
                , details = [ "You can replace this call by String.fromChar with the contained char." ]
                }
                checkInfo.fnRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = keep }
                    ++ [ Fix.insertAt checkInfo.parentRange.start "String.fromChar " ]
                )
            ]

        _ ->
            []


stringIsEmptyChecks : CheckInfo -> List (Error {})
stringIsEmptyChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.Literal str ->
            let
                replacementValue : String
                replacementValue =
                    boolToString (str == "")
            in
            [ Rule.errorWithFix
                { message = "The call to String.isEmpty will result in " ++ replacementValue
                , details = [ "You can replace this call by " ++ replacementValue ++ "." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange replacementValue ]
            ]

        _ ->
            []


stringConcatChecks : CheckInfo -> List (Error {})
stringConcatChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using String.concat on an empty list will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "\"\"" ]
            ]

        _ ->
            []


stringWordsChecks : CheckInfo -> List (Error {})
stringWordsChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.Literal "" ->
            [ Rule.errorWithFix
                { message = "Using String.words on an empty string will result in an empty list"
                , details = [ "You can replace this call by an empty list." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "[]" ]
            ]

        _ ->
            []


stringLinesChecks : CheckInfo -> List (Error {})
stringLinesChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.Literal "" ->
            [ Rule.errorWithFix
                { message = "Using String.lines on an empty string will result in an empty list"
                , details = [ "You can replace this call by an empty list." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "[]" ]
            ]

        _ ->
            []


stringReverseChecks : CheckInfo -> List (Error {})
stringReverseChecks ({ parentRange, fnRange, firstArg } as checkInfo) =
    case Node.value firstArg of
        Expression.Literal "" ->
            [ Rule.errorWithFix
                { message = "Using String.reverse on an empty string will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "\"\"" ]
            ]

        _ ->
            removeAlongWithOtherFunctionCheck
                reverseReverseCompositionErrorMessage
                (getSpecificFunction ( [ "String" ], "reverse" ))
                checkInfo


stringSliceChecks : CheckInfo -> List (Error {})
stringSliceChecks checkInfo =
    case ( checkInfo.firstArg, secondArg checkInfo, thirdArg checkInfo ) of
        ( _, _, Just (Node _ (Expression.Literal "")) ) ->
            [ Rule.errorWithFix
                { message = "Using String.slice on an empty string will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (thirdArg checkInfo))
            ]

        ( _, Just (Node _ (Expression.Integer 0)), _ ) ->
            [ Rule.errorWithFix
                { message = "Using String.slice with end index 0 will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (thirdArg checkInfo))
            ]

        ( Node startArgumentRange (Expression.Integer 0), _, _ ) ->
            [ Rule.errorWithFix
                { message = "Use String.left instead"
                , details = [ "Using String.slice with start index 0 is the same as using String.left." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy
                    { start = checkInfo.fnRange.start, end = startArgumentRange.end }
                    "String.left"
                ]
            ]

        ( start, Just end, _ ) ->
            Maybe.map2
                (\startInt endInt ->
                    if
                        (startInt >= endInt)
                            && -- have the same sign
                               ((startInt <= -1 && endInt <= -1)
                                    || (startInt >= 0 && endInt >= 0)
                               )
                    then
                        [ Rule.errorWithFix
                            { message = "The call to String.slice will result in \"\""
                            , details = [ "You can replace this slice operation by \"\"." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (thirdArg checkInfo))
                        ]
                            |> Just

                    else
                        -- either is negative or startInt < endInt
                        Nothing
                )
                (Evaluate.getInt checkInfo start)
                (Evaluate.getInt checkInfo end)
                |> Maybe.withDefault
                    (if Normalize.areAllTheSame checkInfo start [ end ] then
                        [ Rule.errorWithFix
                            { message = "Using String.slice with equal start and end index will result in an empty string"
                            , details = [ "You can replace this call by an empty string." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (thirdArg checkInfo))
                        ]
                            |> Just

                     else
                        Nothing
                    )
                |> Maybe.withDefault []

        ( _, Nothing, _ ) ->
            []


stringLeftChecks : CheckInfo -> List (Error {})
stringLeftChecks checkInfo =
    case ( checkInfo.firstArg, secondArg checkInfo ) of
        ( _, Just (Node _ (Expression.Literal "")) ) ->
            [ Rule.errorWithFix
                { message = "Using String.left on an empty string will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "\"\"" ]
            ]

        ( Node _ (Expression.Integer 0), _ ) ->
            [ Rule.errorWithFix
                { message = "Using String.left with length 0 will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (secondArg checkInfo))
            ]

        ( Node _ (Expression.Negation (Node _ (Expression.Integer _))), _ ) ->
            [ Rule.errorWithFix
                { message = "Using String.left with negative length will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (secondArg checkInfo))
            ]

        _ ->
            []


stringRightChecks : CheckInfo -> List (Error {})
stringRightChecks checkInfo =
    case ( checkInfo.firstArg, secondArg checkInfo ) of
        ( _, Just (Node _ (Expression.Literal "")) ) ->
            [ Rule.errorWithFix
                { message = "Using String.right on an empty string will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "\"\"" ]
            ]

        ( Node _ (Expression.Integer 0), _ ) ->
            [ Rule.errorWithFix
                { message = "Using String.right with length 0 will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (secondArg checkInfo))
            ]

        ( Node _ (Expression.Negation (Node _ (Expression.Integer _))), _ ) ->
            [ Rule.errorWithFix
                { message = "Using String.right with negative length will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (secondArg checkInfo))
            ]

        _ ->
            []


reverseReverseCompositionErrorMessage : { message : String, details : List String }
reverseReverseCompositionErrorMessage =
    { message = "Unnecessary double reversal"
    , details = [ "Composing `reverse` with `reverse` cancel each other out." ]
    }


stringJoinChecks : CheckInfo -> List (Error {})
stringJoinChecks checkInfo =
    case secondArg checkInfo of
        Just (Node _ (Expression.ListExpr [])) ->
            [ Rule.errorWithFix
                { message = "Using String.join on an empty list will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "\"\"" ]
            ]

        _ ->
            case Node.value checkInfo.firstArg of
                Expression.Literal "" ->
                    [ Rule.errorWithFix
                        { message = "Use String.concat instead"
                        , details = [ "Using String.join with an empty separator is the same as using String.concat." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end }
                            "String.concat"
                        ]
                    ]

                _ ->
                    []


stringLengthChecks : CheckInfo -> List (Error {})
stringLengthChecks { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.Literal str ->
            [ Rule.errorWithFix
                { message = "The length of the string is " ++ String.fromInt (String.length str)
                , details = [ "The length of the string can be determined by looking at the code." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange (String.fromInt (String.length str)) ]
            ]

        _ ->
            []


stringRepeatChecks : CheckInfo -> List (Error {})
stringRepeatChecks checkInfo =
    case secondArg checkInfo of
        Just (Node _ (Expression.Literal "")) ->
            [ Rule.errorWithFix
                { message = "Using String.repeat with an empty string will result in an empty string"
                , details = [ "You can replace this call by an empty string." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "\"\"" ]
            ]

        _ ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just intValue ->
                    if intValue == 1 then
                        [ Rule.errorWithFix
                            { message = "String.repeat 1 won't do anything"
                            , details = [ "Using String.repeat with 1 will result in the second argument." ]
                            }
                            checkInfo.fnRange
                            [ Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end } ]
                        ]

                    else if intValue < 1 then
                        [ Rule.errorWithFix
                            { message = "String.repeat will result in an empty string"
                            , details = [ "Using String.repeat with a number less than 1 will result in an empty string. You can replace this call by an empty string." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix emptyStringAsString checkInfo.parentRange (secondArg checkInfo))
                        ]

                    else
                        []

                _ ->
                    []


stringReplaceChecks : CheckInfo -> List (Error {})
stringReplaceChecks checkInfo =
    case secondArg checkInfo of
        Just secondArg_ ->
            case Normalize.compare checkInfo checkInfo.firstArg secondArg_ of
                Normalize.ConfirmedEquality ->
                    [ Rule.errorWithFix
                        { message = "The result of String.replace will be the original string"
                        , details = [ "The pattern to replace and the replacement are equal, therefore the result of the String.replace call will be the original string." ]
                        }
                        checkInfo.fnRange
                        (case thirdArg checkInfo of
                            Just thirdArg_ ->
                                [ Fix.removeRange
                                    { start = checkInfo.fnRange.start
                                    , end = (Node.range thirdArg_).start
                                    }
                                ]

                            Nothing ->
                                [ Fix.replaceRangeBy
                                    { start = checkInfo.fnRange.start
                                    , end = (Node.range secondArg_).end
                                    }
                                    "identity"
                                ]
                        )
                    ]

                _ ->
                    case ( Node.value checkInfo.firstArg, Node.value secondArg_, thirdArg checkInfo ) of
                        ( _, _, Just (Node thirdRange (Expression.Literal "")) ) ->
                            [ Rule.errorWithFix
                                { message = "The result of String.replace will be the empty string"
                                , details = [ "Replacing anything on an empty string results in an empty string." ]
                                }
                                checkInfo.fnRange
                                [ Fix.removeRange
                                    { start = checkInfo.fnRange.start
                                    , end = thirdRange.start
                                    }
                                ]
                            ]

                        ( Expression.Literal first, Expression.Literal second, Just (Node thirdRange (Expression.Literal third)) ) ->
                            if String.replace first second third == third then
                                [ Rule.errorWithFix
                                    { message = "The result of String.replace will be the original string"
                                    , details = [ "The replacement doesn't haven't any noticeable impact. You can remove the call to String.replace." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.removeRange
                                        { start = checkInfo.fnRange.start
                                        , end = thirdRange.start
                                        }
                                    ]
                                ]

                            else
                                []

                        _ ->
                            []

        Nothing ->
            []



-- MAYBE FUNCTIONS


maybeMapChecks : CheckInfo -> List (Error {})
maybeMapChecks checkInfo =
    firstThatReportsError
        [ \() -> collectionMapChecks maybeCollection checkInfo
        , \() ->
            case Match.maybeAndThen (getMaybeValues checkInfo.lookupTable) (secondArg checkInfo) of
                Determined (Just justRanges) ->
                    [ Rule.errorWithFix
                        { message = "Calling Maybe.map on a value that is Just"
                        , details = [ "The function can be called without Maybe.map." ]
                        }
                        checkInfo.fnRange
                        (if checkInfo.usingRightPizza then
                            [ Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                            , Fix.insertAt (Node.range checkInfo.firstArg).end " |> Just"
                            ]
                                ++ List.map Fix.removeRange justRanges

                         else
                            [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start } "Just ("
                            , Fix.insertAt checkInfo.parentRange.end ")"
                            ]
                                ++ List.map Fix.removeRange justRanges
                        )
                    ]

                _ ->
                    []
        ]
        ()


maybeMapCompositionChecks : CompositionCheckInfo -> List (Error {})
maybeMapCompositionChecks { lookupTable, fromLeftToRight, parentRange, left, right } =
    if fromLeftToRight then
        case ( AstHelpers.removeParens left, Node.value (AstHelpers.removeParens right) ) of
            ( Node justRange (Expression.FunctionOrValue _ "Just"), Expression.Application ((Node maybeMapRange (Expression.FunctionOrValue _ "map")) :: mapperFunction :: []) ) ->
                if
                    (ModuleNameLookupTable.moduleNameAt lookupTable justRange == Just [ "Maybe" ])
                        && (ModuleNameLookupTable.moduleNameAt lookupTable maybeMapRange == Just [ "Maybe" ])
                then
                    [ Rule.errorWithFix
                        { message = "Calling Maybe.map on a value that is Just"
                        , details = [ "The function can be called without Maybe.map." ]
                        }
                        maybeMapRange
                        [ Fix.removeRange { start = parentRange.start, end = (Node.range mapperFunction).start }
                        , Fix.insertAt (Node.range mapperFunction).end " >> Just"
                        ]
                    ]

                else
                    []

            _ ->
                []

    else
        case ( Node.value (AstHelpers.removeParens left), AstHelpers.removeParens right ) of
            ( Expression.Application ((Node maybeMapRange (Expression.FunctionOrValue _ "map")) :: mapperFunction :: []), Node justRange (Expression.FunctionOrValue _ "Just") ) ->
                if ModuleNameLookupTable.moduleNameAt lookupTable justRange == Just [ "Maybe" ] && ModuleNameLookupTable.moduleNameAt lookupTable maybeMapRange == Just [ "Maybe" ] then
                    [ Rule.errorWithFix
                        { message = "Calling Maybe.map on a value that is Just"
                        , details = [ "The function can be called without Maybe.map." ]
                        }
                        maybeMapRange
                        [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range mapperFunction).start } "Just << "
                        , Fix.removeRange { start = (Node.range mapperFunction).end, end = parentRange.end }
                        ]
                    ]

                else
                    []

            _ ->
                []


resultMapCompositionChecks : CompositionCheckInfo -> List (Error {})
resultMapCompositionChecks { lookupTable, fromLeftToRight, parentRange, left, right } =
    if fromLeftToRight then
        case ( AstHelpers.removeParens left, Node.value (AstHelpers.removeParens right) ) of
            ( Node justRange (Expression.FunctionOrValue _ "Ok"), Expression.Application ((Node resultMapRange (Expression.FunctionOrValue _ "map")) :: mapperFunction :: []) ) ->
                if
                    (ModuleNameLookupTable.moduleNameAt lookupTable justRange == Just [ "Result" ])
                        && (ModuleNameLookupTable.moduleNameAt lookupTable resultMapRange == Just [ "Result" ])
                then
                    [ Rule.errorWithFix
                        { message = "Calling Result.map on a value that is Ok"
                        , details = [ "The function can be called without Result.map." ]
                        }
                        resultMapRange
                        [ Fix.removeRange { start = parentRange.start, end = (Node.range mapperFunction).start }
                        , Fix.insertAt (Node.range mapperFunction).end " >> Ok"
                        ]
                    ]

                else
                    []

            _ ->
                []

    else
        case ( Node.value (AstHelpers.removeParens left), AstHelpers.removeParens right ) of
            ( Expression.Application ((Node resultMapRange (Expression.FunctionOrValue _ "map")) :: mapperFunction :: []), Node justRange (Expression.FunctionOrValue _ "Ok") ) ->
                if ModuleNameLookupTable.moduleNameAt lookupTable justRange == Just [ "Result" ] && ModuleNameLookupTable.moduleNameAt lookupTable resultMapRange == Just [ "Result" ] then
                    [ Rule.errorWithFix
                        { message = "Calling Result.map on a value that is Ok"
                        , details = [ "The function can be called without Result.map." ]
                        }
                        resultMapRange
                        [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range mapperFunction).start } "Ok << "
                        , Fix.removeRange { start = (Node.range mapperFunction).end, end = parentRange.end }
                        ]
                    ]

                else
                    []

            _ ->
                []



-- RESULT FUNCTIONS


resultMapChecks : CheckInfo -> List (Error {})
resultMapChecks checkInfo =
    firstThatReportsError
        [ \() -> collectionMapChecks resultCollection checkInfo
        , \() ->
            case Maybe.andThen (getResultValues checkInfo.lookupTable) (secondArg checkInfo) of
                Just (Ok okRanges) ->
                    [ Rule.errorWithFix
                        { message = "Calling Result.map on a value that is Ok"
                        , details = [ "The function can be called without Result.map." ]
                        }
                        checkInfo.fnRange
                        (if checkInfo.usingRightPizza then
                            [ Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                            , Fix.insertAt (Node.range checkInfo.firstArg).end " |> Ok"
                            ]
                                ++ List.map Fix.removeRange okRanges

                         else
                            [ Fix.replaceRangeBy { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start } "Ok ("
                            , Fix.insertAt checkInfo.parentRange.end ")"
                            ]
                                ++ List.map Fix.removeRange okRanges
                        )
                    ]

                _ ->
                    []
        ]
        ()



-- LIST FUNCTIONS


listConcatChecks : CheckInfo -> List (Error {})
listConcatChecks ({ lookupTable, parentRange, fnRange, firstArg } as checkInfo) =
    case Node.value firstArg of
        Expression.ListExpr list ->
            case list of
                [ Node elementRange _ ] ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary use of List.concat on a list with 1 element"
                        , details = [ "The value of the operation will be the element itself. You should replace this expression by that." ]
                        }
                        parentRange
                        [ Fix.removeRange { start = parentRange.start, end = elementRange.start }
                        , Fix.removeRange { start = elementRange.end, end = parentRange.end }
                        ]
                    ]

                (firstListElement :: restOfListElements) as args ->
                    if List.all isListLiteral list then
                        [ Rule.errorWithFix
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            }
                            parentRange
                            (Fix.removeRange fnRange
                                :: List.concatMap removeBoundariesFix args
                            )
                        ]

                    else
                        case findConsecutiveListLiterals firstListElement restOfListElements of
                            [] ->
                                []

                            fixes ->
                                [ Rule.errorWithFix
                                    { message = "Consecutive literal lists should be merged"
                                    , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                                    }
                                    fnRange
                                    fixes
                                ]

                _ ->
                    []

        _ ->
            case getSpecificFunctionCall ( [ "List" ], "map" ) lookupTable firstArg of
                Just match ->
                    [ Rule.errorWithFix
                        { message = "List.map and List.concat can be combined using List.concatMap"
                        , details = [ "List.concatMap is meant for this exact purpose and will also be faster." ]
                        }
                        fnRange
                        [ removeFunctionFromFunctionCall checkInfo
                        , Fix.replaceRangeBy match.fnRange "List.concatMap"
                        ]
                    ]

                Nothing ->
                    []


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


listConcatMapChecks : CheckInfo -> List (Error {})
listConcatMapChecks checkInfo =
    if isIdentity checkInfo.lookupTable checkInfo.firstArg then
        [ Rule.errorWithFix
            { message = "Using List.concatMap with an identity function is the same as using List.concat"
            , details = [ "You can replace this call by List.concat." ]
            }
            checkInfo.fnRange
            [ Fix.replaceRangeBy
                { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).end }
                "List.concat"
            ]
        ]

    else if isAlwaysEmptyList checkInfo.lookupTable checkInfo.firstArg then
        [ Rule.errorWithFix
            { message = "List.concatMap will result in on an empty list"
            , details = [ "You can replace this call by an empty list." ]
            }
            checkInfo.fnRange
            (replaceByEmptyFix "[]" checkInfo.parentRange (secondArg checkInfo))
        ]

    else
        case replaceSingleElementListBySingleValue_RENAME checkInfo.lookupTable checkInfo.fnRange checkInfo.firstArg of
            Just errors ->
                errors

            Nothing ->
                case secondArg checkInfo of
                    Just (Node listRange (Expression.ListExpr [ listElement ])) ->
                        [ Rule.errorWithFix
                            { message = "Using List.concatMap on an element with a single item is the same as calling the function directly on that lone element."
                            , details = [ "You can replace this call by a call to the function directly." ]
                            }
                            checkInfo.fnRange
                            (Fix.removeRange checkInfo.fnRange
                                :: replaceBySubExpressionFix listRange listElement
                            )
                        ]

                    _ ->
                        []


concatAndMapCompositionCheck : CompositionCheckInfo -> List (Error {})
concatAndMapCompositionCheck { lookupTable, fromLeftToRight, left, right } =
    if fromLeftToRight then
        if isSpecificValueOrFunction [ "List" ] "concat" lookupTable right then
            case Node.value (AstHelpers.removeParens left) of
                Expression.Application [ leftFunction, _ ] ->
                    if isSpecificValueOrFunction [ "List" ] "map" lookupTable leftFunction then
                        [ Rule.errorWithFix
                            { message = "List.map and List.concat can be combined using List.concatMap"
                            , details = [ "List.concatMap is meant for this exact purpose and will also be faster." ]
                            }
                            (Node.range right)
                            [ Fix.removeRange { start = (Node.range left).end, end = (Node.range right).end }
                            , Fix.replaceRangeBy (Node.range leftFunction) "List.concatMap"
                            ]
                        ]

                    else
                        []

                _ ->
                    []

        else
            []

    else if isSpecificValueOrFunction [ "List" ] "concat" lookupTable left then
        case Node.value (AstHelpers.removeParens right) of
            Expression.Application [ rightFunction, _ ] ->
                if isSpecificValueOrFunction [ "List" ] "map" lookupTable rightFunction then
                    [ Rule.errorWithFix
                        { message = "List.map and List.concat can be combined using List.concatMap"
                        , details = [ "List.concatMap is meant for this exact purpose and will also be faster." ]
                        }
                        (Node.range left)
                        [ Fix.removeRange { start = (Node.range left).start, end = (Node.range right).start }
                        , Fix.replaceRangeBy (Node.range rightFunction) "List.concatMap"
                        ]
                    ]

                else
                    []

            _ ->
                []

    else
        []


listIndexedMapChecks : CheckInfo -> List (Error {})
listIndexedMapChecks { lookupTable, fnRange, firstArg } =
    case AstHelpers.removeParens firstArg of
        Node lambdaRange (Expression.LambdaExpression { args, expression }) ->
            case Maybe.map AstHelpers.removeParensFromPattern (List.head args) of
                Just (Node patternRange Pattern.AllPattern) ->
                    let
                        rangeToRemove : Range
                        rangeToRemove =
                            case args of
                                [] ->
                                    Range.emptyRange

                                [ _ ] ->
                                    -- Only one argument, remove the entire lambda except the expression
                                    { start = lambdaRange.start, end = (Node.range expression).start }

                                first :: second :: _ ->
                                    { start = (Node.range first).start, end = (Node.range second).start }
                    in
                    [ Rule.errorWithFix
                        { message = "Use List.map instead"
                        , details = [ "Using List.indexedMap while ignoring the first argument is the same thing as calling List.map." ]
                        }
                        patternRange
                        [ Fix.replaceRangeBy fnRange "List.map"
                        , Fix.removeRange rangeToRemove
                        ]
                    ]

                _ ->
                    []

        _ ->
            case getAlwaysArgument lookupTable firstArg of
                Just { alwaysRange, rangeToRemove } ->
                    [ Rule.errorWithFix
                        { message = "Use List.map instead"
                        , details = [ "Using List.indexedMap while ignoring the first argument is the same thing as calling List.map." ]
                        }
                        alwaysRange
                        [ Fix.replaceRangeBy fnRange "List.map"
                        , Fix.removeRange rangeToRemove
                        ]
                    ]

                Nothing ->
                    []


listAppendEmptyErrorInfo : { message : String, details : List String }
listAppendEmptyErrorInfo =
    { message = "Appending [] doesn't have any effect"
    , details = [ "You can remove the List.append function and the []." ]
    }


listAppendChecks : CheckInfo -> List (Error {})
listAppendChecks checkInfo =
    case ( checkInfo.firstArg, secondArg checkInfo ) of
        ( Node _ (Expression.ListExpr []), secondListArgument ) ->
            case secondListArgument of
                Nothing ->
                    [ Rule.errorWithFix
                        { listAppendEmptyErrorInfo
                            | details = [ "You can replace this call by identity." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange "identity" ]
                    ]

                Just (Node secondListRange _) ->
                    [ Rule.errorWithFix
                        listAppendEmptyErrorInfo
                        checkInfo.fnRange
                        (keepOnlyFix { parentRange = checkInfo.parentRange, keep = secondListRange })
                    ]

        ( Node firstListRange _, Just (Node _ (Expression.ListExpr [])) ) ->
            [ Rule.errorWithFix
                listAppendEmptyErrorInfo
                checkInfo.fnRange
                (keepOnlyFix { parentRange = checkInfo.parentRange, keep = firstListRange })
            ]

        ( Node firstListRange (Expression.ListExpr (_ :: _)), Just (Node secondListRange (Expression.ListExpr (_ :: _))) ) ->
            [ Rule.errorWithFix
                { message = "Appending literal lists could be simplified to be a single List"
                , details = [ "Try moving all the elements into a single list." ]
                }
                checkInfo.fnRange
                (Fix.replaceRangeBy
                    { start = { row = firstListRange.end.row, column = firstListRange.end.column - 1 }
                    , end = { row = secondListRange.start.row, column = secondListRange.start.column + 1 }
                    }
                    ","
                    :: keepOnlyFix
                        { parentRange = checkInfo.parentRange
                        , keep = Range.combine [ firstListRange, secondListRange ]
                        }
                )
            ]

        _ ->
            []


listHeadExistsError : { message : String, details : List String }
listHeadExistsError =
    { message = "Using List.head on a list with a first element will result in Just that element"
    , details = [ "You can replace this call by Just the first list element." ]
    }


listHeadChecks : CheckInfo -> List (Error {})
listHeadChecks checkInfo =
    let
        justFirstElementError : Range -> List (Error {})
        justFirstElementError keep =
            [ Rule.errorWithFix
                listHeadExistsError
                checkInfo.fnRange
                (keepOnlyFix { parentRange = Node.range listArg, keep = keep }
                    ++ [ Fix.replaceRangeBy checkInfo.fnRange "Just" ]
                )
            ]

        listArg : Node Expression
        listArg =
            AstHelpers.removeParens checkInfo.firstArg
    in
    case Node.value listArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using List.head on an empty list will result in Nothing"
                , details = [ "You can replace this call by Nothing." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "Nothing" ]
            ]

        Expression.ListExpr ((Node headRange head) :: _) ->
            if needsParens head then
                [ Rule.errorWithFix
                    listHeadExistsError
                    checkInfo.fnRange
                    (keepOnlyFix { parentRange = Node.range listArg, keep = headRange }
                        ++ [ Fix.insertAt headRange.start "("
                           , Fix.insertAt headRange.end ")"
                           , Fix.replaceRangeBy checkInfo.fnRange "Just"
                           ]
                    )
                ]

            else
                justFirstElementError headRange

        Expression.OperatorApplication "::" _ head _ ->
            justFirstElementError (Node.range head)

        _ ->
            case getListSingletonCall checkInfo.lookupTable listArg of
                Just single ->
                    justFirstElementError (Node.range single.element)

                Nothing ->
                    []


listTailExistsError : { message : String, details : List String }
listTailExistsError =
    { message = "Using List.tail on a list with some elements will result in Just the elements after the first"
    , details = [ "You can replace this call by Just the list elements after the first." ]
    }


listEmptyTailExistsError : { message : String, details : List String }
listEmptyTailExistsError =
    { message = "Using List.tail on a list with a single element will result in Just the empty list"
    , details = [ "You can replace this call by Just the empty list." ]
    }


listTailChecks : CheckInfo -> List (Error {})
listTailChecks checkInfo =
    let
        listArg : Node Expression
        listArg =
            AstHelpers.removeParens checkInfo.firstArg

        listArgRange : Range
        listArgRange =
            Node.range listArg
    in
    case Node.value listArg of
        Expression.ListExpr listLiteral ->
            case listLiteral of
                [] ->
                    [ Rule.errorWithFix
                        { message = "Using List.tail on an empty list will result in Nothing"
                        , details = [ "You can replace this call by Nothing." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange "Nothing" ]
                    ]

                _ :: [] ->
                    [ Rule.errorWithFix
                        listEmptyTailExistsError
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy listArgRange listCollection.emptyAsString
                        , Fix.replaceRangeBy checkInfo.fnRange "Just"
                        ]
                    ]

                (Node headRange _) :: (Node tailFirstRange _) :: _ ->
                    [ Rule.errorWithFix
                        listTailExistsError
                        checkInfo.fnRange
                        [ Fix.removeRange { start = headRange.start, end = tailFirstRange.start }
                        , Fix.replaceRangeBy checkInfo.fnRange "Just"
                        ]
                    ]

        Expression.OperatorApplication "::" _ _ tail ->
            [ Rule.errorWithFix
                listTailExistsError
                checkInfo.fnRange
                (keepOnlyFix { parentRange = listArgRange, keep = Node.range tail }
                    ++ [ Fix.replaceRangeBy checkInfo.fnRange "Just" ]
                )
            ]

        _ ->
            case getListSingletonCall checkInfo.lookupTable listArg of
                Just _ ->
                    [ Rule.errorWithFix
                        listEmptyTailExistsError
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy (Node.range checkInfo.firstArg) listCollection.emptyAsString
                        , Fix.replaceRangeBy checkInfo.fnRange "Just"
                        ]
                    ]

                Nothing ->
                    []


listMemberChecks : CheckInfo -> List (Error {})
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

                listMemberExistsError : List (Error {})
                listMemberExistsError =
                    [ Rule.errorWithFix
                        { message = "Using List.member on a list which contains the given element will result in True"
                        , details = [ "You can replace this call by True." ]
                        }
                        checkInfo.fnRange
                        [ Fix.replaceRangeBy checkInfo.parentRange "True" ]
                    ]

                singleNonNormalizedEqualElementError : Node Expression -> List (Error {})
                singleNonNormalizedEqualElementError element =
                    let
                        elementRange : Range
                        elementRange =
                            Node.range element
                    in
                    [ Rule.errorWithFix
                        { message = "Using List.member on an list with a single element is equivalent to directly checking for equality"
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
                    ]
            in
            case Node.value (AstHelpers.removeParens listArg) of
                Expression.ListExpr listLiteral ->
                    case listLiteral of
                        [] ->
                            [ Rule.errorWithFix
                                { message = "Using List.member on an empty list will result in False"
                                , details = [ "You can replace this call by False." ]
                                }
                                checkInfo.fnRange
                                [ Fix.replaceRangeBy checkInfo.parentRange "False" ]
                            ]

                        head :: tail ->
                            if List.any isNeedle (head :: tail) then
                                listMemberExistsError

                            else
                                case tail of
                                    [] ->
                                        singleNonNormalizedEqualElementError head

                                    _ :: _ ->
                                        []

                Expression.OperatorApplication "::" _ head tail ->
                    if List.any isNeedle (head :: getBeforeLastCons tail) then
                        listMemberExistsError

                    else
                        []

                _ ->
                    case getListSingletonCall checkInfo.lookupTable listArg of
                        Just single ->
                            if isNeedle single.element then
                                listMemberExistsError

                            else
                                singleNonNormalizedEqualElementError single.element

                        Nothing ->
                            []

        Nothing ->
            []


getBeforeLastCons : Node Expression -> List (Node Expression)
getBeforeLastCons expressionNode =
    case Node.value expressionNode of
        Expression.OperatorApplication "::" _ beforeCons afterCons ->
            beforeCons :: getBeforeLastCons afterCons

        _ ->
            []


listSumChecks : CheckInfo -> List (Error {})
listSumChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using List.sum on [] will result in 0"
                , details = [ "You can replace this call by 0." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "0" ]
            ]

        Expression.ListExpr ((Node elementRange _) :: []) ->
            [ Rule.errorWithFix
                { message = "Summing a list with a single element will result in the element itself"
                , details = [ "You can replace this call by the single element itself." ]
                }
                checkInfo.fnRange
                (keepOnlyFix
                    { parentRange = checkInfo.parentRange
                    , keep = elementRange
                    }
                )
            ]

        _ ->
            []


listProductChecks : CheckInfo -> List (Error {})
listProductChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using List.product on [] will result in 1"
                , details = [ "You can replace this call by 1." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "1" ]
            ]

        Expression.ListExpr ((Node elementRange _) :: []) ->
            [ Rule.errorWithFix
                { message = "List.product on a list with a single element will result in the element itself"
                , details = [ "You can replace this call by the single element itself." ]
                }
                checkInfo.fnRange
                (keepOnlyFix
                    { parentRange = checkInfo.parentRange
                    , keep = elementRange
                    }
                )
            ]

        _ ->
            []


listMinimumChecks : CheckInfo -> List (Error {})
listMinimumChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using List.minimum on [] will result in Nothing"
                , details = [ "You can replace this call by Nothing." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "Nothing" ]
            ]

        Expression.ListExpr ((Node elementRange _) :: []) ->
            [ Rule.errorWithFix
                { message = "List.minimum on a list with a single element will result in Just the element itself"
                , details = [ "You can replace this call by Just the single element itself." ]
                }
                checkInfo.fnRange
                (keepOnlyFix
                    { parentRange = checkInfo.parentRange
                    , keep = elementRange
                    }
                    ++ [ Fix.insertAt checkInfo.parentRange.start "Just " ]
                )
            ]

        _ ->
            []


listMaximumChecks : CheckInfo -> List (Error {})
listMaximumChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using List.maximum on [] will result in Nothing"
                , details = [ "You can replace this call by Nothing." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "Nothing" ]
            ]

        Expression.ListExpr ((Node elementRange _) :: []) ->
            [ Rule.errorWithFix
                { message = "List.maximum on a list with a single element will result in Just the element itself"
                , details = [ "You can replace this call by Just the single element itself." ]
                }
                checkInfo.fnRange
                (keepOnlyFix
                    { parentRange = checkInfo.parentRange
                    , keep = elementRange
                    }
                    ++ [ Fix.insertAt checkInfo.parentRange.start "Just " ]
                )
            ]

        _ ->
            []


listFoldlChecks : CheckInfo -> List (Error {})
listFoldlChecks checkInfo =
    listFoldAnyDirectionChecks "foldl" checkInfo


listFoldrChecks : CheckInfo -> List (Error {})
listFoldrChecks checkInfo =
    listFoldAnyDirectionChecks "foldr" checkInfo


listFoldAnyDirectionChecks : String -> CheckInfo -> List (Error {})
listFoldAnyDirectionChecks foldOperationName checkInfo =
    case secondArg checkInfo of
        Nothing ->
            []

        Just initialArgument ->
            let
                listArg : Maybe (Node Expression)
                listArg =
                    thirdArg checkInfo
            in
            case Maybe.andThen (getSpecificFunctionCall ( [ "Set" ], "toList" ) checkInfo.lookupTable) listArg of
                Just setToListCall ->
                    [ Rule.errorWithFix
                        { message = "To fold a set, you don't need to convert to a List"
                        , details = [ "Using Set." ++ foldOperationName ++ " directly is meant for this exact purpose and will also be faster." ]
                        }
                        checkInfo.fnRange
                        (keepOnlyFix { parentRange = setToListCall.nodeRange, keep = Node.range setToListCall.firstArg }
                            ++ [ Fix.replaceRangeBy checkInfo.fnRange ("Set." ++ foldOperationName) ]
                        )
                    ]

                Nothing ->
                    let
                        initialNumber : Maybe Float
                        initialNumber =
                            getUncomputedNumberValue initialArgument

                        numberBinaryOperationChecks : { identity : Int, two : String, list : String } -> List (Error {})
                        numberBinaryOperationChecks operation =
                            let
                                fixWith : List Fix -> List (Error {})
                                fixWith fixes =
                                    [ Rule.errorWithFix
                                        { message = "Use List." ++ operation.list ++ " instead"
                                        , details =
                                            [ "Using List." ++ foldOperationName ++ " (" ++ operation.two ++ ") " ++ String.fromInt operation.identity ++ " is the same as using List." ++ operation.list ++ "." ]
                                        }
                                        checkInfo.fnRange
                                        fixes
                                    ]
                            in
                            if initialNumber == Just (Basics.toFloat operation.identity) then
                                fixWith
                                    [ Fix.replaceRangeBy
                                        { start = checkInfo.parentRange.start
                                        , end = (Node.range initialArgument).end
                                        }
                                        ("List." ++ operation.list)
                                    ]

                            else
                                case listArg of
                                    Nothing ->
                                        []

                                    Just _ ->
                                        if checkInfo.usingRightPizza then
                                            -- list |> fold op initial --> ((list |> List.op) op initial)
                                            fixWith
                                                [ Fix.insertAt (Node.range initialArgument).end ")"
                                                , Fix.insertAt (Node.range initialArgument).start (operation.two ++ " ")
                                                , Fix.replaceRangeBy
                                                    { start = checkInfo.fnRange.start
                                                    , end = (Node.range checkInfo.firstArg).end
                                                    }
                                                    ("List." ++ operation.list ++ ")")
                                                , Fix.insertAt checkInfo.parentRange.start "(("
                                                ]

                                        else
                                            -- <| or application
                                            -- fold op initial list --> (initial op (List.op list))
                                            fixWith
                                                [ Fix.insertAt checkInfo.parentRange.end ")"
                                                , Fix.insertAt (Node.range initialArgument).end
                                                    (" " ++ operation.two ++ " (List." ++ operation.list)
                                                , Fix.removeRange
                                                    { start = checkInfo.parentRange.start
                                                    , end = (Node.range initialArgument).start
                                                    }
                                                ]

                        boolBinaryOperationChecks : { two : String, list : String, determining : Bool } -> Bool -> List (Rule.Error {})
                        boolBinaryOperationChecks operation initialIsDetermining =
                            if initialIsDetermining == operation.determining then
                                [ Rule.errorWithFix
                                    { message = "The call to List." ++ foldOperationName ++ " will result in " ++ boolToString operation.determining
                                    , details = [ "You can replace this call by " ++ boolToString operation.determining ++ "." ]
                                    }
                                    checkInfo.fnRange
                                    (replaceByEmptyFix (boolToString operation.determining) checkInfo.parentRange (thirdArg checkInfo))
                                ]

                            else
                                -- initialIsTrue /= operation.determining
                                [ Rule.errorWithFix
                                    { message = "Use List." ++ operation.list ++ " identity instead"
                                    , details = [ "Using List." ++ foldOperationName ++ " (" ++ operation.two ++ ") " ++ boolToString (not operation.determining) ++ " is the same as using List." ++ operation.list ++ " identity." ]
                                    }
                                    checkInfo.fnRange
                                    [ Fix.replaceRangeBy
                                        { start = checkInfo.parentRange.start, end = (Node.range initialArgument).end }
                                        ("List." ++ operation.list ++ " identity")
                                    ]
                                ]
                    in
                    if Maybe.withDefault False (Maybe.map isEmptyList listArg) then
                        [ Rule.errorWithFix
                            { message = "The call to List." ++ foldOperationName ++ " will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            }
                            checkInfo.fnRange
                            [ Fix.removeRange
                                { start = checkInfo.parentRange.start, end = (Node.range initialArgument).start }
                            , Fix.removeRange
                                { start = (Node.range initialArgument).end, end = checkInfo.parentRange.end }
                            ]
                        ]

                    else if Maybe.withDefault False (Maybe.map (isIdentity checkInfo.lookupTable) (getAlwaysResult checkInfo checkInfo.firstArg)) then
                        [ Rule.errorWithFix
                            { message = "The call to List." ++ foldOperationName ++ " will result in the initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator." ]
                            }
                            checkInfo.fnRange
                            (case listArg of
                                Nothing ->
                                    [ Fix.replaceRangeBy
                                        { start = checkInfo.parentRange.start
                                        , end = (Node.range checkInfo.firstArg).end
                                        }
                                        "always"
                                    ]

                                Just _ ->
                                    [ Fix.removeRange
                                        { start = (Node.range initialArgument).end
                                        , end = checkInfo.parentRange.end
                                        }
                                    , Fix.removeRange
                                        { start = checkInfo.parentRange.start
                                        , end = (Node.range initialArgument).start
                                        }
                                    ]
                            )
                        ]

                    else if isBinaryOperation "*" checkInfo checkInfo.firstArg then
                        numberBinaryOperationChecks { two = "*", list = "product", identity = 1 }

                    else if isBinaryOperation "+" checkInfo checkInfo.firstArg then
                        numberBinaryOperationChecks { two = "+", list = "sum", identity = 0 }

                    else
                        case Evaluate.getBoolean checkInfo initialArgument of
                            Undetermined ->
                                []

                            Determined initialBool ->
                                if isBinaryOperation "&&" checkInfo checkInfo.firstArg then
                                    boolBinaryOperationChecks { two = "&&", list = "all", determining = False } initialBool

                                else if isBinaryOperation "||" checkInfo checkInfo.firstArg then
                                    boolBinaryOperationChecks { two = "||", list = "any", determining = True } initialBool

                                else
                                    []


foldAndSetToListCompositionChecks : String -> CompositionCheckInfo -> List (Error {})
foldAndSetToListCompositionChecks foldOperationName checkInfo =
    let
        ( earlier, later ) =
            if checkInfo.fromLeftToRight then
                ( checkInfo.left, checkInfo.right )

            else
                ( checkInfo.right, checkInfo.left )
    in
    case getSpecificFunctionCall ( [ "List" ], foldOperationName ) checkInfo.lookupTable later of
        Just listFoldCall ->
            case listFoldCall.argsAfterFirst of
                -- initial and reduce arguments are present
                _ :: [] ->
                    if isSpecificValueOrFunction [ "Set" ] "toList" checkInfo.lookupTable earlier then
                        [ Rule.errorWithFix
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set." ++ foldOperationName ++ " directly is meant for this exact purpose and will also be faster." ]
                            }
                            listFoldCall.fnRange
                            (keepOnlyFix { parentRange = checkInfo.parentRange, keep = Node.range later }
                                ++ [ Fix.replaceRangeBy listFoldCall.fnRange ("Set." ++ foldOperationName) ]
                            )
                        ]

                    else
                        []

                -- composition onto fully constructed value (compile-time error)
                _ :: _ :: _ ->
                    []

                -- reduce argument is missing
                [] ->
                    []

        _ ->
            []


listAllChecks : CheckInfo -> List (Error {})
listAllChecks checkInfo =
    case Maybe.map (AstHelpers.removeParens >> Node.value) (secondArg checkInfo) of
        Just (Expression.ListExpr []) ->
            [ Rule.errorWithFix
                { message = "The call to List.all will result in True"
                , details = [ "You can replace this call by True." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "True" ]
            ]

        _ ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined True ->
                    [ Rule.errorWithFix
                        { message = "The call to List.all will result in True"
                        , details = [ "You can replace this call by True." ]
                        }
                        checkInfo.fnRange
                        (replaceByBoolFix checkInfo.parentRange (secondArg checkInfo) True)
                    ]

                _ ->
                    []


listAnyChecks : CheckInfo -> List (Error {})
listAnyChecks checkInfo =
    case Maybe.map (AstHelpers.removeParens >> Node.value) (secondArg checkInfo) of
        Just (Expression.ListExpr []) ->
            [ Rule.errorWithFix
                { message = "The call to List.any will result in False"
                , details = [ "You can replace this call by False." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "False" ]
            ]

        listArg ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined False ->
                    [ Rule.errorWithFix
                        { message = "The call to List.any will result in False"
                        , details = [ "You can replace this call by False." ]
                        }
                        checkInfo.fnRange
                        (replaceByBoolFix checkInfo.parentRange listArg False)
                    ]

                _ ->
                    []


listFilterMapChecks : CheckInfo -> List (Error {})
listFilterMapChecks ({ lookupTable, parentRange, fnRange, firstArg } as checkInfo) =
    case isAlwaysMaybe lookupTable firstArg of
        Determined (Just { ranges, throughLambdaFunction }) ->
            if throughLambdaFunction then
                [ Rule.errorWithFix
                    { message = "Using List.filterMap with a function that will always return Just is the same as using List.map"
                    , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                    }
                    fnRange
                    (Fix.replaceRangeBy fnRange "List.map"
                        :: List.map Fix.removeRange ranges
                    )
                ]

            else
                [ Rule.errorWithFix
                    { message = "Using List.filterMap with a function that will always return Just is the same as not using List.filterMap"
                    , details = [ "You can remove this call and replace it by the list itself." ]
                    }
                    fnRange
                    (noopFix checkInfo)
                ]

        Determined Nothing ->
            [ Rule.errorWithFix
                { message = "Using List.filterMap with a function that will always return Nothing will result in an empty list"
                , details = [ "You can remove this call and replace it by an empty list." ]
                }
                fnRange
                (replaceByEmptyFix "[]" parentRange (secondArg checkInfo))
            ]

        Undetermined ->
            if isIdentity lookupTable firstArg then
                case Maybe.andThen (getSpecificFunctionCall ( [ "List" ], "map" ) lookupTable) (secondArg checkInfo) of
                    Just listArg ->
                        [ Rule.errorWithFix
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            }
                            { start = fnRange.start, end = (Node.range firstArg).end }
                            [ removeFunctionAndFirstArg checkInfo listArg.nodeRange
                            , Fix.replaceRangeBy listArg.fnRange "List.filterMap"
                            ]
                        ]

                    Nothing ->
                        case secondArg checkInfo of
                            Just (Node listRange (Expression.ListExpr list)) ->
                                case collectJusts lookupTable list [] of
                                    Just justRanges ->
                                        [ Rule.errorWithFix
                                            { message = "Unnecessary use of List.filterMap identity"
                                            , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                                            }
                                            { start = fnRange.start, end = (Node.range firstArg).end }
                                            ((if checkInfo.usingRightPizza then
                                                Fix.removeRange { start = listRange.end, end = (Node.range firstArg).end }

                                              else
                                                Fix.removeRange { start = fnRange.start, end = listRange.start }
                                             )
                                                :: List.map Fix.removeRange justRanges
                                            )
                                        ]

                                    Nothing ->
                                        []

                            _ ->
                                []

            else
                []


collectJusts : ModuleNameLookupTable -> List (Node Expression) -> List Range -> Maybe (List Range)
collectJusts lookupTable list acc =
    case list of
        [] ->
            Just acc

        element :: restOfList ->
            case Node.value element of
                Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Just")) :: justArg :: []) ->
                    case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                        Just [ "Maybe" ] ->
                            collectJusts lookupTable restOfList ({ start = justRange.start, end = (Node.range justArg).start } :: acc)

                        _ ->
                            Nothing

                _ ->
                    Nothing


filterAndMapCompositionCheck : CompositionCheckInfo -> List (Error {})
filterAndMapCompositionCheck { lookupTable, fromLeftToRight, left, right } =
    if fromLeftToRight then
        case Node.value (AstHelpers.removeParens right) of
            Expression.Application [ rightFunction, arg ] ->
                if isSpecificValueOrFunction [ "List" ] "filterMap" lookupTable rightFunction && isIdentity lookupTable arg then
                    case Node.value (AstHelpers.removeParens left) of
                        Expression.Application [ leftFunction, _ ] ->
                            if isSpecificValueOrFunction [ "List" ] "map" lookupTable leftFunction then
                                [ Rule.errorWithFix
                                    { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                                    , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                                    }
                                    (Node.range right)
                                    [ Fix.removeRange { start = (Node.range left).end, end = (Node.range right).end }
                                    , Fix.replaceRangeBy (Node.range leftFunction) "List.filterMap"
                                    ]
                                ]

                            else
                                []

                        _ ->
                            []

                else
                    []

            _ ->
                []

    else
        case Node.value (AstHelpers.removeParens left) of
            Expression.Application [ leftFunction, arg ] ->
                if isSpecificValueOrFunction [ "List" ] "filterMap" lookupTable leftFunction && isIdentity lookupTable arg then
                    case Node.value (AstHelpers.removeParens right) of
                        Expression.Application [ rightFunction, _ ] ->
                            if isSpecificValueOrFunction [ "List" ] "map" lookupTable rightFunction then
                                [ Rule.errorWithFix
                                    { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                                    , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                                    }
                                    (Node.range left)
                                    [ Fix.removeRange { start = (Node.range left).start, end = (Node.range right).start }
                                    , Fix.replaceRangeBy (Node.range rightFunction) "List.filterMap"
                                    ]
                                ]

                            else
                                []

                        _ ->
                            []

                else
                    []

            _ ->
                []


listRangeChecks : CheckInfo -> List (Error {})
listRangeChecks checkInfo =
    case Maybe.andThen (Evaluate.getInt checkInfo) (secondArg checkInfo) of
        Just second ->
            case Evaluate.getInt checkInfo checkInfo.firstArg of
                Just first ->
                    if first > second then
                        [ Rule.errorWithFix
                            { message = "The call to List.range will result in []"
                            , details = [ "The second argument to List.range is bigger than the first one, therefore you can replace this list by an empty list." ]
                            }
                            checkInfo.fnRange
                            (replaceByEmptyFix "[]" checkInfo.parentRange (Just second))
                        ]

                    else
                        []

                Nothing ->
                    []

        Nothing ->
            []


listRepeatChecks : CheckInfo -> List (Error {})
listRepeatChecks checkInfo =
    case Evaluate.getInt checkInfo checkInfo.firstArg of
        Just intValue ->
            if intValue < 1 then
                [ Rule.errorWithFix
                    { message = "List.repeat will result in an empty list"
                    , details = [ "Using List.repeat with a number less than 1 will result in an empty list. You can replace this call by an empty list." ]
                    }
                    checkInfo.fnRange
                    (replaceByEmptyFix "[]" checkInfo.parentRange (secondArg checkInfo))
                ]

            else
                []

        _ ->
            []


listReverseChecks : CheckInfo -> List (Error {})
listReverseChecks ({ parentRange, fnRange, firstArg } as checkInfo) =
    case Node.value (AstHelpers.removeParens firstArg) of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using List.reverse on [] will result in []"
                , details = [ "You can replace this call by []." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "[]" ]
            ]

        _ ->
            removeAlongWithOtherFunctionCheck
                reverseReverseCompositionErrorMessage
                (getSpecificFunction ( [ "List" ], "reverse" ))
                checkInfo


listSortChecks : CheckInfo -> List (Error {})
listSortChecks checkInfo =
    case checkInfo.firstArg of
        Node _ (Expression.ListExpr []) ->
            [ Rule.errorWithFix
                { message = "Using List.sort on [] will result in []"
                , details = [ "You can replace this call by []." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
            ]

        Node singletonListRange (Expression.ListExpr (_ :: [])) ->
            [ Rule.errorWithFix
                { message = "Sorting a list with a single element will result in the list itself"
                , details = [ "You can replace this call by the list itself." ]
                }
                checkInfo.fnRange
                (keepOnlyFix
                    { parentRange = checkInfo.parentRange
                    , keep = singletonListRange
                    }
                )
            ]

        _ ->
            []


listSortByChecks : CheckInfo -> List (Error {})
listSortByChecks checkInfo =
    case secondArg checkInfo of
        Just (Node _ (Expression.ListExpr [])) ->
            [ Rule.errorWithFix
                { message = "Using List.sortBy on [] will result in []"
                , details = [ "You can replace this call by []." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
            ]

        Just (Node singletonListRange (Expression.ListExpr (_ :: []))) ->
            [ Rule.errorWithFix
                { message = "Sorting a list with a single element will result in the list itself"
                , details = [ "You can replace this call by the list itself." ]
                }
                checkInfo.fnRange
                (keepOnlyFix
                    { parentRange = checkInfo.parentRange
                    , keep = singletonListRange
                    }
                )
            ]

        _ ->
            case getAlwaysResult checkInfo checkInfo.firstArg of
                Just _ ->
                    [ Rule.errorWithFix
                        (toIdentityErrorInfo { toFix = "List.sortBy (always a)", lastArgName = "list" })
                        checkInfo.fnRange
                        (toIdentityFix { lastArg = secondArg checkInfo, parentRange = checkInfo.parentRange })
                    ]

                Nothing ->
                    if isIdentity checkInfo.lookupTable checkInfo.firstArg then
                        [ Rule.errorWithFix
                            { message = "Using List.sortBy identity is the same as using List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            }
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy
                                { start = checkInfo.fnRange.start
                                , end = (Node.range checkInfo.firstArg).end
                                }
                                "List.sort"
                            ]
                        ]

                    else
                        -- firstArg isn't identity
                        []


listSortWithChecks : CheckInfo -> List (Error {})
listSortWithChecks checkInfo =
    case secondArg checkInfo of
        Just (Node _ (Expression.ListExpr [])) ->
            [ Rule.errorWithFix
                { message = "Using List.sortWith on [] will result in []"
                , details = [ "You can replace this call by []." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
            ]

        Just (Node singletonListRange (Expression.ListExpr (_ :: []))) ->
            [ Rule.errorWithFix
                { message = "Sorting a list with a single element will result in the list itself"
                , details = [ "You can replace this call by the list itself." ]
                }
                checkInfo.fnRange
                (keepOnlyFix
                    { parentRange = checkInfo.parentRange
                    , keep = singletonListRange
                    }
                )
            ]

        _ ->
            let
                alwaysAlwaysOrder : Maybe Order
                alwaysAlwaysOrder =
                    Maybe.andThen (getOrder checkInfo.lookupTable)
                        (Maybe.andThen (getAlwaysResult checkInfo)
                            (getAlwaysResult checkInfo checkInfo.firstArg)
                        )
            in
            case alwaysAlwaysOrder of
                Nothing ->
                    []

                Just order ->
                    let
                        fixToIdentity : List (Error {})
                        fixToIdentity =
                            [ Rule.errorWithFix
                                (toIdentityErrorInfo { toFix = "List.sortWith (\\_ _ -> " ++ orderToString order ++ ")", lastArgName = "list" })
                                checkInfo.fnRange
                                (toIdentityFix { lastArg = secondArg checkInfo, parentRange = checkInfo.parentRange })
                            ]
                    in
                    case order of
                        LT ->
                            [ Rule.errorWithFix
                                { message = "Using List.sortWith (\\_ _ -> LT) is the same as using List.reverse"
                                , details = [ "You can replace this call by List.reverse." ]
                                }
                                checkInfo.fnRange
                                [ Fix.replaceRangeBy
                                    { start = checkInfo.fnRange.start
                                    , end = (Node.range checkInfo.firstArg).end
                                    }
                                    "List.reverse"
                                ]
                            ]

                        EQ ->
                            fixToIdentity

                        GT ->
                            fixToIdentity


listTakeChecks : CheckInfo -> List (Error {})
listTakeChecks checkInfo =
    let
        listArg : Maybe (Node Expression)
        listArg =
            secondArg checkInfo
    in
    if getUncomputedNumberValue checkInfo.firstArg == Just 0 then
        [ Rule.errorWithFix
            { message = "Taking 0 items from a list will result in []"
            , details = [ "You can replace this call by []." ]
            }
            checkInfo.fnRange
            (replaceByEmptyFix "[]" checkInfo.parentRange listArg)
        ]

    else
        case Maybe.andThen (determineListLength checkInfo.lookupTable) listArg of
            Just (Exactly 0) ->
                [ Rule.errorWithFix
                    { message = "Using List.take on [] will result in []"
                    , details = [ "You can replace this call by []." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                []


listDropChecks : CheckInfo -> List (Error {})
listDropChecks checkInfo =
    if getUncomputedNumberValue checkInfo.firstArg == Just 0 then
        case secondArg checkInfo of
            Just (Node secondArgRange _) ->
                [ Rule.errorWithFix
                    { message = "Dropping 0 items from a list will result in the list itself"
                    , details = [ "You can replace this call by the list itself." ]
                    }
                    checkInfo.fnRange
                    [ if checkInfo.usingRightPizza then
                        Fix.removeRange { start = secondArgRange.end, end = checkInfo.parentRange.end }

                      else
                        Fix.removeRange { start = checkInfo.parentRange.start, end = secondArgRange.start }
                    ]
                ]

            Nothing ->
                [ Rule.errorWithFix
                    { message = "Dropping 0 items from a list will result in the list itself"
                    , details = [ "You can replace this function by identity." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "identity" ]
                ]

    else
        case Maybe.andThen (determineListLength checkInfo.lookupTable) (secondArg checkInfo) of
            Just (Exactly 0) ->
                [ Rule.errorWithFix
                    { message = "Using List.drop on [] will result in []"
                    , details = [ "You can replace this call by []." ]
                    }
                    checkInfo.fnRange
                    [ Fix.replaceRangeBy checkInfo.parentRange "[]" ]
                ]

            _ ->
                []


listMapNChecks : { n : Int } -> CheckInfo -> List (Error {})
listMapNChecks { n } checkInfo =
    if List.any (\(Node _ list) -> list == Expression.ListExpr []) checkInfo.argsAfterFirst then
        let
            callReplacement : String
            callReplacement =
                multiAlways (n - List.length checkInfo.argsAfterFirst) "[]"
        in
        [ Rule.errorWithFix
            { message = "Using List.map" ++ String.fromInt n ++ " with any list being [] will result in []"
            , details = [ "You can replace this call by " ++ callReplacement ++ "." ]
            }
            checkInfo.fnRange
            [ Fix.replaceRangeBy checkInfo.parentRange callReplacement ]
        ]

    else
        []


multiAlways : Int -> String -> String
multiAlways alwaysCount alwaysResultExpressionAsString =
    case alwaysCount of
        0 ->
            alwaysResultExpressionAsString

        1 ->
            "always " ++ alwaysResultExpressionAsString

        alwaysCountPositive ->
            "(\\" ++ String.repeat alwaysCountPositive "_ " ++ "-> " ++ alwaysResultExpressionAsString ++ ")"


listUnzipChecks : CheckInfo -> List (Error {})
listUnzipChecks checkInfo =
    case Node.value checkInfo.firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Using List.unzip on [] will result in ( [], [] )"
                , details = [ "You can replace this call by ( [], [] )." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange "( [], [] )" ]
            ]

        _ ->
            []


subAndCmdBatchChecks : String -> CheckInfo -> List (Error {})
subAndCmdBatchChecks moduleName { lookupTable, parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "Replace by " ++ moduleName ++ ".batch"
                , details = [ moduleName ++ ".batch [] and " ++ moduleName ++ ".none are equivalent but the latter is more idiomatic in Elm code" ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange (moduleName ++ ".none") ]
            ]

        Expression.ListExpr [ listElement ] ->
            [ Rule.errorWithFix
                { message = "Unnecessary " ++ moduleName ++ ".batch"
                , details = [ moduleName ++ ".batch with a single element is equal to that element." ]
                }
                fnRange
                (replaceBySubExpressionFix parentRange listElement)
            ]

        Expression.ListExpr args ->
            List.map3 (\a b c -> ( a, b, c ))
                (Nothing :: List.map (Node.range >> Just) args)
                args
                (List.map (Node.range >> Just) (List.drop 1 args) ++ [ Nothing ])
                |> List.filterMap
                    (\( prev, arg, next ) ->
                        case AstHelpers.removeParens arg of
                            Node batchRange (Expression.FunctionOrValue _ "none") ->
                                if ModuleNameLookupTable.moduleNameAt lookupTable batchRange == Just [ "Platform", moduleName ] then
                                    Just
                                        (Rule.errorWithFix
                                            { message = "Unnecessary " ++ moduleName ++ ".none"
                                            , details = [ moduleName ++ ".none will be ignored by " ++ moduleName ++ ".batch." ]
                                            }
                                            (Node.range arg)
                                            (case prev of
                                                Just prevRange ->
                                                    [ Fix.removeRange { start = prevRange.end, end = (Node.range arg).end } ]

                                                Nothing ->
                                                    case next of
                                                        Just nextRange ->
                                                            [ Fix.removeRange { start = (Node.range arg).start, end = nextRange.start } ]

                                                        Nothing ->
                                                            [ Fix.replaceRangeBy parentRange (moduleName ++ ".none") ]
                                            )
                                        )

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )

        _ ->
            []



-- HTML.ATTRIBUTES


htmlAttributesClassListFalseElementError : { message : String, details : List String }
htmlAttributesClassListFalseElementError =
    { message = "In a Html.Attributes.classList, a tuple paired with False can be removed"
    , details = [ "You can remove the tuple list element where the second part is False." ]
    }


htmlAttributesClassListChecks : CheckInfo -> List (Error {})
htmlAttributesClassListChecks checkInfo =
    let
        listArg : Node Expression
        listArg =
            checkInfo.firstArg

        getTupleWithSecond : Node Expression -> Maybe { range : Range, first : Node Expression, second : Bool }
        getTupleWithSecond expressionNode =
            case getTuple expressionNode of
                Just tuple ->
                    case getBool checkInfo.lookupTable tuple.second of
                        Just bool ->
                            Just { range = tuple.range, first = tuple.first, second = bool }

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        getTupleWithSpecificSecond : Bool -> Node Expression -> Maybe { range : Range, first : Node Expression }
        getTupleWithSpecificSecond specificBool expressionNode =
            case getTuple expressionNode of
                Just tuple ->
                    if isSpecificBool specificBool checkInfo.lookupTable tuple.second then
                        Just { range = tuple.range, first = tuple.first }

                    else
                        Nothing

                Nothing ->
                    Nothing

        singleElementListChecks : { a | element : Node Expression } -> List (Error {})
        singleElementListChecks single =
            case getTupleWithSecond single.element of
                Just tuple ->
                    if tuple.second then
                        singleTrueChecks tuple

                    else
                        [ Rule.errorWithFix htmlAttributesClassListFalseElementError
                            checkInfo.fnRange
                            [ Fix.replaceRangeBy (Node.range listArg) listCollection.emptyAsString ]
                        ]

                Nothing ->
                    []

        singleTrueChecks : { a | first : Node Expression } -> List (Error {})
        singleTrueChecks { first } =
            [ Rule.errorWithFix
                { message = "Html.Attributes.classList with a single tuple paired with True can be replaced with Html.Attributes.class"
                , details = [ "You can replace this call by Html.Attributes.class with the String from the single tuple list element." ]
                }
                checkInfo.fnRange
                (keepOnlyFix { parentRange = Node.range listArg, keep = Node.range first }
                    ++ parenthesizeIfNeededFix first
                    ++ [ Fix.replaceRangeBy checkInfo.fnRange "Html.Attributes.class"
                       ]
                )
            ]
    in
    case getListLiteral listArg of
        Just (single :: []) ->
            singleElementListChecks { element = single }

        Just nonSingletonList ->
            case findMapNeighboring (getTupleWithSpecificSecond False) nonSingletonList of
                Just classPart ->
                    [ Rule.errorWithFix htmlAttributesClassListFalseElementError
                        checkInfo.fnRange
                        (listLiteralElementRemoveFix classPart)
                    ]

                Nothing ->
                    []

        Nothing ->
            case getCollapsedCons listArg of
                Just classParts ->
                    case findMapNeighboring (getTupleWithSpecificSecond False) classParts.consed of
                        Just classPart ->
                            [ Rule.errorWithFix htmlAttributesClassListFalseElementError
                                checkInfo.fnRange
                                (collapsedConsRemoveElementFix
                                    { toRemove = classPart
                                    , tailRange = Node.range classParts.tail
                                    }
                                )
                            ]

                        Nothing ->
                            []

                Nothing ->
                    case getListSingletonCall checkInfo.lookupTable listArg of
                        Just single ->
                            singleElementListChecks single

                        Nothing ->
                            []



-- PARSER


oneOfChecks : CheckInfo -> List (Error {})
oneOfChecks { parentRange, fnRange, firstArg } =
    case AstHelpers.removeParens firstArg of
        Node _ (Expression.ListExpr [ listElement ]) ->
            [ Rule.errorWithFix
                { message = "Unnecessary oneOf"
                , details = [ "There is only a single element in the list of elements to try out." ]
                }
                fnRange
                (replaceBySubExpressionFix parentRange listElement)
            ]

        _ ->
            []


type alias Collection =
    { moduleName : String
    , represents : String
    , emptyAsString : String
    , emptyDescription : String
    , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    , nameForSize : String
    , determineSize : ModuleNameLookupTable -> Node Expression -> Maybe CollectionSize
    }


listCollection : Collection
listCollection =
    { moduleName = "List"
    , represents = "list"
    , emptyAsString = "[]"
    , emptyDescription = "[]"
    , isEmpty = \_ -> isEmptyList
    , nameForSize = "length"
    , determineSize = determineListLength
    }


setCollection : Collection
setCollection =
    { moduleName = "Set"
    , represents = "set"
    , emptyAsString = "Set.empty"
    , emptyDescription = "Set.empty"
    , isEmpty = isSpecificValueOrFunction [ "Set" ] "empty"
    , nameForSize = "size"
    , determineSize = determineIfCollectionIsEmpty [ "Set" ] 1
    }


dictCollection : Collection
dictCollection =
    { moduleName = "Dict"
    , represents = "Dict"
    , emptyAsString = "Dict.empty"
    , emptyDescription = "Dict.empty"
    , isEmpty = isSpecificValueOrFunction [ "Dict" ] "empty"
    , nameForSize = "size"
    , determineSize = determineIfCollectionIsEmpty [ "Dict" ] 2
    }


type alias Mappable =
    { moduleName : String
    , represents : String
    , emptyAsString : String
    , emptyDescription : String
    , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    }


type alias Defaultable =
    { moduleName : String
    , represents : String
    , emptyAsString : String
    , emptyDescription : String
    , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    , isSomethingConstructor : String
    }


maybeCollection : Defaultable
maybeCollection =
    { moduleName = "Maybe"
    , represents = "maybe"
    , emptyAsString = "Nothing"
    , emptyDescription = "Nothing"
    , isEmpty = isSpecificValueOrFunction [ "Maybe" ] "Nothing"
    , isSomethingConstructor = "Just"
    }


resultCollection : Defaultable
resultCollection =
    { moduleName = "Result"
    , represents = "result"
    , emptyAsString = "Nothing"
    , emptyDescription = "an error"
    , isEmpty = isSpecificCall [ "Result" ] "Err"
    , isSomethingConstructor = "Ok"
    }


cmdCollection : Mappable
cmdCollection =
    { moduleName = "Cmd"
    , represents = "command"
    , emptyAsString = "Cmd.none"
    , emptyDescription = "Cmd.none"
    , isEmpty = isSpecificValueOrFunction [ "Platform", "Cmd" ] "none"
    }


subCollection : Mappable
subCollection =
    { moduleName = "Sub"
    , represents = "subscription"
    , emptyAsString = "Sub.none"
    , emptyDescription = "Sub.none"
    , isEmpty = isSpecificValueOrFunction [ "Platform", "Sub" ] "none"
    }


collectionMapChecks :
    { a
        | moduleName : String
        , represents : String
        , emptyDescription : String
        , emptyAsString : String
        , isEmpty : ModuleNameLookupTable -> Node Expression -> Bool
    }
    -> CheckInfo
    -> List (Error {})
collectionMapChecks collection checkInfo =
    case Maybe.map (collection.isEmpty checkInfo.lookupTable) (secondArg checkInfo) of
        Just True ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".map on " ++ collection.emptyDescription ++ " will result in " ++ collection.emptyDescription
                , details = [ "You can replace this call by " ++ collection.emptyDescription ++ "." ]
                }
                checkInfo.fnRange
                (noopFix checkInfo)
            ]

        _ ->
            if isIdentity checkInfo.lookupTable checkInfo.firstArg then
                [ Rule.errorWithFix
                    { message = "Using " ++ collection.moduleName ++ ".map with an identity function is the same as not using " ++ collection.moduleName ++ ".map"
                    , details = [ "You can remove this call and replace it by the " ++ collection.represents ++ " itself." ]
                    }
                    checkInfo.fnRange
                    (noopFix checkInfo)
                ]

            else
                []


maybeAndThenChecks : CheckInfo -> List (Error {})
maybeAndThenChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case Match.maybeAndThen (getMaybeValues checkInfo.lookupTable) (secondArg checkInfo) of
                Determined (Just justRanges) ->
                    [ Rule.errorWithFix
                        { message = "Calling " ++ maybeCollection.moduleName ++ ".andThen on a value that is known to be Just"
                        , details = [ "You can remove the Just and just call the function directly." ]
                        }
                        checkInfo.fnRange
                        (Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                            :: List.map Fix.removeRange justRanges
                        )
                    ]

                Determined Nothing ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ maybeCollection.moduleName ++ ".andThen on " ++ maybeCollection.emptyAsString ++ " will result in " ++ maybeCollection.emptyAsString
                        , details = [ "You can replace this call by " ++ maybeCollection.emptyAsString ++ "." ]
                        }
                        checkInfo.fnRange
                        (noopFix checkInfo)
                    ]

                _ ->
                    []
        , \() ->
            case isAlwaysMaybe checkInfo.lookupTable checkInfo.firstArg of
                Determined (Just { ranges, throughLambdaFunction }) ->
                    if throughLambdaFunction then
                        [ Rule.errorWithFix
                            { message = "Use " ++ maybeCollection.moduleName ++ ".map instead"
                            , details = [ "Using " ++ maybeCollection.moduleName ++ ".andThen with a function that always returns Just is the same thing as using Maybe.map." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange (maybeCollection.moduleName ++ ".map")
                                :: List.map Fix.removeRange ranges
                            )
                        ]

                    else
                        [ Rule.errorWithFix
                            { message = "Using Maybe.andThen with a function that will always return Just is the same as not using Maybe.andThen"
                            , details = [ "You can remove this call and replace it by the value itself." ]
                            }
                            checkInfo.fnRange
                            (noopFix checkInfo)
                        ]

                Determined Nothing ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ maybeCollection.moduleName ++ ".andThen with a function that will always return Nothing will result in Nothing"
                        , details = [ "You can remove this call and replace it by Nothing." ]
                        }
                        checkInfo.fnRange
                        (replaceByEmptyFix maybeCollection.emptyAsString checkInfo.parentRange (secondArg checkInfo))
                    ]

                Undetermined ->
                    []
        ]
        ()


resultAndThenChecks : CheckInfo -> List (Error {})
resultAndThenChecks checkInfo =
    firstThatReportsError
        [ \() ->
            case Maybe.andThen (getResultValues checkInfo.lookupTable) (secondArg checkInfo) of
                Just (Ok okRanges) ->
                    [ Rule.errorWithFix
                        { message = "Calling " ++ resultCollection.moduleName ++ ".andThen on a value that is known to be Ok"
                        , details = [ "You can remove the Ok and just call the function directly." ]
                        }
                        checkInfo.fnRange
                        (Fix.removeRange { start = checkInfo.fnRange.start, end = (Node.range checkInfo.firstArg).start }
                            :: List.map Fix.removeRange okRanges
                        )
                    ]

                Just (Err _) ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ resultCollection.moduleName ++ ".andThen on an error will result in the error"
                        , details = [ "You can replace this call by the error itself." ]
                        }
                        checkInfo.fnRange
                        (noopFix checkInfo)
                    ]

                _ ->
                    []
        , \() ->
            case isAlwaysResult checkInfo.lookupTable checkInfo.firstArg of
                Just (Ok { ranges, throughLambdaFunction }) ->
                    if throughLambdaFunction then
                        [ Rule.errorWithFix
                            { message = "Use Result.map instead"
                            , details = [ "Using Result.andThen with a function that always returns Ok is the same thing as using Result.map." ]
                            }
                            checkInfo.fnRange
                            (Fix.replaceRangeBy checkInfo.fnRange (resultCollection.moduleName ++ ".map")
                                :: List.map Fix.removeRange ranges
                            )
                        ]

                    else
                        [ Rule.errorWithFix
                            { message = "Using Result.andThen with a function that will always return Just is the same as not using Result.andThen"
                            , details = [ "You can remove this call and replace it by the value itself." ]
                            }
                            checkInfo.fnRange
                            (noopFix checkInfo)
                        ]

                _ ->
                    []
        ]
        ()


resultWithDefaultChecks : CheckInfo -> List (Error {})
resultWithDefaultChecks checkInfo =
    case Maybe.andThen (getResultValues checkInfo.lookupTable) (secondArg checkInfo) of
        Just (Ok okRanges) ->
            [ Rule.errorWithFix
                { message = "Using Result.withDefault on a value that is Ok will result in that value"
                , details = [ "You can replace this call by the value wrapped in Ok." ]
                }
                checkInfo.fnRange
                (List.map Fix.removeRange okRanges ++ noopFix checkInfo)
            ]

        Just (Err _) ->
            [ Rule.errorWithFix
                { message = "Using Result.withDefault on an error will result in the default value"
                , details = [ "You can replace this call by the default value." ]
                }
                checkInfo.fnRange
                [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                ]
            ]

        Nothing ->
            []


collectionFilterChecks : Collection -> CheckInfo -> List (Error {})
collectionFilterChecks collection checkInfo =
    let
        collectionArg : Maybe (Node Expression)
        collectionArg =
            secondArg checkInfo
    in
    case Maybe.andThen (collection.determineSize checkInfo.lookupTable) collectionArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".filter on " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                }
                checkInfo.fnRange
                (noopFix checkInfo)
            ]

        _ ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined True ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ collection.moduleName ++ ".filter with a function that will always return True is the same as not using " ++ collection.moduleName ++ ".filter"
                        , details = [ "You can remove this call and replace it by the " ++ collection.represents ++ " itself." ]
                        }
                        checkInfo.fnRange
                        (noopFix checkInfo)
                    ]

                Determined False ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ collection.moduleName ++ ".filter with a function that will always return False will result in " ++ collection.emptyAsString
                        , details = [ "You can remove this call and replace it by " ++ collection.emptyAsString ++ "." ]
                        }
                        checkInfo.fnRange
                        (replaceByEmptyFix collection.emptyAsString checkInfo.parentRange collectionArg)
                    ]

                Undetermined ->
                    []


collectionRemoveChecks : Collection -> CheckInfo -> List (Error {})
collectionRemoveChecks collection checkInfo =
    case Maybe.andThen (collection.determineSize checkInfo.lookupTable) (secondArg checkInfo) of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".remove on " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                }
                checkInfo.fnRange
                (noopFix checkInfo)
            ]

        _ ->
            []


collectionIntersectChecks : Collection -> CheckInfo -> List (Error {})
collectionIntersectChecks collection checkInfo =
    let
        collectionArg : Maybe (Node Expression)
        collectionArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case collection.determineSize checkInfo.lookupTable checkInfo.firstArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ collection.moduleName ++ ".intersect on " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                        , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                        }
                        checkInfo.fnRange
                        (replaceByEmptyFix collection.emptyAsString checkInfo.parentRange collectionArg)
                    ]

                _ ->
                    []
        , \() ->
            case Maybe.andThen (collection.determineSize checkInfo.lookupTable) collectionArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Using " ++ collection.moduleName ++ ".intersect on " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                        , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                        }
                        checkInfo.fnRange
                        (replaceByEmptyFix collection.emptyAsString checkInfo.parentRange collectionArg)
                    ]

                _ ->
                    []
        ]
        ()


collectionDiffChecks : Collection -> CheckInfo -> List (Error {})
collectionDiffChecks collection checkInfo =
    let
        collectionArg : Maybe (Node Expression)
        collectionArg =
            secondArg checkInfo
    in
    firstThatReportsError
        [ \() ->
            case collection.determineSize checkInfo.lookupTable checkInfo.firstArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Diffing " ++ collection.emptyAsString ++ " will result in " ++ collection.emptyAsString
                        , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                        }
                        checkInfo.fnRange
                        (replaceByEmptyFix collection.emptyAsString checkInfo.parentRange collectionArg)
                    ]

                _ ->
                    []
        , \() ->
            case Maybe.andThen (collection.determineSize checkInfo.lookupTable) collectionArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Diffing a " ++ collection.represents ++ " with " ++ collection.emptyAsString ++ " will result in the " ++ collection.represents ++ " itself"
                        , details = [ "You can replace this call by the " ++ collection.represents ++ " itself." ]
                        }
                        checkInfo.fnRange
                        [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                        , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                        ]
                    ]

                _ ->
                    []
        ]
        ()


collectionUnionChecks : Collection -> CheckInfo -> List (Error {})
collectionUnionChecks collection checkInfo =
    firstThatReportsError
        [ \() ->
            case collection.determineSize checkInfo.lookupTable checkInfo.firstArg of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary union with Set.empty"
                        , details = [ "You can replace this call by the set itself." ]
                        }
                        checkInfo.fnRange
                        (noopFix checkInfo)
                    ]

                _ ->
                    []
        , \() ->
            case Maybe.andThen (collection.determineSize checkInfo.lookupTable) (secondArg checkInfo) of
                Just (Exactly 0) ->
                    [ Rule.errorWithFix
                        { message = "Unnecessary union with Set.empty"
                        , details = [ "You can replace this call by the set itself." ]
                        }
                        checkInfo.fnRange
                        [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                        , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                        ]
                    ]

                _ ->
                    []
        ]
        ()


collectionInsertChecks : Collection -> CheckInfo -> List (Error {})
collectionInsertChecks collection checkInfo =
    case Maybe.andThen (collection.determineSize checkInfo.lookupTable) (secondArg checkInfo) of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Use " ++ collection.moduleName ++ ".singleton instead of inserting in " ++ collection.emptyAsString
                , details = [ "You can replace this call by " ++ collection.moduleName ++ ".singleton." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.fnRange (collection.moduleName ++ ".singleton")
                , if checkInfo.usingRightPizza then
                    Fix.removeRange { start = checkInfo.parentRange.start, end = checkInfo.fnRange.start }

                  else
                    Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                ]
            ]

        _ ->
            []


collectionMemberChecks : Collection -> CheckInfo -> List (Error {})
collectionMemberChecks collection checkInfo =
    let
        collectionArg : Maybe (Node Expression)
        collectionArg =
            secondArg checkInfo
    in
    case Maybe.andThen (collection.determineSize checkInfo.lookupTable) collectionArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".member on " ++ collection.emptyAsString ++ " will result in False"
                , details = [ "You can replace this call by False." ]
                }
                checkInfo.fnRange
                (replaceByBoolFix checkInfo.parentRange collectionArg False)
            ]

        _ ->
            []


collectionIsEmptyChecks : Collection -> CheckInfo -> List (Error {})
collectionIsEmptyChecks collection { lookupTable, parentRange, fnRange, firstArg } =
    case collection.determineSize lookupTable firstArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "The call to " ++ collection.moduleName ++ ".isEmpty will result in True"
                , details = [ "You can replace this call by True." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "True" ]
            ]

        Just _ ->
            [ Rule.errorWithFix
                { message = "The call to " ++ collection.moduleName ++ ".isEmpty will result in False"
                , details = [ "You can replace this call by False." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "False" ]
            ]

        Nothing ->
            []


collectionSizeChecks : Collection -> CheckInfo -> List (Error {})
collectionSizeChecks collection { lookupTable, parentRange, fnRange, firstArg } =
    case collection.determineSize lookupTable firstArg of
        Just (Exactly size) ->
            [ Rule.errorWithFix
                { message = "The " ++ collection.nameForSize ++ " of the " ++ collection.represents ++ " is " ++ String.fromInt size
                , details = [ "The " ++ collection.nameForSize ++ " of the " ++ collection.represents ++ " can be determined by looking at the code." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange (String.fromInt size) ]
            ]

        _ ->
            []


collectionFromListChecks : Collection -> CheckInfo -> List (Error {})
collectionFromListChecks collection { parentRange, fnRange, firstArg } =
    case Node.value firstArg of
        Expression.ListExpr [] ->
            [ Rule.errorWithFix
                { message = "The call to " ++ collection.moduleName ++ ".fromList will result in " ++ collection.emptyAsString
                , details = [ "You can replace this call by " ++ collection.emptyAsString ++ "." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange collection.emptyAsString ]
            ]

        _ ->
            []


collectionToListChecks : Collection -> CheckInfo -> List (Error {})
collectionToListChecks collection { lookupTable, parentRange, fnRange, firstArg } =
    case collection.determineSize lookupTable firstArg of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "The call to " ++ collection.moduleName ++ ".toList will result in []"
                , details = [ "You can replace this call by []." ]
                }
                fnRange
                [ Fix.replaceRangeBy parentRange "[]" ]
            ]

        _ ->
            []


collectionPartitionChecks : Collection -> CheckInfo -> List (Error {})
collectionPartitionChecks collection checkInfo =
    case Maybe.andThen (collection.determineSize checkInfo.lookupTable) (secondArg checkInfo) of
        Just (Exactly 0) ->
            [ Rule.errorWithFix
                { message = "Using " ++ collection.moduleName ++ ".partition on " ++ collection.emptyAsString ++ " will result in ( " ++ collection.emptyAsString ++ ", " ++ collection.emptyAsString ++ " )"
                , details = [ "You can replace this call by ( " ++ collection.emptyAsString ++ ", " ++ collection.emptyAsString ++ " )." ]
                }
                checkInfo.fnRange
                [ Fix.replaceRangeBy checkInfo.parentRange ("( " ++ collection.emptyAsString ++ ", " ++ collection.emptyAsString ++ " )") ]
            ]

        _ ->
            case Evaluate.isAlwaysBoolean checkInfo checkInfo.firstArg of
                Determined True ->
                    case secondArg checkInfo of
                        Just listArg ->
                            [ Rule.errorWithFix
                                { message = "All elements will go to the first " ++ collection.represents
                                , details = [ "Since the predicate function always returns True, the second " ++ collection.represents ++ " will always be " ++ collection.emptyAsString ++ "." ]
                                }
                                checkInfo.fnRange
                                [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range listArg).start } "( "
                                , Fix.insertAt (Node.range listArg).end (", " ++ collection.emptyAsString ++ " )")
                                ]
                            ]

                        Nothing ->
                            []

                Determined False ->
                    [ Rule.errorWithFix
                        { message = "All elements will go to the second " ++ collection.represents
                        , details = [ "Since the predicate function always returns False, the first " ++ collection.represents ++ " will always be " ++ collection.emptyAsString ++ "." ]
                        }
                        checkInfo.fnRange
                        (case secondArg checkInfo of
                            Just listArg ->
                                [ Fix.replaceRangeBy { start = checkInfo.fnRange.start, end = (Node.range listArg).start } ("( " ++ collection.emptyAsString ++ ", ")
                                , Fix.insertAt (Node.range listArg).end " )"
                                ]

                            Nothing ->
                                [ Fix.replaceRangeBy checkInfo.parentRange ("(Tuple.pair " ++ collection.emptyAsString ++ ")") ]
                        )
                    ]

                Undetermined ->
                    []


maybeWithDefaultChecks : CheckInfo -> List (Error {})
maybeWithDefaultChecks checkInfo =
    case Match.maybeAndThen (getMaybeValues checkInfo.lookupTable) (secondArg checkInfo) of
        Determined (Just justRanges) ->
            [ Rule.errorWithFix
                { message = "Using Maybe.withDefault on a value that is Just will result in that value"
                , details = [ "You can replace this call by the value wrapped in Just." ]
                }
                checkInfo.fnRange
                (List.map Fix.removeRange justRanges ++ noopFix checkInfo)
            ]

        Determined Nothing ->
            [ Rule.errorWithFix
                { message = "Using Maybe.withDefault on Nothing will result in the default value"
                , details = [ "You can replace this call by the default value." ]
                }
                checkInfo.fnRange
                [ Fix.removeRange { start = checkInfo.parentRange.start, end = (Node.range checkInfo.firstArg).start }
                , Fix.removeRange { start = (Node.range checkInfo.firstArg).end, end = checkInfo.parentRange.end }
                ]
            ]

        Undetermined ->
            []


type CollectionSize
    = Exactly Int
    | NotEmpty


determineListLength : ModuleNameLookupTable -> Node Expression -> Maybe CollectionSize
determineListLength lookupTable node =
    case Node.value (AstHelpers.removeParens node) of
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


replaceSingleElementListBySingleValue_RENAME : ModuleNameLookupTable -> Range -> Node Expression -> Maybe (List (Error {}))
replaceSingleElementListBySingleValue_RENAME lookupTable fnRange node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.LambdaExpression { expression } ->
            case replaceSingleElementListBySingleValue lookupTable expression of
                Just fixes ->
                    Just
                        [ Rule.errorWithFix
                            { message = "Use List.map instead"
                            , details = [ "The function passed to List.concatMap always returns a list with a single element." ]
                            }
                            fnRange
                            (Fix.replaceRangeBy fnRange "List.map" :: fixes)
                        ]

                Nothing ->
                    Nothing

        _ ->
            Nothing


replaceSingleElementListBySingleValue : ModuleNameLookupTable -> Node Expression -> Maybe (List Fix)
replaceSingleElementListBySingleValue lookupTable node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.ListExpr [ listElement ] ->
            Just (replaceBySubExpressionFix (Node.range node) listElement)

        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "singleton")) :: _ :: []) ->
            if ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just [ "List" ] then
                Just [ Fix.removeRange fnRange ]

            else
                Nothing

        Expression.IfBlock _ thenBranch elseBranch ->
            combineSingleElementFixes lookupTable [ thenBranch, elseBranch ] []

        Expression.CaseExpression { cases } ->
            combineSingleElementFixes lookupTable (List.map Tuple.second cases) []

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


determineIfCollectionIsEmpty : ModuleName -> Int -> ModuleNameLookupTable -> Node Expression -> Maybe CollectionSize
determineIfCollectionIsEmpty moduleName singletonNumberOfArgs lookupTable node =
    if isSpecificValueOrFunction moduleName "empty" lookupTable node then
        Just (Exactly 0)

    else
        case Node.value (AstHelpers.removeParens node) of
            Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "singleton")) :: args) ->
                if List.length args == singletonNumberOfArgs && ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just moduleName then
                    Just (Exactly 1)

                else
                    Nothing

            Expression.Application ((Node fnRange (Expression.FunctionOrValue _ "fromList")) :: (Node _ (Expression.ListExpr list)) :: []) ->
                if ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just moduleName then
                    if moduleName == [ "Set" ] then
                        case list of
                            [] ->
                                Just (Exactly 0)

                            [ _ ] ->
                                Just (Exactly 1)

                            _ ->
                                case traverse getComparableExpression list of
                                    Nothing ->
                                        Just NotEmpty

                                    Just comparableExpressions ->
                                        comparableExpressions |> unique |> List.length |> Exactly |> Just

                    else
                        Just (Exactly (List.length list))

                else
                    Nothing

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



-- RECORD UPDATE


removeRecordFields : Range -> Node String -> List (Node Expression.RecordSetter) -> List (Error {})
removeRecordFields recordUpdateRange variable fields =
    case fields of
        [] ->
            -- Not possible
            []

        (Node _ ( field, valueWithParens )) :: [] ->
            let
                value : Node Expression
                value =
                    AstHelpers.removeParens valueWithParens
            in
            if isUnnecessaryRecordUpdateSetter variable field value then
                [ Rule.errorWithFix
                    { message = "Unnecessary field assignment"
                    , details = [ "The field is being set to its own value." ]
                    }
                    (Node.range value)
                    [ Fix.removeRange { start = recordUpdateRange.start, end = (Node.range variable).start }
                    , Fix.removeRange { start = (Node.range variable).end, end = recordUpdateRange.end }
                    ]
                ]

            else
                []

        (Node firstRange _) :: second :: _ ->
            List.filterMap
                (\( Node range ( field, valueWithParens ), previousRange ) ->
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
                                (case previousRange of
                                    Just prevRange ->
                                        [ Fix.removeRange { start = prevRange.end, end = range.end } ]

                                    Nothing ->
                                        -- It's the first element, so we can remove until the second element
                                        [ Fix.removeRange { start = firstRange.start, end = (Node.range second).start } ]
                                )
                            )

                    else
                        Nothing
                )
                (List.map2 Tuple.pair fields (Nothing :: List.map (Node.range >> Just) fields))


isUnnecessaryRecordUpdateSetter : Node String -> Node String -> Node Expression -> Bool
isUnnecessaryRecordUpdateSetter variable field value =
    case Node.value value of
        Expression.RecordAccess (Node _ (Expression.FunctionOrValue [] valueHolder)) fieldName ->
            Node.value field == Node.value fieldName && Node.value variable == valueHolder

        _ ->
            False



-- IF


ifChecks :
    ModuleContext
    -> Range
    ->
        { condition : Node Expression
        , trueBranch : Node Expression
        , falseBranch : Node Expression
        }
    -> { errors : List (Error {}), rangesToIgnore : List Range, rightSidesOfPlusPlus : List Range, inferredConstants : List ( Range, Infer.Inferred ) }
ifChecks context nodeRange { condition, trueBranch, falseBranch } =
    case Evaluate.getBoolean context condition of
        Determined True ->
            errorsAndRangesToIgnore
                [ Rule.errorWithFix
                    { message = "The condition will always evaluate to True"
                    , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                    }
                    (targetIfKeyword nodeRange)
                    [ Fix.removeRange
                        { start = nodeRange.start
                        , end = (Node.range trueBranch).start
                        }
                    , Fix.removeRange
                        { start = (Node.range trueBranch).end
                        , end = nodeRange.end
                        }
                    ]
                ]
                [ Node.range condition ]

        Determined False ->
            errorsAndRangesToIgnore
                [ Rule.errorWithFix
                    { message = "The condition will always evaluate to False"
                    , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                    }
                    (targetIfKeyword nodeRange)
                    [ Fix.removeRange
                        { start = nodeRange.start
                        , end = (Node.range falseBranch).start
                        }
                    ]
                ]
                [ Node.range condition ]

        Undetermined ->
            case ( Evaluate.getBoolean context trueBranch, Evaluate.getBoolean context falseBranch ) of
                ( Determined True, Determined False ) ->
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "The if expression's value is the same as the condition"
                            , details = [ "The expression can be replaced by the condition." ]
                            }
                            (targetIfKeyword nodeRange)
                            [ Fix.removeRange
                                { start = nodeRange.start
                                , end = (Node.range condition).start
                                }
                            , Fix.removeRange
                                { start = (Node.range condition).end
                                , end = nodeRange.end
                                }
                            ]
                        ]

                ( Determined False, Determined True ) ->
                    onlyErrors
                        [ Rule.errorWithFix
                            { message = "The if expression's value is the inverse of the condition"
                            , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                            }
                            (targetIfKeyword nodeRange)
                            [ Fix.replaceRangeBy
                                { start = nodeRange.start
                                , end = (Node.range condition).start
                                }
                                "not ("
                            , Fix.replaceRangeBy
                                { start = (Node.range condition).end
                                , end = nodeRange.end
                                }
                                ")"
                            ]
                        ]

                _ ->
                    case Normalize.compare context trueBranch falseBranch of
                        Normalize.ConfirmedEquality ->
                            onlyErrors
                                [ Rule.errorWithFix
                                    { message = "The values in both branches is the same."
                                    , details = [ "The expression can be replaced by the contents of either branch." ]
                                    }
                                    (targetIfKeyword nodeRange)
                                    [ Fix.removeRange
                                        { start = nodeRange.start
                                        , end = (Node.range trueBranch).start
                                        }
                                    , Fix.removeRange
                                        { start = (Node.range trueBranch).end
                                        , end = nodeRange.end
                                        }
                                    ]
                                ]

                        _ ->
                            { errors = []
                            , rangesToIgnore = []
                            , rightSidesOfPlusPlus = []
                            , inferredConstants =
                                Infer.inferForIfCondition
                                    (Node.value (Normalize.normalize context condition))
                                    { trueBranchRange = Node.range trueBranch
                                    , falseBranchRange = Node.range falseBranch
                                    }
                                    (Tuple.first context.inferredConstants)
                            }



-- CASE OF


caseOfChecks : ModuleContext -> Range -> Expression.CaseBlock -> List (Error {})
caseOfChecks context parentRange caseBlock =
    firstThatReportsError
        [ \() -> sameBodyForCaseOfChecks context parentRange caseBlock.cases
        , \() -> booleanCaseOfChecks context.lookupTable parentRange caseBlock
        , \() -> destructuringCaseOfChecks context.extractSourceCode parentRange caseBlock
        ]
        ()


sameBodyForCaseOfChecks : ModuleContext -> Range -> List ( Node Pattern, Node Expression ) -> List (Error {})
sameBodyForCaseOfChecks context parentRange cases =
    case cases of
        [] ->
            []

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
                []

            else
                let
                    firstBodyRange : Range
                    firstBodyRange =
                        Node.range firstBody
                in
                [ Rule.errorWithFix
                    { message = "Unnecessary case expression"
                    , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                    }
                    (caseKeyWordRange parentRange)
                    [ Fix.removeRange { start = parentRange.start, end = firstBodyRange.start }
                    , Fix.removeRange { start = firstBodyRange.end, end = parentRange.end }
                    ]
                ]


caseKeyWordRange : Range -> Range
caseKeyWordRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 4 }
    }


introducesVariableOrUsesTypeConstructor : ModuleContext -> List (Node Pattern) -> Bool
introducesVariableOrUsesTypeConstructor context nodesToLookAt =
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
                    introducesVariableOrUsesTypeConstructor context (pattern :: remaining)

                Pattern.TuplePattern nodes ->
                    introducesVariableOrUsesTypeConstructor context (nodes ++ remaining)

                Pattern.UnConsPattern first rest ->
                    introducesVariableOrUsesTypeConstructor context (first :: rest :: remaining)

                Pattern.ListPattern nodes ->
                    introducesVariableOrUsesTypeConstructor context (nodes ++ remaining)

                Pattern.NamedPattern { name } nodes ->
                    case ModuleNameLookupTable.fullModuleNameFor context.lookupTable node of
                        Just moduleName ->
                            if Set.member ( moduleName, name ) context.customTypesToReportInCases then
                                introducesVariableOrUsesTypeConstructor context (nodes ++ remaining)

                            else
                                True

                        Nothing ->
                            True

                _ ->
                    introducesVariableOrUsesTypeConstructor context remaining


booleanCaseOfChecks : ModuleNameLookupTable -> Range -> Expression.CaseBlock -> List (Error {})
booleanCaseOfChecks lookupTable parentRange { expression, cases } =
    case cases of
        [ ( firstPattern, Node firstRange _ ), ( Node secondPatternRange _, Node secondExprRange _ ) ] ->
            case getBooleanPattern lookupTable firstPattern of
                Just isTrueFirst ->
                    [ Rule.errorWithFix
                        { message = "Replace `case..of` by an `if` condition"
                        , details =
                            [ "The idiomatic way to check for a condition is to use an `if` expression."
                            , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
                            ]
                        }
                        (Node.range firstPattern)
                        (if isTrueFirst then
                            [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range expression).start } "if "
                            , Fix.replaceRangeBy { start = (Node.range expression).end, end = firstRange.start } " then "
                            , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                            ]

                         else
                            [ Fix.replaceRangeBy { start = parentRange.start, end = (Node.range expression).start } "if not ("
                            , Fix.replaceRangeBy { start = (Node.range expression).end, end = firstRange.start } ") then "
                            , Fix.replaceRangeBy { start = secondPatternRange.start, end = secondExprRange.start } "else "
                            ]
                        )
                    ]

                _ ->
                    []

        _ ->
            []


destructuringCaseOfChecks : (Range -> String) -> Range -> Expression.CaseBlock -> List (Error {})
destructuringCaseOfChecks extractSourceCode parentRange { expression, cases } =
    case cases of
        [ ( rawSinglePattern, Node bodyRange _ ) ] ->
            let
                singlePattern : Node Pattern
                singlePattern =
                    AstHelpers.removeParensFromPattern rawSinglePattern
            in
            if isSimpleDestructurePattern singlePattern then
                let
                    exprRange : Range
                    exprRange =
                        Node.range expression

                    caseIndentation : String
                    caseIndentation =
                        String.repeat (parentRange.start.column - 1) " "

                    bodyIndentation : String
                    bodyIndentation =
                        String.repeat (bodyRange.start.column - 1) " "
                in
                [ Rule.errorWithFix
                    { message = "Use a let expression to destructure data"
                    , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                    }
                    (Node.range singlePattern)
                    [ Fix.replaceRangeBy { start = parentRange.start, end = exprRange.start } ("let " ++ extractSourceCode (Node.range singlePattern) ++ " = ")
                    , Fix.replaceRangeBy { start = exprRange.end, end = bodyRange.start } ("\n" ++ caseIndentation ++ "in\n" ++ bodyIndentation)
                    ]
                ]

            else
                []

        _ ->
            []


isSimpleDestructurePattern : Node Pattern -> Bool
isSimpleDestructurePattern pattern =
    case Node.value pattern of
        Pattern.TuplePattern _ ->
            True

        Pattern.RecordPattern _ ->
            True

        Pattern.VarPattern _ ->
            True

        _ ->
            False


isSpecificValueOrFunction : ModuleName -> String -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificValueOrFunction moduleName fnName lookupTable node =
    case AstHelpers.removeParens node of
        Node noneRange (Expression.FunctionOrValue _ foundFnName) ->
            (foundFnName == fnName)
                && (ModuleNameLookupTable.moduleNameAt lookupTable noneRange == Just moduleName)

        _ ->
            False


isSpecificCall : ModuleName -> String -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificCall moduleName fnName lookupTable node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.Application ((Node noneRange (Expression.FunctionOrValue _ foundFnName)) :: _ :: []) ->
            (foundFnName == fnName)
                && (ModuleNameLookupTable.moduleNameAt lookupTable noneRange == Just moduleName)

        _ ->
            False


getUncomputedNumberValue : Node Expression -> Maybe Float
getUncomputedNumberValue node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.Integer n ->
            Just (toFloat n)

        Expression.Hex n ->
            Just (toFloat n)

        Expression.Floatable n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getUncomputedNumberValue expr)

        _ ->
            Nothing


letInChecks : Expression.LetBlock -> List (Error {})
letInChecks letBlock =
    case Node.value letBlock.expression of
        Expression.LetExpression _ ->
            let
                letRange : Range
                letRange =
                    letKeyWordRange (Node.range letBlock.expression)
            in
            [ Rule.errorWithFix
                { message = "Let blocks can be joined together"
                , details = [ "Let blocks can contain multiple declarations, and there is no advantage to having multiple chained let expressions rather than one longer let expression." ]
                }
                letRange
                (case lastElementRange letBlock.declarations of
                    Just lastDeclRange ->
                        [ Fix.replaceRangeBy { start = lastDeclRange.end, end = letRange.end } "\n" ]

                    Nothing ->
                        []
                )
            ]

        _ ->
            []


letKeyWordRange : Range -> Range
letKeyWordRange range =
    { start = range.start
    , end = { row = range.start.row, column = range.start.column + 3 }
    }


lastElementRange : List (Node a) -> Maybe Range
lastElementRange nodes =
    case nodes of
        [] ->
            Nothing

        last :: [] ->
            Just (Node.range last)

        _ :: rest ->
            lastElementRange rest



-- FIX HELPERS


parenthesizeIfNeededFix : Node Expression -> List Fix
parenthesizeIfNeededFix expressionNode =
    if needsParens (Node.value expressionNode) then
        parenthesizeFix (Node.range expressionNode)

    else
        []


parenthesizeFix : Range -> List Fix
parenthesizeFix toSurround =
    [ Fix.insertAt toSurround.start "("
    , Fix.insertAt toSurround.end ")"
    ]


rangeBetweenExclusive : ( Range, Range ) -> Range
rangeBetweenExclusive ( aRange, bRange ) =
    case locationsCompare ( aRange.start, bRange.start ) of
        GT ->
            { start = bRange.end, end = aRange.start }

        -- EQ | LT
        _ ->
            { start = aRange.end, end = bRange.start }


locationsCompare : ( Location, Location ) -> Order
locationsCompare ( aEnd, bEnd ) =
    if aEnd.column == bEnd.column then
        compare aEnd.row bEnd.row

    else
        compare aEnd.column bEnd.column


removeFunctionFromFunctionCall : { a | fnRange : Range, firstArg : Node b, usingRightPizza : Bool } -> Fix
removeFunctionFromFunctionCall { fnRange, firstArg, usingRightPizza } =
    if usingRightPizza then
        Fix.removeRange { start = (Node.range firstArg).end, end = fnRange.end }

    else
        Fix.removeRange { start = fnRange.start, end = (Node.range firstArg).start }


removeFunctionAndFirstArg : { a | fnRange : Range, firstArg : Node b, usingRightPizza : Bool } -> Range -> Fix
removeFunctionAndFirstArg { fnRange, firstArg, usingRightPizza } secondArgRange =
    if usingRightPizza then
        Fix.removeRange { start = secondArgRange.end, end = (Node.range firstArg).end }

    else
        Fix.removeRange { start = fnRange.start, end = secondArgRange.start }


keepOnlyFix : { parentRange : Range, keep : Range } -> List Fix
keepOnlyFix { parentRange, keep } =
    [ Fix.removeRange
        { start = parentRange.start
        , end = keep.start
        }
    , Fix.removeRange
        { start = keep.end
        , end = parentRange.end
        }
    ]


removeBoundariesFix : Node a -> List Fix
removeBoundariesFix node =
    let
        { start, end } =
            Node.range node
    in
    [ Fix.removeRange
        { start = { row = start.row, column = start.column }
        , end = { row = start.row, column = start.column + 1 }
        }
    , Fix.removeRange
        { start = { row = end.row, column = end.column - 1 }
        , end = { row = end.row, column = end.column }
        }
    ]


replaceByEmptyFix : String -> Range -> Maybe a -> List Fix
replaceByEmptyFix empty parentRange lastArg =
    [ case lastArg of
        Just _ ->
            Fix.replaceRangeBy parentRange empty

        Nothing ->
            Fix.replaceRangeBy parentRange ("always " ++ empty)
    ]


emptyStringAsString : String
emptyStringAsString =
    "\"\""


replaceByBoolFix : Range -> Maybe a -> Bool -> List Fix
replaceByBoolFix parentRange lastArg replacementValue =
    [ case lastArg of
        Just _ ->
            Fix.replaceRangeBy parentRange (boolToString replacementValue)

        Nothing ->
            Fix.replaceRangeBy parentRange ("(always " ++ boolToString replacementValue ++ ")")
    ]


toIdentityErrorInfo : { toFix : String, lastArgName : String } -> { message : String, details : List String }
toIdentityErrorInfo config =
    { message = "Using " ++ config.toFix ++ " will always return the same " ++ config.lastArgName
    , details = [ "You can replace this call by the " ++ config.lastArgName ++ " itself." ]
    }


{-| identity if there's no second argument, else the second argument.

TODO replace uses with `toIdentityFix` to be more explicit

-}
noopFix : CheckInfo -> List Fix
noopFix checkInfo =
    toIdentityFix
        { lastArg = secondArg checkInfo
        , parentRange = checkInfo.parentRange
        }


toIdentityFix : { lastArg : Maybe (Node lastArgument), parentRange : Range } -> List Fix
toIdentityFix config =
    case config.lastArg of
        Nothing ->
            [ Fix.replaceRangeBy config.parentRange "identity"
            ]

        Just (Node lastArgRange _) ->
            keepOnlyFix { parentRange = config.parentRange, keep = lastArgRange }


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
        ( Nothing, Just after ) ->
            [ Fix.removeRange
                { start = toRemove.found.range.start
                , end = (Node.range after).start
                }
            ]

        -- found after first element
        ( Just before, _ ) ->
            [ Fix.removeRange
                { start = (Node.range before).end
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
collapsedConsRemoveElementFix { toRemove, tailRange } =
    case ( toRemove.before, toRemove.after ) of
        -- found the only consed element
        ( Nothing, Nothing ) ->
            [ Fix.removeRange
                { start = toRemove.found.range.start, end = tailRange.start }
            ]

        -- found first consed element
        ( Nothing, Just after ) ->
            [ Fix.removeRange
                { start = toRemove.found.range.start
                , end = (Node.range after).start
                }
            ]

        -- found after first consed element
        ( Just before, _ ) ->
            [ Fix.removeRange
                { start = (Node.range before).end
                , end = toRemove.found.range.end
                }
            ]


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


orderToString : Order -> String
orderToString order =
    case order of
        LT ->
            "LT"

        EQ ->
            "EQ"

        GT ->
            "GT"



-- MATCHERS


isIdentity : ModuleNameLookupTable -> Node Expression -> Bool
isIdentity lookupTable baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "identity" ->
            ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ]

        Expression.LambdaExpression { args, expression } ->
            case args of
                arg :: [] ->
                    case getVarPattern arg of
                        Just patternName ->
                            getExpressionName expression
                                == Just patternName

                        _ ->
                            False

                _ ->
                    False

        _ ->
            False


getVarPattern : Node Pattern -> Maybe String
getVarPattern node =
    case Node.value node of
        Pattern.VarPattern name ->
            Just name

        Pattern.ParenthesizedPattern pattern ->
            getVarPattern pattern

        _ ->
            Nothing


getExpressionName : Node Expression -> Maybe String
getExpressionName node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.FunctionOrValue [] name ->
            Just name

        _ ->
            Nothing


isListLiteral : Node Expression -> Bool
isListLiteral node =
    case Node.value node of
        Expression.ListExpr _ ->
            True

        _ ->
            False


getListLiteral : Node Expression -> Maybe (List (Node Expression))
getListLiteral expressionNode =
    case Node.value expressionNode of
        Expression.ListExpr list ->
            Just list

        _ ->
            Nothing


getCollapsedCons : Node Expression -> Maybe { consed : List (Node Expression), tail : Node Expression }
getCollapsedCons expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.OperatorApplication "::" _ head tail ->
            let
                tailCollapsed : Maybe { consed : List (Node Expression), tail : Node Expression }
                tailCollapsed =
                    getCollapsedCons tail
            in
            case tailCollapsed of
                Nothing ->
                    Just { consed = [ head ], tail = tail }

                Just tailCollapsedList ->
                    Just { consed = head :: tailCollapsedList.consed, tail = tailCollapsedList.tail }

        _ ->
            Nothing


getBool : ModuleNameLookupTable -> Node Expression -> Maybe Bool
getBool lookupTable expressionNode =
    if isSpecificBool True lookupTable expressionNode then
        Just True

    else if isSpecificBool False lookupTable expressionNode then
        Just False

    else
        Nothing


isSpecificBool : Bool -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificBool specificBool lookupTable expressionNode =
    isSpecificValueOrFunction [ "Basics" ] (boolToString specificBool) lookupTable expressionNode


getTuple : Node Expression -> Maybe { range : Range, first : Node Expression, second : Node Expression }
getTuple expressionNode =
    case Node.value expressionNode of
        Expression.TupledExpression (first :: second :: []) ->
            Just { range = Node.range expressionNode, first = first, second = second }

        _ ->
            Nothing


getBooleanPattern : ModuleNameLookupTable -> Node Pattern -> Maybe Bool
getBooleanPattern lookupTable node =
    case Node.value node of
        Pattern.NamedPattern { name } _ ->
            case name of
                "True" ->
                    if ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ] then
                        Just True

                    else
                        Nothing

                "False" ->
                    if ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ] then
                        Just False

                    else
                        Nothing

                _ ->
                    Nothing

        Pattern.ParenthesizedPattern pattern ->
            getBooleanPattern lookupTable pattern

        _ ->
            Nothing


getAlwaysResult : Infer.Resources a -> Node Expression -> Maybe (Node Expression)
getAlwaysResult inferResources expressionNode =
    case Node.value (AstHelpers.removeParens expressionNode) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: result :: []) ->
            case ModuleNameLookupTable.moduleNameAt inferResources.lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    Just result

                _ ->
                    Nothing

        Expression.OperatorApplication "<|" _ (Node alwaysRange (Expression.FunctionOrValue _ "always")) result ->
            case ModuleNameLookupTable.moduleNameAt inferResources.lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    Just result

                _ ->
                    Nothing

        Expression.OperatorApplication "|>" _ result (Node alwaysRange (Expression.FunctionOrValue _ "always")) ->
            case ModuleNameLookupTable.moduleNameAt inferResources.lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    Just result

                _ ->
                    Nothing

        Expression.LambdaExpression lambda ->
            case lambda.args of
                -- invalid syntax
                [] ->
                    Nothing

                (Node _ Pattern.AllPattern) :: [] ->
                    Just lambda.expression

                (Node _ Pattern.AllPattern) :: patternsAfter_ ->
                    Just
                        (Node (Node.range expressionNode)
                            (Expression.LambdaExpression
                                { args = patternsAfter_
                                , expression = lambda.expression
                                }
                            )
                        )

                -- not an ignore-first
                _ :: _ ->
                    Nothing

        _ ->
            Nothing


getOrder : ModuleNameLookupTable -> Node Expression -> Maybe Order
getOrder lookupTable expression =
    if isSpecificValueOrFunction [ "Basics" ] "LT" lookupTable expression then
        Just LT

    else if isSpecificValueOrFunction [ "Basics" ] "EQ" lookupTable expression then
        Just EQ

    else if isSpecificValueOrFunction [ "Basics" ] "GT" lookupTable expression then
        Just GT

    else
        Nothing


isAlwaysMaybe : ModuleNameLookupTable -> Node Expression -> Match (Maybe { ranges : List Range, throughLambdaFunction : Bool })
isAlwaysMaybe lookupTable baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "Just" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Maybe" ] ->
                    Determined (Just { ranges = [ Node.range node ], throughLambdaFunction = False })

                _ ->
                    Undetermined

        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: value :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getMaybeValues lookupTable value
                        |> Match.map (Maybe.map (\ranges -> { ranges = ranges, throughLambdaFunction = False }))

                _ ->
                    Undetermined

        Expression.LambdaExpression { expression } ->
            getMaybeValues lookupTable expression
                |> Match.map (Maybe.map (\ranges -> { ranges = ranges, throughLambdaFunction = True }))

        _ ->
            Undetermined


getMaybeValues : ModuleNameLookupTable -> Node Expression -> Match (Maybe (List Range))
getMaybeValues lookupTable baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Just")) :: arg :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Maybe" ] ->
                    Determined (Just [ { start = justRange.start, end = (Node.range arg).start } ])

                _ ->
                    Undetermined

        Expression.OperatorApplication "|>" _ arg (Node justRange (Expression.FunctionOrValue _ "Just")) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Maybe" ] ->
                    Determined (Just [ { start = (Node.range arg).end, end = justRange.end } ])

                _ ->
                    Undetermined

        Expression.OperatorApplication "<|" _ (Node justRange (Expression.FunctionOrValue _ "Just")) arg ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Maybe" ] ->
                    Determined (Just [ { start = justRange.start, end = (Node.range arg).start } ])

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ "Nothing" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Maybe" ] ->
                    Determined Nothing

                _ ->
                    Undetermined

        Expression.LetExpression { expression } ->
            getMaybeValues lookupTable expression

        Expression.ParenthesizedExpression expression ->
            getMaybeValues lookupTable expression

        Expression.IfBlock _ thenBranch elseBranch ->
            combineMaybeValues lookupTable [ thenBranch, elseBranch ]

        Expression.CaseExpression { cases } ->
            combineMaybeValues lookupTable (List.map Tuple.second cases)

        _ ->
            Undetermined


combineMaybeValues : ModuleNameLookupTable -> List (Node Expression) -> Match (Maybe (List Range))
combineMaybeValues lookupTable nodes =
    case nodes of
        node :: restOfNodes ->
            case getMaybeValues lookupTable node of
                Undetermined ->
                    Undetermined

                Determined nodeValue ->
                    combineMaybeValuesHelp lookupTable restOfNodes nodeValue

        [] ->
            Undetermined


combineMaybeValuesHelp : ModuleNameLookupTable -> List (Node Expression) -> Maybe (List Range) -> Match (Maybe (List Range))
combineMaybeValuesHelp lookupTable nodes soFar =
    case nodes of
        node :: restOfNodes ->
            case getMaybeValues lookupTable node of
                Undetermined ->
                    Undetermined

                Determined nodeValue ->
                    case ( nodeValue, soFar ) of
                        ( Just _, Nothing ) ->
                            Undetermined

                        ( Nothing, Just _ ) ->
                            Undetermined

                        ( Nothing, Nothing ) ->
                            combineMaybeValuesHelp lookupTable restOfNodes Nothing

                        ( Just a, Just b ) ->
                            combineMaybeValuesHelp lookupTable restOfNodes (Just (a ++ b))

        [] ->
            Determined soFar


isAlwaysResult : ModuleNameLookupTable -> Node Expression -> Maybe (Result Range { ranges : List Range, throughLambdaFunction : Bool })
isAlwaysResult lookupTable baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "Ok" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Result" ] ->
                    Just (Ok { ranges = [ Node.range node ], throughLambdaFunction = False })

                _ ->
                    Nothing

        Expression.FunctionOrValue _ "Err" ->
            case ModuleNameLookupTable.moduleNameFor lookupTable node of
                Just [ "Result" ] ->
                    Just (Err (Node.range node))

                _ ->
                    Nothing

        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: value :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getResultValues lookupTable value
                        |> Maybe.map (Result.map (\ranges -> { ranges = ranges, throughLambdaFunction = False }))

                _ ->
                    Nothing

        Expression.LambdaExpression { expression } ->
            getResultValues lookupTable expression
                |> Maybe.map (Result.map (\ranges -> { ranges = ranges, throughLambdaFunction = True }))

        _ ->
            Nothing


getResultValues : ModuleNameLookupTable -> Node Expression -> Maybe (Result Range (List Range))
getResultValues lookupTable baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Ok")) :: arg :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Ok [ { start = justRange.start, end = (Node.range arg).start } ])

                _ ->
                    Nothing

        Expression.OperatorApplication "|>" _ arg (Node justRange (Expression.FunctionOrValue _ "Ok")) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Ok [ { start = (Node.range arg).end, end = justRange.end } ])

                _ ->
                    Nothing

        Expression.OperatorApplication "<|" _ (Node justRange (Expression.FunctionOrValue _ "Ok")) arg ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Ok [ { start = justRange.start, end = (Node.range arg).start } ])

                _ ->
                    Nothing

        Expression.Application ((Node justRange (Expression.FunctionOrValue _ "Err")) :: arg :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Err { start = justRange.start, end = (Node.range arg).start })

                _ ->
                    Nothing

        Expression.OperatorApplication "|>" _ arg (Node justRange (Expression.FunctionOrValue _ "Err")) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Err { start = (Node.range arg).end, end = justRange.end })

                _ ->
                    Nothing

        Expression.OperatorApplication "<|" _ (Node justRange (Expression.FunctionOrValue _ "Err")) arg ->
            case ModuleNameLookupTable.moduleNameAt lookupTable justRange of
                Just [ "Result" ] ->
                    Just (Err { start = justRange.start, end = (Node.range arg).start })

                _ ->
                    Nothing

        Expression.LetExpression { expression } ->
            getResultValues lookupTable expression

        Expression.ParenthesizedExpression expression ->
            getResultValues lookupTable expression

        Expression.IfBlock _ thenBranch elseBranch ->
            combineResultValues lookupTable [ thenBranch, elseBranch ]

        Expression.CaseExpression { cases } ->
            combineResultValues lookupTable (List.map Tuple.second cases)

        _ ->
            Nothing


combineResultValues : ModuleNameLookupTable -> List (Node Expression) -> Maybe (Result Range (List Range))
combineResultValues lookupTable nodes =
    case nodes of
        node :: restOfNodes ->
            case getResultValues lookupTable node of
                Nothing ->
                    Nothing

                Just nodeValue ->
                    combineResultValuesHelp lookupTable restOfNodes nodeValue

        [] ->
            Nothing


combineResultValuesHelp : ModuleNameLookupTable -> List (Node Expression) -> Result Range (List Range) -> Maybe (Result Range (List Range))
combineResultValuesHelp lookupTable nodes soFar =
    case nodes of
        node :: restOfNodes ->
            case getResultValues lookupTable node of
                Nothing ->
                    Nothing

                Just nodeValue ->
                    case ( nodeValue, soFar ) of
                        ( Ok _, Err _ ) ->
                            Nothing

                        ( Err _, Ok _ ) ->
                            Nothing

                        ( Err _, Err soFarRange ) ->
                            combineResultValuesHelp lookupTable restOfNodes (Err soFarRange)

                        ( Ok a, Ok b ) ->
                            combineResultValuesHelp lookupTable restOfNodes (Ok (a ++ b))

        [] ->
            Just soFar


isAlwaysEmptyList : ModuleNameLookupTable -> Node Expression -> Bool
isAlwaysEmptyList lookupTable node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: alwaysValue :: []) ->
            case ModuleNameLookupTable.moduleNameAt lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    isEmptyList alwaysValue

                _ ->
                    False

        Expression.LambdaExpression { expression } ->
            isEmptyList expression

        _ ->
            False


isEmptyList : Node Expression -> Bool
isEmptyList node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.ListExpr [] ->
            True

        _ ->
            False


isBinaryOperation : String -> Infer.Resources a -> Node Expression -> Bool
isBinaryOperation symbol checkInfo expression =
    case expression |> Normalize.normalize checkInfo |> Node.value of
        Expression.PrefixOperator operatorSymbol ->
            operatorSymbol == symbol

        Expression.LambdaExpression lambda ->
            case lambda.args of
                -- invalid syntax
                [] ->
                    False

                [ Node _ (Pattern.VarPattern element) ] ->
                    case Node.value lambda.expression of
                        Expression.Application [ Node _ (Expression.PrefixOperator operatorSymbol), Node _ (Expression.FunctionOrValue [] argument) ] ->
                            (operatorSymbol == symbol)
                                && (argument == element)

                        -- no simple application
                        _ ->
                            False

                [ Node _ (Pattern.VarPattern element), Node _ (Pattern.VarPattern soFar) ] ->
                    case Node.value lambda.expression of
                        Expression.Application [ Node _ (Expression.PrefixOperator operatorSymbol), Node _ (Expression.FunctionOrValue [] left), Node _ (Expression.FunctionOrValue [] right) ] ->
                            (operatorSymbol == symbol)
                                && ((left == element && right == soFar)
                                        || (left == soFar && right == element)
                                   )

                        Expression.OperatorApplication operatorSymbol _ (Node _ (Expression.FunctionOrValue [] left)) (Node _ (Expression.FunctionOrValue [] right)) ->
                            (operatorSymbol == symbol)
                                && ((left == element && right == soFar)
                                        || (left == soFar && right == element)
                                   )

                        _ ->
                            False

                -- too many/unsimplified patterns
                _ ->
                    False

        -- not a known simple operator function
        _ ->
            False



-- LIST HELPERS


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


findMapNeighboring : (a -> Maybe b) -> List a -> Maybe { before : Maybe a, found : b, after : Maybe a }
findMapNeighboring tryMap list =
    findMapNeighboringAfter Nothing tryMap list


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
