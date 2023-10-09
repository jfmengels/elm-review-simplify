module SimplifyTest exposing (all)

import Review.Test
import Simplify exposing (defaults, ignoreCaseOfForTypes, rule)
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleExpectingNaN, ruleWithDefaults)


all : Test
all =
    describe "Simplify"
        [ configurationTests
        , qualifyTests
        , lambdaReduceTests
        , identityTests
        , alwaysTests
        , toFloatTests
        , roundTests
        , ceilingTests
        , floorTests
        , truncateTests
        , booleanTests
        , caseOfTests
        , booleanCaseOfTests
        , ifTests
        , duplicatedIfTests
        , recordUpdateTests
        , numberTests
        , fullyAppliedPrefixOperatorTests
        , appliedLambdaTests
        , usingPlusPlusTests
        , tupleTests
        , stringSimplificationTests
        , listSimplificationTests
        ]



-- CONFIGURATION


configurationTests : Test
configurationTests =
    describe "Configuration"
        [ test "should not report configuration error if all ignored constructors exist" <|
            \() ->
                """module A exposing (..)
type B = B
type C = C
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe", "Result.Result" ] defaults)
                    |> Review.Test.expectNoErrors
        , test "should report configuration error if passed an invalid module name" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "_.B" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `_.B`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report configuration error if passed an invalid type name" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "A.f" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `A.f`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report configuration error if passed an empty type name" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: ``"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report configuration error if passed a type name without a module name" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "B" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `B`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report configuration error if passed multiple invalid types" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "_.B", "A.f", "B", "Maybe.Maybe" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `A.f`, `B`, `_.B`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should report global error if ignored types were not found in the project" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "A.B", "B.C" ] defaults)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Could not find type names: `A.B`, `B.C`"
                          , details =
                                [ "I expected to find these custom types in the dependencies, but I could not find them."
                                , "Please check whether these types and have not been removed, and if so, remove them from the configuration of this rule."
                                , "If you find that these types have been moved or renamed, please update your configuration."
                                , "Note that I may have provided fixes for things you didn't wish to be fixed, so you might want to undo the changes I have applied."
                                , "Also note that the configuration for this rule changed in v2.0.19: types that are custom to your project are ignored by default, so this configuration setting can only be used to avoid simplifying case expressions that use custom types defined in dependencies."
                                ]
                          }
                        ]
        , test "should not report global error if ignored type was found in the dependencies" <|
            \() ->
                """module A exposing (..)
a = 1
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe" ] defaults)
                    |> Review.Test.expectNoErrors
        ]



-- QUALIFY


qualifyTests : Test
qualifyTests =
    Test.describe "qualify"
        [ test "should respect implicit imports" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always x
"""
                        ]
        , test "should fully qualify if import missing" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x
"""
                        ]
        , test "should qualify if not exposed" <|
            \() ->
                """module A exposing (..)
import Set exposing (toList)
a = List.foldl f x << toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (toList)
a = Set.foldl f x
"""
                        ]
        , test "should not qualify if directly imported (exposed) explicitly" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a = foldl f x
"""
                        ]
        , test "should not qualify if directly imported (exposed) explicitly even if an alias exists" <|
            \() ->
                """module A exposing (..)
import Set as UniqueList exposing (foldl)
a = List.foldl f x << UniqueList.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set as UniqueList exposing (foldl)
a = foldl f x
"""
                        ]
        , test "should not qualify if directly imported from (..)" <|
            \() ->
                """module A exposing (..)
import Set exposing (..)
a = List.foldl f x << toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (..)
a = foldl f x
"""
                        ]
        , test "should not qualify if directly imported from (..) even if an alias exists" <|
            \() ->
                """module A exposing (..)
import Set as UniqueList exposing (..)
a = List.foldl f x << toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set as UniqueList exposing (..)
a = foldl f x
"""
                        ]
        , test "should qualify using alias" <|
            \() ->
                """module A exposing (..)
import Set as UniqueList
a = List.foldl f x << UniqueList.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set as UniqueList
a = UniqueList.foldl f x
"""
                        ]
        , qualifyShadowingTests
        ]


qualifyShadowingTests : Test
qualifyShadowingTests =
    Test.describe "qualify shadowing"
        [ test "should qualify if imported and exposed but shadowed by module function/value declaration" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a = List.foldl f x << Set.toList
foldl = ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a = Set.foldl f x
foldl = ()
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by module variant" <|
            \() ->
                """module A exposing (..)
a = List.head []
type MaybeExists
    = Nothing
    | Just ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Maybe.Nothing
type MaybeExists
    = Nothing
    | Just ()
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by declaration argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a foldl = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a foldl = Set.foldl f x
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by lambda argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a = \\( _, Node _ ({ foldl } :: _) ) -> List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a = \\( _, Node _ ({ foldl } :: _) ) -> Set.foldl f x
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by case pattern argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    case info of
        ( _, Node [] _ ) ->
            []

        ( _, Node _ ({ foldl } :: _) ) ->
            List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    case info of
        ( _, Node [] _ ) ->
            []

        ( _, Node _ ({ foldl } :: _) ) ->
            Set.foldl f x
"""
                        ]
        , test "should not qualify if imported and exposed and same binding only in different case pattern argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    case info of
        ( _, Node [] _ ) ->
            List.foldl f x << Set.toList

        ( _, Node _ ({ foldl } :: _) ) ->
            []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    case info of
        ( _, Node [] _ ) ->
            foldl f x

        ( _, Node _ ({ foldl } :: _) ) ->
            []
"""
                        ]
        , test "should not qualify if imported and exposed and same binding only in case pattern argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    case List.foldl f x << Set.toList of
        ( _, Node [] _ ) ->
            []
    
        ( _, Node _ ({ foldl } :: _) ) ->
            []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    case foldl f x of
        ( _, Node [] _ ) ->
            []
    
        ( _, Node _ ({ foldl } :: _) ) ->
            []
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by let declaration argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            List.foldl f x << Set.toList
    in
    doIt
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            Set.foldl f x
    in
    doIt
"""
                        ]
        , test "should not qualify if imported and exposed and same binding in argument of different let declaration" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            ()
        
        doItBetter x =
            List.foldl f x << Set.toList
    in
    doItBetter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            ()
        
        doItBetter x =
            foldl f x
    in
    doItBetter
"""
                        ]
        , test "should not qualify if imported and exposed and same binding in let declaration argument" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            ()
    in
    List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        doIt ( _, Node _ ({ foldl } :: _) ) =
            ()
    in
    foldl f x
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by let destructured binding" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        ( _, Node _ ({ foldl } :: _) ) =
            something
    in
    List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        ( _, Node _ ({ foldl } :: _) ) =
            something
    in
    Set.foldl f x
"""
                        ]
        , test "should qualify if imported and exposed but shadowed by let declaration name" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    let
        foldl init =
            FoldingMachine.foldl init
    in
    List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    let
        foldl init =
            FoldingMachine.foldl init
    in
    Set.foldl f x
"""
                        ]
        , test "should not qualify if imported and exposed and same binding in a different branches" <|
            \() ->
                """module A exposing (..)
import Set exposing (foldl)
a =
    if condition then
        \\foldl -> doIt
    else
        [ \\foldl -> doIt
        , (\\foldl -> doIt) >> (\\x -> List.foldl f x << Set.toList)
        ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set exposing (foldl)
a =
    if condition then
        \\foldl -> doIt
    else
        [ \\foldl -> doIt
        , (\\foldl -> doIt) >> (\\x -> foldl f x)
        ]
"""
                        ]
        ]



-- LAMBDA REDUCE


lambdaReduceTests : Test
lambdaReduceTests =
    describe "lambda reduce"
        [ test "should detect (\\error -> Err error) as Err function" <|
            \() ->
                """module A exposing (..)
a = Result.mapError f << (\\error -> Err error)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> error |> Err) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> error |> Err)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> Err <| error) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> Err <| error)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error sorry -> (error |> Err) <| sorry) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> \\sorry -> (error |> Err) <| sorry)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\error -> \\sorry -> (error |> Err) <| sorry) as Err function" <|
            \() ->
                -- Err in practice only takes 1 argument; this is just for testing reducing functionality
                """module A exposing (..)
a = Result.mapError f << (\\error -> \\sorry -> (error |> Err) <| sorry)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\result -> Result.mapError f result) as Result.mapError f call" <|
            \() ->
                """module A exposing (..)
a = (\\result -> Result.mapError f result) << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        , test "should detect (\\result -> \\sorry -> (result |> Result.mapError f) <| sorry) as Result.mapError f call" <|
            \() ->
                """module A exposing (..)
a = (\\result -> \\sorry -> (result |> Result.mapError f) <| sorry) << Err
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Result.mapError on an error will result in Err with the function applied to the value inside"
                            , details = [ "You can replace this call by Err with the function directly applied to the value inside the error itself." ]
                            , under = "Result.mapError"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Err << f
"""
                        ]
        ]



-- BASICS


identityTests : Test
identityTests =
    describe "Basics.identity"
        [ test "should not report identity function on its own" <|
            \() ->
                """module A exposing (..)
a = identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace identity x by x" <|
            \() ->
                """module A exposing (..)
a = identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "`identity` can be a useful function to be passed as arguments to other functions, but calling it manually with an argument is the same thing as writing the argument on its own." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace f >> identity by f" <|
            \() ->
                """module A exposing (..)
a = f >> identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace identity >> f by f" <|
            \() ->
                """module A exposing (..)
a = identity >> f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace f << identity by f" <|
            \() ->
                """module A exposing (..)
a = f << identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace identity << f by f" <|
            \() ->
                """module A exposing (..)
a = identity << f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`identity` should be removed"
                            , details = [ "Composing a function with `identity` is the same as simplify referencing the function." ]
                            , under = "identity"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        ]


alwaysTests : Test
alwaysTests =
    describe "Basics.always"
        [ test "should not report always function on its own" <|
            \() ->
                """module A exposing (..)
a = always
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report always with 1 argument" <|
            \() ->
                """module A exposing (..)
a = always x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace always x y by x" <|
            \() ->
                """module A exposing (..)
a = always x y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace always x <| y by x" <|
            \() ->
                """module A exposing (..)
a = always x <| y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace y |> always x by x" <|
            \() ->
                """module A exposing (..)
a = y |> always x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression can be replaced by the first argument given to `always`"
                            , details = [ "The second argument will be ignored because of the `always` call." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace f >> always g by always g" <|
            \() ->
                """module A exposing (..)
a = f >> always g
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always g
"""
                        ]
        , test "should replace always g << f by always g" <|
            \() ->
                """module A exposing (..)
a = always g << f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Function composed with always will be ignored"
                            , details = [ "`always` will swallow the function composed into it." ]
                            , under = "always"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always g
"""
                        ]
        ]


toFloatTests : Test
toFloatTests =
    describe "Basics.toFloat"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = toFloat
b = toFloat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify toFloat 1 to 1" <|
            \() ->
                """module A exposing (..)
a = toFloat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify toFloat -1 to -1" <|
            \() ->
                """module A exposing (..)
a = toFloat -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify toFloat 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = toFloat 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary toFloat on a literal number"
                            , details =
                                [ "A literal integer is considered a number which means it can be used as both an Int and a Float and there is no need to explicitly convert it to a Float."
                                , "You can replace this function call by the literal number."
                                ]
                            , under = "toFloat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should not report toFloat 1.2" <|
            \() ->
                """module A exposing (..)
a = toFloat 1.2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


roundTests : Test
roundTests =
    describe "Basics.round"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = round
b = round n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify round 1 to 1" <|
            \() ->
                """module A exposing (..)
a = round 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify round -1 to -1" <|
            \() ->
                """module A exposing (..)
a = round -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify round 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = round 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify round <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = round <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.round cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify round << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = round << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.round cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "round"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


ceilingTests : Test
ceilingTests =
    describe "Basics.ceiling"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = ceiling
b = ceiling n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify ceiling 1 to 1" <|
            \() ->
                """module A exposing (..)
a = ceiling 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify ceiling -1 to -1" <|
            \() ->
                """module A exposing (..)
a = ceiling -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify ceiling 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = ceiling 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify ceiling <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = ceiling <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.ceiling cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify ceiling << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = ceiling << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.ceiling cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "ceiling"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


floorTests : Test
floorTests =
    describe "Basics.floor"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = floor
b = floor n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify floor 1 to 1" <|
            \() ->
                """module A exposing (..)
a = floor 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify floor -1 to -1" <|
            \() ->
                """module A exposing (..)
a = floor -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify floor 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = floor 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify floor <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = floor <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.floor cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify floor << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = floor << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.floor cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "floor"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


truncateTests : Test
truncateTests =
    describe "Basics.truncate"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = truncate
b = truncate n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify truncate 1 to 1" <|
            \() ->
                """module A exposing (..)
a = truncate 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should simplify truncate -1 to -1" <|
            \() ->
                """module A exposing (..)
a = truncate -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -1
"""
                        ]
        , test "should simplify truncate 0x1 to 0x1" <|
            \() ->
                """module A exposing (..)
a = truncate 0x1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary integer conversion on a literal integer"
                            , details =
                                [ "A literal integer is already considered to be an Int which means converting it further is not necessary."
                                , "You can replace this function call by the literal integer."
                                ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0x1
"""
                        ]
        , test "should simplify truncate <| toFloat <| n to n" <|
            \() ->
                """module A exposing (..)
a = truncate <| toFloat <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.truncate cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.toFloat." ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify truncate << toFloat to identity" <|
            \() ->
                """module A exposing (..)
a = truncate << toFloat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.toFloat, then Basics.truncate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "truncate"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]



-- BOOLEANS


booleanTests : Test
booleanTests =
    describe "Booleans"
        [ orTests
        , andTests
        , notTests
        , equalTests
        ]


sameThingOnBothSidesDetails : String -> List String
sameThingOnBothSidesDetails value =
    [ "Based on the values and/or the context, we can determine the result. You can replace this operation by " ++ value ++ "."
    ]


orTests : Test
orTests =
    describe "||"
        [ test "should not report unsimplifiable condition" <|
            \() ->
                """module A exposing (..)
a = x || y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'True || x' to True" <|
            \() ->
                """module A exposing (..)
a = True || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(||) with any side being True will result in True"
                            , details =
                                [ "You can replace this operation by True."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "True || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'x || True' to True" <|
            \() ->
                """module A exposing (..)
a = x || True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(||) with any side being True will result in True"
                            , details =
                                [ "You can replace this operation by True."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "x || True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'False || x' to x" <|
            \() ->
                """module A exposing (..)
a = False || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary || False"
                            , details = [ "You can replace this operation by the right bool." ]
                            , under = "False ||"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x || False' to x" <|
            \() ->
                """module A exposing (..)
a = x || False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary || False"
                            , details = [ "You can replace this operation by the left bool." ]
                            , under = "|| False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should ignore parens around False" <|
            \() ->
                """module A exposing (..)
a = x || (False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary || False"
                            , details = [ "You can replace this operation by the left bool." ]
                            , under = "|| (False)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should ignore parens around True" <|
            \() ->
                """module A exposing (..)
a = (True) || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(||) with any side being True will result in True"
                            , details =
                                [ "You can replace this operation by True."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "(True) || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (True)
"""
                        ]
        , test "should simply x || x to x" <|
            \() ->
                """module A exposing (..)
a = x || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simply x || y || x to x || y" <|
            \() ->
                """module A exposing (..)
a = x || y || x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || y
"""
                        ]
        , test "should simply x || x || y to x || y" <|
            \() ->
                """module A exposing (..)
a = x || x || y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || y
"""
                        ]
        , test "should simply x || (x) || y to x || False || y" <|
            \() ->
                """module A exposing (..)
a = x || (x) || y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || (False) || y
"""
                        ]
        , test "should simply x || (x || y) to x || (False || y)" <|
            \() ->
                """module A exposing (..)
a = x || (x || y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x || (False || y)
"""
                        ]
        , test "should simply (x || y) || (z || x) to (x || y) || (z)" <|
            \() ->
                """module A exposing (..)
a = (x || y) || (z || x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (||) operator, therefore one of them can be removed."
                                ]
                            , under = " || x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 19 }, end = { row = 2, column = 24 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x || y) || (z)
"""
                        ]
        , test "should simply x && x to x" <|
            \() ->
                """module A exposing (..)
a = x && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (&&) operator, therefore one of them can be removed."
                                ]
                            , under = " && x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simply x && (x && y) to x && (True && y)" <|
            \() ->
                """module A exposing (..)
a = x && (x && y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Condition is redundant"
                            , details =
                                [ "This condition is the same as another one found on the left side of the (&&) operator, therefore one of them can be removed."
                                ]
                            , under = "x"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x && (True && y)
"""
                        ]
        ]


andTests : Test
andTests =
    describe "&&"
        [ test "should not report unsimplifiable condition" <|
            \() ->
                """module A exposing (..)
a = x && y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'True && x' to x" <|
            \() ->
                """module A exposing (..)
a = True && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary && True"
                            , details = [ "You can replace this operation by the right bool." ]
                            , under = "True &&"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x && True' to x" <|
            \() ->
                """module A exposing (..)
a = x && True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary && True"
                            , details = [ "You can replace this operation by the left bool." ]
                            , under = "&& True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'False && x' to False" <|
            \() ->
                """module A exposing (..)
a = False && x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(&&) with any side being False will result in False"
                            , details =
                                [ "You can replace this operation by False."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "False && x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'x && False' to False" <|
            \() ->
                """module A exposing (..)
a = x && False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(&&) with any side being False will result in False"
                            , details =
                                [ "You can replace this operation by False."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "x && False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


notTests : Test
notTests =
    describe "not calls"
        [ test "should simplify 'not True' to False" <|
            \() ->
                """module A exposing (..)
a = not True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'not False' to True" <|
            \() ->
                """module A exposing (..)
a = not False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be False can be replaced by True"
                            , details = [ "You can replace this call by True." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 'not (True)' to False" <|
            \() ->
                """module A exposing (..)
a = not (True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'not <| True' to False" <|
            \() ->
                """module A exposing (..)
a = not <| True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 'True |> not' to False" <|
            \() ->
                """module A exposing (..)
a = True |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify not >> not to identity" <|
            \() ->
                """module A exposing (..)
a = not >> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify a >> not >> not to a >> identity" <|
            \() ->
                """module A exposing (..)
a = a >> not >> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 17 }, end = { row = 2, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a >> identity
"""
                        ]
        , test "should simplify not >> not >> a to a" <|
            \() ->
                """module A exposing (..)
a = not >> not >> a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 15 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify not << not to identity" <|
            \() ->
                """module A exposing (..)
a = not << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify not << not << a to identity << a" <|
            \() ->
                """module A exposing (..)
a = not << not << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity << a
"""
                        ]
        , test "should simplify a << not << not to a" <|
            \() ->
                """module A exposing (..)
a = a << not << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 10 }, end = { row = 2, column = 13 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify (not >> a) << not to a" <|
            \() ->
                """module A exposing (..)
a = (not >> a) << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (a)
"""
                        ]
        , test "should not simplify (not << a) << not" <|
            \() ->
                """module A exposing (..)
a = (not << a) << not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'not (not x)' to x" <|
            \() ->
                """module A exposing (..)
a = not (not x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'x |> not |> not' to x" <|
            \() ->
                """module A exposing (..)
a = x |> not |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 17 }, end = { row = 2, column = 20 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify '(x |> not) |> not' to x" <|
            \() ->
                """module A exposing (..)
a = (x |> not) |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 19 }, end = { row = 2, column = 22 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify '(not <| x) |> not' to x" <|
            \() ->
                """module A exposing (..)
a = (not <| x) |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 19 }, end = { row = 2, column = 22 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'not x |> not' to x" <|
            \() ->
                """module A exposing (..)
a = not x |> not
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 14 }, end = { row = 2, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'not <| not x' to x" <|
            \() ->
                """module A exposing (..)
a = not <| not x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.not, then Basics.not cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.not." ]
                            , under = "not"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify 'not (b < c) to (b >= c)" <|
            \() ->
                """module A exposing (..)
a = not (b < c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `>=` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b >= c)
"""
                        ]
        , test "should simplify 'not (b <= c) to (b > c)" <|
            \() ->
                """module A exposing (..)
a = not (b <= c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `>` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b > c)
"""
                        ]
        , test "should simplify 'not (b > c) to (b <= c)" <|
            \() ->
                """module A exposing (..)
a = not (b > c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `<=` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b <= c)
"""
                        ]
        , test "should simplify 'not (b >= c) to (b < c)" <|
            \() ->
                """module A exposing (..)
a = not (b >= c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `<` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b < c)
"""
                        ]
        , test "should simplify 'not (b == c) to (b /= c)" <|
            \() ->
                """module A exposing (..)
a = not (b == c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `/=` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b /= c)
"""
                        ]
        , test "should simplify 'not (b /= c) to (b == c)" <|
            \() ->
                """module A exposing (..)
a = not (b /= c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` is used on a negatable boolean operation"
                            , details = [ "You can remove the `not` call and use `==` instead." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b == c)
"""
                        ]
        ]



-- CASE OF


caseOfTests : Test
caseOfTests =
    describe "Case of"
        [ test "should not report case of when the body of the branches are different" <|
            \() ->
                """module A exposing (..)
a = case value of
      A -> 1
      B -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace case of with a single wildcard case by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      _ -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not replace case of with a single case by the body of the case" <|
            \() ->
                """module A exposing (..)
type B = C
a = case value of
      C -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors

        -- TODO Create a project with a union with a single constructor
        --        , test "should not replace case of with a single case when the constructor is ignored" <|
        --            \() ->
        --                """module A exposing (..)
        --type B = C
        --a = case value of
        --      C -> x
        --"""
        --                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "A.B" ] <| defaults)
        --                    |> Review.Test.expectNoErrors
        , test "should replace case of with multiple cases that have the same body" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not replace case of with a single case when the constructor from a dependency is ignored" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| defaults)
                    |> Review.Test.expectNoErrors
        , test "should not replace case of with multiple cases when all constructors of ignored type are used" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      Nothing -> x
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| defaults)
                    |> Review.Test.expectNoErrors
        , test "should replace case of with multiple cases when not all constructors of ignored type are used" <|
            \() ->
                """module A exposing (..)
a = case value of
      Just _ -> x
      _ -> x
"""
                    |> Review.Test.run (rule <| ignoreCaseOfForTypes [ "Maybe.Maybe" ] <| defaults)
                    |> Review.Test.expectNoErrors
        , test "should not replace case of with a single case with ignored arguments by the body of the case" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B C) -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace case of where a pattern introduces a variable" <|
            \() ->
                """module A exposing (..)
a = case value of
      A (_) (B c) -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace boolean case of with the same body by that body" <|
            \() ->
                """module A exposing (..)
a = case value of
      True -> x
      False -> x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary case expression"
                            , details = [ "All the branches of this case expression resolve to the same value. You can remove the case expression and replace it with the body of one of the branches." ]
                            , under = "case"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace case expression that destructures a tuple by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        ( x, y ) ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "( x, y )"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let ( x, y ) = value
    in
            1
"""
                        ]
        , test "should replace case expression that destructures a record by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        { x, y } ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "{ x, y }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let { x, y } = value
    in
            1
"""
                        ]
        , test "should replace case expression that destructures a variable by a let declaration" <|
            \() ->
                """module A exposing (..)
a =
    case value of
        var ->
            1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Use a let expression to destructure data"
                            , details = [ "It is more idiomatic in Elm to use a let expression to define a new variable rather than to use pattern matching. This will also make the code less indented, therefore easier to read." ]
                            , under = "var"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    let var = value
    in
            1
"""
                        ]
        ]


booleanCaseOfMessage : String
booleanCaseOfMessage =
    "Replace `case..of` by an `if` condition"


booleanCaseOfDetails : List String
booleanCaseOfDetails =
    [ "The idiomatic way to check for a condition is to use an `if` expression."
    , "Read more about it at: https://guide.elm-lang.org/core_language.html#if-expressions"
    ]


booleanCaseOfTests : Test
booleanCaseOfTests =
    describe "Boolean case of"
        [ test "should not report pattern matches for non-boolean values" <|
            \() ->
                """module A exposing (..)
a = case thing of
      Thing -> 1
      Bar -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report pattern matches when the evaluated expression is a tuple of with a boolean" <|
            \() ->
                """module A exposing (..)
a = case ( bool1, bool2 ) of
      ( True, True ) -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report pattern matches when one of the patterns is a bool constructor (True and False)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      True -> 1
      False -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (on multiple lines)" <|
            \() ->
                """module A exposing (..)
a =
    case bool of
        True ->
            1
        False ->
            2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    if bool then 1
        else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (False and True)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      False -> 1
      True -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if not (bool) then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (True and wildcard)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      True -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches when one of the patterns is a bool constructor (False and wildcard)" <|
            \() ->
                """module A exposing (..)
a = case bool of
      False -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if not (bool) then 1
      else 2
"""
                        ]
        , test "should report pattern matches for booleans even when one of the patterns starts with `Basics.`" <|
            \() ->
                """module A exposing (..)
a = case bool of
      Basics.True -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = booleanCaseOfMessage
                            , details = booleanCaseOfDetails
                            , under = "Basics.True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = if bool then 1
      else 2
"""
                        ]
        , test "should report pattern matches for booleans even when the constructor seems to be for booleans but comes from an unknown module" <|
            \() ->
                """module A exposing (..)
a = case bool of
      OtherModule.True -> 1
      _ -> 2

b = case bool of
      OtherModule.False -> 1
      _ -> 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]



-- NUMBER


numberTests : Test
numberTests =
    describe "Number tests"
        [ plusTests
        , minusTests
        , multiplyTests
        , divisionTests
        , intDivideTests
        , negationTest
        , basicsNegateTests
        , comparisonTests
        ]


plusTests : Test
plusTests =
    describe "(+)"
        [ test "should not simplify (+) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b + 1
b = 2 + 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n + 0 to n" <|
            \() ->
                """module A exposing (..)
a = n + 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary adding 0"
                            , details = [ "You can replace this operation by the left number you added 0 to." ]
                            , under = "+ 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n + 0.0 to n" <|
            \() ->
                """module A exposing (..)
a = n + 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary adding 0"
                            , details = [ "You can replace this operation by the left number you added 0 to." ]
                            , under = "+ 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 + n to n" <|
            \() ->
                """module A exposing (..)
a = 0 + n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary adding 0"
                            , details = [ "You can replace this operation by the right number you added 0 to." ]
                            , under = "0 +"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n + (-n) to 0" <|
            \() ->
                """module A exposing (..)
a = n + (-n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Adding opposite numbers will result in 0"
                            , details = [ "Adding two numbers with an equal absolute value and an opposite sign will cancel each other out. You can replace this operation by 0." ]
                            , under = "n + (-n)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify -n + n to 0" <|
            \() ->
                """module A exposing (..)
a = -n + n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Adding opposite numbers will result in 0"
                            , details = [ "Adding two numbers with an equal absolute value and an opposite sign will cancel each other out. You can replace this operation by 0." ]
                            , under = "-n + n"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should not simplify n + (-n) to 0 when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = n + (-n) to 0
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        ]


minusTests : Test
minusTests =
    describe "(-)"
        [ test "should not simplify (-) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b - 1
b = 2 - 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n - 0 to n" <|
            \() ->
                """module A exposing (..)
a = n - 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtracting 0"
                            , details = [ "You can replace this operation by the left number you subtracted 0 from." ]
                            , under = "-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n - 0.0 to n" <|
            \() ->
                """module A exposing (..)
a = n - 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary subtracting 0"
                            , details = [ "You can replace this operation by the left number you subtracted 0 from." ]
                            , under = "-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 - n to -n" <|
            \() ->
                """module A exposing (..)
a = 0 - n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Subtracting from 0 is the same as negating"
                            , details = [ "You can replace this operation by the negated right number you subtracted from 0, like `-n`." ]
                            , under = "-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -n
"""
                        ]
        , test "should simplify 0 - List.length list to -(List.length list)" <|
            \() ->
                """module A exposing (..)
a = 0 - List.length list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Subtracting from 0 is the same as negating"
                            , details = [ "You can replace this operation by the negated right number you subtracted from 0, like `-n`." ]
                            , under = "-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = -(List.length list)
"""
                        ]
        , test "should simplify n - n to 0" <|
            \() ->
                """module A exposing (..)
a = n - n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Subtracting equal numbers will result in 0"
                            , details = [ "You can replace this operation by 0." ]
                            , under = "n - n"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should not simplify n - n to 0 when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = n - n
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        ]


multiplyTests : Test
multiplyTests =
    describe "(*)"
        [ test "should not simplify (*) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = b * 2
b = 2 * 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n * 1 to n" <|
            \() ->
                """module A exposing (..)
a = n * 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary * 1"
                            , details = [ "You can replace this operation by the left number." ]
                            , under = "* 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n * 1.0 to n" <|
            \() ->
                """module A exposing (..)
a = n * 1.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary * 1"
                            , details = [ "You can replace this operation by the left number." ]
                            , under = "* 1.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 1 * n to n" <|
            \() ->
                """module A exposing (..)
a = 1 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary * 1"
                            , details = [ "You can replace this operation by the right number." ]
                            , under = "1 *"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n * 0 by 0" <|
            \() ->
                """module A exposing (..)
a = n * 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "* 0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should report but not fix n * 0 when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = n * 0
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "* 0"
                            }
                        ]
        , test "should simplify n * 0.0 to 0" <|
            \() ->
                """module A exposing (..)
a = n * 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "* 0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should report but not fix n * 0.0 when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = n * 0.0
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "* 0.0"
                            }
                        ]
        , test "should simplify 0 * n to 0" <|
            \() ->
                """module A exposing (..)
a = 0 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "0 *"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify 0.0 * n to 0" <|
            \() ->
                """module A exposing (..)
a = 0.0 * n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Multiplication by 0 should be replaced"
                            , details =
                                [ "Multiplying by 0 will turn finite numbers into 0 and keep NaN and (-)Infinity"
                                , "Most likely, multiplying by 0 was unintentional and you had a different factor in mind."
                                , """If you do want the described behavior, though, make your intention clear for the reader
by explicitly checking for `Basics.isNaN` and `Basics.isInfinite`."""
                                , """Basics.isNaN: https://package.elm-lang.org/packages/elm/core/latest/Basics#isNaN
Basics.isInfinite: https://package.elm-lang.org/packages/elm/core/latest/Basics#isInfinite"""
                                ]
                            , under = "0.0 *"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        ]


divisionTests : Test
divisionTests =
    describe "(/)"
        [ test "should not simplify (/) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = 1 / 2
b = 2 / 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n / 1 to n" <|
            \() ->
                """module A exposing (..)
a = n / 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary dividing by 1"
                            , details = [ "You can replace this operation by the left number you divided by 1." ]
                            , under = "/"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n / 1.0 to n" <|
            \() ->
                """module A exposing (..)
a = n / 1.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary dividing by 1"
                            , details = [ "You can replace this operation by the left number you divided by 1." ]
                            , under = "/"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify 0 / n to 0" <|
            \() ->
                """module A exposing (..)
a = 0 / n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dividing 0 will result in 0"
                            , details =
                                [ "Dividing 0 by anything, even infinite numbers, gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                                ]
                            , under = "/"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify 0.0 / n to 0.0" <|
            \() ->
                """module A exposing (..)
a = 0.0 / n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dividing 0 will result in 0"
                            , details =
                                [ "Dividing 0 by anything, even infinite numbers, gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                                ]
                            , under = "/"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0.0
"""
                        ]
        , test "should report but not fix 0 / 0" <|
            \() ->
                """module A exposing (..)
a = 0 / 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                            , details =
                                [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                                , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                                , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                                , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                                ]
                            , under = "/"
                            }
                        ]
        , test "should report but not fix 0 / 0.0" <|
            \() ->
                """module A exposing (..)
a = 0 / 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                            , details =
                                [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                                , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                                , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                                , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                                ]
                            , under = "/"
                            }
                        ]
        , test "should report but not fix 0.0 / 0" <|
            \() ->
                """module A exposing (..)
a = 0.0 / 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                            , details =
                                [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                                , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                                , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                                , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                                ]
                            , under = "/"
                            }
                        ]
        , test "should report but not fix 0.0 / 0.0" <|
            \() ->
                """module A exposing (..)
a = 0.0 / 0.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "0 / 0 is NaN but the configuration option expectNaN is not enabled"
                            , details =
                                [ "Dividing 0 by 0 is the simplest way to obtain a NaN value in elm. NaN is a special Float value that signifies a failure of a mathematical operation and tends to spread through code."
                                , "By default, Simplify assumes that your code does not expect NaN values so it can enable a few more checks. If creating NaN here was not your intention, replace this division by a more fitting number like 0."
                                , "If you do want to use NaN here, please add expectNaN to your Simplify configuration to let it know NaN is a possible value in your code."
                                , "expectNaN: https://package.elm-lang.org/packages/jfmengels/elm-review-simplify/latest/Simplify#expectNaN"
                                ]
                            , under = "/"
                            }
                        ]
        ]


intDivideTests : Test
intDivideTests =
    describe "(//)"
        [ test "should not simplify (//) used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = 1 // 2
b = 2 // 3
c = n // n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify n // 1 to n" <|
            \() ->
                """module A exposing (..)
a = n // 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary dividing by 1"
                            , details = [ "You can replace this operation by the left integer you divided by 1." ]
                            , under = "//"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify n // m // 1 to n // m" <|
            \() ->
                """module A exposing (..)
a = n // m // 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary dividing by 1"
                            , details = [ "You can replace this operation by the left integer you divided by 1." ]
                            , under = "//"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 12 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n // m
"""
                        ]
        , test "should simplify 0 // n to 0" <|
            \() ->
                """module A exposing (..)
a = 0 // n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dividing 0 will result in 0"
                            , details =
                                [ "Dividing 0 by anything using (//), even 0, gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing 0 was unintentional and you had a different number in mind."
                                ]
                            , under = "//"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should simplify n // 0 to 0" <|
            \() ->
                """module A exposing (..)
a = n // 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dividing by 0 will result in 0"
                            , details =
                                [ "Dividing anything by 0 using (//) gives 0 which means you can replace the whole division operation by 0."
                                , "Most likely, dividing by 0 was unintentional and you had a different number in mind."
                                ]
                            , under = "//"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        ]


negationTest : Test
negationTest =
    describe "Unary negation"
        [ test "should not report negation used in okay situations" <|
            \() ->
                """module A exposing (..)
a = -1
a = -(-1 + 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify -(-n) to n" <|
            \() ->
                """module A exposing (..)
a = -(-n)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            , under = "-(-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = n
"""
                        ]
        , test "should simplify -(-(f n)) to (f n)" <|
            \() ->
                """module A exposing (..)
a = -(-(f n))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary double number negation"
                            , details = [ "Negating a number twice is the same as the number itself." ]
                            , under = "-(-"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n)
"""
                        ]
        ]


basicsNegateTests : Test
basicsNegateTests =
    describe "Basics.negate"
        [ test "should simplify negate >> negate to identity" <|
            \() ->
                """module A exposing (..)
a = negate >> negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 15 }, end = { row = 2, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify a >> negate >> negate to a >> identity" <|
            \() ->
                """module A exposing (..)
a = a >> negate >> negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 20 }, end = { row = 2, column = 26 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a >> identity
"""
                        ]
        , test "should simplify negate >> negate >> a to a" <|
            \() ->
                """module A exposing (..)
a = negate >> negate >> a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 15 }, end = { row = 2, column = 21 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify negate << negate to identity" <|
            \() ->
                """module A exposing (..)
a = negate << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify negate << negate << a to identity << a" <|
            \() ->
                """module A exposing (..)
a = negate << negate << a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity << a
"""
                        ]
        , test "should simplify a << negate << negate to a" <|
            \() ->
                """module A exposing (..)
a = a << negate << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 10 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should simplify (negate >> a) << negate to a" <|
            \() ->
                """module A exposing (..)
a = (negate >> a) << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 6 }, end = { row = 2, column = 12 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (a)
"""
                        ]
        , test "should negate simplify (negate << a) << negate" <|
            \() ->
                """module A exposing (..)
a = (negate << a) << negate
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify 'negate <| negate x' to x" <|
            \() ->
                """module A exposing (..)
a = negate <| negate x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Basics.negate, then Basics.negate cancels each other out"
                            , details = [ "You can replace this call by the argument given to Basics.negate." ]
                            , under = "negate"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 11 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        ]


comparisonTests : Test
comparisonTests =
    describe "Comparison operators"
        [ lessThanTests
        ]


lessThanTests : Test
lessThanTests =
    describe "<"
        [ test "should simplify 1 < 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 < 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "1 < 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 1 < 2 + 3 to False" <|
            \() ->
                """module A exposing (..)
a = 1 < 2 + 3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "1 < 2 + 3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify 2 < 1 to False" <|
            \() ->
                """module A exposing (..)
a = 2 < 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "2 < 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 > 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 > 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 > 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 >= 2 to False" <|
            \() ->
                """module A exposing (..)
a = 1 >= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(>=) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 >= 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify 1 <= 2 to True" <|
            \() ->
                """module A exposing (..)
a = 1 <= 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(<=) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "1 <= 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        ]


equalTests : Test
equalTests =
    describe "(==)"
        [ test "should not simplify values that can't be determined" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify x == True to x" <|
            \() ->
                """module A exposing (..)
a = x == True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "== True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify x == False" <|
            \() ->
                """module A exposing (..)
a = x == False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify True == x to x" <|
            \() ->
                """module A exposing (..)
a = True == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "True =="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify False == x" <|
            \() ->
                """module A exposing (..)
a = False == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify x /= True" <|
            \() ->
                """module A exposing (..)
a = x /= True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify x /= False to x" <|
            \() ->
                """module A exposing (..)
a = x /= False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "/= False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should not simplify True /= x" <|
            \() ->
                """module A exposing (..)
a = True /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify False /= x to x" <|
            \() ->
                """module A exposing (..)
a = False /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary comparison with boolean"
                            , details = [ "The result of the expression will be the same with or without the comparison." ]
                            , under = "False /="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify not x == not y to x == y" <|
            \() ->
                """module A exposing (..)
a = not x == not y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (==)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "=="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x == y
"""
                        ]
        , test "should simplify (x |> f |> not) == (y |> g |> not) to (x |> f) == (y |> g)" <|
            \() ->
                """module A exposing (..)
a = (x |> f |> not) == (y |> g |> not)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (==)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "=="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x |> f) == (y |> g)
"""
                        ]
        , test "should simplify not x /= not y to x /= y" <|
            \() ->
                """module A exposing (..)
a = not x /= not y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary `not` on both sides of (/=)"
                            , details = [ "You can replace the bool on each side by the value given to `not`." ]
                            , under = "/="
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x /= y
"""
                        ]
        , test "should simplify x == x to True" <|
            \() ->
                """module A exposing (..)
a = x == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify x == x when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = x == x
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectNoErrors
        , test "should simplify x == (x) to True" <|
            \() ->
                """module A exposing (..)
a = x == (x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == (x)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify x /= x to False" <|
            \() ->
                """module A exposing (..)
a = x /= x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(/=) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x /= x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify more complex calls (function call and lambda)" <|
            \() ->
                """module A exposing (..)
a = List.map (\\a -> a.value) things == List.map (\\a -> a.value) things
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "List.map (\\a -> a.value) things == List.map (\\a -> a.value) things"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with a single arg that use `<|`" <|
            \() ->
                """module A exposing (..)
a = (f b) == (f <| b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b) == (f <| b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with multiple args that use `<|`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (f b <| c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (f b <| c)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with a single arg that use `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b) == (b |> f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b) == (b |> f)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify calls with multiple args that use `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (c |> f b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (c |> f b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`" <|
            \() ->
                """module A exposing (..)
a = (f b c) == (c |> (b |> f))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(f b c) == (c |> (b |> f))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in let expression" <|
            \() ->
                """module A exposing (..)
a = (let x = 1 in f b c) == (c |> (let x = 1 in f b))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(let x = 1 in f b c) == (c |> (let x = 1 in f b))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in if expression" <|
            \() ->
                """module A exposing (..)
a = (if cond then f b c else g d c) == (c |> (if cond then f b else g d))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(if cond then f b c else g d c) == (c |> (if cond then f b else g d))"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify nested function calls using `|>`, even when function is wrapped in case expression" <|
            \() ->
                """module A exposing (..)
a = (case x of
        X -> f b c
        Y -> g d c
    )
    ==
    ((case x of
        X -> f b
        Y -> g d
    ) <| c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = """(case x of
        X -> f b c
        Y -> g d c
    )
    ==
    ((case x of
        X -> f b
        Y -> g d
    ) <| c)"""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record access comparison" <|
            \() ->
                """module A exposing (..)
a = (b.c) == (.c b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(b.c) == (.c b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record access comparison using pipeline" <|
            \() ->
                """module A exposing (..)
a = (b.c) == (.c <| b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(b.c) == (.c <| b)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different literals to False" <|
            \() ->
                """module A exposing (..)
a = "a" == "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "\"a\" == \"b\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different char literals to False" <|
            \() ->
                """module A exposing (..)
a = 'a' == 'b'
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "'a' == 'b'"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify inequality of different literal comparisons to True" <|
            \() ->
                """module A exposing (..)
a = "a" /= "b"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(/=) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "\"a\" /= \"b\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different number literal comparisons to False" <|
            \() ->
                """module A exposing (..)
a = 1 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (integer left)" <|
            \() ->
                """module A exposing (..)
a = 1 == 2.0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 == 2.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (float left)" <|
            \() ->
                """module A exposing (..)
a = 1.0 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1.0 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (hex left)" <|
            \() ->
                """module A exposing (..)
a = 0x10 == 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "0x10 == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (addition left)" <|
            \() ->
                """module A exposing (..)
a = 1 + 3 == 2 + 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 + 3 == 2 + 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (subtraction left)" <|
            \() ->
                """module A exposing (..)
a = 1 - 3 == 2 - 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 - 3 == 2 - 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (multiplication left)" <|
            \() ->
                """module A exposing (..)
a = 2 * 3 == 2 * 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "2 * 3 == 2 * 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of different integer and float comparisons to False (division left)" <|
            \() ->
                """module A exposing (..)
a = 1 / 3 == 2 / 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "1 / 3 == 2 / 5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of same value (() left)" <|
            \() ->
                """module A exposing (..)
a = () == x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "() == x"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of same value (() right)" <|
            \() ->
                """module A exposing (..)
a = x == ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == ()"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of lists (different lengths)" <|
            \() ->
                """module A exposing (..)
a = [ 1 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "[ 1 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of lists (same lengths but different values)" <|
            \() ->
                """module A exposing (..)
a = [ 1, 2 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "[ 1, 2 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of lists (same values)" <|
            \() ->
                """module A exposing (..)
a = [ 1, 2 - 1 ] == [ 1, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "[ 1, 2 - 1 ] == [ 1, 1 ]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify equality of different integers comparisons to False (wrapped in parens)" <|
            \() ->
                """module A exposing (..)
a = (1) == (2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "(1) == (2)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of tuples" <|
            \() ->
                """module A exposing (..)
a = ( 1, 2 ) == ( 1, 1 )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "( 1, 2 ) == ( 1, 1 )"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of records" <|
            \() ->
                """module A exposing (..)
a = { a = 1, b = 2 } == { b = 1, a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ a = 1, b = 2 } == { b = 1, a = 1 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of record updates with same base values and different field values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | a = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ x | a = 1 } == { x | a = 2 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should simplify equality of record updates with same base values and field values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "{ x | a = 1 } == { x | a = 1 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify equality of record updates with same base values and different fields" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { x | b = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify equality of record updates (different base values)" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "{ x | a = 1 } == { y | a = 2 }"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should not simplify equality of record updates with same field values but different base values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify equality of record updates with non-corresponding fields but otherwise similar field values and different base values" <|
            \() ->
                """module A exposing (..)
a = { x | a = 1 } == { y | a = 1, b = 2 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify comparison of values for which we don't know if they're equal" <|
            \() ->
                """module A exposing (..)
a = x == y
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should normalize module names" <|
            \() ->
                [ """module A exposing (..)
import B exposing (b)
a = B.b == b
""", """module Other exposing (..)
b = 1
""" ]
                    |> Review.Test.runOnModules ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "(==) comparison will result in True"
                                , details = sameThingOnBothSidesDetails "True"
                                , under = "B.b == b"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B exposing (b)
a = True
"""
                            ]
                          )
                        ]
        , test "should simplify function calls with the same function and similar arguments" <|
            \() ->
                """module A exposing (..)
import List exposing (map)
a = List.map fn 1 == map fn (2 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "List.map fn 1 == map fn (2 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import List exposing (map)
a = True
"""
                        ]
        , test "should not simplify function calls of the same function but with different arguments" <|
            \() ->
                """module A exposing (..)
import List exposing (map)
a = List.map fn 1 == List.map fn 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify if expressions that look like each other" <|
            \() ->
                """module A exposing (..)
a = (if 1 then 2 else 3) == (if 2 - 1 then 3 - 1 else 4 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(if 1 then 2 else 3) == (if 2 - 1 then 3 - 1 else 4 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify if expressions that don't look like each other" <|
            \() ->
                """module A exposing (..)
a = (if a then 2 else 3) == (if a then 1 else 2)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify negations that look like each other" <|
            \() ->
                """module A exposing (..)
a = -1 == -(2 - 1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "-1 == -(2 - 1)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should simplify record accesses that look like each other" <|
            \() ->
                """module A exposing (..)
a = ({ a = 1 }).a == ({ a = 2 - 1 }).a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "({ a = 1 }).a == ({ a = 2 - 1 }).a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        , Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 37 }, end = { row = 2, column = 39 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ({ a = 1 }).a == (2 - 1)
"""
                        , Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 16 }, end = { row = 2, column = 18 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1 == ({ a = 2 - 1 }).a
"""
                        ]
        , test "should simplify operator expressions" <|
            \() ->
                """module A exposing (..)
a = (1 |> fn) == (2 - 1 |> fn)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "(1 |> fn) == (2 - 1 |> fn)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not simplify with different fields" <|
            \() ->
                """module A exposing (..)
a = ({ a = 1 }).a == ({ a = 1 }).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".a"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1 == ({ a = 1 }).b
"""
                        ]
        ]



-- IF


ifTests : Test
ifTests =
    describe "if expressions"
        [ test "should remove the else branch when a condition is True" <|
            \() ->
                """module A exposing (..)
a = if True then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should remove the if branch when a condition is False" <|
            \() ->
                """module A exposing (..)
a = if False then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 2
"""
                        ]
        , test "should not remove anything if the condition is not statically knowable" <|
            \() ->
                """module A exposing (..)
a = if condition then 1 else 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should only keep the condition if then is True and else is False" <|
            \() ->
                """module A exposing (..)
a = if condition then True else False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The if expression's value is the same as the condition"
                            , details = [ "The expression can be replaced by the condition." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = condition
"""
                        ]
        , test "should only keep the negated condition if then is False and else is True" <|
            \() ->
                """module A exposing (..)
a = if condition then False else True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The if expression's value is the inverse of the condition"
                            , details = [ "The expression can be replaced by the condition wrapped by `not`." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = not condition
"""
                        ]
        , test "should replace the expression by the branch if both branches have the same value" <|
            \() ->
                """module A exposing (..)
a = if condition then x else x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The values in both branches is the same."
                            , details = [ "The expression can be replaced by the contents of either branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace the expression by the branch if both branches are known to be equal" <|
            \() ->
                """module A exposing (..)
a = if condition then 1 else 2 - 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The values in both branches is the same."
                            , details = [ "The expression can be replaced by the contents of either branch." ]
                            , under = "if"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        ]



-- DUPLICATED IF CONDITIONS


duplicatedIfTests : Test
duplicatedIfTests =
    describe "Duplicated if conditions"
        [ test "should not remove nested conditions if they're not duplicate" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove duplicate nested conditions (x inside the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (x inside the then, with parens)" <|
            \() ->
                """module A exposing (..)
a =
  if (x) then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if (x) then
    1
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (not x inside the top condition)" <|
            \() ->
                """module A exposing (..)
a =
  if not x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not x then
    2
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (not <| x inside the top condition)" <|
            \() ->
                """module A exposing (..)
a =
  if not <| x then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not <| x then
    2
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (not x inside the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if not x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be True can be replaced by False"
                            , details = [ "You can replace this call by False." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove duplicate nested conditions (x inside the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (not x inside the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    1
  else
    if not x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be False can be replaced by True"
                            , details = [ "You can replace this call by True." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    1
  else
    if True then
      2
    else
      3
"""
                        ]
        , test "should remove duplicate nested conditions (x part of a nested condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if x && y then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary && True"
                            , details = [ "You can replace this operation by the right bool." ]
                            , under = "x &&"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove duplicate deeply nested conditions" <|
            \() ->
                """module A exposing (..)
a =
  if x then
    if y then
      if x then
        1
      else
        2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x then
    if y then
      1
    else
      3
  else
    4
"""
                        ]
        , test "should remove duplicate nested conditions (x part of the top && condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x && y then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x && y then
    1
  else
    3
"""
                        ]
        , test "should remove opposite nested conditions (x part of the top || condition)" <|
            \() ->
                """module A exposing (..)
a =
  if x || y then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x || y then
    1
  else
    3
"""
                        ]
        , test "should not remove condition when we don't have enough information (x part of the top && condition, other condition in the else)" <|
            \() ->
                """module A exposing (..)
a =
  if x && y then
    1
  else
    if x then
      2
    else
      3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not remove condition when we don't have enough information (x part of the top || condition, other condition in the then)" <|
            \() ->
                """module A exposing (..)
a =
  if x || y then
    if x then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove branches where the condition always matches" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    if x == 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == 1"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 8 }, end = { row = 4, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    if True then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (strings == with different values)" <|
            \() ->
                """module A exposing (..)
a =
  if x == "a" then
    if x == "b" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x == \"b\""
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 8 }, end = { row = 4, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == "a" then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (not function or value)" <|
            \() ->
                """module A exposing (..)
a =
  if item.name == "Aged Brie" then
    if item.name == "Sulfuras, Hand of Ragnaros" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "item.name == \"Sulfuras, Hand of Ragnaros\""
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if item.name == "Aged Brie" then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    if x == 2 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x == 2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    if False then
      1
    else
      2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (strings)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= "a" then
    if x == "a" then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= "a" then
    2
  else
    3
"""
                        ]
        , test "should not spread inferred things from one branch to another" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    1
  else if x == 2 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove branches where the condition always matches (/=)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    if x /= 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (/= in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    1
  else if x /= 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(/=) comparison will result in False"
                            , details = sameThingOnBothSidesDetails "False"
                            , under = "x /= 1"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 11 }, end = { row = 5, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else if False then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (== in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition always matches (/= <then> == in else)" <|
            \() ->
                """module A exposing (..)
a =
  if x /= 1 then
    if x == 1 then
      1
    else
      2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x /= 1 then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (literal on the left using ==, second if)" <|
            \() ->
                """module A exposing (..)
a =
  if x == 1 then
    1
  else if 1 == x then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if x == 1 then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition never matches (literal on the left using ==, first if)" <|
            \() ->
                """module A exposing (..)
a =
  if 1 == x then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if 1 == x then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition always matches (literal on the left using /=)" <|
            \() ->
                """module A exposing (..)
a =
  if 1 /= x then
    1
  else if x == 1 then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(==) comparison will result in True"
                            , details = sameThingOnBothSidesDetails "True"
                            , under = "x == 1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if 1 /= x then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition never matches (&&)" <|
            \() ->
                """module A exposing (..)
a =
  if a && b then
    1
  else if a && b then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a && b then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> a)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if a then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 8 }, end = { row = 5, column = 10 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else 3
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> a && b)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if a && b then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "(&&) with any side being False will result in False"
                            , details =
                                [ "You can replace this operation by False."
                                , "Maybe you have hardcoded a value or mistyped a condition?"
                                ]
                            , under = "a && b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else if a then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition may not match (a && b --> a --> b)" <|
            \() ->
                """module A exposing (..)
a =
  if a && b then
    1
  else if a then
    if b then
      2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to False"
                            , details = [ "The expression can be replaced by what is inside the 'else' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a && b then
    1
  else if a then
    3
  else
    4
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> <then> a --> b)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    if not a then
      if b then
        1
      else
        2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    if not a then
      1
    else
      3
  else
    4
"""
                        ]
        , test "should remove branches where the condition may not match (a || b --> not a)" <|
            \() ->
                """module A exposing (..)
a =
  if a || b then
    1
  else if not a then
    2
  else
    3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "`not` on a bool known to be False can be replaced by True"
                            , details = [ "You can replace this call by True." ]
                            , under = "not"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if a || b then
    1
  else if True then
    2
  else
    3
"""
                        ]
        , test "should remove branches where the condition may not match (not (a || b) --> not a --> not b)" <|
            \() ->
                -- TODO Probably best to normalize inside Evaluate.getBoolean?
                """module A exposing (..)
a =
  if not (a || b) then
    1
  else if not a then
    if b then
      2
    else
      3
  else
    4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The condition will always evaluate to True"
                            , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
                            , under = "if"
                            }
                            |> Review.Test.atExactly { start = { row = 6, column = 5 }, end = { row = 6, column = 7 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  if not (a || b) then
    1
  else if not a then
    2
  else
    4
"""
                        ]

        --        ,   test "should not lose information as more conditions add up" <|
        --                \() ->
        --                    """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a == f b then
        --      if a == 1 then
        --        1
        --      else
        --        2
        --    else
        --      3
        --  else
        --    4
        --"""
        --                        |> Review.Test.run ruleWithDefaults
        --                        |> Review.Test.expectErrors
        --                            [ Review.Test.error
        --                                { message = "The condition will always evaluate to True"
        --                                , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
        --                                , under = "if"
        --                                }
        --                                |> Review.Test.atExactly { start = { row = 4, column = 5 }, end = { row = 4, column = 7 } }
        --                                |> Review.Test.whenFixed """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a == 1 then
        --        1
        --      else
        --        2
        --  else
        --    4
        --"""
        --                            , Review.Test.error
        --                                { message = "The condition will always evaluate to True"
        --                                , details = [ "The expression can be replaced by what is inside the 'then' branch." ]
        --                                , under = "if"
        --                                }
        --                                |> Review.Test.atExactly { start = { row = 5, column = 7 }, end = { row = 5, column = 9 } }
        --                                |> Review.Test.whenFixed """module A exposing (..)
        --a =
        --  if a == 1 then
        --    if a /= 2 then
        --      1
        --    else
        --      3
        --  else
        --    4
        --"""
        --                            ]
        -- TODO
        -- Unhappy && and || cases:
        --   if a && b then ... else <not a || not b>
        --   if a || b then ... else <not a && not b>
        ]



-- RECORD UPDATE


recordUpdateTests : Test
recordUpdateTests =
    describe "Record update"
        [ test "should not simplify when assigning a different field or a value" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = b.c, e = c.e, f = g b.f, g = b.g.h }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not simplify when assigning a field in a non-update record assignment" <|
            \() ->
                """module A exposing (..)
a = { d = b.d, c = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should remove the updates that assigns the previous value of a field to itself (first)" <|
            \() ->
                """module A exposing (..)
a = { b | d = b.d, c = 1 }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1 }
"""
                        ]
        , test "should remove the update record syntax when it assigns the previous value of a field to itself and it is the only assignment" <|
            \() ->
                """module A exposing (..)
a = { b | d = b.d }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should remove the updates that assigns the previous value of a field to itself (not first)" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = b.d }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1}
"""
                        ]
        , test "should remove the updates that assigns the previous value of a field to itself (using parens)" <|
            \() ->
                """module A exposing (..)
a = { b | c = 1, d = (b.d) }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary field assignment"
                            , details = [ "The field is being set to its own value." ]
                            , under = "b.d"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = { b | c = 1}
"""
                        ]
        ]



-- FULLY APPLIED PREFIX OPERATOR


fullyAppliedPrefixOperatorMessage : String
fullyAppliedPrefixOperatorMessage =
    "Use the infix form (a + b) over the prefix form ((+) a b)"


fullyAppliedPrefixOperatorDetails : List String
fullyAppliedPrefixOperatorDetails =
    [ "The prefix form is generally more unfamiliar to Elm developers, and therefore it is nicer when the infix form is used."
    ]


fullyAppliedPrefixOperatorTests : Test
fullyAppliedPrefixOperatorTests =
    describe "Fully applied prefix operators"
        [ test "should not report a lonely operator" <|
            \() ->
                """
module A exposing (..)
a = (++)
b = (::)
c = (//)
d = (+)
e = (/)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report an operator used in infix position" <|
            \() ->
                """
module A exposing (..)
a = y ++ z
b = y :: z
c = y // z
d = y + z
e = y / z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report an operator used in prefix position with one argument" <|
            \() ->
                """
module A exposing (..)
a = (++) z
b = (::) z
c = (//) z
d = (+) z
e = (/) z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace (++) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (++) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y ++ z
"""
                        ]
        , test "should replace (::) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (::) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(::)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y :: z
"""
                        ]
        , test "should replace (//) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (//) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(//)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y // z
"""
                        ]
        , test "should replace (+) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (+) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(+)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y + z
"""
                        ]
        , test "should replace (/) used with both arguments in prefix position by an infix operator expression" <|
            \() ->
                """module A exposing (..)
a = (/) y z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(/)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = y / z
"""
                        ]
        , test "should replace infix operator with 2 arguments, used on several lines" <|
            \() ->
                """module A exposing (..)
a =
    (++)
        y
        z
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    y
        ++ z
"""
                        ]
        , test "should replace infix operator with 2 arguments wrapped in parens and braces" <|
            \() ->
                """module A exposing (..)
a =
    (++) (y + 1)
        [z]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = fullyAppliedPrefixOperatorMessage
                            , details = fullyAppliedPrefixOperatorDetails
                            , under = "(++)"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    (y + 1)
        ++ [z]
"""
                        ]
        ]


appliedLambdaTests : Test
appliedLambdaTests =
    describe "Applied lambda functions"
        [ test "should not report okay function calls" <|
            \() ->
                """module A exposing (..)
a = f ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace (\\() -> x) () by x" <|
            \() ->
                """module A exposing (..)
a = (\\() -> x) ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace (\\_ -> x) a by x" <|
            \() ->
                """module A exposing (..)
a = (\\_ -> x) a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary wildcard argument argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace (\\() y -> x) () by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\() y -> x) ()
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary unit argument"
                            , details =
                                [ "This function is expecting a unit, but also passing it directly."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "()"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 9 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x)
"""
                        ]
        , test "should replace (\\_ y -> x) a by (\\y -> x)" <|
            \() ->
                """module A exposing (..)
a = (\\_ y -> x) a
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary wildcard argument argument"
                            , details =
                                [ "This function is being passed an argument that is directly ignored."
                                , "Maybe this was made in attempt to make the computation lazy, but in practice the function will be evaluated eagerly."
                                ]
                            , under = "_"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 7 }, end = { row = 2, column = 8 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\y -> x)
"""
                        ]
        , test "should report but not fix non-simplifiable lambdas that are directly called with an argument" <|
            \() ->
                """module A exposing (..)
a = (\\x y -> x + y) n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report but not fix non-simplifiable lambdas that are directly called in a |> pipeline" <|
            \() ->
                """module A exposing (..)
a = n |> (\\x y -> x + y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report lambdas that are directly called in a |> pipeline if the argument is a pipeline itself" <|
            \() ->
                """module A exposing (..)
a = n |> f |> (\\x y -> x + y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report but not fix non-simplifiable lambdas that are directly called in a <| pipeline" <|
            \() ->
                """module A exposing (..)
a = (\\x y -> x + y) <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report lambdas that are directly called in a <| pipeline if the argument is a pipeline itself" <|
            \() ->
                """module A exposing (..)
a = (\\x y -> x + y) <| f <| n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]



-- (++)


usingPlusPlusTests : Test
usingPlusPlusTests =
    describe "(++)"
        [ test "should not report a single list literal" <|
            \() ->
                """module A exposing (..)
a = []
b = [1]
c = [ "string", "foo", "bar" ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report simple strings" <|
            \() ->
                """module A exposing (..)
a = "abc" ++ value
b = \"\"\"123\"\"\"
c = \"\"\"multi
line
string
\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace "a" ++ "" by "a\"""" <|
            \() ->
                """module A exposing (..)
a = "a" ++ ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending \"\""
                            , details = [ "You can replace this operation by the left string." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a"
"""
                        ]
        , test """should not report x ++ "" (because this can lead to better performance)""" <|
            \() ->
                """module A exposing (..)
a = x ++ ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace "" ++ "a" by "a\"""" <|
            \() ->
                """module A exposing (..)
a = "" ++ "a"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending \"\""
                            , details = [ "You can replace this operation by the right string." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "a"
"""
                        ]
        , test "should report concatenating two list literals" <|
            \() ->
                """module A exposing (..)
a = [ 1 ] ++ [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1 , 2, 3 ]
"""
                        ]
        , test "should report concatenating two list literals, even they contain variables" <|
            \() ->
                """module A exposing (..)
a = [ a, 1 ] ++ [ b, 2 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a, 1 , b, 2 ]
"""
                        ]
        , test "should report concatenating [] and something" <|
            \() ->
                """module A exposing (..)
a = [] ++ something
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending []"
                            , details = [ "You can replace this operation by the right list." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something
"""
                        ]
        , test "should report concatenating something and []" <|
            \() ->
                """module A exposing (..)
a = something ++ []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary appending []"
                            , details = [ "You can replace this operation by the left list." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = something
"""
                        ]
        , test "should replace [b] ++ c by b :: c" <|
            \() ->
                """module A exposing (..)
a = [ b ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending a singleton list to the beginning is the same as using (::) with the value inside"
                            , details = [ "You can replace this (++) operation by using (::) with the value inside the left singleton list on the right list." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b :: c
"""
                        ]
        , test "should replace [ f n ] ++ c by (f n) :: c" <|
            \() ->
                """module A exposing (..)
a = [ f n ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Appending a singleton list to the beginning is the same as using (::) with the value inside"
                            , details = [ "You can replace this (++) operation by using (::) with the value inside the left singleton list on the right list." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n) :: c
"""
                        ]
        , test "should not replace [b] ++ c when on the right of a ++ operator" <|
            \() ->
                """module A exposing (..)
a = left ++ [ b ] ++ c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace [b] ++ c when on the right of a ++ operator but inside parens" <|
            \() ->
                """module A exposing (..)
a = left ++ ([ b ] ++ c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.fromList [ b, c ] ++ String.fromList [ d, e ] by String.fromList [ b, c , d, e ]" <|
            \() ->
                """module A exposing (..)
a = String.fromList [ b, c ] ++ String.fromList [ d, e ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "++ on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "++"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromList [ b, c , d, e ]
"""
                        ]
        ]



-- STRING


stringSimplificationTests : Test
stringSimplificationTests =
    describe "String"
        [ stringToListTests
        , stringFromListTests
        , stringIsEmptyTests
        , stringLengthTests
        , stringConcatTests
        , stringJoinTests
        , stringRepeatTests
        , stringReplaceTests
        , stringWordsTests
        , stringLinesTests
        , stringAppendTests
        , stringReverseTests
        , stringSliceTests
        , stringRightTests
        , stringLeftTests
        , stringFoldlTests
        , stringFoldrTests
        ]


stringIsEmptyTests : Test
stringIsEmptyTests =
    describe "String.isEmpty"
        [ test "should not report String.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty
b = String.isEmpty value
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.isEmpty \"\" by True" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on \"\" will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace String.isEmpty \"a\" by False" <|
            \() ->
                """module A exposing (..)
a = String.isEmpty "a"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.isEmpty on this string will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "String.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


stringLengthTests : Test
stringLengthTests =
    describe "String.length"
        [ test "should not report String.length that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.length
b = String.length str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.length \"\" by 0" <|
            \() ->
                """module A exposing (..)
a = String.length ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 0"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace String.length \"abc\" by 3" <|
            \() ->
                """module A exposing (..)
a = String.length "abc"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 3"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace String.length \"a\\t🚀b\\c🇲🇻\\u{000D}\\r\" by 13" <|
            \() ->
                """module A exposing (..)
a = String.length "a\\t🚀b\\\\c🇲🇻\\u{000D}\\r"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 13"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = 13
"""
                        ]
        , test "should replace String.length \"\"\"a\\t🚀b\\c🇲🇻\\u{000D}\\r\"\"\" by 13" <|
            \() ->
                """module A exposing (..)
a = String.length \"\"\"a\\t🚀b\\\\c🇲🇻\\u{000D}\\r\"\"\"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the string is 13"
                            , details = [ "The length of the string can be determined by looking at the code." ]
                            , under = "String.length"
                            }
                            |> Review.Test.whenFixed
                                """module A exposing (..)
a = 13
"""
                        ]
        ]


stringConcatTests : Test
stringConcatTests =
    describe "String.concat"
        [ test "should not report String.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.concat list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.concat [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.concat []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.concat on [] will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.concat (List.repeat n str) by (String.repeat n str)" <|
            \() ->
                """module A exposing (..)
a = String.concat (List.repeat n str)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat, then String.concat can be combined into String.repeat"
                            , details = [ "You can replace this call by String.repeat with the same arguments given to List.repeat which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.repeat n str)
"""
                        ]
        , test "should replace str |> List.repeat n |> String.concat by str |> String.repeat n" <|
            \() ->
                """module A exposing (..)
a = str |> List.repeat n |> String.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat, then String.concat can be combined into String.repeat"
                            , details = [ "You can replace this call by String.repeat with the same arguments given to List.repeat which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str |> String.repeat n
"""
                        ]
        , test "should replace String.concat << List.repeat n by String.repeat n" <|
            \() ->
                """module A exposing (..)
a = String.concat << List.repeat n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat, then String.concat can be combined into String.repeat"
                            , details = [ "You can replace this composition by String.repeat with the same arguments given to List.repeat which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.repeat n
"""
                        ]
        , test "should replace String.concat (List.intersperse str strings) by (String.join str strings)" <|
            \() ->
                """module A exposing (..)
a = String.concat (List.intersperse str strings)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse, then String.concat can be combined into String.join"
                            , details = [ "You can replace this call by String.join with the same arguments given to List.intersperse which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.join str strings)
"""
                        ]
        , test "should replace str |> List.intersperse str |> String.concat by str |> String.join str" <|
            \() ->
                """module A exposing (..)
a = str |> List.intersperse str |> String.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse, then String.concat can be combined into String.join"
                            , details = [ "You can replace this call by String.join with the same arguments given to List.intersperse which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str |> String.join str
"""
                        ]
        , test "should replace String.concat << List.intersperse str by String.join str" <|
            \() ->
                """module A exposing (..)
a = String.concat << List.intersperse str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse, then String.concat can be combined into String.join"
                            , details = [ "You can replace this composition by String.join with the same arguments given to List.intersperse which is meant for this exact purpose." ]
                            , under = "String.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.join str
"""
                        ]
        ]


stringJoinTests : Test
stringJoinTests =
    describe "String.join"
        [ test "should not report String.join that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.join b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.join b [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.join b []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.join on [] will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.join "" list by String.concat list""" <|
            \() ->
                """module A exposing (..)
a = String.join "" list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.join with separator \"\" is the same as String.concat"
                            , details = [ "You can replace this call by String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat list
"""
                        ]
        , test """should replace String.join "" by String.concat""" <|
            \() ->
                """module A exposing (..)
a = String.join ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.join with separator \"\" is the same as String.concat"
                            , details = [ "You can replace this call by String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.concat
"""
                        ]
        , test """should replace list |> String.join "" by list |> String.concat""" <|
            \() ->
                """module A exposing (..)
a = list |> String.join ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.join with separator \"\" is the same as String.concat"
                            , details = [ "You can replace this call by String.concat." ]
                            , under = "String.join"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> String.concat
"""
                        ]
        ]


stringRepeatTests : Test
stringRepeatTests =
    describe "String.repeat"
        [ test "should not report String.repeat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.repeat n str
b = String.repeat 5 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.repeat n "" by \"\"""" <|
            \() ->
                """module A exposing (..)
a = String.repeat n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat with \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.repeat 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.repeat 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat with length 0 will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test """should replace String.repeat 0 by (always "")""" <|
            \() ->
                """module A exposing (..)
a = String.repeat 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat with length 0 will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.repeat -5 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.repeat -5 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat with negative length will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.repeat 1 str by str" <|
            \() ->
                """module A exposing (..)
a = String.repeat 1 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat 1 will always return the same given string to repeat"
                            , details = [ "You can replace this call by the string to repeat itself." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = str
"""
                        ]
        , test "should replace String.repeat 1 by identity" <|
            \() ->
                """module A exposing (..)
a = String.repeat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.repeat 1 will always return the same given string to repeat"
                            , details = [ "You can replace this call by identity." ]
                            , under = "String.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


stringReplaceTests : Test
stringReplaceTests =
    describe "String.replace"
        [ test "should not report String.replace that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.replace n str
b = String.replace 5 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.replace n n by identity" <|
            \() ->
                """module A exposing (..)
a = String.replace n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace where the pattern to replace and the replacement are equal will always return the same given string"
                            , details = [ "You can replace this call by identity." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.replace n n x by x" <|
            \() ->
                """module A exposing (..)
a = String.replace n n x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace where the pattern to replace and the replacement are equal will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace String.replace x y \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.replace x y ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.replace x y z by z when we know what the value will be and that it is unchanged" <|
            \() ->
                """module A exposing (..)
a = String.replace "x" "y" "z"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace with a pattern not present in the given string will result in the given string"
                            , details = [ "You can replace this call by the given string itself." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "z"
"""
                        ]
        , test "should replace z |> String.replace x y z by z when we know what the value will be and that it is unchanged" <|
            \() ->
                """module A exposing (..)
a = "z" |> String.replace "x" "y"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.replace with a pattern not present in the given string will result in the given string"
                            , details = [ "You can replace this call by the given string itself." ]
                            , under = "String.replace"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = "z"
"""
                        ]
        , test "should not replace String.replace x y z by z when we know what the value will be but it will be different" <|
            \() ->
                """module A exposing (..)
a = String.replace "x" "y" "xz"
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not remove String.replace call when the replacement is \u{000D}" <|
            \() ->
                """module A exposing (..)
a = \"\"\"
foo
bar
\"\"\"
                    |> String.replace "\\r" ""

b = \"\"\"
foo
bar
\"\"\"
                    |> String.replace "\\u{000D}" ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


stringWordsTests : Test
stringWordsTests =
    describe "String.words"
        [ test "should not report String.words that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.words
b = String.words str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.words "" by []""" <|
            \() ->
                """module A exposing (..)
a = String.words ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.words on \"\" will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "String.words"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


stringLinesTests : Test
stringLinesTests =
    describe "String.lines"
        [ test "should not report String.lines that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = String.lines
b = String.lines str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test """should replace String.lines "" by []""" <|
            \() ->
                """module A exposing (..)
a = String.lines ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.lines on \"\" will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "String.lines"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


stringToListTests : Test
stringToListTests =
    describe "String.toList"
        [ test "should not report String.toList that contains a variable" <|
            \() ->
                """module A exposing (..)
a = String.toList
b = String.toList str
c = String.toList << f << String.fromList
d = (String.toList << f) << String.fromList
e = String.toList << (f << String.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace x |> f |> String.fromList |> String.toList by x |> f" <|
            \() ->
                """module A exposing (..)
a = x |> f |> String.fromList |> String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can replace this call by the argument given to String.fromList." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f
"""
                        ]
        , test "should replace String.toList << String.fromList by identity" <|
            \() ->
                """module A exposing (..)
a = String.toList << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.toList << (String.fromList << f) by (f)" <|
            \() ->
                """module A exposing (..)
a = String.toList << (String.fromList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace String.toList << (String.fromList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
a = String.toList << (String.fromList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g << f)
"""
                        ]
        , test "should replace String.toList << (f >> String.fromList) by (f)" <|
            \() ->
                """module A exposing (..)
a = String.toList << (f >> String.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace String.toList << (f >> g >> String.fromList) by (f >> g)" <|
            \() ->
                """module A exposing (..)
a = String.toList << (f >> g >> String.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f >> g)
"""
                        ]
        , test "should replace (f << String.toList) << String.fromList by (f)" <|
            \() ->
                """module A exposing (..)
a = (f << String.toList) << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (g << f << String.toList) << String.fromList by (g << f)" <|
            \() ->
                """module A exposing (..)
a = (g << f << String.toList) << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g << f)
"""
                        ]
        , test "should replace (String.toList >> f) << String.fromList by (f)" <|
            \() ->
                """module A exposing (..)
a = (String.toList >> f) << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (String.toList >> f >> g) << String.fromList by (f >> g)" <|
            \() ->
                """module A exposing (..)
a = (String.toList >> f >> g) << String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList, then String.toList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.toList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f >> g)
"""
                        ]
        ]


stringFromListTests : Test
stringFromListTests =
    describe "String.fromList"
        [ test "should not report String.fromList that contains a variable" <|
            \() ->
                """module A exposing (..)
a = String.fromList
b = String.fromList list
c = String.fromList << f << String.toList
d = (String.fromList << f) << String.toList
e = String.fromList << (f << String.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.fromList [] by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.fromList []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on [] will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.fromList [ char ] by String.fromChar char" <|
            \() ->
                """module A exposing (..)
a = String.fromList [ char ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on a singleton list will result in String.fromChar with the value inside"
                            , details = [ "You can replace this call by String.fromChar with the value inside the singleton list." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar char
"""
                        ]
        , test "should replace String.fromList [ f a ] by String.fromChar (f a)" <|
            \() ->
                """module A exposing (..)
a = String.fromList [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on a singleton list will result in String.fromChar with the value inside"
                            , details = [ "You can replace this call by String.fromChar with the value inside the singleton list." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar (f b)
"""
                        ]
        , test "should replace String.fromList (List.singleton char) by String.fromChar char" <|
            \() ->
                """module A exposing (..)
a = String.fromList (List.singleton char)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on a singleton list will result in String.fromChar with the value inside"
                            , details = [ "You can replace this call by String.fromChar with the value inside the singleton list." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar char
"""
                        ]
        , test "should replace List.singleton >> String.fromList by String.fromChar" <|
            \() ->
                """module A exposing (..)
a = List.singleton >> String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.fromList on a singleton list will result in String.fromChar with the value inside"
                            , details = [ "You can replace this call by String.fromChar." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar
"""
                        ]
        , test "should replace x |> f |> String.toList |> String.fromList by x |> f" <|
            \() ->
                """module A exposing (..)
a = x |> f |> String.toList |> String.fromList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can replace this call by the argument given to String.toList." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> f
"""
                        ]
        , test "should replace String.fromList << String.toList by identity" <|
            \() ->
                """module A exposing (..)
a = String.fromList << String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace String.fromList << (String.toList << f) by (f)" <|
            \() ->
                """module A exposing (..)
a = String.fromList << (String.toList << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace String.fromList << (String.toList << g << f) by (g << f)" <|
            \() ->
                """module A exposing (..)
a = String.fromList << (String.toList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (g << f)
"""
                        ]
        , test "should replace (i << h << String.fromList) << (String.toList << g << f) by (i << h) << (g << f)" <|
            \() ->
                """module A exposing (..)
a = (i << h << String.fromList) << (String.toList << g << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (i << h) << (g << f)
"""
                        ]
        , test "should replace (String.fromList >> h >> i) << (f >> g >> String.toList) by (h >> i) << (f >> g)" <|
            \() ->
                """module A exposing (..)
a = (String.fromList >> h >> i) << (f >> g >> String.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (h >> i) << (f >> g)
"""
                        ]
        , test "should replace (i << (h << String.fromList)) << ((String.toList << g) << f) by (i << (h)) << ((g) << f)" <|
            \() ->
                """module A exposing (..)
a = (i << (h << String.fromList)) << ((String.toList << g) << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (i << (h)) << ((g) << f)
"""
                        ]
        , test "should replace String.fromList << (f >> String.toList) by (f)" <|
            \() ->
                """module A exposing (..)
a = String.fromList << (f >> String.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (f << String.fromList) << String.toList by (f)" <|
            \() ->
                """module A exposing (..)
a = (f << String.fromList) << String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (String.fromList >> f) << String.toList by (f)" <|
            \() ->
                """module A exposing (..)
a = (String.fromList >> f) << String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should replace (String.fromList >> f >> g) << String.toList by (f >> g)" <|
            \() ->
                """module A exposing (..)
a = (String.fromList >> f >> g) << String.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.toList, then String.fromList cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.fromList"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f >> g)
"""
                        ]
        ]


stringAppendTests : Test
stringAppendTests =
    describe "String.append"
        [ test "should not report String.append used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = String.append string1 string2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.append \"\" string by string" <|
            \() ->
                """module A exposing (..)
a = String.append "" string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append \"\" will always return the same given string"
                            , details = [ "You can replace this call by the string itself." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should replace String.append string \"\" by string" <|
            \() ->
                """module A exposing (..)
a = String.append string ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.append with \"\""
                            , details = [ "You can replace this call by the given first string." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should replace \"\" |> String.append string by string" <|
            \() ->
                """module A exposing (..)
a = "" |> String.append string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.append with \"\""
                            , details = [ "You can replace this call by the given first string." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should replace string |> String.append \"\" by string" <|
            \() ->
                """module A exposing (..)
a = "" |> String.append string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary String.append with \"\""
                            , details = [ "You can replace this call by the given first string." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = string
"""
                        ]
        , test "should report String.append applied on two string literals" <|
            \() ->
                """module A exposing (..)
a = String.append (String.fromList [b,c]) (String.fromList [d,e])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromList [b,c,d,e])
"""
                        ]
        , test "should report String.append applied on two string literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = String.append (String.fromList [ b, z ]) (String.fromList [c,d,0])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromList [ b, z ,c,d,0])
"""
                        ]
        , test "should report String.append <| on two string literals" <|
            \() ->
                """module A exposing (..)
a = String.append (String.fromList [b, c]) <| String.fromList [d,e]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromList [b, c,d,e]
"""
                        ]
        , test "should report String.append |> on two string literals" <|
            \() ->
                """module A exposing (..)
a = String.fromList [d,e] |> String.append (String.fromList [b,c])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromList [b,c,d,e]
"""
                        ]
        , test "should report String.append |> on two string literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = String.fromList [c,d,0] |> String.append (String.fromList [ b, z ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromList [ b, z ,c,d,0]
"""
                        ]
        , test "should replace String.append ([ b, c ] |> String.fromList) (String.fromList [ d, e ]) by (String.fromList [ b, c, d, e ])" <|
            \() ->
                """module A exposing (..)
a = String.append ([ b, c ] |> String.fromList) (String.fromList [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromList [ b, c , d, e ])
"""
                        ]
        , test "should replace String.append ([ b, c ] |> String.fromList) (String.fromList <| [ d, e ]) by (String.fromList <| [ b, c, d, e ])" <|
            \() ->
                """module A exposing (..)
a = String.append ([ b, c ] |> String.fromList) (String.fromList <| [ d, e ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromList <| [ b, c , d, e ])
"""
                        ]
        , test "should replace String.append (String.fromList <| [ b, c ]) ([ d, e ] |> String.fromList) by ([ b, c , d, e ] |> String.fromList)" <|
            \() ->
                """module A exposing (..)
a = String.append (String.fromList <| [ b, c ]) ([ d, e ] |> String.fromList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ([ b, c , d, e ] |> String.fromList)
"""
                        ]
        , test "should replace [ d, e ] |> String.fromList |> String.append (String.fromList <| [ b, c ]) by [ b, c , d, e ] |> String.fromList" <|
            \() ->
                """module A exposing (..)
a = [ d, e ] |> String.fromList |> String.append (String.fromList <| [ b, c ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.append on String.fromList calls can be turned into a single String.fromList call"
                            , details = [ "Try moving all the elements into a single String.fromList call." ]
                            , under = "String.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b, c , d, e ] |> String.fromList
"""
                        ]
        ]


stringReverseTests : Test
stringReverseTests =
    describe "String.reverse"
        [ test "should not report String.reverse with okay arguments" <|
            \() ->
                """module A exposing (..)
a = String.reverse
b = String.reverse str
c = (String.reverse << f) << String.reverse
d = String.reverse << (f << String.reverse)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.reverse \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.reverse ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.reverse (String.fromChar a) by (String.fromChar a)" <|
            \() ->
                """module A exposing (..)
a = String.reverse (String.fromChar b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on a single-char string will result in the given single-char string"
                            , details = [ "You can replace this call by the given single-char string." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (String.fromChar b)
"""
                        ]
        , test "should replace a |> String.fromChar |> String.reverse by a |> String.fromChar" <|
            \() ->
                """module A exposing (..)
a = b |> String.fromChar |> String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on a single-char string will result in the given single-char string"
                            , details = [ "You can replace this call by the given single-char string." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> String.fromChar
"""
                        ]
        , test "should replace String.reverse << String.fromChar by String.fromChar" <|
            \() ->
                """module A exposing (..)
a = String.reverse << String.fromChar
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on a single-char string will result in the unchanged single-char string"
                            , details = [ "You can replace this composition by String.fromChar." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar
"""
                        ]
        , test "should replace String.fromChar >> String.reverse by String.fromChar" <|
            \() ->
                """module A exposing (..)
a = String.fromChar >> String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse on a single-char string will result in the unchanged single-char string"
                            , details = [ "You can replace this composition by String.fromChar." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = String.fromChar
"""
                        ]
        , test "should replace String.reverse <| String.reverse <| x by x" <|
            \() ->
                """module A exposing (..)
a = String.reverse <| String.reverse <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse, then String.reverse cancels each other out"
                            , details = [ "You can replace this call by the argument given to String.reverse." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should simplify String.reverse >> String.reverse to identity" <|
            \() ->
                """module A exposing (..)
a = String.reverse >> String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse, then String.reverse cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 23 }, end = { row = 2, column = 37 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify (f << String.reverse) << String.reverse to (f)" <|
            \() ->
                """module A exposing (..)
a = (f << String.reverse) << String.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse, then String.reverse cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 25 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should simplify String.reverse << (String.reverse << f) to (f)" <|
            \() ->
                """module A exposing (..)
a = String.reverse << (String.reverse << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.reverse, then String.reverse cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "String.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        ]


stringSliceTests : Test
stringSliceTests =
    describe "String.slice"
        [ test "should not report String.slice that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.slice b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report String.slice 0 n" <|
            \() ->
                """module A exposing (..)
a = String.slice 0
b = String.slice 0 n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.slice b 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice b 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with end index 0 will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice b 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice b 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with end index 0 will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice n n by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice n n
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with equal start and end index will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice n n str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice n n str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with equal start and end index will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice a z \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice a z ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.slice with natural start >= natural end by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice 2 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with a start index greater than the end index will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.slice with negative start >= negative end by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.slice -1 -2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.slice with a negative start index closer to the right than the negative end index will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.slice"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should not report String.slice with negative start, natural end" <|
            \() ->
                """module A exposing (..)
a = String.slice -1 2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        []
        , test "should not report String.slice with natural start, negative end" <|
            \() ->
                """module A exposing (..)
a = String.slice 1 -2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        []
        ]


stringLeftTests : Test
stringLeftTests =
    describe "String.left"
        [ test "should not report String.left that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.left b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.left 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.left with length 0 will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.left 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.left with length 0 will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.left -literal by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.left with negative length will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.left n \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.left n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.left on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.left"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        ]


stringRightTests : Test
stringRightTests =
    describe "String.right"
        [ test "should not report String.right that contains variables or expressions" <|
            \() ->
                """module A exposing (..)
a = String.right b c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.right 0 str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right 0 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right with length 0 will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.right 0 by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right with length 0 will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.right -literal str by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right -1 str
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right with negative length will always result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        , test "should replace String.right -literal by always \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right with negative length will always result in \"\""
                            , details = [ "You can replace this call by always \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always ""
"""
                        ]
        , test "should replace String.right n \"\" by \"\"" <|
            \() ->
                """module A exposing (..)
a = String.right n ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.right on \"\" will result in \"\""
                            , details = [ "You can replace this call by \"\"." ]
                            , under = "String.right"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ""
"""
                        ]
        ]


stringFoldlTests : Test
stringFoldlTests =
    describe "String.foldl"
        [ test "should not report String.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = String.foldl
b = String.foldl (\\el soFar -> soFar - el)
c = String.foldl (\\el soFar -> soFar - el) 20
d = String.foldl (\\el soFar -> soFar - el) 20 string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.foldl f initial \"\" by initial" <|
            \() ->
                """module A exposing (..)
a = String.foldl f initial ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldl on \"\" will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "String.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial
"""
                        ]
        , test "should replace String.foldl (always identity) initial string by initial" <|
            \() ->
                """module A exposing (..)
a = String.foldl (always identity) initial string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "String.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial
"""
                        ]
        , test "should replace String.foldl (always identity) initial by always initial" <|
            \() ->
                """module A exposing (..)
a = String.foldl (always identity) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "String.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always initial
"""
                        ]
        , test "should replace String.foldl (always identity) by always" <|
            \() ->
                """module A exposing (..)
a = String.foldl (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which string is supplied next." ]
                            , under = "String.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always
"""
                        ]
        ]


stringFoldrTests : Test
stringFoldrTests =
    describe "String.foldr"
        [ test "should not report String.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = String.foldr
b = String.foldr (\\el soFar -> soFar - el)
c = String.foldr (\\el soFar -> soFar - el) 20
d = String.foldr (\\el soFar -> soFar - el) 20 string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace String.foldr f initial \"\" by initial" <|
            \() ->
                """module A exposing (..)
a = String.foldr f initial ""
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldr on \"\" will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "String.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial
"""
                        ]
        , test "should replace String.foldr (always identity) initial string by initial" <|
            \() ->
                """module A exposing (..)
a = String.foldr (always identity) initial string
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "String.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial
"""
                        ]
        , test "should replace String.foldr (always identity) initial by always initial" <|
            \() ->
                """module A exposing (..)
a = String.foldr (always identity) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "String.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always initial
"""
                        ]
        , test "should replace String.foldr (always identity) by always" <|
            \() ->
                """module A exposing (..)
a = String.foldr (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "String.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which string is supplied next." ]
                            , under = "String.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always
"""
                        ]
        ]



-- LIST


listSimplificationTests : Test
listSimplificationTests =
    describe "List"
        [ usingConsTests
        , listAppendTests
        , usingListConcatTests
        , listConcatMapTests
        , listHeadTests
        , listTailTests
        , listMemberTests
        , listMapTests
        , listFilterTests
        , listFilterMapTests
        , listIndexedMapTests
        , listIsEmptyTests
        , listSumTests
        , listProductTests
        , listMinimumTests
        , listMaximumTests
        , listFoldlTests
        , listFoldrTests
        , listAllTests
        , listAnyTests
        , listRangeTests
        , listLengthTests
        , listRepeatTests
        , listPartitionTests
        , listSortTests
        , listSortByTests
        , listSortWithTests
        , listReverseTests
        , listTakeTests
        , listDropTests
        , listIntersperseTests
        , listMap2Tests
        , listMap3Tests
        , listMap4Tests
        , listMap5Tests
        , listUnzipTests
        ]


usingConsTests : Test
usingConsTests =
    describe "(::)"
        [ test "should not report using :: to a variable or expression" <|
            \() ->
                """module A exposing (..)
a = 1 :: list
b = 1 :: foo bar
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report using :: to a list literal" <|
            \() ->
                """module A exposing (..)
a = 1::[ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1, 2, 3 ]
"""
                        ]
        , test
            "should report using :: to a list literal, between is additional white space"
          <|
            \() ->
                """module A exposing (..)
a =
  1


    ::     [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  [ 1


    ,      2, 3 ]
"""
                        ]
        , test "should report using :: to a list literal, between is additional white space and different comments" <|
            \() ->
                """module A exposing (..)
a =
  1
    {- -- comment {- nested -} here we go! -}
    -- important
    ::     [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  [ 1
    {- -- comment {- nested -} here we go! -}
    -- important
    ,      2, 3 ]
"""
                        ]
        , test "should report using :: to a list literal, between is additional white space and different comments that use the operator symbol" <|
            \() ->
                """module A exposing (..)
a =
  1
    {- -- comment {-
     nested :: -} here we :: go!
         -}
    -- important: ::
    ::
    -- why is there a rabbit in here ::)
           [ 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.atExactly
                                { start = { row = 8, column = 5 }
                                , end = { row = 8, column = 7 }
                                }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
  [ 1
    {- -- comment {-
     nested :: -} here we :: go!
         -}
    -- important: ::
    ,
    -- why is there a rabbit in here ::)
            2, 3 ]
"""
                        ]
        , test "should report using :: to [] literal" <|
            \() ->
                """module A exposing (..)
a = 1 :: []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "Try moving the element inside the list it is being added to." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ 1 ]
"""
                        ]
        , test "should replace a :: (List.singleton <| b + c) by [ a, b + c ]" <|
            \() ->
                """module A exposing (..)
a = a
    :: (List.singleton <| b + c)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Element added to the beginning of the list could be included in the list"
                            , details = [ "You can replace this operation by a list that contains both the added element and the value inside the singleton list." ]
                            , under = "::"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a, b + c ]
"""
                        ]
        ]


listAppendTests : Test
listAppendTests =
    describe "List.append"
        [ test "should not report List.append with a list variable" <|
            \() ->
                """module A exposing (..)
a = List.append
b = List.append list
c = List.append [ 1 ]
d = List.append [ 1 ] list
e = List.append list [ 1 ]
f = List.append list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.append applied on two list literals" <|
            \() ->
                """module A exposing (..)
a = List.append [b] [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append applied on two list literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = List.append [ b, z ] [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b, z ,c,d,0]
"""
                        ]
        , test "should report List.append <| on two list literals" <|
            \() ->
                """module A exposing (..)
a = List.append [b] <| [c,d,0]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append |> on two list literals" <|
            \() ->
                """module A exposing (..)
a = [c,d,0] |> List.append [b]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [b,c,d,0]
"""
                        ]
        , test "should report List.append |> on two list literals (multiple elements)" <|
            \() ->
                """module A exposing (..)
a = [c,d,0] |> List.append [ b, z ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append on list literals can be turned into a single list literal"
                            , details = [ "Try moving all the elements into a single list literal." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b, z ,c,d,0]
"""
                        ]
        , test "should replace List.append [] list by list" <|
            \() ->
                """module A exposing (..)
a = List.append [] list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append [] will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.append [] <| list by list" <|
            \() ->
                """module A exposing (..)
a = List.append [] <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append [] will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace list |> List.append [] by list" <|
            \() ->
                """module A exposing (..)
a = list |> List.append []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append [] will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.append [] by identity" <|
            \() ->
                """module A exposing (..)
a = List.append []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.append [] will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.append list [] by list" <|
            \() ->
                """module A exposing (..)
a = List.append list []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.append with []"
                            , details = [ "You can replace this call by the given first list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.append list <| [] by list" <|
            \() ->
                """module A exposing (..)
a = List.append list <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.append with []"
                            , details = [ "You can replace this call by the given first list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace [] |> List.append list by list" <|
            \() ->
                """module A exposing (..)
a = [] |> List.append list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.append with []"
                            , details = [ "You can replace this call by the given first list." ]
                            , under = "List.append"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        ]


usingListConcatTests : Test
usingListConcatTests =
    describe "List.concat"
        [ test "should not report List.concat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.concat [ foo, bar ]
b = List.concat [ [ 1 ], foo ]
c = List.concat [ foo, [ 1 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.concat with no items" <|
            \() ->
                """module A exposing (..)
a = List.concat []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should report List.concat with a single item" <|
            \() ->
                """module A exposing (..)
a = List.concat [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat with a single item, using (<|)" <|
            \() ->
                """module A exposing (..)
a = List.concat <| [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should report List.concat with a single item, using (|>)" <|
            \() ->
                """module A exposing (..)
a = [ b ] |> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b
"""
                        ]
        , test "should replace List.concat << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = List.concat << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should report List.concat that only contains list literals" <|
            \() ->
                """module A exposing (..)
a = List.concat [ [ 1, 2, 3 ], [ 4, 5, 6] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [  1, 2, 3 ,  4, 5, 6 ]
"""
                        ]
        , test "should report List.concat that only contains list literals, using (<|)" <|
            \() ->
                """module A exposing (..)
a = List.concat <| [ [ 1, 2, 3 ], [ 4, 5, 6 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [  1, 2, 3 ,  4, 5, 6  ]
"""
                        ]
        , test "should report List.concat that only contains list literals, using (|>)" <|
            \() ->
                """module A exposing (..)
a = [ [ 1, 2, 3 ], [ 4, 5, 6 ] ] |> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Expression could be simplified to be a single List"
                            , details = [ "Try moving all the elements into a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [  1, 2, 3 ,  4, 5, 6  ]
"""
                        ]
        , test "should concatenate consecutive list literals in passed to List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concat [ a, [ 0 ], b, [ 1, 2, 3 ], [ 4, 5, 6], [7], c, [8], [9 ] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Consecutive literal lists can be merged"
                            , details = [ "Try moving all the elements from consecutive list literals so that they form a single list." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ a, [ 0 ], b, [ 1, 2, 3 ,  4, 5, 6, 7], c, [8, 9 ] ]
"""
                        ]
        , test "should remove empty list literals passed to List.concat (last item)" <|
            \() ->
                """module A exposing (..)
a = List.concat [ a, [] ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a list containing an irrelevant []"
                            , details = [ "Including [] in the list does not change the result of this call. You can remove the [] element." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ a ]
"""
                        ]
        , test "should remove empty list literals passed to List.concat (first item)" <|
            \() ->
                """module A exposing (..)
a = List.concat [ [], b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concat on a list containing an irrelevant []"
                            , details = [ "Including [] in the list does not change the result of this call. You can remove the [] element." ]
                            , under = "[]"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat [ b ]
"""
                        ]
        , test "should replace List.concat (List.map f x) by List.concatMap f x" <|
            \() ->
                """module A exposing (..)
a = List.concat (List.map f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this call by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.concatMap f x)
"""
                        ]
        , test "should replace List.concat <| List.map f <| x by List.concatMap f <| x" <|
            \() ->
                """module A exposing (..)
a = List.concat <| List.map f <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this call by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f <| x
"""
                        ]
        , test "should replace x |> List.map f |> List.concat by x |> List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = x |> List.map f |> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this call by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x |> List.concatMap f
"""
                        ]
        ]


listConcatMapTests : Test
listConcatMapTests =
    describe "List.concatMap"
        [ test "should replace List.concatMap identity x by List.concat x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with an identity function is the same as List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat x
"""
                        ]
        , test "should replace List.concatMap identity by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with an identity function is the same as List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat
"""
                        ]
        , test "should replace List.concatMap (\\x->x) by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\x->x) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with an identity function is the same as List.concat"
                            , details = [ "You can replace this call by List.concat." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concat x
"""
                        ]
        , test "should not report List.concatMap with a non-identity lambda" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\x->y) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.concatMap without an identity function by List.concat" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should report List.concatMap with no items" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should not report List.concatMap f [ a, [], b ]" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f [ a, [], b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.concatMap (always []) x by []" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (always []) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that will always return [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.concatMap (always []) by always []" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (always [])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that will always return [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.concatMap List.singleton x by x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap List.singleton x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function equivalent to List.singleton will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.concatMap List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = List.concatMap List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function equivalent to List.singleton will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.concatMap (\\_ -> [a]) x by List.map (\\_ -> a) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> ([a])) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that always returns a singleton list is the same as List.map with the function returning the value inside"
                            , details = [ "You can replace this call by List.map with the function returning the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ -> a) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> List.singleton a) x by List.map (\\_ -> a) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> List.singleton a) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that always returns a singleton list is the same as List.map with the function returning the value inside"
                            , details = [ "You can replace this call by List.map with the function returning the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ -> a) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> if cond then [a] else [b]) x by List.map (\\_ -> if cond then a else b) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap (\\_ -> if cond then [a] else [b]) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that always returns a singleton list is the same as List.map with the function returning the value inside"
                            , details = [ "You can replace this call by List.map with the function returning the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\_ -> if cond then a else b) x
"""
                        ]
        , test "should replace List.concatMap (\\_ -> case y of A -> [a] ; B -> [b]) x by List.map (\\_ -> case y of A -> a ; B -> b) x" <|
            \() ->
                """module A exposing (..)
a = List.concatMap
    (\\_ ->
        case y of
            A -> [a]
            B -> [b]
    ) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap with a function that always returns a singleton list is the same as List.map with the function returning the value inside"
                            , details = [ "You can replace this call by List.map with the function returning the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map
    (\\_ ->
        case y of
            A -> a
            B -> b
    ) x
"""
                        ]
        , test "should replace List.concatMap f [ a ] by f a" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f a
"""
                        ]
        , test "should replace List.concatMap f [ b c ] by f (b c)" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f [ b c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f (b c)
"""
                        ]
        , test "should replace List.concatMap f <| [ a ] by f <| a" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f <| [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f <| a
"""
                        ]
        , test "should replace List.concatMap f <| [ b c ] by f <| (b c)" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f <| [ b c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f <| (b c)
"""
                        ]
        , test "should replace [ c ] |> List.concatMap f by c |> f" <|
            \() ->
                """module A exposing (..)
a = [ c ] |> List.concatMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c |> f
"""
                        ]
        , test "should replace [ b c ] |> List.concatMap f by (b c) |> f" <|
            \() ->
                """module A exposing (..)
a = [ b c ] |> List.concatMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this call by the function directly applied to the value inside the singleton list." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (b c) |> f
"""
                        ]
        , test "should replace List.concatMap f << List.singleton by f" <|
            \() ->
                """module A exposing (..)
a = List.concatMap f << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.concatMap on a singleton list is the same as applying the function to the value from the singleton list"
                            , details = [ "You can replace this composition by the function given to List.concatMap." ]
                            , under = "List.concatMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = f
"""
                        ]
        , test "should replace List.map f >> List.concat by List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = List.map f >> List.concat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this composition by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f
"""
                        ]
        , test "should replace List.concat << List.map f by List.concatMap f" <|
            \() ->
                """module A exposing (..)
a = List.concat << List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map, then List.concat can be combined into List.concatMap"
                            , details = [ "You can replace this composition by List.concatMap with the same arguments given to List.map which is meant for this exact purpose." ]
                            , under = "List.concat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.concatMap f
"""
                        ]
        ]


listHeadTests : Test
listHeadTests =
    describe "List.head"
        [ test "should not report List.head used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.head
b = List.head list
c = List.head (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.head [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.head []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.head (List.singleton a) by Just a" <|
            \() ->
                """module A exposing (..)
a = List.head (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.head <| List.singleton a by Just <| a" <|
            \() ->
                """module A exposing (..)
a = List.head <| List.singleton b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| b
"""
                        ]
        , test "should replace List.singleton a |> List.head by a |> Just" <|
            \() ->
                """module A exposing (..)
a = List.singleton b |> List.head
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> Just
"""
                        ]
        , test "should replace List.head [ a ] by Just a" <|
            \() ->
                """module A exposing (..)
a = List.head [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.head [ f a ] by Just (f a)" <|
            \() ->
                """module A exposing (..)
a = List.head [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f b)
"""
                        ]
        , test "should replace List.head [ a, b, c ] by Just a" <|
            \() ->
                """module A exposing (..)
a = List.head [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        , test "should replace List.head [ f a, b, c ] by Just (f a)" <|
            \() ->
                """module A exposing (..)
a = List.head [ f b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f b)
"""
                        ]
        , test "should replace List.head (a :: bToZ) by Just a" <|
            \() ->
                """module A exposing (..)
a = List.head (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.head on a list with a first element will result in Just that element"
                            , details = [ "You can replace this call by Just the first list element." ]
                            , under = "List.head"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just b
"""
                        ]
        ]


listTailTests : Test
listTailTests =
    describe "List.tail"
        [ test "should not report List.tail used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.tail
b = List.tail list
c = List.tail (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.tail [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.tail []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.tail (List.singleton a) by Just []" <|
            \() ->
                """module A exposing (..)
a = List.tail (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a list with a single element will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just []
"""
                        ]
        , test "should replace List.tail <| List.singleton a by Just <| []" <|
            \() ->
                """module A exposing (..)
a = List.tail <| List.singleton b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a list with a single element will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just <| []
"""
                        ]
        , test "should replace List.singleton a |> List.tail by [] |> Just" <|
            \() ->
                """module A exposing (..)
a = List.singleton b |> List.tail
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a list with a single element will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [] |> Just
"""
                        ]
        , test "should replace List.tail [ a ] by Just []" <|
            \() ->
                """module A exposing (..)
a = List.tail [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a list with a single element will result in Just []"
                            , details = [ "You can replace this call by Just []." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just []
"""
                        ]
        , test "should replace List.tail [ a, b, c ] by Just [ b, c ]" <|
            \() ->
                """module A exposing (..)
a = List.tail [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a list with some elements will result in Just the elements after the first"
                            , details = [ "You can replace this call by Just the list elements after the first." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just [ c, d ]
"""
                        ]
        , test "should replace List.tail (a :: bToZ) by Just bToZ" <|
            \() ->
                """module A exposing (..)
a = List.tail (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.tail on a list with some elements will result in Just the elements after the first"
                            , details = [ "You can replace this call by Just the list elements after the first." ]
                            , under = "List.tail"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just cToZ
"""
                        ]
        ]


listMemberTests : Test
listMemberTests =
    describe "List.member"
        [ test "should not report List.member used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.member
b = List.member g
c = List.member g list
d = List.member g (List.filter f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.member a [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.member a []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on [] will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.member a (List.singleton a) by True" <|
            \() ->
                """module A exposing (..)
a = List.member b (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member b (List.singleton a) by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member b (List.singleton b) by b == b when expecting NaN" <|
            \() ->
                """module A exposing (..)
a = List.member b (List.singleton b)
"""
                    |> Review.Test.run ruleExpectingNaN
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b == b
"""
                        ]
        , test "should replace List.member a [ a ] by True" <|
            \() ->
                """module A exposing (..)
a = List.member b [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member b [ a ] by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member b <| [ a ] by b == a" <|
            \() ->
                """module A exposing (..)
a = List.member c <| [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == b
"""
                        ]
        , test "should replace List.member b [ f a ] by b == (f a)" <|
            \() ->
                """module A exposing (..)
a = List.member c [ f b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = c == (f b)
"""
                        ]
        , test "should replace [ a ] |> List.member b by a == b" <|
            \() ->
                """module A exposing (..)
a = [ b ] |> List.member c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a singleton list is the same as directly checking for equality"
                            , details = [ "You can replace this call by checking whether the member to find and the value inside the singleton list are equal." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b == c
"""
                        ]
        , test "should replace List.member c [ a, b, c ] by True" <|
            \() ->
                """module A exposing (..)
a = List.member d [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report List.member d [ a, b, c ]" <|
            \() ->
                """module A exposing (..)
a = List.member e [ b, c, d ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.member a (a :: bToZ) by True" <|
            \() ->
                """module A exposing (..)
a = List.member b (b :: cToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.member c (a :: b :: c :: dToZ) by True" <|
            \() ->
                """module A exposing (..)
a = List.member d (b :: c :: d :: eToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.member on a list which contains the given element will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.member"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should not report List.member d (a :: b :: c :: dToZ)" <|
            \() ->
                """module A exposing (..)
a = List.member e (b :: c :: d :: eToZ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        ]


listMapTests : Test
listMapTests =
    describe "List.map"
        [ test "should not report List.map used with okay arguments" <|
            \() ->
                """module A exposing (..)
import Array
import Dict
a0 = List.map
a1 = List.map f
a2 = List.map f x
a3 = List.map f (Dict.fromList dict)
a4 = List.map (\\( a, b ) -> a + b) (Dict.fromList dict)
a5 = List.map (f >> Tuple.first) (Dict.fromList dict)
a6 = List.map f << Dict.fromList
a7 = List.map (\\( a, b ) -> a + b) << Dict.fromList
a8 = List.map (f >> Tuple.first) << Dict.fromList
a9 = List.map f (Array.toIndexedList array)
a10 = List.map (\\( a, b ) -> a) (Array.toIndexedList array)
a11 = List.map Tuple.first (Array.toIndexedList array)
a12 = List.map (f >> Tuple.second) (Array.toIndexedList array)
a13 = List.map Tuple.first << Array.toIndexedList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.map f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map identity x by x" <|
            \() ->
                """module A exposing (..)
a = List.map identity x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.map identity <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.map identity <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.map identity by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.map identity by identity" <|
            \() ->
                """module A exposing (..)
a = List.map identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.map <| identity by identity" <|
            \() ->
                """module A exposing (..)
a = List.map <| identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace identity |> List.map by identity" <|
            \() ->
                """module A exposing (..)
a = identity |> List.map
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map with an identity function will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.map f [ x ] by [ (f x) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (f x) ]
"""
                        ]
        , test "should replace List.map f [ g x ] by [ (f (g x)) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f [ g x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (f (g x)) ]
"""
                        ]
        , test "should replace List.map f (if cond then [ x ] else [ y ]) by [ f (if cond then x else y) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f (if cond then [ x ] else [ y ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ f (if cond then x else y) ]
"""
                        ]
        , test "should replace List.map f <| if cond then [ x ] else [ y ] by [ f <| if cond then x else y ]" <|
            \() ->
                """module A exposing (..)
a = List.map f <| if cond then [ x ] else [ y ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ f <| if cond then x else y ]
"""
                        ]
        , test "should replace (if cond then [ x ] else [ y ]) |> List.map f by [ (if cond then x else y) |> f ]" <|
            \() ->
                """module A exposing (..)
a = (if cond then [ x ] else [ y ]) |> List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (if cond then x else y) |> f ]
"""
                        ]
        , test "should replace List.map f (if cond then List.singleton x else List.singleton y) by [ f (if cond then x else y) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f (if cond then List.singleton x else List.singleton y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ f (if cond then x else y) ]
"""
                        ]
        , test "should replace List.map f (List.singleton x) by (List.singleton (f x))" <|
            \() ->
                """module A exposing (..)
a = List.map f (List.singleton x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.singleton (f x))
"""
                        ]
        , test "should replace List.map f <| [ x ] by [ (f <| x) ]" <|
            \() ->
                """module A exposing (..)
a = List.map f <| [ x ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (f <| x) ]
"""
                        ]
        , test "should replace [ x ] |> List.map f by [ (x |> f) ]" <|
            \() ->
                """module A exposing (..)
a = [ x ] |> List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in a singleton list with the function applied to the value inside"
                            , details = [ "You can replace this call by a singleton list with the function directly applied to the value inside the given singleton list." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ (x |> f) ]
"""
                        ]
        , test "should replace List.map f << List.singleton by List.singleton << f" <|
            \() ->
                """module A exposing (..)
a = List.map f << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map on a singleton list will result in List.singleton with the function applied to the value inside"
                            , details = [ "You can replace this call by List.singleton with the function directly applied to the value inside the singleton list itself." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton << f
"""
                        ]
        , test "should replace Dict.toList >> List.map Tuple.first by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace Dict.toList >> List.map (\\( part0, _ ) -> part0) by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map (\\( part0, _ ) -> part0)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace List.map Tuple.first << Dict.toList Tuple.first by Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys
"""
                        ]
        , test "should replace Dict.toList >> List.map Tuple.second by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.second is the same as Dict.values"
                            , details = [ "Using Dict.values directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace Dict.toList >> List.map (\\( _, part1 ) -> part1) by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList >> List.map (\\( _, part1 ) -> part1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.second is the same as Dict.values"
                            , details = [ "Using Dict.values directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace List.map Tuple.second << Dict.toList Tuple.second by Dict.values" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.second << Dict.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.second is the same as Dict.values"
                            , details = [ "Using Dict.values directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.values
"""
                        ]
        , test "should replace List.map Tuple.first (Dict.toList dict) by Dict.keys dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (Dict.toList dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys dict
"""
                        ]
        , test "should replace List.map Tuple.first (Dict.toList <| dict) by Dict.keys dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (Dict.toList <| dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys dict
"""
                        ]
        , test "should replace List.map Tuple.first (dict |> Dict.toList) by Dict.keys dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first (dict |> Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys dict
"""
                        ]
        , test "should replace List.map Tuple.first <| Dict.toList dict by Dict.keys <| dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| Dict.toList dict
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys <| dict
"""
                        ]
        , test "should replace List.map Tuple.first <| (Dict.toList <| dict) by Dict.keys <| dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| (Dict.toList <| dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys <| dict
"""
                        ]
        , test "should replace List.map Tuple.first <| (dict |> Dict.toList) by Dict.keys <| dict" <|
            \() ->
                """module A exposing (..)
import Dict
a = List.map Tuple.first <| (dict |> Dict.toList)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.keys <| dict
"""
                        ]
        , test "should replace Dict.toList dict |> List.map Tuple.first by dict |> Dict.keys" <|
            \() ->
                """module A exposing (..)
import Dict
a = Dict.toList dict |> List.map Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.toList, then List.map Tuple.first is the same as Dict.keys"
                            , details = [ "Using Dict.keys directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = dict |> Dict.keys
"""
                        ]
        , test "should replace array |> Array.toIndexedList |> List.map Tuple.second by array |> Array.toList" <|
            \() ->
                """module A exposing (..)
import Array
a = array |> Array.toIndexedList |> List.map Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.map Tuple.second is the same as Array.toList"
                            , details = [ "You can replace this call by Array.toList on the array given to Array.toIndexedList which is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array |> Array.toList
"""
                        ]
        , test "should replace array |> Array.toIndexedList |> List.map (\\( _, part1 ) -> part1) by array |> Array.toList" <|
            \() ->
                """module A exposing (..)
import Array
a = array |> Array.toIndexedList |> List.map (\\( _, part1 ) -> part1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.map Tuple.second is the same as Array.toList"
                            , details = [ "You can replace this call by Array.toList on the array given to Array.toIndexedList which is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = array |> Array.toList
"""
                        ]
        , test "should replace Array.toIndexedList >> List.map Tuple.second by Array.toList" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toIndexedList >> List.map Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.map Tuple.second is the same as Array.toList"
                            , details = [ "You can replace this composition by Array.toList which is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.toList
"""
                        ]
        , test "should replace Array.toIndexedList >> List.map (\\( _, part1 ) -> part1) by Array.toList" <|
            \() ->
                """module A exposing (..)
import Array
a = Array.toIndexedList >> List.map (\\( _, part1 ) -> part1)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Array.toIndexedList, then List.map Tuple.second is the same as Array.toList"
                            , details = [ "You can replace this composition by Array.toList which is meant for this exact purpose and will also be faster." ]
                            , under = "List.map"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Array
a = Array.toList
"""
                        ]
        ]


listFilterTests : Test
listFilterTests =
    describe "List.filter"
        [ test "should not report List.filter used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.filter f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.filter f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filter f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filter f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.filter f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.filter f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always True) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filter (always True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filter (\\_ -> True) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filter (\\_ -> True) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filter (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filter (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filter <| (always True) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filter <| (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace always True |> List.filter by identity" <|
            \() ->
                """module A exposing (..)
a = always True |> List.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return True will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filter (always False) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (\\_ -> False) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (\\_ -> False) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always False) <| x by []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False) <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace x |> List.filter (always False) by []" <|
            \() ->
                """module A exposing (..)
a = x |> List.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filter (always False) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filter (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filter <| (always False) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filter <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace always False |> List.filter by always []" <|
            \() ->
                """module A exposing (..)
a = always False |> List.filter
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filter with a function that will always return False will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filter"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        ]


listFilterMapTests : Test
listFilterMapTests =
    describe "List.filterMap"
        [ test "should not report List.filterMap used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.filterMap f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap f <| [] by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace [] |> List.filterMap f by []" <|
            \() ->
                """module A exposing (..)
a = [] |> List.filterMap f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) <| x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing) <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace x |> List.filterMap (always Nothing) by []" <|
            \() ->
                """module A exposing (..)
a = x |> List.filterMap (always Nothing)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (always Nothing) by always []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (always Nothing)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filterMap <| always Nothing by always []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap <| always Nothing
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace always Nothing |> List.filterMap by always []" <|
            \() ->
                """module A exposing (..)
a = always Nothing |> List.filterMap
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.filterMap Just x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap Just <| x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace x |> List.filterMap Just by x" <|
            \() ->
                """module A exposing (..)
a = x |> List.filterMap Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.filterMap Just by identity" <|
            \() ->
                """module A exposing (..)
a = List.filterMap Just
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.filterMap (\\a -> Nothing) x by []" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Nothing) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Nothing will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.filterMap (\\a -> Just b) x by x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just b) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just is the same as List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> b) x
"""
                        ]
        , test "should replace List.filterMap (\\a -> Just b) by identity" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just is the same as List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> b)
"""
                        ]
        , test "should replace List.map (\\a -> Just b) x by List.filterMap (\\a -> b) x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap (\\a -> Just b) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with a function that will always return Just is the same as List.map"
                            , details = [ "You can remove the `Just`s and replace the call by List.map." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\a -> b) x
"""
                        ]
        , test "should replace List.filterMap identity (List.map f x) by List.filterMap f x" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity (List.map f x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.filterMap f x)
"""
                        ]
        , test "should replace List.filterMap identity <| List.map f <| x by (List.filterMap f <| x)" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity <| List.map f <| x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.filterMap f <| x)
"""
                        ]
        , test "should replace x |> List.map f |> List.filterMap identity by (x |> List.filterMap f)" <|
            \() ->
                """module A exposing (..)
a = x |> List.map f |> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (x |> List.filterMap f)
"""
                        ]
        , test "should replace List.map f >> List.filterMap identity by List.filterMap f" <|
            \() ->
                """module A exposing (..)
a = List.map f >> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap f
"""
                        ]
        , test "should not report List.filterMap f [ a, Nothing, b ] with f not being an identity function" <|
            \() ->
                """module A exposing (..)
a = List.filterMap f [ a, Nothing, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.filterMap identity [ a, Nothing, b ] by List.filterMap identity [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity [ a, Nothing, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.filterMap with an identity function on a list containing an irrelevant Nothing"
                            , details = [ "Including Nothing in the list does not change the result of this call. You can remove the Nothing element." ]
                            , under = "Nothing"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap identity [ a, b ]
"""
                        ]
        , test "should replace List.filterMap identity << List.map f by List.filterMap f" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity << List.map f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map and List.filterMap identity can be combined using List.filterMap"
                            , details = [ "List.filterMap is meant for this exact purpose and will also be faster." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filterMap f
"""
                        ]
        , test "should replace List.filterMap identity [ Just x, Just y ] by [ x, y ]" <|
            \() ->
                """module A exposing (..)
a = List.filterMap identity [ Just x, Just y ]
b = List.filterMap f [ Just x, Just y ]
c = List.filterMap identity [ Just x, y ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.filterMap identity"
                            , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 19 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ x, y ]
b = List.filterMap f [ Just x, Just y ]
c = List.filterMap identity [ Just x, y ]
"""
                        ]
        , test "should replace [ Just x, Just y ] |> List.filterMap identity by [ x, y ]" <|
            \() ->
                """module A exposing (..)
a = [ Just x, Just y ] |> List.filterMap identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary use of List.filterMap identity"
                            , details = [ "All of the elements in the list are `Just`s, which can be simplified by removing all of the `Just`s." ]
                            , under = "List.filterMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ x, y ]
"""
                        ]
        ]


listIndexedMapTests : Test
listIndexedMapTests =
    describe "List.indexedMap"
        [ test "should not report List.indexedMap used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap f list
b = List.indexedMap (\\i value -> i + value) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.indexedMap f [] by []" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.indexedMap (\\_ y -> y) list by List.map (\\y -> y) list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (\\_ y -> y) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\y -> y) list
"""
                        ]
        , test "should replace List.indexedMap (\\(_) (y) -> y) list by List.map (\\(y) -> y) list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (\\(_) (y) -> y) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (\\(y) -> y) list
"""
                        ]
        , test "should replace List.indexedMap (\\_ -> f) list by List.map f list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (\\_ -> f) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map f list
"""
                        ]
        , test "should replace List.indexedMap (always f) list by List.map f list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (always f) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map f list
"""
                        ]
        , test "should replace List.indexedMap (always <| f y) list by List.map (f y) list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (always <| f y) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f y) list
"""
                        ]
        , test "should replace List.indexedMap (f y |> always) list by List.map (f y) list" <|
            \() ->
                """module A exposing (..)
a = List.indexedMap (f y |> always) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.indexedMap with a function that ignores the first argument is the same as List.map"
                            , details = [ "You can replace this call by List.map." ]
                            , under = "List.indexedMap"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.map (f y) list
"""
                        ]
        ]


listIsEmptyTests : Test
listIsEmptyTests =
    describe "List.isEmpty"
        [ test "should not report List.isEmpty with a non-literal argument" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.isEmpty [] by True" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on [] will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.isEmpty [x] by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty [x]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (x :: xs) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (x :: xs)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace x :: xs |> List.isEmpty by False" <|
            \() ->
                """module A exposing (..)
a = x :: xs |> List.isEmpty
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (List.singleton x) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (List.singleton x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.isEmpty (a |> List.singleton) by False" <|
            \() ->
                """module A exposing (..)
a = List.isEmpty (b |> List.singleton)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.isEmpty on this list will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.isEmpty"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


listSumTests : Test
listSumTests =
    describe "List.sum"
        [ test "should not report List.sum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sum
b = List.sum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sum (a :: bToZ)
b = List.sum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sum [] by 0" <|
            \() ->
                """module A exposing (..)
a = List.sum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on [] will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.sum [ a ] by a" <|
            \() ->
                """module A exposing (..)
a = List.sum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should replace List.sum << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = List.sum << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sum [ a, 0, b ] by List.sum [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.sum [ b, 0, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list containing an irrelevant 0"
                            , details = [ "Including 0 in the list does not change the result of this call. You can remove the 0 element." ]
                            , under = "0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum [ b, c ]
"""
                        ]
        , test "should replace List.sum [ a, 0.0, b ] by List.sum [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.sum [ b, 0.0, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list containing an irrelevant 0"
                            , details = [ "Including 0 in the list does not change the result of this call. You can remove the 0 element." ]
                            , under = "0.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum [ b, c ]
"""
                        ]
        , test "should replace List.sum [ a, 0 ] by List.sum [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.sum [ b, 0 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list containing an irrelevant 0"
                            , details = [ "Including 0 in the list does not change the result of this call. You can remove the 0 element." ]
                            , under = "0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum [ b ]
"""
                        ]
        , test "should replace List.sum [ 0, a ] by List.sum [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.sum [ 0, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list containing an irrelevant 0"
                            , details = [ "Including 0 in the list does not change the result of this call. You can remove the 0 element." ]
                            , under = "0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum [ b ]
"""
                        ]
        , test "should replace [ a, 0 / 0.0, b ] |> List.sum by (0 / 0.0) when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = [ a, 0 / 0.0, b ] |> List.sum
"""
                    |> Review.Test.run (rule (defaults |> Simplify.expectNaN))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sum on a list with NaN will result in NaN"
                            , details = [ "You can replace this call by (0 / 0)." ]
                            , under = "List.sum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (0 / 0.0)
"""
                        ]
        ]


listProductTests : Test
listProductTests =
    describe "List.product"
        [ test "should not report List.product on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.product
b = List.product list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.product on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.product (a :: bToZ)
b = List.product [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.product [] by 1" <|
            \() ->
                """module A exposing (..)
a = List.product []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on [] will result in 1"
                            , details = [ "You can replace this call by 1." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 1
"""
                        ]
        , test "should replace List.product [ a ] by a" <|
            \() ->
                """module A exposing (..)
a = List.product [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a singleton list will result in the value inside"
                            , details = [ "You can replace this call by the value inside the singleton list." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = a
"""
                        ]
        , test "should replace List.product << List.singleton by identity" <|
            \() ->
                """module A exposing (..)
a = List.product << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a singleton list will always result in the value inside"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.product [ a, 1, b ] by List.product [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.product [ b, 1, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list containing an irrelevant 1"
                            , details = [ "Including 1 in the list does not change the result of this call. You can remove the 1 element." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product [ b, c ]
"""
                        ]
        , test "should replace List.product [ a, 1.0, b ] by List.product [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.product [ b, 1.0, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list containing an irrelevant 1"
                            , details = [ "Including 1 in the list does not change the result of this call. You can remove the 1 element." ]
                            , under = "1.0"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product [ b, c ]
"""
                        ]
        , test "should replace List.product [ a, 1 ] by List.product [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.product [ b, 1 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list containing an irrelevant 1"
                            , details = [ "Including 1 in the list does not change the result of this call. You can remove the 1 element." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product [ b ]
"""
                        ]
        , test "should replace List.product [ 1, a ] by List.product [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.product [ 1, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list containing an irrelevant 1"
                            , details = [ "Including 1 in the list does not change the result of this call. You can remove the 1 element." ]
                            , under = "1"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product [ b ]
"""
                        ]
        , test "should replace List.product [ a, 0, b ] by 0" <|
            \() ->
                """module A exposing (..)
a = List.product [ a, 0, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with 0 will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.product (a :: 0 :: bs) by 0" <|
            \() ->
                """module A exposing (..)
a = List.product (a :: 0 :: bs)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with 0 will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.product [ a, 0.0, b ] by 0.0" <|
            \() ->
                """module A exposing (..)
a = List.product [ a, 0.0, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with 0 will result in 0"
                            , details = [ "You can replace this call by 0." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0.0
"""
                        ]
        , test "should not report List.product [ a, 0.0, 0, b ] when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = List.product [ a, 0.0, 0, b ]
"""
                    |> Review.Test.run (rule (defaults |> Simplify.expectNaN))
                    |> Review.Test.expectNoErrors
        , test "should replace [ a, 0 / 0.0, b ] |> List.product by (0 / 0.0) when expectNaN is enabled" <|
            \() ->
                """module A exposing (..)
a = [ a, 0 / 0.0, b ] |> List.product
"""
                    |> Review.Test.run (rule (defaults |> Simplify.expectNaN))
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.product on a list with NaN will result in NaN"
                            , details = [ "You can replace this call by (0 / 0)." ]
                            , under = "List.product"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (0 / 0.0)
"""
                        ]
        ]


listMinimumTests : Test
listMinimumTests =
    describe "List.minimum"
        [ test "should not report List.minimum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.minimum
b = List.minimum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.minimum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.minimum (a :: bToZ)
b = List.minimum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.minimum [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.minimum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.minimum [ a ] by Just a" <|
            \() ->
                """module A exposing (..)
a = List.minimum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a singleton list will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the singleton list." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just a
"""
                        ]
        , test "should replace List.minimum [ f a ] by Just (f a)" <|
            \() ->
                """module A exposing (..)
a = List.minimum [ f a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a singleton list will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the singleton list." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (f a)
"""
                        ]
        , test "should replace List.minimum (if c then [ a ] else [ b ]) by Just (if c then a else b)" <|
            \() ->
                """module A exposing (..)
a = List.minimum (if c then [ a ] else [ b ])
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a singleton list will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the singleton list." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just (if c then a else b)
"""
                        ]
        , test "should replace List.minimum << List.singleton by Just" <|
            \() ->
                """module A exposing (..)
a = List.minimum << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.minimum on a singleton list will always result in Just the value inside"
                            , details = [ "You can replace this call by Just." ]
                            , under = "List.minimum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just
"""
                        ]
        ]


listMaximumTests : Test
listMaximumTests =
    describe "List.maximum"
        [ test "should not report List.maximum on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.maximum
b = List.maximum list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.maximum on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.maximum (a :: bToZ)
b = List.maximum [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.maximum [] by Nothing" <|
            \() ->
                """module A exposing (..)
a = List.maximum []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.maximum on [] will result in Nothing"
                            , details = [ "You can replace this call by Nothing." ]
                            , under = "List.maximum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Nothing
"""
                        ]
        , test "should replace List.maximum [ a ] by Just a" <|
            \() ->
                """module A exposing (..)
a = List.maximum [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.maximum on a singleton list will result in Just the value inside"
                            , details = [ "You can replace this call by Just the value inside the singleton list." ]
                            , under = "List.maximum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just a
"""
                        ]
        , test "should replace List.maximum << List.singleton by Just" <|
            \() ->
                """module A exposing (..)
a = List.maximum << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.maximum on a singleton list will always result in Just the value inside"
                            , details = [ "You can replace this call by Just." ]
                            , under = "List.maximum"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Just
"""
                        ]
        ]


listFoldlTests : Test
listFoldlTests =
    describe "List.foldl"
        [ test "should not report List.foldl used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\el soFar -> soFar - el) 20 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.foldl f x [] by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl on [] will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (\\_ -> identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\_ -> identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (\\_ a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\_ a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always <| \\a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always <| \\a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldl (always identity) x by always x" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always x
"""
                        ]
        , test "should replace List.foldl (always identity) by always" <|
            \() ->
                """module A exposing (..)
a = List.foldl (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which list is supplied next." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always
"""
                        ]
        , test "should replace List.foldl f x (Set.toList set) by Set.foldl f x set" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x (Set.toList set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x set
"""
                        ]
        , test "should replace List.foldl f x << Set.toList by Set.foldl f x" <|
            \() ->
                """module A exposing (..)
a = List.foldl f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x
"""
                        ]
        , test "should replace Set.toList >> List.foldl f x by Set.foldl f x" <|
            \() ->
                """module A exposing (..)
a = Set.toList >> List.foldl f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldl directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldl f x
"""
                        ]
        , listFoldlSumTests
        , listFoldlProductTests
        , listFoldlAllTests
        , listFoldlAnyTests
        ]


listFoldlAnyTests : Test
listFoldlAnyTests =
    describe "any"
        [ test "should replace List.foldl (||) True by always True" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with (||) and the initial accumulator True will always result in True"
                            , details = [ "You can replace this call by always True." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should replace List.foldl (||) True list by True" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with (||) and the initial accumulator True will always result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.foldl (||) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (||) False list by List.any identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity list
"""
                        ]
        , test "should replace List.foldl (||) False <| list by List.any identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (||) False <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity <| list
"""
                        ]
        , test "should replace list |> List.foldl (||) False by list |> List.any identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x -> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x || y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x || y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y || x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y || x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (||) y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (||) y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        ]


listFoldlAllTests : Test
listFoldlAllTests =
    describe "all"
        [ test "should replace List.foldl (&&) False by always False" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with (&&) and the initial accumulator False will always result in False"
                            , details = [ "You can replace this call by always False." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace List.foldl (&&) False list by False" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl with (&&) and the initial accumulator False will always result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.foldl (&&) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (&&) True <| list by List.all identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity <| list
"""
                        ]
        , test "should replace list |> List.foldl (&&) True by list |> List.all identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.all identity
"""
                        ]
        , test "should replace List.foldl (&&) True list by List.all identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (&&) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity list
"""
                        ]
        , test "should replace List.foldl (\\x -> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x && y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x && y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y && x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y && x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (&&) y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (&&) y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        ]


listFoldlSumTests : Test
listFoldlSumTests =
    describe "sum"
        [ test "should replace List.foldl (+) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (+) 0 list by List.sum list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum list
"""
                        ]
        , test "should replace List.foldl (+) 0 <| list by List.sum <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) 0 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum <| list
"""
                        ]
        , test "should replace list |> List.foldl (+) 0 by list |> List.sum" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.sum
"""
                        ]
        , test "should replace List.foldl (\\x -> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> x + y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x + y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> y + x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y + x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (+) y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (+) y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldl (+) initial list by initial + (List.sum list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum list)
"""
                        ]
        , test "should replace List.foldl (+) initial <| list by initial + (List.sum <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (+) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum <| list)
"""
                        ]
        , test "should replace list |> List.foldl (+) initial by ((list |> List.sum) + initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (+) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.sum) + initial)
"""
                        ]
        ]


listFoldlProductTests : Test
listFoldlProductTests =
    describe "product"
        [ test "should replace List.foldl (*) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (*) 1 list by List.product list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product list
"""
                        ]
        , test "should replace List.foldl (*) 1 <| list by List.product <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) 1 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product <| list
"""
                        ]
        , test "should replace list |> List.foldl (*) 1 by list |> List.product" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.product
"""
                        ]
        , test "should replace List.foldl (\\x -> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x -> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> x * y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x * y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> y * x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y * x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> x |> (*) y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> x |> (*) y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (\\x y -> y |> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldl (\\x y -> y |> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldl (*) initial list by initial * (List.product list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product list)
"""
                        ]
        , test "should replace List.foldl (*) initial <| list by initial * (List.product <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldl (*) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product <| list)
"""
                        ]
        , test "should replace list |> List.foldl (*) initial by ((list |> List.product) * initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldl (*) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldl (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldl"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.product) * initial)
"""
                        ]
        ]


listFoldrTests : Test
listFoldrTests =
    describe "List.foldr"
        [ test "should not report List.foldr used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\el soFar -> soFar - el) 20 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.foldr fn x [] by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr fn x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr on [] will always return the same given initial accumulator"
                            , details = [ "You can replace this call by the initial accumulator itself." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (\\_ -> identity) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\_ -> identity) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (\\_ a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\_ a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always <| \\a -> a) x list by x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always <| \\a -> a) x list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = x
"""
                        ]
        , test "should replace List.foldr (always identity) x by always x" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity) x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` with the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always x
"""
                        ]
        , test "should replace List.foldr (always identity) by always" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by `always` because the incoming accumulator will be returned, no matter which list is supplied next." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always
"""
                        ]
        , test "should replace List.foldr (always identity) initial data extraData by initial extraArgument" <|
            \() ->
                """module A exposing (..)
a = List.foldr (always identity) initial data extraArgument
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with a function that always returns the unchanged accumulator will result in the initial accumulator"
                            , details = [ "You can replace this call by the given initial accumulator." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial extraArgument
"""
                        ]
        , test "should replace List.foldr f x (Set.toList set) by Set.foldl f x set" <|
            \() ->
                """module A exposing (..)
a = List.foldr f x (Set.toList set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x set
"""
                        ]
        , test "should replace List.foldr f x << Set.toList by Set.foldr f x" <|
            \() ->
                """module A exposing (..)
a = List.foldr f x << Set.toList
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x
"""
                        ]
        , test "should replace Set.toList >> List.foldr f x by Set.foldr f x" <|
            \() ->
                """module A exposing (..)
a = Set.toList >> List.foldr f x
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To fold a set, you don't need to convert to a List"
                            , details = [ "Using Set.foldr directly is meant for this exact purpose and will also be faster." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Set.foldr f x
"""
                        ]
        , listFoldrSumTests
        , listFoldrProductTests
        , listFoldrAllTests
        , listFoldrAnyTests
        ]


listFoldrAnyTests : Test
listFoldrAnyTests =
    describe "any"
        [ test "should replace List.foldr (||) True by always True" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with (||) and the initial accumulator True will always result in True"
                            , details = [ "You can replace this call by always True." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should replace List.foldr (||) True list by True" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with (||) and the initial accumulator True will always result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.foldr (||) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (||) False list by List.any identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity list
"""
                        ]
        , test "should replace List.foldr (||) False <| list by List.any identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (||) False <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity <| list
"""
                        ]
        , test "should replace list |> List.foldr (||) False by list |> List.any identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (||) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x -> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x || y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x || y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y || x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y || x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (||) y) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (||) y) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (||) x) False by List.any identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (||) x) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (||) False is the same as List.any identity"
                            , details =
                                [ "You can replace this call by List.any identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity
"""
                        ]
        ]


listFoldrAllTests : Test
listFoldrAllTests =
    describe "all"
        [ test "should replace List.foldr (&&) False by always False" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) False
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with (&&) and the initial accumulator False will always result in False"
                            , details = [ "You can replace this call by always False." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace List.foldr (&&) False list by False" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) False list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr with (&&) and the initial accumulator False will always result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.foldr (&&) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (&&) True list by List.all identity list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity list
"""
                        ]
        , test "should replace List.foldr (&&) True <| list by List.all identity <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (&&) True <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity <| list
"""
                        ]
        , test "should replace list |> List.foldr (&&) True by list |> List.all identity" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (&&) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x -> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x && y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x && y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y && x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y && x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (&&) y) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (&&) y) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (&&) x) True by List.all identity" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (&&) x) True
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (&&) True is the same as List.all identity"
                            , details =
                                [ "You can replace this call by List.all identity which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity
"""
                        ]
        ]


listFoldrSumTests : Test
listFoldrSumTests =
    describe "sum"
        [ test "should replace List.foldr (+) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (+) 0 list by List.sum list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum list
"""
                        ]
        , test "should replace List.foldr (+) 0 <| list by List.sum <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) 0 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum <| list
"""
                        ]
        , test "should replace list |> List.foldr (+) 0 by list |> List.sum" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (+) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.sum
"""
                        ]
        , test "should replace List.foldr (\\x -> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> x + y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x + y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> y + x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y + x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (+) y) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (+) y) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (+) x) 0 by List.sum" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (+) x) 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sum
"""
                        ]
        , test "should replace List.foldr (+) initial list by initial + (List.sum list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum list)
"""
                        ]
        , test "should replace List.foldr (+) initial <| list by initial + (List.sum <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (+) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial + (List.sum <| list)
"""
                        ]
        , test "should replace list |> List.foldr (+) initial by ((list |> List.sum) + initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (+) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (+) 0 is the same as List.sum"
                            , details =
                                [ "You can replace this call by List.sum which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.sum) + initial)
"""
                        ]
        ]


listFoldrProductTests : Test
listFoldrProductTests =
    describe "product"
        [ test "should replace List.foldr (*) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (*) 1 list by List.product list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product list
"""
                        ]
        , test "should replace List.foldr (*) 1 <| list by List.product <| list" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) 1 <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product <| list
"""
                        ]
        , test "should replace list |> List.foldr (*) 1 by list |> List.product" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (*) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.product
"""
                        ]
        , test "should replace List.foldr (\\x -> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x -> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> x * y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x * y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> y * x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y * x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> x |> (*) y) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> x |> (*) y) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (\\x y -> y |> (*) x) 1 by List.product" <|
            \() ->
                """module A exposing (..)
a = List.foldr (\\x y -> y |> (*) x) 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.product
"""
                        ]
        , test "should replace List.foldr (*) initial list by initial * (List.product list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) initial list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product list)
"""
                        ]
        , test "should replace List.foldr (*) initial <| list by initial * (List.product <| list)" <|
            \() ->
                """module A exposing (..)
a = List.foldr (*) initial <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = initial * (List.product <| list)
"""
                        ]
        , test "should replace list |> List.foldr (*) initial by ((list |> List.product) * initial)" <|
            \() ->
                """module A exposing (..)
a = list |> List.foldr (*) initial
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.foldr (*) 1 is the same as List.product"
                            , details =
                                [ "You can replace this call by List.product which is meant for this exact purpose." ]
                            , under = "List.foldr"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ((list |> List.product) * initial)
"""
                        ]
        ]


listAllTests : Test
listAllTests =
    describe "List.all"
        [ test "should not report List.all used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.all f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.all f [] by True" <|
            \() ->
                """module A exposing (..)
a = List.all f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all on [] will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.all (always True) list by True" <|
            \() ->
                """module A exposing (..)
a = List.all (always True) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with a function that will always return True will always result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.all (always True) by always True" <|
            \() ->
                """module A exposing (..)
a = List.all (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with a function that will always return True will always result in True"
                            , details = [ "You can replace this call by always True." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always True
"""
                        ]
        , test "should not report List.all on list with False but non-identity and non-not function" <|
            \() ->
                """module A exposing (..)
a = List.all f [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.all identity [ a, True, b ] by List.all identity [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.all identity [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with an identity function on a list containing an irrelevant True"
                            , details = [ "Including True in the list does not change the result of this call. You can remove the True element." ]
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all identity [ b, c ]
"""
                        ]
        , test "should replace List.all identity [ a, False, b ] by False" <|
            \() ->
                """module A exposing (..)
a = List.all identity [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with an identity function on a list with False will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.all identity (a :: False :: bs) by False" <|
            \() ->
                """module A exposing (..)
a = List.all identity (b :: False :: cs)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with an identity function on a list with False will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.all not [ a, False, b ] by List.all not [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.all not [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with `not` on a list containing an irrelevant False"
                            , details = [ "Including False in the list does not change the result of this call. You can remove the False element." ]
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.all not [ b, c ]
"""
                        ]
        , test "should replace List.all not [ a, True, b ] by False" <|
            \() ->
                """module A exposing (..)
a = List.all not [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.all with `not` on a list with True will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.all"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        ]


listAnyTests : Test
listAnyTests =
    describe "List.any"
        [ test "should not report List.any used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.any f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.any f [] by False" <|
            \() ->
                """module A exposing (..)
a = List.any f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any on [] will result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.any (always False) list by False" <|
            \() ->
                """module A exposing (..)
a = List.any (always False) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a function that will always return False will always result in False"
                            , details = [ "You can replace this call by False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = False
"""
                        ]
        , test "should replace List.any (always False) by always False" <|
            \() ->
                """module A exposing (..)
a = List.any (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a function that will always return False will always result in False"
                            , details = [ "You can replace this call by always False." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always False
"""
                        ]
        , test "should replace List.any ((==) x) by List.member x" <|
            \() ->
                """module A exposing (..)
a = List.any ((==) x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a check for equality with a specific value can be replaced by List.member with that value"
                            , details = [ "You can replace this call by List.member with the specific value to find which meant for this exact purpose." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.member x
"""
                        ]
        , test "should replace List.any (\\y -> y == x) by List.member x" <|
            \() ->
                """module A exposing (..)
a = List.any (\\y -> y == x)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a check for equality with a specific value can be replaced by List.member with that value"
                            , details = [ "You can replace this call by List.member with the specific value to find which meant for this exact purpose." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.member x
"""
                        ]
        , test "should replace List.any (\\y -> x == y) by List.member x" <|
            \() ->
                """module A exposing (..)
a = List.any (\\y -> x == y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with a check for equality with a specific value can be replaced by List.member with that value"
                            , details = [ "You can replace this call by List.member with the specific value to find which meant for this exact purpose." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.member x
"""
                        ]
        , test "should not replace List.any (\\z -> x == y)" <|
            \() ->
                """module A exposing (..)
a = List.any (\\z -> x == y)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.any on list with True but non-identity and non-not function" <|
            \() ->
                """module A exposing (..)
a = List.any f [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.any identity [ a, False, b ] and by List.any identity [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.any identity [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with an identity function on a list containing an irrelevant False"
                            , details = [ "Including False in the list does not change the result of this call. You can remove the False element." ]
                            , under = "False"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any identity [ b, c ]
"""
                        ]
        , test "should replace List.any identity [ a, True, b ] by True" <|
            \() ->
                """module A exposing (..)
a = List.any identity [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with an identity function on a list with True will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        , test "should replace List.any not [ a, True, b ] by List.any not [ a, b ]" <|
            \() ->
                """module A exposing (..)
a = List.any not [ b, True, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with `not` on a list containing an irrelevant True"
                            , details = [ "Including True in the list does not change the result of this call. You can remove the True element." ]
                            , under = "True"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.any not [ b, c ]
"""
                        ]
        , test "should replace List.any not [ a, False, b ] by True" <|
            \() ->
                """module A exposing (..)
a = List.any not [ b, False, c ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.any with `not` on a list with False will result in True"
                            , details = [ "You can replace this call by True." ]
                            , under = "List.any"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = True
"""
                        ]
        ]


listRangeTests : Test
listRangeTests =
    describe "List.range"
        [ test "should not report List.range used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.range
a = List.range 5
a = List.range 5 10
a = List.range 5 0xF
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.range 10 5 by []" <|
            \() ->
                """module A exposing (..)
a = List.range 10 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.range with a start index greater than the end index will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.range 0xF 5 by []" <|
            \() ->
                """module A exposing (..)
a = List.range 0xF 5
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.range with a start index greater than the end index will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace 5 |> List.range 10 by []" <|
            \() ->
                """module A exposing (..)
a = 5 |> List.range 10
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.range with a start index greater than the end index will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.range"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listLengthTests : Test
listLengthTests =
    describe "List.length"
        [ test "should not report List.length used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.length
a = List.length b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.length [] by 0" <|
            \() ->
                """module A exposing (..)
a = List.length []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 0"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        , test "should replace List.length [b, c, d] by 3" <|
            \() ->
                """module A exposing (..)
a = List.length [b, c, d]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 3"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should replace [] |> List.length by 0" <|
            \() ->
                """module A exposing (..)
a = [] |> List.length
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "The length of the list is 0"
                            , details = [ "The length of the list can be determined by looking at the code." ]
                            , under = "List.length"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 0
"""
                        ]
        ]


listRepeatTests : Test
listRepeatTests =
    describe "List.repeat"
        [ test "should not report List.repeat that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.repeat n list
b = List.repeat 5 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not replace List.repeat n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.repeat 0 list by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 0 will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.repeat 0 by always []" <|
            \() ->
                """module A exposing (..)
a = List.repeat 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 0 will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.repeat -5 list by []" <|
            \() ->
                """module A exposing (..)
a = List.repeat -5 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with negative length will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.repeat 1 by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.repeat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace 1 |> List.repeat by List.singleton" <|
            \() ->
                """module A exposing (..)
a = 1 |> List.repeat
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace List.repeat 1 element by List.singleton element" <|
            \() ->
                """module A exposing (..)
a = List.repeat 1 element
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton element
"""
                        ]
        , test "should replace List.repeat 1 <| element by List.singleton <| element" <|
            \() ->
                """module A exposing (..)
a = List.repeat 1 <| element
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton <| element
"""
                        ]
        , test "should replace element |> List.repeat 1 by element |> List.singleton" <|
            \() ->
                """module A exposing (..)
a = element |> List.repeat 1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.repeat with length 1 will result in List.singleton"
                            , details = [ "You can replace this call by List.singleton." ]
                            , under = "List.repeat"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = element |> List.singleton
"""
                        ]
        ]


listSortTests : Test
listSortTests =
    describe "List.sort"
        [ test "should not report List.sort on a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sort
b = List.sort list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sort on a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sort (a :: bToZ)
b = List.sort [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sort [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sort []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sort on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sort [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.sort [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sort on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ a ]
"""
                        ]
        , test "should replace List.sort (List.sort list) by (List.sort list)" <|
            \() ->
                """module A exposing (..)
a = List.sort (List.sort list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sort after List.sort"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.sort list)
"""
                        ]
        , test "should replace List.sort << List.sort by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sort << List.sort
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sort after List.sort"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.sort"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 14 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        ]


listSortByTests : Test
listSortByTests =
    describe "List.sortBy"
        [ test "should not report List.sortBy with a function variable and a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn
b = List.sortBy fn list
c = List.sortBy << List.sortBy fn
c = List.sortBy f << List.sortWith g
c = List.sortBy f << List.sort
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortBy with a function variable and a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn (a :: bToZ)
b = List.sortBy fn [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sortBy fn [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sortBy fn []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sortBy fn [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
b = List.sortBy fn [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
b = [ a ]
"""
                        ]
        , test "should replace List.sortBy (always a) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (always b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy (always a) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortBy (always a) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (always b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy (always a) will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortBy (\\_ -> a) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\_ -> b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy (always a) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortBy (\\_ -> a) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\_ -> b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy (always a) will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortBy identity by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy identity
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy (\\a -> a) by List.sort" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\b -> b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort
"""
                        ]
        , test "should replace List.sortBy identity list by List.sort list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy identity list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort list
"""
                        ]
        , test "should replace List.sortBy (\\a -> a) list by List.sort list" <|
            \() ->
                """module A exposing (..)
a = List.sortBy (\\b -> b) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortBy with an identity function is the same as List.sort"
                            , details = [ "You can replace this call by List.sort." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sort list
"""
                        ]
        , test "should replace List.sortBy f (List.sortBy f list) by (List.sortBy f list)" <|
            \() ->
                """module A exposing (..)
a = List.sortBy f (List.sortBy f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortBy after equivalent List.sortBy"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.sortBy f list)
"""
                        ]
        , test "should replace List.sortBy f << List.sortBy f by List.sortBy f" <|
            \() ->
                """module A exposing (..)
a = List.sortBy f << List.sortBy f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary List.sortBy after equivalent List.sortBy"
                            , details = [ "You can remove this additional operation." ]
                            , under = "List.sortBy"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 16 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.sortBy f
"""
                        ]
        ]


listSortWithTests : Test
listSortWithTests =
    describe "List.sortWith"
        [ test "should not report List.sortWith with a function variable and a list variable" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn
b = List.sortWith fn list
b = List.sortWith (always fn) list
b = List.sortWith (always (always fn)) list
e = List.sortWith f (List.sortWith f list) -- because e.g. List.sortWith (\\_ _ -> LT) is equivalent to List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should not report List.sortWith with a function variable and a list with >= 2 elements" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn (a :: bToZ)
b = List.sortWith fn [ a, b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.sortWith fn [] by []" <|
            \() ->
                """module A exposing (..)
a = List.sortWith fn []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.sortWith fn [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
b = List.sortWith fn [ a ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
b = [ a ]
"""
                        ]
        , test "should replace List.sortWith (always (always GT)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always GT)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always GT)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always GT) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> GT) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> GT)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> GT)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> GT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> GT) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always EQ)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always EQ)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always EQ)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always EQ) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> EQ) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> EQ)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> EQ)) by identity" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> EQ))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> EQ) will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> LT) is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT)) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> LT) is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse list
"""
                        ]
        , test "should replace List.sortWith (always (always LT)) <| list by list" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (always LT)) <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> LT) is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse <| list
"""
                        ]
        , test "should replace list |> List.sortWith (always (always LT)) by list" <|
            \() ->
                """module A exposing (..)
a = list |> List.sortWith (always (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> LT) is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list |> List.reverse
"""
                        ]
        , test "should replace List.sortWith (\\_ -> always LT) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ -> (always LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> LT) is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (\\_ _ -> LT) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (\\_ _ -> LT)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> LT) is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        , test "should replace List.sortWith (always (\\_ -> LT)) by List.reverse" <|
            \() ->
                """module A exposing (..)
a = List.sortWith (always (\\_ -> LT))
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.sortWith (\\_ _ -> LT) is the same as List.reverse"
                            , details = [ "You can replace this call by List.reverse." ]
                            , under = "List.sortWith"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.reverse
"""
                        ]
        ]


listReverseTests : Test
listReverseTests =
    describe "List.reverse"
        [ test "should not report List.reverse with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.reverse
b = List.reverse str
c = (List.reverse << f) << List.reverse
d = List.reverse << (f << List.reverse)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.reverse [] by []" <|
            \() ->
                """module A exposing (..)
a = List.reverse []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.reverse [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.reverse [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b ]
"""
                        ]
        , test "should replace List.reverse (List.singleton a) by (List.singleton a)" <|
            \() ->
                """module A exposing (..)
a = List.reverse (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.singleton b)
"""
                        ]
        , test "should replace a |> List.singleton |> List.reverse by a |> List.singleton" <|
            \() ->
                """module A exposing (..)
a = b |> List.singleton |> List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> List.singleton
"""
                        ]
        , test "should replace List.reverse << List.singleton by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.reverse << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the unchanged singleton list"
                            , details = [ "You can replace this composition by List.singleton." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace List.singleton >> List.reverse by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.singleton >> List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse on a singleton list will result in the unchanged singleton list"
                            , details = [ "You can replace this composition by List.singleton." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace List.reverse <| List.reverse <| list by list" <|
            \() ->
                """module A exposing (..)
a = List.reverse <| List.reverse <| list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse, then List.reverse cancels each other out"
                            , details = [ "You can replace this call by the argument given to List.reverse." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should simplify List.reverse >> List.reverse to identity" <|
            \() ->
                """module A exposing (..)
a = List.reverse >> List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse, then List.reverse cancels each other out"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 21 }, end = { row = 2, column = 33 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should simplify (f << List.reverse) << List.reverse to (f)" <|
            \() ->
                """module A exposing (..)
a = (f << List.reverse) << List.reverse
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse, then List.reverse cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 11 }, end = { row = 2, column = 23 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        , test "should simplify List.reverse << (List.reverse << f) to (f)" <|
            \() ->
                """module A exposing (..)
a = List.reverse << (List.reverse << f)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.reverse, then List.reverse cancels each other out"
                            , details = [ "You can remove these two functions." ]
                            , under = "List.reverse"
                            }
                            |> Review.Test.atExactly { start = { row = 2, column = 5 }, end = { row = 2, column = 17 } }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f)
"""
                        ]
        ]


listTakeTests : Test
listTakeTests =
    describe "List.take"
        [ test "should not report List.take that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.take 2 list
b = List.take y [ 1, 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.take n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.take n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.take on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.take 0 list by []" <|
            \() ->
                """module A exposing (..)
a = List.take 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.take with length 0 will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.take 0 by always []" <|
            \() ->
                """module A exposing (..)
a = List.take 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.take with length 0 will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.take -literal by always []" <|
            \() ->
                """module A exposing (..)
a = List.take -1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.take with negative length will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.take"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        ]


listDropTests : Test
listDropTests =
    describe "List.drop"
        [ test "should not report List.drop that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.drop 2 list
b = List.drop y [ 1, 2, 3 ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.drop n [] by []" <|
            \() ->
                """module A exposing (..)
a = List.drop n []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.drop 0 list by list" <|
            \() ->
                """module A exposing (..)
a = List.drop 0 list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop 0 will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace list |> List.drop 0 by list" <|
            \() ->
                """module A exposing (..)
a = list |> List.drop 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop 0 will always return the same given list"
                            , details = [ "You can replace this call by the list itself." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = list
"""
                        ]
        , test "should replace List.drop 0 by identity" <|
            \() ->
                """module A exposing (..)
a = List.drop 0
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.drop 0 will always return the same given list"
                            , details = [ "You can replace this call by identity." ]
                            , under = "List.drop"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        ]


listPartitionTests : Test
listPartitionTests =
    describe "List.partition"
        [ test "should not report List.partition used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = List.partition f list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.partition f [] by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace List.partition f <| [] by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition f <| []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace [] |> List.partition f by ( [], [] )" <|
            \() ->
                """module A exposing (..)
a = [] |> List.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        , test "should replace List.partition (always True) list by ( list, [] )" <|
            \() ->
                """module A exposing (..)
a = List.partition (always True) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the first list"
                            , details = [ "Since the predicate function always returns True, the second list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( list, [] )
"""
                        ]
        , test "should not replace List.partition (always True)" <|
            -- We'd likely need an anonymous function which could introduce naming conflicts
            -- Could be improved if we knew what names are available at this point in scope (or are used anywhere)
            -- so that we can generate a unique variable.
            \() ->
                """module A exposing (..)
a = List.partition (always True)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.partition (always False) list by ( [], list )" <|
            \() ->
                """module A exposing (..)
a = List.partition (always False) list
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], list )
"""
                        ]
        , test "should replace List.partition (always False) by (Tuple.pair [])" <|
            \() ->
                """module A exposing (..)
a = List.partition (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        , test "should replace List.partition <| (always False) by (Tuple.pair [])" <|
            \() ->
                """module A exposing (..)
a = List.partition <| (always False)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        , test "should replace always False |> List.partition by Tuple.pair []" <|
            \() ->
                """module A exposing (..)
a = always False |> List.partition
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "All elements will go to the second list"
                            , details = [ "Since the predicate function always returns False, the first list will always be []." ]
                            , under = "List.partition"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (Tuple.pair [])
"""
                        ]
        ]


listIntersperseTests : Test
listIntersperseTests =
    describe "List.intersperse"
        [ test "should not report List.intersperse that contains a variable or expression" <|
            \() ->
                """module A exposing (..)
a = List.intersperse 2 list
b = List.intersperse y [ 1, 2, 3 ]
c = List.intersperse << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.intersperse x [] by []" <|
            \() ->
                """module A exposing (..)
a = List.intersperse x []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on [] will result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.intersperse s [ a ] by [ a ]" <|
            \() ->
                """module A exposing (..)
a = List.intersperse s [ b ]
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = [ b ]
"""
                        ]
        , test "should replace List.intersperse s (List.singleton a) by (List.singleton a)" <|
            \() ->
                """module A exposing (..)
a = List.intersperse s (List.singleton b)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.singleton b)
"""
                        ]
        , test "should replace a |> List.singleton |> List.intersperse s by a |> List.singleton" <|
            \() ->
                """module A exposing (..)
a = b |> List.singleton |> List.intersperse s
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the given singleton list"
                            , details = [ "You can replace this call by the given singleton list." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = b |> List.singleton
"""
                        ]
        , test "should replace List.intersperse s << List.singleton by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.intersperse s << List.singleton
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the unchanged singleton list"
                            , details = [ "You can replace this composition by List.singleton." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        , test "should replace List.singleton >> List.intersperse s by List.singleton" <|
            \() ->
                """module A exposing (..)
a = List.singleton >> List.intersperse s
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.intersperse on a singleton list will result in the unchanged singleton list"
                            , details = [ "You can replace this composition by List.singleton." ]
                            , under = "List.intersperse"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.singleton
"""
                        ]
        ]


listUnzipTests : Test
listUnzipTests =
    describe "List.unzip"
        [ test "should not report List.unzip on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.unzip
b = List.unzip list
c = List.unzip [ h ]
d = List.unzip (h :: t)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.unzip [] by []" <|
            \() ->
                """module A exposing (..)
a = List.unzip []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.unzip on [] will result in ( [], [] )"
                            , details = [ "You can replace this call by ( [], [] )." ]
                            , under = "List.unzip"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( [], [] )
"""
                        ]
        ]


listMap2Tests : Test
listMap2Tests =
    describe "List.map2"
        [ test "should not report List.map2 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map2 f
a = List.map2 f list0
b = List.map2 f list0 list1
c = List.map2 f [ h ] list1
d = List.map2 f (h :: t) list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map2 f [] list1 by []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map2 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map2 f [] by always []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map2 with any list being [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map2 f list0 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map2 f list0 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map2 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map2"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap3Tests : Test
listMap3Tests =
    describe "List.map3"
        [ test "should not report List.map3 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map3 f
a = List.map3 f list0
b = List.map3 f list0 list1
b = List.map3 f list0 list1 list2
c = List.map3 f [ h ] list1 list2
d = List.map3 f (h :: t) list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map3 f [] list1 list2 by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map3 f [] list1 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map3 f [] list1 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map3 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map3 f list0 [] list2 by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f list0 [] list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map3 f list0 list1 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map3 f list0 list1 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map3 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map3"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap4Tests : Test
listMap4Tests =
    describe "List.map4"
        [ test "should not report List.map4 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map4 f
a = List.map4 f list0
b = List.map4 f list0 list1
b = List.map4 f list0 list1 list2
b = List.map4 f list0 list1 list2 list3
c = List.map4 f [ h ] list1 list2 list3
d = List.map4 f (h :: t) list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map4 f [] list1 list2 list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f [] list1 list2 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map4 f [] list1 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map4 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map4 f [] by (\\_ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map4 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ _ -> [])." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ -> [])
"""
                        ]
        , test "should replace List.map4 f list0 [] list2 list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 [] list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f list0 list1 [] list3 by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 list1 [] list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map4 f list0 list1 list2 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map4 f list0 list1 list2 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map4 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map4"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]


listMap5Tests : Test
listMap5Tests =
    describe "List.map5"
        [ test "should not report List.map5 on a list argument containing a variable" <|
            \() ->
                """module A exposing (..)
a = List.map5 f
a = List.map5 f list0
b = List.map5 f list0 list1
b = List.map5 f list0 list1 list2
b = List.map5 f list0 list1 list2 list3
b = List.map5 f list0 list1 list2 list3 list4
c = List.map5 f [ h ] list1 list2 list3 list4
d = List.map5 f (h :: t) list1 list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace List.map5 f [] list1 list2 list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f [] list1 list2 list3 by always []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2 list3
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by always []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always []
"""
                        ]
        , test "should replace List.map5 f [] list1 list2 by (\\_ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1 list2
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ -> [])
"""
                        ]
        , test "should replace List.map5 f [] list1 by (\\_ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f [] list1
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ -> [])
"""
                        ]
        , test "should replace List.map5 f [] by (\\_ _ _ _ -> [])" <|
            \() ->
                """module A exposing (..)
a = List.map5 f []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by (\\_ _ _ _ -> [])." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (\\_ _ _ _ -> [])
"""
                        ]
        , test "should replace List.map5 f list0 [] list2 list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 [] list2 list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 [] list3 list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 [] list3 list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 list2 [] list4 by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 list2 [] list4
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        , test "should replace List.map5 f list0 list1 list2 list3 [] by []" <|
            \() ->
                """module A exposing (..)
a = List.map5 f list0 list1 list2 list3 []
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.map5 with any list being [] will always result in []"
                            , details = [ "You can replace this call by []." ]
                            , under = "List.map5"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = []
"""
                        ]
        ]



-- Tuple


tupleTests : Test
tupleTests =
    describe "Tuple"
        [ tuplePairTests
        , tupleFirstTests
        , tupleSecondTests
        ]


tuplePairTests : Test
tuplePairTests =
    describe "Tuple.pair"
        [ test "should not report Tuple.pair used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Tuple.pair
b = Tuple.pair first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Tuple.pair first second by ( first, second )" <|
            \() ->
                """module A exposing (..)
a = Tuple.pair first second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Fully constructed Tuple.pair can be replaced by tuple literal"
                            , details = [ "You can replace this call by a tuple literal ( _, _ ). Consistently using ( _, _ ) to create a tuple is more idiomatic in elm." ]
                            , under = "Tuple.pair"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = ( first, second )
"""
                        ]
        , test "should replace multiline Tuple.pair (let x = y in first) <| let x = y in second by ( (let x = y in first), let x = y in second )" <|
            \() ->
                """module A exposing (..)
a =
    Tuple.pair
        (let
            x =
                y
         in
         first
        )
    <|
        let
            x =
                y
        in
        second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Fully constructed Tuple.pair can be replaced by tuple literal"
                            , details = [ "You can replace this call by a tuple literal ( _, _ ). Consistently using ( _, _ ) to create a tuple is more idiomatic in elm." ]
                            , under = "Tuple.pair"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    (
        (let
            x =
                y
         in
         first
        )
    ,
        let
            x =
                y
        in
        second
    )
"""
                        ]
        ]


tupleFirstTests : Test
tupleFirstTests =
    describe "Tuple.first"
        [ test "should not report Tuple.first used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Tuple.first
b = Tuple.first tuple
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Tuple.first ( first |> f, second ) by (first |> f)" <|
            \() ->
                """module A exposing (..)
a = Tuple.first ( first |> f, second )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on a known tuple will result in the tuple's first part"
                            , details = [ "You can replace this call by the tuple's first part." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (first |> f)
"""
                        ]
        , test "should replace Tuple.first (second |> Tuple.pair first) by first" <|
            \() ->
                """module A exposing (..)
a = Tuple.first (second |> Tuple.pair first)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.first on a known tuple will result in the tuple's first part"
                            , details = [ "You can replace this call by the tuple's first part." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = first
"""
                        ]
        , test "should replace Tuple.first << (first |> f |> Tuple.pair) by always (first |> f)" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << (first |> f |> Tuple.pair)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.pair with a first part, then Tuple.first will always result in that first part"
                            , details = [ "You can replace this call by always with the first argument given to Tuple.pair." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = always (first |> f)
"""
                        ]
        , test "should replace Tuple.mapSecond changeSecond tuple |> Tuple.first by tuple |> Tuple.first" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapSecond changeSecond tuple |> Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Tuple.mapSecond before Tuple.first"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapSecond call by the unchanged tuple." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = tuple |> Tuple.first
"""
                        ]
        , test "should replace Tuple.first << Tuple.mapSecond changeSecond by Tuple.first" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << Tuple.mapSecond changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Tuple.mapSecond before Tuple.first"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can remove the Tuple.mapSecond call." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.first
"""
                        ]
        , test "should replace Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.first by Tuple.mapFirst changeFirst tuple |> Tuple.first" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.first
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.mapBoth before Tuple.first is the same as Tuple.mapFirst"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapBoth call by Tuple.mapFirst with the same first mapping and tuple." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.mapFirst changeFirst tuple |> Tuple.first
"""
                        ]
        , test "should replace Tuple.first << Tuple.mapBoth changeFirst changeSecond by Tuple.first << Tuple.mapFirst changeFirst" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << Tuple.mapBoth changeFirst changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.mapBoth before Tuple.first is the same as Tuple.mapFirst"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapBoth call by Tuple.mapFirst with the same first mapping." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.first << Tuple.mapFirst changeFirst
"""
                        ]
        , test "should replace Tuple.first (List.partition f list) by (List.filter f list)" <|
            \() ->
                """module A exposing (..)
a = Tuple.first (List.partition f list)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition, then Tuple.first can be combined into List.filter"
                            , details = [ "You can replace this call by List.filter with the same arguments given to List.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (List.filter f list)
"""
                        ]
        , test "should replace Tuple.first (Set.partition f set) by (Set.filter f set)" <|
            \() ->
                """module A exposing (..)
import Set
a = Tuple.first (Set.partition f set)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.partition, then Tuple.first can be combined into Set.filter"
                            , details = [ "You can replace this call by Set.filter with the same arguments given to Set.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = (Set.filter f set)
"""
                        ]
        , test "should replace Tuple.first (Dict.partition f dict) by (Dict.filter f dict)" <|
            \() ->
                """module A exposing (..)
import Dict
a = Tuple.first (Dict.partition f dict)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition, then Tuple.first can be combined into Dict.filter"
                            , details = [ "You can replace this call by Dict.filter with the same arguments given to Dict.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = (Dict.filter f dict)
"""
                        ]
        , test "should replace Tuple.first << List.partition f by List.filter f" <|
            \() ->
                """module A exposing (..)
a = Tuple.first << List.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "List.partition, then Tuple.first can be combined into List.filter"
                            , details = [ "You can replace this composition by List.filter with the same arguments given to List.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = List.filter f
"""
                        ]
        , test "should replace Tuple.first << Set.partition f by Set.filter f" <|
            \() ->
                """module A exposing (..)
import Set
a = Tuple.first << Set.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Set.partition, then Tuple.first can be combined into Set.filter"
                            , details = [ "You can replace this composition by Set.filter with the same arguments given to Set.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Set
a = Set.filter f
"""
                        ]
        , test "should replace Tuple.first << Dict.partition f by Dict.filter f" <|
            \() ->
                """module A exposing (..)
import Dict
a = Tuple.first << Dict.partition f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Dict.partition, then Tuple.first can be combined into Dict.filter"
                            , details = [ "You can replace this composition by Dict.filter with the same arguments given to Dict.partition which is meant for this exact purpose." ]
                            , under = "Tuple.first"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
import Dict
a = Dict.filter f
"""
                        ]
        ]


tupleSecondTests : Test
tupleSecondTests =
    describe "Tuple.second"
        [ test "should not report Tuple.second used with okay arguments" <|
            \() ->
                """module A exposing (..)
a = Tuple.second
b = Tuple.second tuple
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should replace Tuple.second ( first, second |> f ) by (second |> f)" <|
            \() ->
                """module A exposing (..)
a = Tuple.second ( first, second |> f )
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on a known tuple will result in the tuple's second part"
                            , details = [ "You can replace this call by the tuple's second part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (second |> f)
"""
                        ]
        , test "should replace Tuple.second (second |> Tuple.pair first) by second" <|
            \() ->
                """module A exposing (..)
a = Tuple.second (second |> Tuple.pair first)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.second on a known tuple will result in the tuple's second part"
                            , details = [ "You can replace this call by the tuple's second part." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = second
"""
                        ]
        , test "should replace Tuple.second << Tuple.pair first by identity" <|
            \() ->
                """module A exposing (..)
a = Tuple.second << (second |> f |> Tuple.pair)
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.pair with a first part, then Tuple.second will always result in the incoming second part"
                            , details = [ "You can replace this composition by identity." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = identity
"""
                        ]
        , test "should replace Tuple.mapFirst changeSecond tuple |> Tuple.second by tuple |> Tuple.second" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapFirst changeSecond tuple |> Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Tuple.mapFirst before Tuple.second"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapFirst call by the unchanged tuple." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = tuple |> Tuple.second
"""
                        ]
        , test "should replace Tuple.second << Tuple.mapFirst changeSecond by Tuple.second" <|
            \() ->
                """module A exposing (..)
a = Tuple.second << Tuple.mapFirst changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Unnecessary Tuple.mapFirst before Tuple.second"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can remove the Tuple.mapFirst call." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.second
"""
                        ]
        , test "should replace Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.second by Tuple.mapFirst changeFirst tuple |> Tuple.second" <|
            \() ->
                """module A exposing (..)
a = Tuple.mapBoth changeFirst changeSecond tuple |> Tuple.second
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.mapBoth before Tuple.second is the same as Tuple.mapSecond"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapBoth call by Tuple.mapSecond with the same second mapping and tuple." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.mapSecond changeSecond tuple |> Tuple.second
"""
                        ]
        , test "should replace Tuple.second << Tuple.mapBoth changeFirst changeSecond by Tuple.second << Tuple.mapSecond changeSecond" <|
            \() ->
                """module A exposing (..)
a = Tuple.second << Tuple.mapBoth changeFirst changeSecond
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Tuple.mapBoth before Tuple.second is the same as Tuple.mapSecond"
                            , details = [ "Changing a tuple part which ultimately isn't accessed is unnecessary. You can replace the Tuple.mapBoth call by Tuple.mapSecond with the same second mapping." ]
                            , under = "Tuple.second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = Tuple.second << Tuple.mapSecond changeSecond
"""
                        ]
        ]
