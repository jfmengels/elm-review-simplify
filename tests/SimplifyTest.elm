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
