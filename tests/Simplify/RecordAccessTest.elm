module Simplify.RecordAccessTest exposing (all)

import Review.Project
import Review.Test
import Test exposing (Test, describe, test)
import TestHelpers exposing (ruleWithDefaults)


all : Test
all =
    describe "Simplify.RecordAccess"
        [ test "should simplify record accesses for explicit records" <|
            \() ->
                """module A exposing (..)
a = { b = 3 }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "should simplify record accesses for explicit records and add parens when necessary" <|
            \() ->
                """module A exposing (..)
a = { b = f n }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (f n)
"""
                        ]
        , test "should simplify record accesses for explicit records in parentheses" <|
            \() ->
                """module A exposing (..)
a = (({ b = 3 })).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = 3
"""
                        ]
        , test "shouldn't simplify record accesses for explicit records if it can't find the field" <|
            \() ->
                """module A exposing (..)
a = { b = 3 }.c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify record accesses for record updates" <|
            \() ->
                """module A exposing (..)
a = foo { d | b = f x y }.b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo (f x y)
"""
                        ]
        , test "should simplify record accesses for record updates in parentheses" <|
            \() ->
                """module A exposing (..)
a = foo (({ d | b = f x y })).b
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".b"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = foo (f x y)
"""
                        ]
        , test "should simplify record accesses for record updates if it can't find the field" <|
            \() ->
                """module A exposing (..)
a = { d | b = 3 }.c
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of an unrelated record update can be simplified to just the original field's value." ]
                            , under = ".c"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = d.c
"""
                        ]
        , test "should simplify record accesses for let/in expressions" <|
            \() ->
                """module A exposing (..)
a = (let b = c in { e = 3 }).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in { e = 3 }.e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression" <|
            \() ->
                """module A exposing (..)
a = (let b = c in f x).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in (f x).e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions, even if the leaf is not a record expression, without adding unnecessary parentheses" <|
            \() ->
                """module A exposing (..)
a = (let b = c in x).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in x.e)
"""
                        ]
        , test "should simplify record accesses for let/in expressions in parentheses" <|
            \() ->
                """module A exposing (..)
a = (((let b = c in {e = 2}))).e
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (((let b = c in {e = 2}.e)))
"""
                        ]
        , test "should simplify nested record accesses for let/in expressions (inner)" <|
            \() ->
                """module A exposing (..)
a = (let b = c in { e = { f = 2 } }).e.f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it." ]
                            , under = ".e"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in { e = { f = 2 } }.e).f
"""
                        ]
        , test "should simplify nested record accesses for let/in expressions (outer)" <|
            \() ->
                """module A exposing (..)
a = (let b = c in (f x).e).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside a let/in expression can be simplified to access the field inside it." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (let b = c in (f x).e.f)
"""
                        ]
        , test "should simplify record accesses for if/then/else expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else { z | f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside an if/then/else expression can be simplified to access the field inside it." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else { z | f = 3 }.f)
"""
                        ]
        , test "should not simplify record accesses if some branches are not records" <|
            \() ->
                """module A exposing (..)
a = (if x then a else { f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectNoErrors
        , test "should simplify record accesses for nested if/then/else expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else if y then { z | f = 4 } else { z | f = 3 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside an if/then/else expression can be simplified to access the field inside it." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else if y then { z | f = 4 }.f else { z | f = 3 }.f)
"""
                        ]
        , test "should simplify record accesses for mixed if/then/else and case expressions" <|
            \() ->
                """module A exposing (..)
a = (if x then { f = 3 } else if y then {f = 2} else
            case b of Nothing -> { f = 4 }
                      Just _ -> { f = 5 }).f
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field outside an if/then/else expression can be simplified to access the field inside it." ]
                            , under = ".f"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a = (if x then { f = 3 }.f else if y then {f = 2}.f else
            case b of Nothing -> { f = 4 }.f
                      Just _ -> { f = 5 }.f)
"""
                        ]
        , test "should simplify record access for module-locally declared record type alias" <|
            \() ->
                """module A exposing (..)
a =
    (second |> (Record first)).second

type alias Record =
    { first : Int, second : Int }
"""
                    |> Review.Test.run ruleWithDefaults
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Field access can be simplified"
                            , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                            , under = ".second"
                            }
                            |> Review.Test.whenFixed """module A exposing (..)
a =
    second

type alias Record =
    { first : Int, second : Int }
"""
                        ]
        , test "should simplify record access for project-locally declared record type alias" <|
            \() ->
                [ """module A exposing (..)
import B

a =
    (second |> (B.Record first)).second

type alias Record =
    { first : Int, second : Int }
"""
                , """module B exposing (Record)

type alias Record =
    { first : Int, second : Int }
"""
                ]
                    |> Review.Test.runOnModules ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Field access can be simplified"
                                , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                                , under = ".second"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B

a =
    second

type alias Record =
    { first : Int, second : Int }
"""
                            ]
                          )
                        ]
        , test "should simplify record access for dependency declared record type alias" <|
            \() ->
                """module A exposing (..)
import B

a =
    (second |> (B.Record first)).second

type alias Record =
    { first : Int, second : Int }
"""
                    |> Review.Test.runWithProjectData
                        (Review.Project.new
                            |> Review.Project.addModule
                                { path = "src/B.elm"
                                , source =
                                    """module B exposing (Record)

type alias Record =
    { first : Int, second : Int }
"""
                                }
                        )
                        ruleWithDefaults
                    |> Review.Test.expectErrorsForModules
                        [ ( "A"
                          , [ Review.Test.error
                                { message = "Field access can be simplified"
                                , details = [ "Accessing the field of a record or record update can be simplified to just that field's value." ]
                                , under = ".second"
                                }
                                |> Review.Test.whenFixed """module A exposing (..)
import B

a =
    second

type alias Record =
    { first : Int, second : Int }
"""
                            ]
                          )
                        ]
        ]
