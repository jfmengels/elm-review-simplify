module InferTest exposing (all)

import AssocList
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Expect exposing (Expectation)
import Simplify.Infer exposing (..)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Infer"
        [ test "should infer a when True" <|
            \() ->
                infer2
                    [ FunctionOrValue [] "a" ]
                    True
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Equals2
                                (FunctionOrValue [] "a")
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , trueExpr
                              )
                            ]
                        }
        , test "should infer a is true when a is True" <|
            \() ->
                empty2
                    |> infer2 [ FunctionOrValue [] "a" ] True
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal (Just trueExpr)
        , test "should infer a when False" <|
            \() ->
                infer2
                    [ FunctionOrValue [] "a" ]
                    False
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Equals2
                                (FunctionOrValue [] "a")
                                falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , falseExpr
                              )
                            ]
                        }
        , test "should infer a is true when a is False" <|
            \() ->
                empty2
                    |> infer2 [ FunctionOrValue [] "a" ] False
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal (Just falseExpr)
        , test "should infer a == True when True" <|
            \() ->
                infer2
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n trueExpr)
                    ]
                    True
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Equals2
                                (FunctionOrValue [] "a")
                                trueExpr
                            , Equals2
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , trueExpr
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                              , trueExpr
                              )
                            ]
                        }
        , test "should infer a == True when False" <|
            \() ->
                infer2
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n trueExpr)
                    ]
                    False
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Equals2
                                (FunctionOrValue [] "a")
                                falseExpr
                            , Equals2
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , falseExpr
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n trueExpr)
                              , falseExpr
                              )
                            ]
                        }
        , test "should infer a == 1 when True" <|
            \() ->
                infer2
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n (Floatable 1))
                    ]
                    True
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Equals2
                                (FunctionOrValue [] "a")
                                (Floatable 1)
                            , Equals2
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "a"
                              , Floatable 1
                              )
                            , ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                              , trueExpr
                              )
                            ]
                        }
        , test "should infer a == 1 when False" <|
            \() ->
                infer2
                    [ OperatorApplication "=="
                        Infix.Non
                        (n (FunctionOrValue [] "a"))
                        (n (Floatable 1))
                    ]
                    False
                    empty2
                    |> expectEqual
                        { constraints =
                            [ NotEquals2
                                (FunctionOrValue [] "a")
                                (Floatable 1)
                            , Equals2
                                (OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( OperatorApplication "=="
                                    Non
                                    (n (FunctionOrValue [] "a"))
                                    (n (Floatable 1))
                              , falseExpr
                              )
                            ]
                        }
        , test "should infer a is 1 when a == 1 is True" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "=="
                            Infix.Non
                            (n (FunctionOrValue [] "a"))
                            (n (Floatable 1))
                        ]
                        True
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal (Just (Floatable 1))
        , test "should not infer a when a == 1 is False" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "=="
                            Infix.Non
                            (n (FunctionOrValue [] "a"))
                            (n (Floatable 1))
                        ]
                        False
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal Nothing
        , test "should infer a && b when True" <|
            \() ->
                infer2
                    [ OperatorApplication "&&"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    True
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Equals2 (FunctionOrValue [] "b") trueExpr
                            , Equals2 (FunctionOrValue [] "a") trueExpr
                            , Equals2
                                (OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", trueExpr )
                            , ( FunctionOrValue [] "a", trueExpr )
                            , ( OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , trueExpr
                              )
                            ]
                        }
        , test "should infer a is true when a && b is True" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "&&"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal (Just trueExpr)
        , test "should infer b is true when a && b is True" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "&&"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get2 (FunctionOrValue [] "b")
                    |> Expect.equal (Just trueExpr)
        , test "should infer a && b when False" <|
            \() ->
                infer2
                    [ OperatorApplication "&&"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    False
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Equals2 (FunctionOrValue [] "b") trueExpr
                            , Equals2 (Application [ n (FunctionOrValue [ "Basics" ] "not"), n (FunctionOrValue [] "b") ]) falseExpr
                            , Equals2 (FunctionOrValue [] "a") trueExpr
                            , Equals2 (Application [ n (FunctionOrValue [ "Basics" ] "not"), n (FunctionOrValue [] "a") ]) falseExpr
                            , Equals2 (OperatorApplication "||" Right (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (Application [ n (FunctionOrValue [ "Basics" ] "not"), n (FunctionOrValue [] "a") ])) (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (Application [ n (FunctionOrValue [ "Basics" ] "not"), n (FunctionOrValue [] "b") ]))) trueExpr
                            , Equals2 (OperatorApplication "&&" Right (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (FunctionOrValue [] "a")) (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (FunctionOrValue [] "b"))) falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", trueExpr )
                            , ( Application [ n (FunctionOrValue [ "Basics" ] "not"), n (FunctionOrValue [] "b") ], falseExpr )
                            , ( FunctionOrValue [] "a", trueExpr )
                            , ( Application [ n (FunctionOrValue [ "Basics" ] "not"), n (FunctionOrValue [] "a") ], falseExpr )
                            , ( OperatorApplication "||" Right (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (Application [ n (FunctionOrValue [ "Basics" ] "not"), n (FunctionOrValue [] "a") ])) (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (Application [ n (FunctionOrValue [ "Basics" ] "not"), n (FunctionOrValue [] "b") ])), trueExpr )
                            , ( OperatorApplication "&&" Right (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (FunctionOrValue [] "a")) (Node { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } } (FunctionOrValue [] "b")), falseExpr )
                            ]
                        }
        ]


expectEqual :
    { constraints : List Constraint2
    , deduced : List ( Expression, Expression )
    }
    -> Inferred2
    -> Expectation
expectEqual record (Inferred2 inferred) =
    { constraints = inferred.constraints
    , deduced = AssocList.toList inferred.deduced
    }
        |> Expect.equal record


n : Expression -> Node Expression
n =
    Node Range.emptyRange
