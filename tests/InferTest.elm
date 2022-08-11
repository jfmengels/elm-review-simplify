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
        [ simpleTests
        , detailedTests
        , Test.only <| deduceTests
        ]


simpleTests : Test
simpleTests =
    describe "get"
        [ test "should infer a is true when a is True" <|
            \() ->
                empty2
                    |> infer2 [ FunctionOrValue [] "a" ] True
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal (Just trueExpr)
        , test "should infer a is true when a is False" <|
            \() ->
                empty2
                    |> infer2 [ FunctionOrValue [] "a" ] False
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal (Just falseExpr)
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
        , test "should not infer a when a || b is True" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal Nothing
        , test "should not infer b when a || b is True" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> get2 (FunctionOrValue [] "b")
                    |> Expect.equal Nothing
        , test "should infer a is false when a || b is False" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        False
                    |> get2 (FunctionOrValue [] "a")
                    |> Expect.equal (Just falseExpr)
        , test "should infer b is false when a || b is False" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        False
                    |> get2 (FunctionOrValue [] "b")
                    |> Expect.equal (Just falseExpr)
        , test "should infer b is true when a || b is True and a is False" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> infer2 [ FunctionOrValue [] "a" ]
                        False
                    |> get2 (FunctionOrValue [] "b")
                    |> Expect.equal (Just trueExpr)
        , test "should not infer b when a || b is True and a is True" <|
            \() ->
                empty2
                    |> infer2
                        [ OperatorApplication "||"
                            Infix.Right
                            (n (FunctionOrValue [] "a"))
                            (n (FunctionOrValue [] "b"))
                        ]
                        True
                    |> infer2 [ FunctionOrValue [] "a" ]
                        True
                    |> get2 (FunctionOrValue [] "b")
                    |> Expect.equal Nothing
        ]


detailedTests : Test
detailedTests =
    describe "infer"
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
                            , And2
                                (Equals2
                                    (FunctionOrValue [] "a")
                                    trueExpr
                                )
                                (Equals2
                                    (FunctionOrValue [] "b")
                                    trueExpr
                                )
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
                            [ Or2
                                (Equals2
                                    (FunctionOrValue [] "a")
                                    falseExpr
                                )
                                (Equals2 (FunctionOrValue [] "b") falseExpr)
                            , Equals2
                                (OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                falseExpr
                            ]
                        , deduced =
                            [ ( OperatorApplication "&&"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , falseExpr
                              )
                            ]
                        }
        , test "should infer a || b when True" <|
            \() ->
                infer2
                    [ OperatorApplication "||"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    True
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Or2
                                (Equals2
                                    (FunctionOrValue [] "a")
                                    trueExpr
                                )
                                (Equals2
                                    (FunctionOrValue [] "b")
                                    trueExpr
                                )
                            , Equals2
                                (OperatorApplication "||"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                )
                                trueExpr
                            ]
                        , deduced =
                            [ ( OperatorApplication "||"
                                    Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                              , trueExpr
                              )
                            ]
                        }
        , test "should infer a || b when False" <|
            \() ->
                infer2
                    [ OperatorApplication "||"
                        Infix.Right
                        (n (FunctionOrValue [] "a"))
                        (n (FunctionOrValue [] "b"))
                    ]
                    False
                    empty2
                    |> expectEqual
                        { constraints =
                            [ Equals2 (FunctionOrValue [] "b") falseExpr
                            , Equals2 (FunctionOrValue [] "a") falseExpr
                            , And2 (Equals2 (FunctionOrValue [] "a") falseExpr) (Equals2 (FunctionOrValue [] "b") falseExpr)
                            , Equals2 (OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b"))) falseExpr
                            ]
                        , deduced =
                            [ ( FunctionOrValue [] "b", falseExpr )
                            , ( FunctionOrValue [] "a", falseExpr )
                            , ( OperatorApplication "||" Right (n (FunctionOrValue [] "a")) (n (FunctionOrValue [] "b")), falseExpr )
                            ]
                        }
        , Test.only <|
            test "should infer a || b when True and a when False" <|
                \() ->
                    empty2
                        |> infer2
                            [ OperatorApplication "||"
                                Infix.Right
                                (n (FunctionOrValue [] "a"))
                                (n (FunctionOrValue [] "b"))
                            ]
                            True
                        |> infer2 [ FunctionOrValue [] "a" ]
                            False
                        |> expectEqual
                            { constraints =
                                [ Equals2 (FunctionOrValue [] "a") falseExpr
                                , Or2
                                    (Equals2
                                        (FunctionOrValue [] "a")
                                        trueExpr
                                    )
                                    (Equals2
                                        (FunctionOrValue [] "b")
                                        trueExpr
                                    )
                                , Equals2
                                    (OperatorApplication "||"
                                        Right
                                        (n (FunctionOrValue [] "a"))
                                        (n (FunctionOrValue [] "b"))
                                    )
                                    trueExpr
                                ]
                            , deduced =
                                [ ( FunctionOrValue [] "a"
                                  , falseExpr
                                  )
                                , ( OperatorApplication "||"
                                        Right
                                        (n (FunctionOrValue [] "a"))
                                        (n (FunctionOrValue [] "b"))
                                  , trueExpr
                                  )
                                ]
                            }
        ]


deduceTests : Test
deduceTests =
    describe "deduce"
        [ test "should deduce b is True when a || b is True and a is False" <|
            \() ->
                let
                    (Inferred2 inferred) =
                        empty2
                            |> infer2
                                [ OperatorApplication "||"
                                    Infix.Right
                                    (n (FunctionOrValue [] "a"))
                                    (n (FunctionOrValue [] "b"))
                                ]
                                True
                            |> infer2 [ FunctionOrValue [] "a" ]
                                False

                    { deduced, updatedConstraints } =
                        deduce
                            { newConstraint = Equals2 (FunctionOrValue [] "a") falseExpr
                            , constraints = inferred.constraints
                            }
                            { alreadySeen = []
                            , deduced = inferred.deduced
                            , updatedConstraints = inferred.constraints
                            }
                in
                deduced
                    |> AssocList.diff inferred.deduced
                    |> AssocList.toList
                    |> Expect.equal
                        [ ( FunctionOrValue [] "a"
                          , falseExpr
                          )
                        , ( OperatorApplication "||"
                                Right
                                (n (FunctionOrValue [] "a"))
                                (n (FunctionOrValue [] "b"))
                          , trueExpr
                          )
                        ]
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
