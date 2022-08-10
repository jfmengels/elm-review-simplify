module InferTest exposing (all)

import AssocList
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Expect
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
                    |> Expect.equal
                        (Inferred2
                            { constraints =
                                [ Equals2
                                    (FunctionOrValue [] "a")
                                    (FunctionOrValue [ "Basics" ] "True")
                                ]
                            , deduced =
                                AssocList.fromList
                                    [ ( FunctionOrValue [] "a"
                                      , FunctionOrValue [ "Basics" ] "True"
                                      )
                                    ]
                            }
                        )
        , test "should infer a when False" <|
            \() ->
                infer2
                    [ FunctionOrValue [] "a" ]
                    False
                    empty2
                    |> Expect.equal
                        (Inferred2
                            { constraints =
                                [ Equals2
                                    (FunctionOrValue [] "a")
                                    falseExpr
                                ]
                            , deduced =
                                AssocList.fromList
                                    [ ( FunctionOrValue [] "a"
                                      , falseExpr
                                      )
                                    ]
                            }
                        )
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
                    |> Expect.equal
                        (Inferred2
                            { constraints =
                                [ Equals2
                                    (FunctionOrValue [] "a")
                                    (FunctionOrValue [ "Basics" ] "True")
                                , Equals2
                                    (OperatorApplication "=="
                                        Non
                                        (n (FunctionOrValue [] "a"))
                                        (n trueExpr)
                                    )
                                    (FunctionOrValue [ "Basics" ] "True")
                                ]
                            , deduced =
                                AssocList.fromList
                                    [ ( OperatorApplication "=="
                                            Non
                                            (n (FunctionOrValue [] "a"))
                                            (n trueExpr)
                                      , FunctionOrValue [ "Basics" ] "True"
                                      )
                                    , ( FunctionOrValue [] "a"
                                      , FunctionOrValue [ "Basics" ] "True"
                                      )
                                    ]
                            }
                        )
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
                    |> Expect.equal
                        (Inferred2
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
                                AssocList.fromList
                                    [ ( OperatorApplication "=="
                                            Non
                                            (n (FunctionOrValue [] "a"))
                                            (n trueExpr)
                                      , falseExpr
                                      )
                                    , ( FunctionOrValue [] "a"
                                      , falseExpr
                                      )
                                    ]
                            }
                        )
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
                    |> Expect.equal
                        (Inferred2
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
                                AssocList.fromList
                                    [ ( OperatorApplication "=="
                                            Non
                                            (n (FunctionOrValue [] "a"))
                                            (n (Floatable 1))
                                      , trueExpr
                                      )
                                    , ( FunctionOrValue [] "a"
                                      , Floatable 1
                                      )
                                    ]
                            }
                        )
        ]


n : Expression -> Node Expression
n =
    Node Range.emptyRange
