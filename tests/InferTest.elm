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
        [ test "should infer a == 1" <|
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
        , test "should infer a == True" <|
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
                                        (n (FunctionOrValue [ "Basics" ] "True"))
                                    )
                                    (FunctionOrValue [ "Basics" ] "True")
                                ]
                            , deduced =
                                AssocList.fromList
                                    [ ( OperatorApplication "=="
                                            Non
                                            (n (FunctionOrValue [] "a"))
                                            (n (FunctionOrValue [ "Basics" ] "True"))
                                      , FunctionOrValue [ "Basics" ] "True"
                                      )
                                    , ( FunctionOrValue [] "a"
                                      , FunctionOrValue [ "Basics" ] "True"
                                      )
                                    ]
                            }
                        )
        ]


n : Expression -> Node Expression
n =
    Node Range.emptyRange
