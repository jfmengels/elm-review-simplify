module InferTest exposing (all)

import AssocList
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range
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
                        (Node r (FunctionOrValue [] "a"))
                        (Node r (Floatable 1))
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
                                        (Node r (FunctionOrValue [] "a"))
                                        (Node r (Floatable 1))
                                    )
                                    trueExpr
                                ]
                            , deduced =
                                AssocList.fromList
                                    [ ( OperatorApplication "=="
                                            Non
                                            (Node r (FunctionOrValue [] "a"))
                                            (Node r (Floatable 1))
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
                        (Node r (FunctionOrValue [] "a"))
                        (Node r trueExpr)
                    ]
                    True
                    empty2
                    |> Expect.equal
                        (Inferred2
                            { constraints =
                                [ Equals2
                                    (FunctionOrValue [] "a")
                                    trueExpr
                                ]
                            , deduced =
                                AssocList.fromList
                                    [ ( FunctionOrValue [] "a"
                                      , trueExpr
                                      )
                                    ]
                            }
                        )
        ]


r : Range.Range
r =
    Range.emptyRange
