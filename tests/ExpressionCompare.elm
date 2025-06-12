module ExpressionCompare exposing (suite)

import Elm.Syntax.Documentation
import Elm.Syntax.Expression exposing (CaseBlock, Expression(..), Lambda, LetBlock, RecordSetter)
import Elm.Syntax.Infix
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Signature
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, TypeAnnotation)
import ElmSyntaxPrint
import Expect
import ExpressionDict
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe)


suite : Test
suite =
    describe "ExpressionDict.compare"
        [ Test.fuzz (expressionFuzzer 3) "Is reflexive" <|
            \e ->
                ExpressionDict.comparer e e
                    |> Expect.equal EQ
                    |> Expect.onFail
                        ("Failed for "
                            ++ (e
                                    |> Node.empty
                                    |> ElmSyntaxPrint.expressionNotParenthesized []
                                    |> ElmSyntaxPrint.toString
                               )
                        )
        , Test.fuzz2 (expressionFuzzer 3) (expressionFuzzer 3) "Is antisymmetric" <|
            \l r ->
                ExpressionDict.comparer l r
                    |> Expect.equal (negateOrder (ExpressionDict.comparer r l))
                    |> Expect.onFail
                        ("Failed for "
                            ++ (l
                                    |> Node.empty
                                    |> ElmSyntaxPrint.expressionNotParenthesized []
                                    |> ElmSyntaxPrint.toString
                               )
                            ++ " and "
                            ++ (l
                                    |> Node.empty
                                    |> ElmSyntaxPrint.expressionNotParenthesized []
                                    |> ElmSyntaxPrint.toString
                               )
                        )
        ]


negateOrder : Order -> Order
negateOrder order =
    case order of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


expressionFuzzer : Int -> Fuzzer Expression
expressionFuzzer depth =
    deepFuzzer depth expressionFuzzerLeaf expressionFuzzerFull


deepFuzzer : Int -> Fuzzer a -> (Fuzzer (Node a) -> Fuzzer a) -> Fuzzer a
deepFuzzer depth leaf full =
    if depth <= 0 then
        leaf

    else
        Fuzz.oneOf
            [ leaf
            , full (nodeFuzzer (deepFuzzer (depth - 1) leaf full))
            ]


nodeFuzzer : Fuzzer a -> Fuzzer (Node a)
nodeFuzzer child =
    Fuzz.map Node.empty child


expressionFuzzerFull : Fuzzer (Node Expression) -> Fuzzer Expression
expressionFuzzerFull child =
    Fuzz.oneOf
        [ Fuzz.map2 (\l r -> TupledExpression [ l, r ]) child child
        , Fuzz.map3 (\l m r -> TupledExpression [ l, m, r ]) child child child
        , Fuzz.map Application (Fuzz.listOfLengthBetween 1 3 child)
        , Fuzz.map CaseExpression (caseBlockFuzzer child)
        , Fuzz.map LambdaExpression (lambdaFuzzer child)
        , Fuzz.map LetExpression (letBlockFuzzer child)
        , Fuzz.map ListExpr (Fuzz.listOfLengthBetween 0 3 child)
        , Fuzz.map Negation child
        , Fuzz.map Negation child
        , Fuzz.map ParenthesizedExpression child
        , Fuzz.map RecordExpr (Fuzz.list (nodeFuzzer (recordSetterFuzzer child)))
        , Fuzz.map2 RecordAccess
            child
            (nodeFuzzer Fuzz.string)
        , Fuzz.map2 RecordUpdateExpression
            (nodeFuzzer Fuzz.string)
            (Fuzz.list (nodeFuzzer (recordSetterFuzzer child)))
        , Fuzz.map3 IfBlock
            child
            child
            child
        , Fuzz.map4 OperatorApplication
            Fuzz.string
            infixDirectionFuzzer
            child
            child
        ]


recordSetterFuzzer : Fuzzer (Node Expression) -> Fuzzer RecordSetter
recordSetterFuzzer child =
    Fuzz.pair (nodeFuzzer Fuzz.string) child


expressionFuzzerLeaf : Fuzzer Expression
expressionFuzzerLeaf =
    Fuzz.oneOf
        [ Fuzz.constant UnitExpr
        , Fuzz.map CharLiteral Fuzz.char
        , Fuzz.map Floatable Fuzz.niceFloat
        , Fuzz.map GLSLExpression Fuzz.string
        , Fuzz.map Hex Fuzz.int
        , Fuzz.map Integer Fuzz.int
        , Fuzz.map Literal Fuzz.string
        , Fuzz.map Operator Fuzz.string
        , Fuzz.map PrefixOperator Fuzz.string
        , Fuzz.map RecordAccessFunction Fuzz.string
        , Fuzz.map2 FunctionOrValue moduleNameFuzzer Fuzz.string
        ]


infixDirectionFuzzer : Fuzzer Elm.Syntax.Infix.InfixDirection
infixDirectionFuzzer =
    Fuzz.oneOfValues [ Elm.Syntax.Infix.Left, Elm.Syntax.Infix.Right, Elm.Syntax.Infix.Non ]


moduleNameFuzzer : Fuzzer ModuleName
moduleNameFuzzer =
    Fuzz.list Fuzz.string


letBlockFuzzer : Fuzzer (Node Expression) -> Fuzzer LetBlock
letBlockFuzzer child =
    Fuzz.map2 LetBlock (Fuzz.list (nodeFuzzer (letDeclarationFuzzer child))) child


letDeclarationFuzzer : Fuzzer (Node Expression) -> Fuzzer Elm.Syntax.Expression.LetDeclaration
letDeclarationFuzzer child =
    Fuzz.oneOf
        [ Fuzz.map Elm.Syntax.Expression.LetFunction
            (functionFuzzer child)
        , Fuzz.map2 Elm.Syntax.Expression.LetDestructuring
            (nodeFuzzer patternFuzzer)
            child
        ]


functionFuzzer : Fuzzer (Node Expression) -> Fuzzer Elm.Syntax.Expression.Function
functionFuzzer child =
    Fuzz.map3
        Elm.Syntax.Expression.Function
        (Fuzz.maybe (nodeFuzzer documentationFuzzer))
        (Fuzz.maybe (nodeFuzzer signatureFuzzer))
        (nodeFuzzer (functionImplementationFuzzer child))


documentationFuzzer : Fuzzer Elm.Syntax.Documentation.Documentation
documentationFuzzer =
    Fuzz.string


signatureFuzzer : Fuzzer Elm.Syntax.Signature.Signature
signatureFuzzer =
    Fuzz.map2 Elm.Syntax.Signature.Signature (nodeFuzzer Fuzz.string) (nodeFuzzer typeAnnotationFuzzer)


typeAnnotationFuzzer : Fuzzer TypeAnnotation
typeAnnotationFuzzer =
    deepFuzzer 3 typeAnnotationFuzzerLeaf typeAnnotationFuzzerFull


typeAnnotationFuzzerLeaf : Fuzzer TypeAnnotation
typeAnnotationFuzzerLeaf =
    Fuzz.oneOf
        [ Fuzz.map TypeAnnotation.GenericType Fuzz.string
        , Fuzz.constant TypeAnnotation.Unit
        ]


typeAnnotationFuzzerFull : Fuzzer (Node TypeAnnotation) -> Fuzzer TypeAnnotation
typeAnnotationFuzzerFull child =
    Fuzz.oneOf
        [ Fuzz.map2 TypeAnnotation.Typed (nodeFuzzer (Fuzz.pair moduleNameFuzzer Fuzz.string)) (Fuzz.listOfLengthBetween 0 3 child)
        , Fuzz.map TypeAnnotation.Tupled (Fuzz.listOfLengthBetween 2 3 child)
        , Fuzz.map2 TypeAnnotation.FunctionTypeAnnotation child child
        , Fuzz.map TypeAnnotation.Record (recordDefinitionFuzzer child)
        , Fuzz.map2 TypeAnnotation.GenericRecord (nodeFuzzer Fuzz.string) (nodeFuzzer (recordDefinitionFuzzer child))
        ]


recordDefinitionFuzzer : Fuzzer (Node TypeAnnotation) -> Fuzzer RecordDefinition
recordDefinitionFuzzer child =
    Fuzz.list (nodeFuzzer (recordFieldFuzzer child))


recordFieldFuzzer : Fuzzer (Node TypeAnnotation) -> Fuzzer TypeAnnotation.RecordField
recordFieldFuzzer child =
    Fuzz.pair (nodeFuzzer Fuzz.string) child


functionImplementationFuzzer : Fuzzer (Node Expression) -> Fuzzer Elm.Syntax.Expression.FunctionImplementation
functionImplementationFuzzer child =
    Fuzz.map3
        Elm.Syntax.Expression.FunctionImplementation
        (nodeFuzzer Fuzz.string)
        (Fuzz.list (nodeFuzzer patternFuzzer))
        child


patternFuzzer : Fuzzer Pattern
patternFuzzer =
    deepFuzzer 3 patternFuzzerLeaf patternFuzzerFull


patternFuzzerLeaf : Fuzzer Pattern
patternFuzzerLeaf =
    Fuzz.oneOf
        [ Fuzz.constant AllPattern
        , Fuzz.constant UnitPattern
        , Fuzz.map CharPattern Fuzz.char
        , Fuzz.map StringPattern Fuzz.string
        , Fuzz.map IntPattern Fuzz.int
        , Fuzz.map HexPattern Fuzz.int
        , Fuzz.map FloatPattern Fuzz.niceFloat
        , Fuzz.map RecordPattern (Fuzz.list (nodeFuzzer Fuzz.string))
        , Fuzz.map VarPattern Fuzz.string
        ]


patternFuzzerFull : Fuzzer (Node Pattern) -> Fuzzer Pattern
patternFuzzerFull child =
    Fuzz.oneOf
        [ Fuzz.map TuplePattern (Fuzz.listOfLengthBetween 2 3 child)
        , Fuzz.map2 UnConsPattern child child
        , Fuzz.map ListPattern (Fuzz.listOfLengthBetween 0 3 child)
        , Fuzz.map2 NamedPattern qualifiedNameRefFuzzer (Fuzz.list child)
        , Fuzz.map2 AsPattern child (nodeFuzzer Fuzz.string)
        , Fuzz.map ParenthesizedPattern child
        ]


qualifiedNameRefFuzzer : Fuzzer QualifiedNameRef
qualifiedNameRefFuzzer =
    Fuzz.map2 QualifiedNameRef (Fuzz.list Fuzz.string) Fuzz.string


caseBlockFuzzer : Fuzzer (Node Expression) -> Fuzzer CaseBlock
caseBlockFuzzer child =
    Fuzz.map2 CaseBlock child (casesFuzzer child)


casesFuzzer : Fuzzer (Node Expression) -> Fuzzer Elm.Syntax.Expression.Cases
casesFuzzer child =
    Fuzz.list (caseFuzzer child)


caseFuzzer : Fuzzer (Node Expression) -> Fuzzer Elm.Syntax.Expression.Case
caseFuzzer child =
    Fuzz.pair (nodeFuzzer patternFuzzer) child


lambdaFuzzer : Fuzzer (Node Expression) -> Fuzzer Lambda
lambdaFuzzer child =
    Fuzz.map2 Lambda (Fuzz.listOfLengthBetween 1 3 (nodeFuzzer patternFuzzer)) child
