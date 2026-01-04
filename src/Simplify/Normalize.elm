module Simplify.Normalize exposing
    ( Resources, normalizeExpression, normalizeButKeepRange
    , Comparison(..), areAllTheSameAs, areTheSame, compare, compareWithoutNormalization
    , getBool, getInt, getNumber, isSpecificUnappliedBinaryOperation
    )

{-| Bring expressions to a normal form,
including simple evaluation using [`Simplify.Infer`](Simplify-Infer)

@docs Resources, normalizeExpression, normalizeButKeepRange


## equality

@docs Comparison, areAllTheSameAs, areTheSame, compare, compareWithoutNormalization


## parse

@docs getBool, getInt, getNumber, isSpecificUnappliedBinaryOperation

-}

import Dict exposing (Dict)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Writer
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer


type alias Resources a =
    Infer.Resources (AstHelpers.ReduceLambdaResources a)


areTheSame : Resources a -> Node Expression -> Node Expression -> Bool
areTheSame resources a b =
    normalizeExpressionNode resources a == normalizeExpressionNode resources b


areAllTheSameAs : Resources res -> Node Expression -> (a -> Node Expression) -> List a -> Bool
areAllTheSameAs resources first restElementToExpressionNode rest =
    let
        normalizedFirst : Node Expression
        normalizedFirst =
            normalizeExpressionNode resources first
    in
    List.all
        (\node ->
            normalizeExpressionNode resources (restElementToExpressionNode node)
                == normalizedFirst
        )
        rest


normalizeExpressionNode : Resources a -> Node Expression -> Node Expression
normalizeExpressionNode resources node =
    Node.empty (normalizeExpression resources node)


normalizeExpression : Resources a -> Node Expression -> Expression
normalizeExpression resources (Node expressionRange expression) =
    case expression of
        Expression.ParenthesizedExpression inParens ->
            normalizeExpression resources inParens

        Expression.Application nodes ->
            case nodes of
                fn :: firstArg :: afterFirstArg ->
                    let
                        normalizedArg1 : Node Expression
                        normalizedArg1 =
                            normalizeExpressionNode resources firstArg
                    in
                    case normalizeExpression resources fn of
                        Expression.RecordAccessFunction fieldAccess ->
                            let
                                recordAccess : Expression
                                recordAccess =
                                    Expression.RecordAccess normalizedArg1 (toNode (String.dropLeft 1 fieldAccess))
                            in
                            case afterFirstArg of
                                [] ->
                                    recordAccess

                                secondArg :: argsAfterSecond ->
                                    Expression.Application
                                        (toNode recordAccess
                                            :: normalizeExpressionNode resources secondArg
                                            :: List.map (\arg -> normalizeExpressionNode resources arg) argsAfterSecond
                                        )

                        normalizedFn ->
                            Expression.Application
                                (toNode normalizedFn
                                    :: normalizedArg1
                                    :: List.map (\arg -> normalizeExpressionNode resources arg) afterFirstArg
                                )

                _ ->
                    expression

        Expression.OperatorApplication "<|" _ function extraArgument ->
            addToFunctionCall resources
                (normalizeExpressionNode resources function)
                (normalizeExpressionNode resources extraArgument)

        Expression.OperatorApplication "|>" _ extraArgument function ->
            addToFunctionCall resources
                (normalizeExpressionNode resources function)
                (normalizeExpressionNode resources extraArgument)

        Expression.OperatorApplication "<<" _ left right ->
            Expression.OperatorApplication ">>" normalizedInfixDirection (normalizeExpressionNode resources right) (normalizeExpressionNode resources left)

        Expression.OperatorApplication "::" _ head tail ->
            let
                normalizedHead : Node Expression
                normalizedHead =
                    normalizeExpressionNode resources head

                normalizedTail : Node Expression
                normalizedTail =
                    normalizeExpressionNode resources tail
            in
            case Node.value normalizedTail of
                Expression.ListExpr tailElements ->
                    Expression.ListExpr (normalizedHead :: tailElements)

                _ ->
                    Expression.OperatorApplication "::" normalizedInfixDirection normalizedHead normalizedTail

        Expression.OperatorApplication ">" _ left right ->
            Expression.OperatorApplication "<" normalizedInfixDirection (normalizeExpressionNode resources right) (normalizeExpressionNode resources left)

        Expression.OperatorApplication ">=" _ left right ->
            Expression.OperatorApplication "<=" normalizedInfixDirection (normalizeExpressionNode resources right) (normalizeExpressionNode resources left)

        Expression.OperatorApplication operator _ left right ->
            createOperation resources operator (normalizeExpressionNode resources left) (normalizeExpressionNode resources right)

        Expression.FunctionOrValue rawModuleName string ->
            Expression.FunctionOrValue
                (ModuleNameLookupTable.moduleNameAt resources.lookupTable expressionRange
                    |> Maybe.withDefault rawModuleName
                )
                string
                |> infer resources

        Expression.IfBlock cond then_ else_ ->
            let
                reverseIfConditionIsNegated : Node Expression -> Node Expression -> Node Expression -> Expression
                reverseIfConditionIsNegated condArg thenArg elseArg =
                    case Node.value condArg of
                        Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), negatedCondition ] ->
                            reverseIfConditionIsNegated negatedCondition elseArg thenArg

                        _ ->
                            Expression.IfBlock condArg thenArg elseArg
            in
            reverseIfConditionIsNegated
                (normalizeExpressionNode resources cond)
                (normalizeExpressionNode resources then_)
                (normalizeExpressionNode resources else_)

        Expression.Negation expr ->
            let
                normalized : Expression
                normalized =
                    normalizeExpression resources expr
            in
            case normalized of
                Expression.Floatable float ->
                    Expression.Floatable -float

                Expression.Negation subExpr ->
                    Node.value subExpr

                _ ->
                    Expression.Negation (toNode normalized)

        Expression.TupledExpression nodes ->
            Expression.TupledExpression (List.map (\part -> normalizeExpressionNode resources part) nodes)

        Expression.LetExpression letBlock ->
            Expression.LetExpression
                { declarations =
                    List.map
                        (\decl ->
                            case Node.value decl of
                                Expression.LetFunction function ->
                                    let
                                        declaration : Expression.FunctionImplementation
                                        declaration =
                                            Node.value function.declaration
                                    in
                                    toNode
                                        (Expression.LetFunction
                                            { documentation = Nothing
                                            , signature = Nothing
                                            , declaration =
                                                toNode
                                                    { name = toNode (Node.value declaration.name)
                                                    , arguments = List.map (\param -> normalizePatternNode resources.lookupTable param) declaration.arguments
                                                    , expression = normalizeExpressionNode resources declaration.expression
                                                    }
                                            }
                                        )

                                Expression.LetDestructuring pattern expr ->
                                    toNode (Expression.LetDestructuring (normalizePatternNode resources.lookupTable pattern) (normalizeExpressionNode resources expr))
                        )
                        letBlock.declarations
                , expression = normalizeExpressionNode resources letBlock.expression
                }

        Expression.CaseExpression caseBlock ->
            Expression.CaseExpression
                { cases = List.map (\( pattern, expr ) -> ( normalizePatternNode resources.lookupTable pattern, normalizeExpressionNode resources expr )) caseBlock.cases
                , expression = normalizeExpressionNode resources caseBlock.expression
                }

        Expression.LambdaExpression lambda ->
            let
                lambdaPatternsNormalized : List (Node Pattern)
                lambdaPatternsNormalized =
                    List.map (\arg -> normalizePatternNode resources.lookupTable arg) lambda.args
            in
            reduceLambda resources
                (case normalizeExpression resources lambda.expression of
                    Expression.LambdaExpression resultLambda ->
                        { args = lambdaPatternsNormalized ++ resultLambda.args
                        , expression = resultLambda.expression
                        }

                    resultNormalized ->
                        { args = lambdaPatternsNormalized
                        , expression = toNode resultNormalized
                        }
                )

        Expression.ListExpr elements ->
            Expression.ListExpr (List.map (\element -> normalizeExpressionNode resources element) elements)

        Expression.RecordAccess expr (Node _ field) ->
            infer resources
                (Expression.RecordAccess (normalizeExpressionNode resources expr) (toNode field))

        Expression.RecordExpr fields ->
            fields
                |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> toNode ( toNode fieldName, normalizeExpressionNode resources expr ))
                |> Expression.RecordExpr

        Expression.RecordUpdateExpression (Node _ value) nodes ->
            nodes
                |> List.sortBy (\(Node _ ( Node _ fieldName, _ )) -> fieldName)
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> toNode ( toNode fieldName, normalizeExpressionNode resources expr ))
                |> Expression.RecordUpdateExpression (toNode value)

        Expression.Hex int ->
            Expression.Floatable (Basics.toFloat int)

        Expression.Integer int ->
            Expression.Floatable (Basics.toFloat int)

        expr ->
            expr


operatorIsSymmetrical : String -> Bool
operatorIsSymmetrical operator =
    case operator of
        "+" ->
            True

        "*" ->
            True

        "||" ->
            True

        "&&" ->
            True

        "==" ->
            True

        "/=" ->
            True

        _ ->
            False


normalizeButKeepRange : Resources a -> Node Expression -> Node Expression
normalizeButKeepRange checkInfo node =
    Node (Node.range node) (normalizeExpression checkInfo node)


infer : Infer.Resources a -> Expression -> Expression
infer resources element =
    case Infer.getAsExpression element (Tuple.first resources.inferredConstants) of
        Just value ->
            value

        Nothing ->
            element


toComparable : Node Expression -> String
toComparable a =
    Elm.Writer.write (Elm.Writer.writeExpression a)


{-| Expects normalized left and right
-}
createOperation : Infer.Resources a -> String -> Node Expression -> Node Expression -> Expression
createOperation resources operator left right =
    if operator == "==" || operator == "/=" || operator == "&&" || operator == "||" then
        infer resources (createFallbackOperation operator left right)

    else
        case operator of
            "+" ->
                createNumberOperation (+) operator left right

            "-" ->
                createNumberOperation (-) operator left right

            "*" ->
                createNumberOperation (*) operator left right

            "/" ->
                createNumberOperation (/) operator left right

            "//" ->
                createNumberOperation
                    (\l r ->
                        -- not truncate because that would drop bits above 32
                        Basics.toFloat (Basics.round l // Basics.round r)
                    )
                    operator
                    left
                    right

            _ ->
                createFallbackOperation operator left right


createFallbackOperation : String -> Node Expression -> Node Expression -> Expression
createFallbackOperation operator left right =
    if
        operatorIsSymmetrical operator
            && (toComparable left > toComparable right)
    then
        Expression.OperatorApplication operator normalizedInfixDirection right left

    else
        Expression.OperatorApplication operator normalizedInfixDirection left right


createNumberOperation : (Float -> Float -> Float) -> String -> Node Expression -> Node Expression -> Expression
createNumberOperation numberOperation operatorSymbol left right =
    case Node.value left of
        Expression.Floatable leftNumber ->
            case Node.value right of
                Expression.Floatable rightNumber ->
                    Expression.Floatable (numberOperation leftNumber rightNumber)

                _ ->
                    createFallbackOperation operatorSymbol left right

        _ ->
            createFallbackOperation operatorSymbol left right


normalizedInfixDirection : Infix.InfixDirection
normalizedInfixDirection =
    Infix.Non


addToFunctionCall : Infer.Resources a -> Node Expression -> Node Expression -> Expression
addToFunctionCall resources functionCall extraArgument =
    case Node.value functionCall of
        Expression.ParenthesizedExpression expr ->
            addToFunctionCall resources expr extraArgument

        Expression.Application [ Node _ (Expression.PrefixOperator operator), left ] ->
            createOperation resources operator left extraArgument

        Expression.Application (fnCall :: args) ->
            Expression.Application (fnCall :: (args ++ [ extraArgument ]))

        Expression.LetExpression { declarations, expression } ->
            Expression.LetExpression
                { declarations = declarations
                , expression = toNode (addToFunctionCall resources expression extraArgument)
                }

        Expression.IfBlock condition ifBranch elseBranch ->
            Expression.IfBlock condition
                (toNode (addToFunctionCall resources ifBranch extraArgument))
                (toNode (addToFunctionCall resources elseBranch extraArgument))

        Expression.CaseExpression { expression, cases } ->
            Expression.CaseExpression
                { expression = expression
                , cases =
                    List.map
                        (\( cond, expr ) ->
                            ( cond, toNode (addToFunctionCall resources expr extraArgument) )
                        )
                        cases
                }

        Expression.RecordAccessFunction fieldAccess ->
            Expression.RecordAccess extraArgument (toNode (String.dropLeft 1 fieldAccess))

        Expression.FunctionOrValue [ "Basics" ] "not" ->
            case Node.value extraArgument of
                Expression.FunctionOrValue [ "Basics" ] "True" ->
                    expressionFalse

                Expression.FunctionOrValue [ "Basics" ] "False" ->
                    expressionTrue

                _ ->
                    Expression.Application [ functionCall, extraArgument ]

        _ ->
            Expression.Application [ functionCall, extraArgument ]


expressionTrue : Expression
expressionTrue =
    Expression.FunctionOrValue [ "Basics" ] "True"


expressionFalse : Expression
expressionFalse =
    Expression.FunctionOrValue [ "Basics" ] "False"


reduceLambda : Resources a -> Expression.Lambda -> Expression
reduceLambda resources lambda =
    case Node.value lambda.expression of
        Expression.Application (called :: callArguments) ->
            let
                reduced : { lambdaPatterns : List (Node Pattern), callArguments : List (Node Expression) }
                reduced =
                    AstHelpers.reduceLambda resources lambda callArguments

                reducedResultCall : Expression
                reducedResultCall =
                    case reduced.callArguments of
                        [] ->
                            Node.value called

                        _ :: _ ->
                            Expression.Application (called :: reduced.callArguments)
            in
            case reduced.lambdaPatterns of
                [] ->
                    reducedResultCall

                _ :: _ ->
                    Expression.LambdaExpression
                        { args = reduced.lambdaPatterns
                        , expression = Node.empty reducedResultCall
                        }

        Expression.OperatorApplication operator _ left right ->
            let
                reduced :
                    { lambdaPatterns : List (Node Pattern)
                    , result : Expression
                    }
                reduced =
                    let
                        leftRightReduced : { lambdaPatterns : List (Node Pattern), callArguments : List (Node Expression) }
                        leftRightReduced =
                            AstHelpers.reduceLambda resources lambda [ left, right ]
                    in
                    case leftRightReduced.callArguments of
                        [ callArgument ] ->
                            { lambdaPatterns = leftRightReduced.lambdaPatterns
                            , result = Expression.Application [ toNode (Expression.PrefixOperator operator), callArgument ]
                            }

                        [] ->
                            { lambdaPatterns = leftRightReduced.lambdaPatterns
                            , result = Expression.PrefixOperator operator
                            }

                        _ :: _ :: _ ->
                            if operatorIsSymmetrical operator then
                                let
                                    rightLeftReduced : { lambdaPatterns : List (Node Pattern), callArguments : List (Node Expression) }
                                    rightLeftReduced =
                                        AstHelpers.reduceLambda resources lambda [ right, left ]
                                in
                                case rightLeftReduced.callArguments of
                                    [ callArgument ] ->
                                        { lambdaPatterns = rightLeftReduced.lambdaPatterns
                                        , result = Expression.Application [ toNode (Expression.PrefixOperator operator), callArgument ]
                                        }

                                    [] ->
                                        { lambdaPatterns = rightLeftReduced.lambdaPatterns
                                        , result = Expression.PrefixOperator operator
                                        }

                                    _ :: _ :: _ ->
                                        { lambdaPatterns = rightLeftReduced.lambdaPatterns
                                        , result =
                                            -- same as before
                                            Node.value lambda.expression
                                        }

                            else
                                { lambdaPatterns = leftRightReduced.lambdaPatterns
                                , result =
                                    -- same as before
                                    Node.value lambda.expression
                                }
            in
            case reduced.lambdaPatterns of
                [] ->
                    reduced.result

                _ :: _ ->
                    Expression.LambdaExpression
                        { args = reduced.lambdaPatterns
                        , expression = Node.empty reduced.result
                        }

        _ ->
            Expression.LambdaExpression lambda


normalizePatternNode : ModuleNameLookupTable -> Node Pattern -> Node Pattern
normalizePatternNode lookupTable node =
    toNode (normalizePattern lookupTable node)


normalizePattern : ModuleNameLookupTable -> Node Pattern -> Pattern
normalizePattern lookupTable node =
    case Node.value node of
        Pattern.ParenthesizedPattern inParens ->
            normalizePattern lookupTable inParens

        Pattern.TuplePattern patterns ->
            Pattern.TuplePattern (List.map (\part -> normalizePatternNode lookupTable part) patterns)

        Pattern.RecordPattern fields ->
            Pattern.RecordPattern
                (fields
                    |> List.sortBy (\(Node _ fieldName) -> fieldName)
                    |> List.map (\(Node _ fieldName) -> toNode fieldName)
                )

        Pattern.UnConsPattern element list ->
            case normalizePattern lookupTable list of
                Pattern.ListPattern elements ->
                    Pattern.ListPattern (normalizePatternNode lookupTable element :: elements)

                normalizedList ->
                    Pattern.UnConsPattern (normalizePatternNode lookupTable element) (toNode normalizedList)

        Pattern.ListPattern elements ->
            Pattern.ListPattern (List.map (\element -> normalizePatternNode lookupTable element) elements)

        Pattern.NamedPattern qualifiedNameRef values ->
            let
                nameRef : Pattern.QualifiedNameRef
                nameRef =
                    { moduleName =
                        ModuleNameLookupTable.moduleNameFor lookupTable node
                            |> Maybe.withDefault qualifiedNameRef.moduleName
                    , name = qualifiedNameRef.name
                    }
            in
            Pattern.NamedPattern nameRef (List.map (\value -> normalizePatternNode lookupTable value) values)

        Pattern.AsPattern aliasPattern (Node _ asName) ->
            Pattern.AsPattern (normalizePatternNode lookupTable aliasPattern) (toNode asName)

        Pattern.HexPattern int ->
            Pattern.IntPattern int

        pattern ->
            pattern


toNode : a -> Node a
toNode =
    Node.empty



-- COMPARE


type Comparison
    = ConfirmedEquality
    | ConfirmedInequality
    | Unconfirmed


compare : Resources a -> Node Expression -> Node Expression -> Comparison
compare resources leftNode right =
    compareHelp
        (normalizeExpression resources leftNode)
        (normalizeExpression resources right)
        True


compareWithoutNormalization : Expression -> Expression -> Comparison
compareWithoutNormalization leftNode right =
    compareHelp leftNode right True


compareHelp : Expression -> Expression -> Bool -> Comparison
compareHelp left right canFlip =
    let
        fallback : () -> Comparison
        fallback () =
            if canFlip then
                compareHelp right left False

            else if left == right then
                ConfirmedEquality

            else
                Unconfirmed
    in
    case left of
        Expression.Floatable leftNumber ->
            case right of
                Expression.Floatable rightNumber ->
                    fromEquality (leftNumber == rightNumber)

                _ ->
                    Unconfirmed

        Expression.Negation (Node _ leftInNegation) ->
            case right of
                Expression.Negation (Node _ rightInNegation) ->
                    compareHelp leftInNegation rightInNegation canFlip

                _ ->
                    fallback ()

        Expression.OperatorApplication leftOp _ (Node _ leftLeft) (Node _ leftRight) ->
            case right of
                Expression.OperatorApplication rightOp _ (Node _ rightLeft) (Node _ rightRight) ->
                    if leftOp == rightOp then
                        compareAll2Help leftLeft rightLeft leftRight rightRight

                    else
                        fallback ()

                _ ->
                    fallback ()

        Expression.Literal leftString ->
            case right of
                Expression.Literal rightString ->
                    fromEquality (leftString == rightString)

                _ ->
                    fallback ()

        Expression.CharLiteral leftChar ->
            case right of
                Expression.CharLiteral rightChar ->
                    fromEquality (leftChar == rightChar)

                _ ->
                    fallback ()

        Expression.FunctionOrValue moduleNameLeft leftName ->
            case right of
                Expression.FunctionOrValue moduleNameRight rightName ->
                    if leftName == rightName && moduleNameRight == moduleNameLeft then
                        ConfirmedEquality

                    else
                        fallback ()

                _ ->
                    fallback ()

        Expression.ListExpr leftList ->
            case right of
                Expression.ListExpr rightList ->
                    compareLists leftList rightList ConfirmedEquality

                _ ->
                    fallback ()

        Expression.TupledExpression leftList ->
            case right of
                Expression.TupledExpression rightList ->
                    compareLists leftList rightList ConfirmedEquality

                _ ->
                    fallback ()

        Expression.RecordExpr leftFields ->
            case right of
                Expression.RecordExpr rightFields ->
                    compareRecords leftFields rightFields ConfirmedEquality

                _ ->
                    fallback ()

        Expression.RecordUpdateExpression (Node _ leftBaseRecordVariableName) leftFields ->
            case right of
                Expression.RecordUpdateExpression (Node _ rightBaseRecordVariableName) rightFields ->
                    compareRecords leftFields
                        rightFields
                        (if leftBaseRecordVariableName == rightBaseRecordVariableName then
                            ConfirmedEquality

                         else
                            Unconfirmed
                        )

                _ ->
                    fallback ()

        Expression.Application ((Node _ leftCalled) :: (Node _ leftArg0) :: leftArg1Up) ->
            case right of
                Expression.Application ((Node _ rightCalled) :: (Node _ rightArg0) :: rightArg1Up) ->
                    case compareAll2Help leftCalled rightCalled leftArg0 rightArg0 of
                        ConfirmedEquality ->
                            compareAllConfirmedEqualityElseUnconfirmedHelp leftArg1Up rightArg1Up

                        _ ->
                            Unconfirmed

                _ ->
                    fallback ()

        Expression.RecordAccess (Node _ leftRecord) (Node _ leftName) ->
            case right of
                Expression.RecordAccess (Node _ rightRecord) (Node _ rightName) ->
                    if leftName == rightName then
                        compareHelp leftRecord rightRecord canFlip

                    else
                        Unconfirmed

                _ ->
                    fallback ()

        Expression.UnitExpr ->
            ConfirmedEquality

        Expression.IfBlock (Node _ leftCond) (Node _ leftThen) (Node _ leftElse) ->
            case right of
                Expression.IfBlock (Node _ rightCond) (Node _ rightThen) (Node _ rightElse) ->
                    case compareHelp leftCond rightCond True of
                        ConfirmedEquality ->
                            case compareHelp leftThen rightThen True of
                                ConfirmedInequality ->
                                    case compareHelp leftElse rightElse True of
                                        ConfirmedInequality ->
                                            ConfirmedInequality

                                        _ ->
                                            Unconfirmed

                                ConfirmedEquality ->
                                    case compareHelp leftElse rightElse True of
                                        ConfirmedEquality ->
                                            ConfirmedEquality

                                        _ ->
                                            Unconfirmed

                                Unconfirmed ->
                                    Unconfirmed

                        ConfirmedInequality ->
                            -- the only way this happens
                            -- is with Basics.not (which gets normalized away)
                            -- or literal True/False (which gets reported by Simplify anyway)
                            Unconfirmed

                        Unconfirmed ->
                            Unconfirmed

                _ ->
                    fallback ()

        _ ->
            fallback ()


compareAll2Help : Expression -> Expression -> Expression -> Expression -> Comparison
compareAll2Help left0 right0 left1 right1 =
    case compareHelp left0 right0 True of
        ConfirmedInequality ->
            ConfirmedInequality

        ConfirmedEquality ->
            compareHelp left1 right1 True

        Unconfirmed ->
            case compareHelp left1 right1 True of
                ConfirmedInequality ->
                    ConfirmedInequality

                _ ->
                    Unconfirmed


compareLists : List (Node Expression) -> List (Node Expression) -> Comparison -> Comparison
compareLists leftList rightList soFar =
    case leftList of
        [] ->
            case rightList of
                [] ->
                    soFar

                _ :: _ ->
                    ConfirmedInequality

        (Node _ left) :: restOfLeft ->
            case rightList of
                [] ->
                    ConfirmedInequality

                (Node _ right) :: restOfRight ->
                    case compareWithoutNormalization left right of
                        ConfirmedInequality ->
                            ConfirmedInequality

                        ConfirmedEquality ->
                            compareLists restOfLeft restOfRight soFar

                        Unconfirmed ->
                            compareLists restOfLeft restOfRight Unconfirmed


compareAllConfirmedEqualityElseUnconfirmedHelp : List (Node Expression) -> List (Node Expression) -> Comparison
compareAllConfirmedEqualityElseUnconfirmedHelp leftList rightList =
    case leftList of
        [] ->
            case rightList of
                [] ->
                    ConfirmedEquality

                _ :: _ ->
                    Unconfirmed

        (Node _ left) :: restOfLeft ->
            case rightList of
                [] ->
                    Unconfirmed

                (Node _ right) :: restOfRight ->
                    case compareHelp left right True of
                        ConfirmedEquality ->
                            compareAllConfirmedEqualityElseUnconfirmedHelp restOfLeft restOfRight

                        _ ->
                            Unconfirmed


type RecordFieldComparison
    = MissingOtherValue
    | HasBothValues Expression Expression


compareRecords : List (Node Expression.RecordSetter) -> List (Node Expression.RecordSetter) -> Comparison -> Comparison
compareRecords leftList rightList acc =
    let
        leftFields : Dict String (Node Expression)
        leftFields =
            List.foldl
                (\(Node _ ( Node _ fieldName, fieldValue )) soFar ->
                    Dict.insert fieldName fieldValue soFar
                )
                Dict.empty
                leftList

        rightFields : Dict String (Node Expression)
        rightFields =
            List.foldl
                (\(Node _ ( Node _ fieldName, fieldValue )) soFar ->
                    Dict.insert fieldName fieldValue soFar
                )
                Dict.empty
                rightList

        recordFieldComparisons : List RecordFieldComparison
        recordFieldComparisons =
            Dict.merge
                (\key _ -> Dict.insert key MissingOtherValue)
                (\key (Node _ a) (Node _ b) -> Dict.insert key (HasBothValues a b))
                (\key _ -> Dict.insert key MissingOtherValue)
                leftFields
                rightFields
                Dict.empty
                |> Dict.values
    in
    compareRecordFields recordFieldComparisons acc


compareRecordFields : List RecordFieldComparison -> Comparison -> Comparison
compareRecordFields recordFieldComparisons acc =
    case recordFieldComparisons of
        [] ->
            acc

        MissingOtherValue :: rest ->
            compareRecordFields rest Unconfirmed

        (HasBothValues a b) :: rest ->
            case compareHelp a b True of
                ConfirmedInequality ->
                    ConfirmedInequality

                ConfirmedEquality ->
                    compareRecordFields rest acc

                Unconfirmed ->
                    compareRecordFields rest Unconfirmed


fromEquality : Bool -> Comparison
fromEquality bool =
    if bool then
        ConfirmedEquality

    else
        ConfirmedInequality


getBool : Resources a -> Node Expression -> Maybe Bool
getBool resources baseNode =
    case normalizeExpression resources baseNode of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            justTrue

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            justFalse

        _ ->
            Nothing


justTrue : Maybe Bool
justTrue =
    Just True


justFalse : Maybe Bool
justFalse =
    Just False


getInt : Resources a -> Node Expression -> Maybe Int
getInt resources expressionNode =
    case normalizeExpression resources expressionNode of
        Expression.Floatable float ->
            let
                asInt : Int
                asInt =
                    Basics.round float
            in
            if Basics.toFloat asInt == float then
                Just asInt

            else
                Nothing

        _ ->
            Nothing


getNumber : Resources a -> Node Expression -> Maybe Float
getNumber resources expressionNode =
    case normalizeExpression resources expressionNode of
        Expression.Floatable float ->
            Just float

        _ ->
            Nothing


{-| Whether a given expression can be called with 2 operands and produces the same result as an operation with a given operator.
Is either a function reducible to the operator in prefix notation `(op)` or a lambda `\a b -> a op b`.
-}
isSpecificUnappliedBinaryOperation : String -> Resources a -> Node Expression -> Bool
isSpecificUnappliedBinaryOperation operator resources expressionNode =
    normalizeExpression resources expressionNode == Expression.PrefixOperator operator
