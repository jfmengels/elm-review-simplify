module Simplify.AstHelpers exposing
    ( boolToString
    , declarationListBindings
    , emptyStringAsString
    , getBool
    , getBoolPattern
    , getCollapsedCons
    , getComposition
    , getListLiteral
    , getListSingleton
    , getListSingletonCall
    , getOrder
    , getSpecificFunctionCall
    , getSpecificValueOrFunction
    , getTuple
    , getTypeExposeIncludingVariants
    , getUncomputedNumberValue
    , getValueOrFunctionCall
    , isBinaryOperation
    , isEmptyList
    , isIdentity
    , isListLiteral
    , isSpecificBool
    , isSpecificCall
    , isSpecificValueOrFunction
    , isTupleFirstAccess
    , isTupleSecondAccess
    , letDeclarationListBindings
    , moduleNameFromString
    , nameOfExpose
    , orderToString
    , patternBindings
    , patternListBindings
    , qualifiedToString
    , removeParens
    , removeParensFromPattern
    )

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Set exposing (Set)
import Simplify.Infer as Infer
import Simplify.Normalize as Normalize


removeParens : Node Expression -> Node Expression
removeParens expressionNode =
    case Node.value expressionNode of
        Expression.ParenthesizedExpression expressionInsideOnePairOfParensNode ->
            removeParens expressionInsideOnePairOfParensNode

        _ ->
            expressionNode


removeParensFromPattern : Node Pattern -> Node Pattern
removeParensFromPattern patternNode =
    case Node.value patternNode of
        Pattern.ParenthesizedPattern patternInsideOnePairOfParensNode ->
            removeParensFromPattern patternInsideOnePairOfParensNode

        _ ->
            patternNode


isSpecificValueOrFunction : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificValueOrFunction ( moduleName, name ) lookupTable expressionNode =
    case removeParens expressionNode of
        Node fnRange (Expression.FunctionOrValue _ foundName) ->
            (foundName == name)
                && (ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just moduleName)

        _ ->
            False


isSpecificCall : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificCall ( moduleName, fnName ) lookupTable expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.Application ((Node noneRange (Expression.FunctionOrValue _ foundFnName)) :: _ :: []) ->
            (foundFnName == fnName)
                && (ModuleNameLookupTable.moduleNameAt lookupTable noneRange == Just moduleName)

        _ ->
            False


getListSingleton : ModuleNameLookupTable -> Node Expression -> Maybe { element : Node Expression }
getListSingleton lookupTable baseExpressionNode =
    case Node.value (removeParens baseExpressionNode) of
        Expression.ListExpr [ element ] ->
            Just { element = element }

        Expression.ListExpr _ ->
            Nothing

        _ ->
            getListSingletonCall lookupTable baseExpressionNode


getListSingletonCall : ModuleNameLookupTable -> Node Expression -> Maybe { element : Node Expression }
getListSingletonCall lookupTable expressionNode =
    case getSpecificFunctionCall ( [ "List" ], "singleton" ) lookupTable expressionNode of
        Just singletonCall ->
            case singletonCall.argsAfterFirst of
                [] ->
                    Just { element = singletonCall.firstArg }

                _ :: _ ->
                    Nothing

        Nothing ->
            Nothing


{-| Parses calls and lambdas that are reducible to a call
-}
getSpecificFunctionCall :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            }
getSpecificFunctionCall ( moduleName, name ) lookupTable expressionNode =
    case getValueOrFunctionCall expressionNode of
        Just call ->
            case call.args of
                firstArg :: argsAfterFirst ->
                    if
                        (call.fnName /= name)
                            || (ModuleNameLookupTable.moduleNameAt lookupTable call.fnRange /= Just moduleName)
                    then
                        Nothing

                    else
                        Just
                            { nodeRange = call.nodeRange
                            , fnRange = call.fnRange
                            , firstArg = firstArg
                            , argsAfterFirst = argsAfterFirst
                            }

                [] ->
                    Nothing

        Nothing ->
            Nothing


{-| Parse a value or the collapsed function or a lambda fully reduced to a function
-}
getValueOrFunctionCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            }
getValueOrFunctionCall expressionNode =
    case getCollapsedUnreducedValueOrFunctionCall expressionNode of
        Just valueOrCall ->
            Just valueOrCall

        Nothing ->
            case getReducedLambda expressionNode of
                Just reducedLambda ->
                    case ( reducedLambda.lambdaPatterns, reducedLambda.callArguments ) of
                        ( [], args ) ->
                            Just
                                { nodeRange = reducedLambda.nodeRange
                                , fnName = reducedLambda.fnName
                                , fnRange = reducedLambda.fnRange
                                , args = args
                                }

                        ( _ :: _, _ ) ->
                            Nothing

                Nothing ->
                    Nothing


{-| Parses functions without arguments and lambdas that are reducible to a function without arguments
-}
getSpecificValueOrFunction : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificValueOrFunction ( moduleName, name ) lookupTable expressionNode =
    case getValueOrFunction expressionNode of
        Just normalFn ->
            if
                (normalFn.name /= name)
                    || (ModuleNameLookupTable.moduleNameAt lookupTable normalFn.range /= Just moduleName)
            then
                Nothing

            else
                Just normalFn.range

        Nothing ->
            Nothing


getValueOrFunction : Node Expression -> Maybe { name : String, range : Range }
getValueOrFunction expressionNode =
    case removeParens expressionNode of
        Node rangeInParens (Expression.FunctionOrValue _ foundName) ->
            Just { range = rangeInParens, name = foundName }

        nonFunctionOrValueNode ->
            case getReducedLambda nonFunctionOrValueNode of
                Just reducedLambdaToFn ->
                    case ( reducedLambdaToFn.lambdaPatterns, reducedLambdaToFn.callArguments ) of
                        ( [], [] ) ->
                            Just { range = reducedLambdaToFn.fnRange, name = reducedLambdaToFn.fnName }

                        ( _ :: _, _ ) ->
                            Nothing

                        ( _, _ :: _ ) ->
                            Nothing

                Nothing ->
                    Nothing


getCollapsedUnreducedValueOrFunctionCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            }
getCollapsedUnreducedValueOrFunctionCall baseNode =
    let
        step :
            { firstArg : Node Expression, argsAfterFirst : List (Node Expression), fed : Node Expression }
            -> Maybe { nodeRange : Range, fnRange : Range, fnName : String, args : List (Node Expression) }
        step layer =
            Maybe.map
                (\fed ->
                    { nodeRange = Node.range baseNode
                    , fnRange = fed.fnRange
                    , fnName = fed.fnName
                    , args = fed.args ++ (layer.firstArg :: layer.argsAfterFirst)
                    }
                )
                (getCollapsedUnreducedValueOrFunctionCall layer.fed)
    in
    case removeParens baseNode of
        Node fnRange (Expression.FunctionOrValue _ fnName) ->
            Just
                { nodeRange = Node.range baseNode
                , fnRange = fnRange
                , fnName = fnName
                , args = []
                }

        Node _ (Expression.Application (fed :: firstArg :: argsAfterFirst)) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = argsAfterFirst
                }

        Node _ (Expression.OperatorApplication "|>" _ firstArg fed) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = []
                }

        Node _ (Expression.OperatorApplication "<|" _ fed firstArg) ->
            step
                { fed = fed
                , firstArg = firstArg
                , argsAfterFirst = []
                }

        _ ->
            Nothing


getComposition : Node Expression -> Maybe { parentRange : Range, earlier : Node Expression, later : Node Expression }
getComposition expressionNode =
    let
        inParensNode : Node Expression
        inParensNode =
            removeParens expressionNode
    in
    case Node.value inParensNode of
        Expression.OperatorApplication "<<" _ composedLater earlier ->
            let
                ( later, parentRange ) =
                    case composedLater of
                        Node _ (Expression.OperatorApplication "<<" _ _ later_) ->
                            ( later_, { start = (Node.range later_).start, end = (Node.range earlier).end } )

                        endLater ->
                            ( endLater, Node.range inParensNode )
            in
            Just { earlier = earlier, later = later, parentRange = parentRange }

        Expression.OperatorApplication ">>" _ earlier composedLater ->
            let
                ( later, parentRange ) =
                    case composedLater of
                        Node _ (Expression.OperatorApplication ">>" _ later_ _) ->
                            ( later_, { start = (Node.range earlier).start, end = (Node.range later_).end } )

                        endLater ->
                            ( endLater, Node.range inParensNode )
            in
            Just { earlier = earlier, later = later, parentRange = parentRange }

        _ ->
            Nothing


isTupleFirstAccess : ModuleNameLookupTable -> Node Expression -> Bool
isTupleFirstAccess lookupTable expressionNode =
    case getSpecificValueOrFunction ( [ "Tuple" ], "first" ) lookupTable expressionNode of
        Just _ ->
            True

        Nothing ->
            isTupleFirstPatternLambda expressionNode


isTupleSecondAccess : ModuleNameLookupTable -> Node Expression -> Bool
isTupleSecondAccess lookupTable expressionNode =
    case getSpecificValueOrFunction ( [ "Tuple" ], "second" ) lookupTable expressionNode of
        Just _ ->
            True

        Nothing ->
            isTupleSecondPatternLambda expressionNode


isTupleFirstPatternLambda : Node Expression -> Bool
isTupleFirstPatternLambda expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.TuplePattern [ Node _ (Pattern.VarPattern firstVariableName), _ ]) ] ->
                    case Node.value lambda.expression of
                        Expression.FunctionOrValue [] resultName ->
                            resultName == firstVariableName

                        _ ->
                            False

                _ ->
                    False

        _ ->
            False


isTupleSecondPatternLambda : Node Expression -> Bool
isTupleSecondPatternLambda expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.LambdaExpression lambda ->
            case lambda.args of
                [ Node _ (Pattern.TuplePattern [ _, Node _ (Pattern.VarPattern firstVariableName) ]) ] ->
                    case Node.value lambda.expression of
                        Expression.FunctionOrValue [] resultName ->
                            resultName == firstVariableName

                        _ ->
                            False

                _ ->
                    False

        _ ->
            False


getUncomputedNumberValue : Node Expression -> Maybe Float
getUncomputedNumberValue expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.Integer n ->
            Just (toFloat n)

        Expression.Hex n ->
            Just (toFloat n)

        Expression.Floatable n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getUncomputedNumberValue expr)

        _ ->
            Nothing


isIdentity : ModuleNameLookupTable -> Node Expression -> Bool
isIdentity lookupTable baseExpressionNode =
    let
        expressionWithoutParensNode : Node Expression
        expressionWithoutParensNode =
            removeParens baseExpressionNode
    in
    case Node.value expressionWithoutParensNode of
        Expression.FunctionOrValue _ "identity" ->
            ModuleNameLookupTable.moduleNameFor lookupTable expressionWithoutParensNode == Just [ "Basics" ]

        Expression.LambdaExpression { args, expression } ->
            case args of
                arg :: [] ->
                    case getVarPattern arg of
                        Just patternName ->
                            getExpressionName expression
                                == Just patternName

                        _ ->
                            False

                _ ->
                    False

        _ ->
            False


getReducedLambda :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , callArguments : List (Node Expression)
            , lambdaPatterns : List (Node Pattern)
            }
getReducedLambda expressionNode =
    -- maybe a version of this is better located in Normalize?
    case getCollapsedLambda expressionNode of
        Just lambda ->
            case getCollapsedUnreducedValueOrFunctionCall lambda.expression of
                Just call ->
                    let
                        ( reducedCallArguments, reducedLambdaPatterns ) =
                            drop2EndingsWhile
                                (\( argument, pattern ) ->
                                    case Node.value (removeParens argument) of
                                        Expression.FunctionOrValue [] argument0Name ->
                                            case getVarPattern pattern of
                                                Just pattern0Name ->
                                                    pattern0Name == argument0Name

                                                _ ->
                                                    False

                                        _ ->
                                            False
                                )
                                ( call.args
                                , lambda.patterns
                                )
                    in
                    Just
                        { nodeRange = Node.range expressionNode
                        , fnName = call.fnName
                        , fnRange = call.fnRange
                        , callArguments = reducedCallArguments
                        , lambdaPatterns = reducedLambdaPatterns
                        }

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| Remove elements at the end of both given lists, then repeat for the previous elements until a given test returns False
-}
drop2EndingsWhile : (( a, b ) -> Bool) -> ( List a, List b ) -> ( List a, List b )
drop2EndingsWhile shouldDrop ( aList, bList ) =
    let
        ( reducedArgumentsReverse, reducedPatternsReverse ) =
            drop2BeginningsWhile
                shouldDrop
                ( List.reverse aList
                , List.reverse bList
                )
    in
    ( List.reverse reducedArgumentsReverse, List.reverse reducedPatternsReverse )


drop2BeginningsWhile : (( a, b ) -> Bool) -> ( List a, List b ) -> ( List a, List b )
drop2BeginningsWhile shouldDrop listPair =
    case listPair of
        ( [], bList ) ->
            ( [], bList )

        ( aList, [] ) ->
            ( aList, [] )

        ( aHead :: aTail, bHead :: bTail ) ->
            if shouldDrop ( aHead, bHead ) then
                drop2BeginningsWhile shouldDrop ( aTail, bTail )

            else
                ( aHead :: aTail, bHead :: bTail )


getCollapsedLambda : Node Expression -> Maybe { patterns : List (Node Pattern), expression : Node Expression }
getCollapsedLambda expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.LambdaExpression lambda ->
            case getCollapsedLambda lambda.expression of
                Nothing ->
                    Just
                        { patterns = lambda.args
                        , expression = lambda.expression
                        }

                Just innerCollapsedLambda ->
                    Just
                        { patterns = lambda.args ++ innerCollapsedLambda.patterns
                        , expression = innerCollapsedLambda.expression
                        }

        _ ->
            Nothing


getVarPattern : Node Pattern -> Maybe String
getVarPattern patternNode =
    case Node.value patternNode of
        Pattern.VarPattern name ->
            Just name

        Pattern.ParenthesizedPattern pattern ->
            getVarPattern pattern

        _ ->
            Nothing


patternListBindings : List (Node Pattern) -> Set String
patternListBindings patterns =
    List.foldl
        (\(Node _ pattern) soFar -> Set.union soFar (patternBindings pattern))
        Set.empty
        patterns


{-| Recursively find all bindings in a pattern.
-}
patternBindings : Pattern -> Set String
patternBindings pattern =
    case pattern of
        Pattern.ListPattern patterns ->
            patternListBindings patterns

        Pattern.TuplePattern patterns ->
            patternListBindings patterns

        Pattern.RecordPattern patterns ->
            Set.fromList (List.map Node.value patterns)

        Pattern.NamedPattern _ patterns ->
            patternListBindings patterns

        Pattern.UnConsPattern (Node _ headPattern) (Node _ tailPattern) ->
            Set.union (patternBindings tailPattern) (patternBindings headPattern)

        Pattern.VarPattern name ->
            Set.singleton name

        Pattern.AsPattern (Node _ pattern_) (Node _ name) ->
            Set.insert name (patternBindings pattern_)

        Pattern.ParenthesizedPattern (Node _ inParens) ->
            patternBindings inParens

        Pattern.AllPattern ->
            Set.empty

        Pattern.UnitPattern ->
            Set.empty

        Pattern.CharPattern _ ->
            Set.empty

        Pattern.StringPattern _ ->
            Set.empty

        Pattern.IntPattern _ ->
            Set.empty

        Pattern.HexPattern _ ->
            Set.empty

        Pattern.FloatPattern _ ->
            Set.empty


declarationListBindings : List (Node Declaration) -> Set String
declarationListBindings declarationList =
    declarationList
        |> List.map (\(Node _ declaration) -> declarationBindings declaration)
        |> List.foldl (\bindings soFar -> Set.union soFar bindings) Set.empty


declarationBindings : Declaration -> Set String
declarationBindings declaration =
    case declaration of
        Declaration.CustomTypeDeclaration variantType ->
            variantType.constructors
                |> List.map (\(Node _ variant) -> Node.value variant.name)
                |> Set.fromList

        Declaration.FunctionDeclaration functionDeclaration ->
            Set.singleton
                (Node.value (Node.value functionDeclaration.declaration).name)

        _ ->
            Set.empty


letDeclarationBindings : Expression.LetDeclaration -> Set String
letDeclarationBindings letDeclaration =
    case letDeclaration of
        Expression.LetFunction fun ->
            Set.singleton
                (fun.declaration |> Node.value |> .name |> Node.value)

        Expression.LetDestructuring (Node _ pattern) _ ->
            patternBindings pattern


letDeclarationListBindings : List (Node Expression.LetDeclaration) -> Set String
letDeclarationListBindings letDeclarationList =
    letDeclarationList
        |> List.map
            (\(Node _ declaration) -> letDeclarationBindings declaration)
        |> List.foldl (\bindings soFar -> Set.union soFar bindings) Set.empty


getExpressionName : Node Expression -> Maybe String
getExpressionName expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.FunctionOrValue [] name ->
            Just name

        _ ->
            Nothing


isListLiteral : Node Expression -> Bool
isListLiteral expressionNode =
    case Node.value expressionNode of
        Expression.ListExpr _ ->
            True

        _ ->
            False


getListLiteral : Node Expression -> Maybe (List (Node Expression))
getListLiteral expressionNode =
    case Node.value expressionNode of
        Expression.ListExpr list ->
            Just list

        _ ->
            Nothing


getCollapsedCons : Node Expression -> Maybe { consed : List (Node Expression), tail : Node Expression }
getCollapsedCons expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.OperatorApplication "::" _ head tail ->
            let
                tailCollapsed : Maybe { consed : List (Node Expression), tail : Node Expression }
                tailCollapsed =
                    getCollapsedCons tail
            in
            case tailCollapsed of
                Nothing ->
                    Just { consed = [ head ], tail = tail }

                Just tailCollapsedList ->
                    Just { consed = head :: tailCollapsedList.consed, tail = tailCollapsedList.tail }

        _ ->
            Nothing


getBool : ModuleNameLookupTable -> Node Expression -> Maybe Bool
getBool lookupTable expressionNode =
    if isSpecificBool True lookupTable expressionNode then
        Just True

    else if isSpecificBool False lookupTable expressionNode then
        Just False

    else
        Nothing


isSpecificBool : Bool -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificBool specificBool lookupTable expressionNode =
    isSpecificValueOrFunction ( [ "Basics" ], boolToString specificBool ) lookupTable expressionNode


getTuple : Node Expression -> Maybe { range : Range, first : Node Expression, second : Node Expression }
getTuple expressionNode =
    case Node.value expressionNode of
        Expression.TupledExpression (first :: second :: []) ->
            Just { range = Node.range expressionNode, first = first, second = second }

        _ ->
            Nothing


getBoolPattern : ModuleNameLookupTable -> Node Pattern -> Maybe Bool
getBoolPattern lookupTable patternNode =
    case Node.value patternNode of
        Pattern.NamedPattern { name } _ ->
            case name of
                "True" ->
                    if ModuleNameLookupTable.moduleNameFor lookupTable patternNode == Just [ "Basics" ] then
                        Just True

                    else
                        Nothing

                "False" ->
                    if ModuleNameLookupTable.moduleNameFor lookupTable patternNode == Just [ "Basics" ] then
                        Just False

                    else
                        Nothing

                _ ->
                    Nothing

        Pattern.ParenthesizedPattern pattern ->
            getBoolPattern lookupTable pattern

        _ ->
            Nothing


isSpecificOrder : Order -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificOrder specificOrder lookupTable expression =
    isSpecificValueOrFunction ( [ "Basics" ], orderToString specificOrder ) lookupTable expression


getOrder : ModuleNameLookupTable -> Node Expression -> Maybe Order
getOrder lookupTable expression =
    if isSpecificOrder LT lookupTable expression then
        Just LT

    else if isSpecificOrder EQ lookupTable expression then
        Just EQ

    else if isSpecificOrder GT lookupTable expression then
        Just GT

    else
        Nothing


isEmptyList : Node Expression -> Bool
isEmptyList expressionNode =
    case Node.value (removeParens expressionNode) of
        Expression.ListExpr [] ->
            True

        _ ->
            False


isBinaryOperation : String -> Infer.Resources a -> Node Expression -> Bool
isBinaryOperation symbol checkInfo expression =
    case expression |> Normalize.normalize checkInfo |> Node.value of
        Expression.PrefixOperator operatorSymbol ->
            operatorSymbol == symbol

        Expression.LambdaExpression lambda ->
            case lambda.args of
                -- invalid syntax
                [] ->
                    False

                [ Node _ (Pattern.VarPattern element) ] ->
                    case Node.value lambda.expression of
                        Expression.Application [ Node _ (Expression.PrefixOperator operatorSymbol), Node _ (Expression.FunctionOrValue [] argument) ] ->
                            (operatorSymbol == symbol)
                                && (argument == element)

                        -- no simple application
                        _ ->
                            False

                [ Node _ (Pattern.VarPattern element), Node _ (Pattern.VarPattern soFar) ] ->
                    case Node.value lambda.expression of
                        Expression.Application [ Node _ (Expression.PrefixOperator operatorSymbol), Node _ (Expression.FunctionOrValue [] left), Node _ (Expression.FunctionOrValue [] right) ] ->
                            (operatorSymbol == symbol)
                                && ((left == element && right == soFar)
                                        || (left == soFar && right == element)
                                   )

                        Expression.OperatorApplication operatorSymbol _ (Node _ (Expression.FunctionOrValue [] left)) (Node _ (Expression.FunctionOrValue [] right)) ->
                            (operatorSymbol == symbol)
                                && ((left == element && right == soFar)
                                        || (left == soFar && right == element)
                                   )

                        _ ->
                            False

                -- too many/unsimplified patterns
                _ ->
                    False

        -- not a known simple operator function
        _ ->
            False


getTypeExposeIncludingVariants : Exposing.TopLevelExpose -> Maybe String
getTypeExposeIncludingVariants expose =
    case expose of
        Exposing.InfixExpose _ ->
            Nothing

        Exposing.FunctionExpose _ ->
            Nothing

        Exposing.TypeOrAliasExpose _ ->
            Nothing

        Exposing.TypeExpose variantType ->
            case variantType.open of
                Nothing ->
                    Nothing

                Just _ ->
                    Just variantType.name


nameOfExpose : Exposing.TopLevelExpose -> String
nameOfExpose topLevelExpose =
    case topLevelExpose of
        Exposing.FunctionExpose name ->
            name

        Exposing.TypeOrAliasExpose name ->
            name

        Exposing.InfixExpose name ->
            name

        Exposing.TypeExpose { name } ->
            name



-- STRING


emptyStringAsString : String
emptyStringAsString =
    "\"\""


boolToString : Bool -> String
boolToString bool =
    if bool then
        "True"

    else
        "False"


orderToString : Order -> String
orderToString order =
    case order of
        LT ->
            "LT"

        EQ ->
            "EQ"

        GT ->
            "GT"


{-| Put a `ModuleName` and thing name together as a string.
If desired, call in combination with `qualify`
-}
qualifiedToString : ( ModuleName, String ) -> String
qualifiedToString ( moduleName, name ) =
    if List.isEmpty moduleName then
        name

    else
        moduleNameToString moduleName ++ "." ++ name


moduleNameToString : ModuleName -> String
moduleNameToString moduleName =
    String.join "." moduleName


moduleNameFromString : String -> ModuleName
moduleNameFromString string =
    String.split "." string
