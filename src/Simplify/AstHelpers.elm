module Simplify.AstHelpers exposing
    ( CaseCatchSomeTree
    , Catch
    , CatchNoneOrSome(..)
    , CatchSomeInfo
    , CatchSomeKind(..)
    , boolToString
    , casePatternCatchFor
    , declarationListBindings
    , emptyStringAsString
    , getBool
    , getBooleanPattern
    , getCollapsedCons
    , getListLiteral
    , getListSingleton
    , getListSingletonCall
    , getNotFunction
    , getOrder
    , getSpecificFunction
    , getSpecificFunctionCall
    , getSpecificReducedFunction
    , getSpecificReducedFunctionCall
    , getSpecificValueOrFunction
    , getTuple
    , getTypeExposeIncludingVariants
    , getUncomputedNumberValue
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
import Tree exposing (Tree)


removeParens : Node Expression -> Node Expression
removeParens node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParens expr

        _ ->
            node


removeParensFromPattern : Node Pattern -> Node Pattern
removeParensFromPattern node =
    case Node.value node of
        Pattern.ParenthesizedPattern pattern ->
            removeParensFromPattern pattern

        _ ->
            node


isSpecificValueOrFunction : ModuleName -> String -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificValueOrFunction moduleName fnName lookupTable node =
    case removeParens node of
        Node noneRange (Expression.FunctionOrValue _ foundFnName) ->
            (foundFnName == fnName)
                && (ModuleNameLookupTable.moduleNameAt lookupTable noneRange == Just moduleName)

        _ ->
            False


getSpecificValueOrFunction : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Maybe { fnRange : Range }
getSpecificValueOrFunction ( moduleName, fnName ) lookupTable node =
    case removeParens node of
        Node noneRange (Expression.FunctionOrValue _ foundFnName) ->
            if
                (foundFnName == fnName)
                    && (ModuleNameLookupTable.moduleNameAt lookupTable noneRange == Just moduleName)
            then
                Just { fnRange = noneRange }

            else
                Nothing

        _ ->
            Nothing


isSpecificCall : ModuleName -> String -> ModuleNameLookupTable -> Node Expression -> Bool
isSpecificCall moduleName fnName lookupTable node =
    case Node.value (removeParens node) of
        Expression.Application ((Node noneRange (Expression.FunctionOrValue _ foundFnName)) :: _ :: []) ->
            (foundFnName == fnName)
                && (ModuleNameLookupTable.moduleNameAt lookupTable noneRange == Just moduleName)

        _ ->
            False


getListSingleton : ModuleNameLookupTable -> Node Expression -> Maybe { element : Node Expression }
getListSingleton lookupTable baseNode =
    case Node.value (removeParens baseNode) of
        Expression.ListExpr [ element ] ->
            Just { element = element }

        Expression.ListExpr _ ->
            Nothing

        _ ->
            getListSingletonCall lookupTable baseNode


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


getSpecificFunction : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Maybe Range
getSpecificFunction ( moduleName, name ) lookupTable baseNode =
    case removeParens baseNode of
        Node fnRange (Expression.FunctionOrValue _ foundName) ->
            if
                (foundName == name)
                    && (ModuleNameLookupTable.moduleNameAt lookupTable fnRange == Just moduleName)
            then
                Just fnRange

            else
                Nothing

        _ ->
            Nothing


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
getSpecificFunctionCall ( moduleName, name ) lookupTable baseNode =
    getFunctionCall baseNode
        |> Maybe.andThen
            (\call ->
                if
                    (call.fnName /= name)
                        || (ModuleNameLookupTable.moduleNameAt lookupTable call.fnRange /= Just moduleName)
                then
                    Nothing

                else
                    Just
                        { nodeRange = call.nodeRange
                        , fnRange = call.fnRange
                        , firstArg = call.firstArg
                        , argsAfterFirst = call.argsAfterFirst
                        }
            )


getFunctionCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , firstArg : Node Expression
            , argsAfterFirst : List (Node Expression)
            }
getFunctionCall baseNode =
    case Node.value (removeParens baseNode) of
        Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: firstArg :: argsAfterFirst) ->
            Just
                { nodeRange = Node.range baseNode
                , fnRange = fnRange
                , fnName = fnName
                , firstArg = firstArg
                , argsAfterFirst = argsAfterFirst
                }

        Expression.OperatorApplication "|>" _ firstArg fedFunction ->
            case fedFunction of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    Just
                        { nodeRange = Node.range baseNode
                        , fnRange = fnRange
                        , fnName = fnName
                        , firstArg = firstArg
                        , argsAfterFirst = []
                        }

                Node _ (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: argsAfterFirst)) ->
                    Just
                        { nodeRange = Node.range baseNode
                        , fnRange = fnRange
                        , fnName = fnName
                        , firstArg = firstArg
                        , argsAfterFirst = argsAfterFirst
                        }

                _ ->
                    Nothing

        Expression.OperatorApplication "<|" _ fedFunction firstArg ->
            case fedFunction of
                Node fnRange (Expression.FunctionOrValue _ fnName) ->
                    Just
                        { nodeRange = Node.range baseNode
                        , fnRange = fnRange
                        , fnName = fnName
                        , firstArg = firstArg
                        , argsAfterFirst = []
                        }

                Node _ (Expression.Application ((Node fnRange (Expression.FunctionOrValue _ fnName)) :: argsAfterFirst)) ->
                    Just
                        { nodeRange = Node.range baseNode
                        , fnRange = fnRange
                        , fnName = fnName
                        , firstArg = firstArg
                        , argsAfterFirst = argsAfterFirst
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


getCollapsedValueOrFunction :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , args : List (Node Expression)
            }
getCollapsedValueOrFunction baseNode =
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
                (getCollapsedValueOrFunction layer.fed)
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


getNotFunction : ModuleNameLookupTable -> Node Expression -> Maybe Range
getNotFunction lookupTable baseNode =
    getSpecificFunction ( [ "Basics" ], "not" ) lookupTable baseNode


isTupleFirstAccess : ModuleNameLookupTable -> Node Expression -> Bool
isTupleFirstAccess lookupTable expressionNode =
    case getSpecificReducedFunction ( [ "Tuple" ], "first" ) lookupTable expressionNode of
        Just _ ->
            True

        Nothing ->
            isTupleFirstPatternLambda expressionNode


isTupleSecondAccess : ModuleNameLookupTable -> Node Expression -> Bool
isTupleSecondAccess lookupTable expressionNode =
    case getSpecificReducedFunction ( [ "Tuple" ], "second" ) lookupTable expressionNode of
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
getUncomputedNumberValue node =
    case Node.value (removeParens node) of
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
isIdentity lookupTable baseNode =
    let
        node : Node Expression
        node =
            removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "identity" ->
            ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ]

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


{-| Parses variables and lambdas that are reducible to a variable
-}
getSpecificReducedFunction : ( ModuleName, String ) -> ModuleNameLookupTable -> Node Expression -> Maybe { fnRange : Range }
getSpecificReducedFunction ( moduleName, name ) lookupTable expressionNode =
    Maybe.andThen
        (\reducedFunction ->
            if
                (reducedFunction.fnName /= name)
                    || (ModuleNameLookupTable.moduleNameAt lookupTable reducedFunction.fnRange /= Just moduleName)
            then
                Nothing

            else
                Just { fnRange = reducedFunction.fnRange }
        )
        (getReducedFunction expressionNode)


{-| Parses variables and lambdas that are reducible to a variable
-}
getReducedFunction : Node Expression -> Maybe { fnRange : Range, fnName : String }
getReducedFunction expressionNode =
    case removeParens expressionNode of
        Node fnRange (Expression.FunctionOrValue _ fnName) ->
            Just { fnRange = fnRange, fnName = fnName }

        _ ->
            Maybe.andThen
                (\reducedLambdaToCall ->
                    case ( reducedLambdaToCall.lambdaPatterns, reducedLambdaToCall.callArguments ) of
                        ( [], [] ) ->
                            Just { fnRange = reducedLambdaToCall.fnRange, fnName = reducedLambdaToCall.fnName }

                        ( _ :: _, [] ) ->
                            Nothing

                        ( [], _ :: _ ) ->
                            Nothing

                        ( _ :: _, _ :: _ ) ->
                            Nothing
                )
                (getReducedLambdaToCall expressionNode)


{-| Parses calls and lambdas that are reducible to a call
-}
getSpecificReducedFunctionCall :
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
getSpecificReducedFunctionCall ( moduleName, name ) lookupTable expressionNode =
    case getSpecificFunctionCall ( moduleName, name ) lookupTable expressionNode of
        Just call ->
            Just call

        Nothing ->
            Maybe.andThen
                (\reducedLambdaToCall ->
                    case ( reducedLambdaToCall.lambdaPatterns, reducedLambdaToCall.callArguments ) of
                        ( [], [] ) ->
                            Nothing

                        ( _ :: _, [] ) ->
                            Nothing

                        ( _ :: _, _ :: _ ) ->
                            Nothing

                        ( [], firstArg :: argsAfterFirst ) ->
                            Just
                                { nodeRange = reducedLambdaToCall.nodeRange
                                , fnRange = reducedLambdaToCall.fnRange
                                , firstArg = firstArg
                                , argsAfterFirst = argsAfterFirst
                                }
                )
                (getSpecificReducedLambdaToCall ( moduleName, name ) lookupTable expressionNode)


getSpecificReducedLambdaToCall :
    ( ModuleName, String )
    -> ModuleNameLookupTable
    -> Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnRange : Range
            , callArguments : List (Node Expression)
            , lambdaPatterns : List (Node Pattern)
            }
getSpecificReducedLambdaToCall ( moduleName, name ) lookupTable expressionNode =
    getReducedLambdaToCall expressionNode
        |> Maybe.andThen
            (\reducedLambdaToCall ->
                if
                    (reducedLambdaToCall.fnName /= name)
                        || (ModuleNameLookupTable.moduleNameAt lookupTable reducedLambdaToCall.fnRange /= Just moduleName)
                then
                    Nothing

                else
                    Just
                        { nodeRange = reducedLambdaToCall.nodeRange
                        , fnRange = reducedLambdaToCall.fnRange
                        , callArguments = reducedLambdaToCall.callArguments
                        , lambdaPatterns = reducedLambdaToCall.lambdaPatterns
                        }
            )


getReducedLambdaToCall :
    Node Expression
    ->
        Maybe
            { nodeRange : Range
            , fnName : String
            , fnRange : Range
            , callArguments : List (Node Expression)
            , lambdaPatterns : List (Node Pattern)
            }
getReducedLambdaToCall expressionNode =
    -- maybe a version of this is better located in Normalize?
    case getCollapsedLambda expressionNode of
        Just lambda ->
            case getCollapsedValueOrFunction lambda.expression of
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
getVarPattern node =
    case Node.value node of
        Pattern.VarPattern name ->
            Just name

        Pattern.ParenthesizedPattern pattern ->
            getVarPattern pattern

        _ ->
            Nothing


getCollapsedPatternList : Pattern -> Maybe { beginning : List (Node Pattern), tail : Maybe (Node Pattern) }
getCollapsedPatternList pattern =
    case pattern of
        Pattern.ListPattern elements ->
            Just { beginning = elements, tail = Nothing }

        Pattern.UnConsPattern head tail ->
            case getCollapsedPatternList (Node.value tail) of
                Nothing ->
                    Just { beginning = [ head ], tail = Just tail }

                Just tailCollapsed ->
                    Just { beginning = head :: tailCollapsed.beginning, tail = tailCollapsed.tail }

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
getExpressionName node =
    case Node.value (removeParens node) of
        Expression.FunctionOrValue [] name ->
            Just name

        _ ->
            Nothing


isListLiteral : Node Expression -> Bool
isListLiteral node =
    case Node.value node of
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
    isSpecificValueOrFunction [ "Basics" ] (boolToString specificBool) lookupTable expressionNode


getTuple : Node Expression -> Maybe { range : Range, first : Node Expression, second : Node Expression }
getTuple expressionNode =
    case Node.value expressionNode of
        Expression.TupledExpression (first :: second :: []) ->
            Just { range = Node.range expressionNode, first = first, second = second }

        _ ->
            Nothing


getBooleanPattern : ModuleNameLookupTable -> Node Pattern -> Maybe Bool
getBooleanPattern lookupTable node =
    case Node.value node of
        Pattern.NamedPattern { name } _ ->
            case name of
                "True" ->
                    if ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ] then
                        Just True

                    else
                        Nothing

                "False" ->
                    if ModuleNameLookupTable.moduleNameFor lookupTable node == Just [ "Basics" ] then
                        Just False

                    else
                        Nothing

                _ ->
                    Nothing

        Pattern.ParenthesizedPattern pattern ->
            getBooleanPattern lookupTable pattern

        _ ->
            Nothing


getOrder : ModuleNameLookupTable -> Node Expression -> Maybe Order
getOrder lookupTable expression =
    if isSpecificValueOrFunction [ "Basics" ] "LT" lookupTable expression then
        Just LT

    else if isSpecificValueOrFunction [ "Basics" ] "EQ" lookupTable expression then
        Just EQ

    else if isSpecificValueOrFunction [ "Basics" ] "GT" lookupTable expression then
        Just GT

    else
        Nothing


isEmptyList : Node Expression -> Bool
isEmptyList node =
    case Node.value (removeParens node) of
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


type alias Catch =
    CatchNoneOrSome CaseCatchSomeTree


type CatchNoneOrSome some
    = CatchSome some
    | -- a case that impossible:
      --     - there is no pattern for this (like function, glsl, ...)
      --     - impossible to match in practice (like case Nothing of Just ...)
      -- impossible patterns can safely be removed.
      -- if all patterns are impossible, there will be be a compiler error
      CatchNone


onCatchSome : (CaseCatchSomeTree -> CaseCatchSomeTree) -> Catch -> Catch
onCatchSome caseCatchSomeTreeChange catch =
    case catch of
        CatchNone ->
            CatchNone

        CatchSome catchSome ->
            CatchSome (caseCatchSomeTreeChange catchSome)


type alias CatchSomeInfo =
    { patternRange : Range
    , catch : CatchSomeKind
    }


type CatchSomeKind
    = -- CatchSub means a pattern that matches only a narrower subset of possible values
      -- (like case result of Ok value -> or case list of [] ->)
      -- if the only case is CatchSub, there will be be a compiler error
      CatchSub
    | -- CatchAll means a pattern that always matches the cased expression:
      --     - a required pattern (like case () of () -> or case 'a' of 'a' ->)
      --     - a (partially required) pattern that catches all sub-patterns (like case a of _ -> or case Ok a of Ok b ->)
      -- if other patterns exist in later cases, their cases can be removed.
      -- if that pattern also isn't (partially) required, there will be be a compiler error
      -- if the patterns for the same expression (part) from all cases are CatchAll,
      -- they are all simplifiable to ()
      CatchAll
        { -- if containsVariables is True, then
          -- the case pattern contains at least 1 pattern variable there (like case Err err of Err error -> or case [ el ] of [ a ] ->)
          containsVariables : Bool
        }


onCatchAll : { containsVariables : Bool -> Bool } -> CatchSomeKind -> CatchSomeKind
onCatchAll catchAllInfoChange catchSomeKind =
    case catchSomeKind of
        CatchSub ->
            CatchSub

        CatchAll catchAll ->
            CatchAll { containsVariables = catchAllInfoChange.containsVariables catchAll.containsVariables }


type alias CaseCatchSomeTree =
    Tree CatchSomeTreeElement


type alias CatchSomeTreeElement =
    { expressionRange : Range
    , patternInCase : CatchSomeInfo
    }


patternInCaseAlter : (CatchSomeInfo -> CatchSomeInfo) -> CatchSomeTreeElement -> CatchSomeTreeElement
patternInCaseAlter catchSomeInfoChange caseCatchSomeTreeElement =
    { caseCatchSomeTreeElement
        | patternInCase =
            catchSomeInfoChange caseCatchSomeTreeElement.patternInCase
    }


{-| See what a given pattern catches
from a given expression, including ranges of matching variants with at least one argument until their first argument.

Make sure to normalize the expression for the best detection (what are calls, what are functions, ...)

-}
casePatternCatchFor :
    ModuleNameLookupTable
    -> Node Expression
    -> Node Pattern
    -> Catch
casePatternCatchFor lookupTable casedExpression pattern =
    let
        -- only use for surface patterns (those directly casing on casedExpression)
        treeWith : CatchSomeKind -> List CaseCatchSomeTree -> CaseCatchSomeTree
        treeWith catch parts =
            Tree.tree
                { expressionRange = Node.range casedExpression
                , patternInCase =
                    { catch = catch, patternRange = Node.range pattern }
                }
                parts
    in
    case Node.value pattern of
        Pattern.AllPattern ->
            CatchSome
                (treeWith (CatchAll { containsVariables = False }) [])

        Pattern.VarPattern _ ->
            CatchSome
                (treeWith (CatchAll { containsVariables = True }) [])

        Pattern.AsPattern destructured _ ->
            onCatchSome
                (\destructuredCatchSome ->
                    Tree.elementAlter
                        (\el ->
                            patternInCaseAlter
                                (\info ->
                                    { patternRange = Node.range pattern
                                    , catch = onCatchAll { containsVariables = \_ -> True } info.catch
                                    }
                                )
                                el
                        )
                        destructuredCatchSome
                )
                (casePatternCatchFor lookupTable casedExpression destructured)

        Pattern.ParenthesizedPattern inParens ->
            onCatchSome
                (\inParensCatchSome ->
                    Tree.elementAlter
                        (\el ->
                            patternInCaseAlter (\info -> { info | patternRange = Node.range pattern }) el
                        )
                        inParensCatchSome
                )
                (casePatternCatchFor lookupTable casedExpression inParens)

        _ ->
            casePatternSpecificCatchFor lookupTable casedExpression pattern


{-| See what a given pattern catches
from an expression that is fully unknown (like value/function variables).
-}
casePatternCatchForUnknown : Pattern -> CatchSomeKind
casePatternCatchForUnknown pattern =
    case pattern of
        Pattern.UnitPattern ->
            CatchAll { containsVariables = False }

        Pattern.AllPattern ->
            CatchAll { containsVariables = False }

        Pattern.VarPattern _ ->
            CatchAll { containsVariables = True }

        Pattern.AsPattern (Node _ destructured) _ ->
            onCatchAll { containsVariables = \_ -> True }
                (casePatternCatchForUnknown destructured)

        Pattern.ParenthesizedPattern (Node _ inParens) ->
            casePatternCatchForUnknown inParens

        Pattern.RecordPattern _ ->
            CatchSub

        -- doesn't exist
        Pattern.FloatPattern _ ->
            CatchSub

        Pattern.HexPattern _ ->
            CatchSub

        Pattern.IntPattern _ ->
            CatchSub

        Pattern.CharPattern _ ->
            CatchSub

        Pattern.UnConsPattern _ _ ->
            CatchSub

        Pattern.ListPattern _ ->
            CatchSub

        Pattern.StringPattern _ ->
            CatchSub

        Pattern.TuplePattern tupleParts ->
            List.foldl
                (\(Node _ part) soFar ->
                    case soFar of
                        CatchSub ->
                            CatchSub

                        CatchAll soFarCatchAll ->
                            case casePatternCatchForUnknown part of
                                CatchSub ->
                                    CatchSub

                                CatchAll catchAll ->
                                    CatchAll
                                        { containsVariables =
                                            soFarCatchAll.containsVariables
                                                || catchAll.containsVariables
                                        }
                )
                (CatchAll { containsVariables = False })
                tupleParts

        Pattern.NamedPattern _ _ ->
            CatchSub


{-| Find out what the sum of the 2 given individual elements catch.
CatchNone > CatchSub > CatchAll. That means

  - if any is CatchNone → CatchNone
  - otherwise if one is CatchSub → CatchSub,
  - otherwise CatchAll

-}
catchMergeWith :
    (() -> Catch)
    -> CatchNoneOrSome { catch : CatchSomeKind, parts : List CaseCatchSomeTree }
    -> CatchNoneOrSome { catch : CatchSomeKind, parts : List CaseCatchSomeTree }
catchMergeWith lazyOtherCatch structureCatch =
    case structureCatch of
        CatchNone ->
            CatchNone

        CatchSome catchSome ->
            case lazyOtherCatch () of
                CatchNone ->
                    CatchNone

                CatchSome otherCatchSomeTree ->
                    CatchSome
                        { catch =
                            case ( catchSome.catch, (Tree.element otherCatchSomeTree).patternInCase.catch ) of
                                ( CatchAll catchAll, CatchAll otherCatchAll ) ->
                                    CatchAll { containsVariables = catchAll.containsVariables || otherCatchAll.containsVariables }

                                ( _, CatchSub ) ->
                                    CatchSub

                                ( CatchSub, _ ) ->
                                    CatchSub
                        , parts = otherCatchSomeTree :: catchSome.parts
                        }


casePatternSpecificCatchFor :
    ModuleNameLookupTable
    -> Node Expression
    -> Node Pattern
    -> Catch
casePatternSpecificCatchFor lookupTable casedExpressionNode patternNode =
    let
        (Node expressionRange expression) =
            casedExpressionNode

        (Node patternRange pattern) =
            patternNode

        -- only use for surface patterns (those directly casing on the expression at expressionRange)
        treeWith : CatchSomeKind -> List CaseCatchSomeTree -> CaseCatchSomeTree
        treeWith catch parts =
            Tree.tree
                { expressionRange = expressionRange
                , patternInCase =
                    { catch = catch, patternRange = patternRange }
                }
                parts

        catchAllIfElseNone : Bool -> Catch
        catchAllIfElseNone requirementSatisfied =
            if requirementSatisfied then
                CatchSome (treeWith (CatchAll { containsVariables = False }) [])

            else
                CatchNone

        casePatternSpecificCatchForInt : Int -> Pattern -> Catch
        casePatternSpecificCatchForInt int patternForInt =
            case patternForInt of
                Pattern.HexPattern patternInt ->
                    catchAllIfElseNone (patternInt == int)

                Pattern.IntPattern patternInt ->
                    catchAllIfElseNone (patternInt == int)

                _ ->
                    CatchNone

        casePatternCatchForRecord : () -> Catch
        casePatternCatchForRecord () =
            case pattern of
                Pattern.RecordPattern _ ->
                    CatchSome (treeWith (CatchAll { containsVariables = True }) [])

                _ ->
                    CatchNone

        catchForPairs : List (Node Expression) -> List (Node Pattern) -> Catch
        catchForPairs expressions patterns =
            case structureCatchForPairs expressions patterns of
                CatchNone ->
                    CatchNone

                CatchSome structureCatchSome ->
                    CatchSome (treeWith structureCatchSome.catch structureCatchSome.parts)

        structureCatchForPairs : List (Node Expression) -> List (Node Pattern) -> CatchNoneOrSome { catch : CatchSomeKind, parts : List CaseCatchSomeTree }
        structureCatchForPairs expressions patterns =
            case ( expressions, patterns ) of
                ( [], _ :: _ ) ->
                    CatchSome { parts = [], catch = CatchAll { containsVariables = False } }

                ( _ :: _, [] ) ->
                    CatchSome { parts = [], catch = CatchAll { containsVariables = False } }

                ( [], [] ) ->
                    CatchSome { parts = [], catch = CatchAll { containsVariables = False } }

                ( expressionPart :: expressionTail, patternPart :: patternTail ) ->
                    let
                        elementCatchEvaluated : () -> Catch
                        elementCatchEvaluated () =
                            casePatternCatchFor lookupTable expressionPart patternPart

                        tailCatchAndSimplification : CatchNoneOrSome { catch : CatchSomeKind, parts : List CaseCatchSomeTree }
                        tailCatchAndSimplification =
                            structureCatchForPairs expressionTail patternTail
                    in
                    catchMergeWith elementCatchEvaluated tailCatchAndSimplification
    in
    case expression of
        Expression.UnitExpr ->
            case pattern of
                Pattern.UnitPattern ->
                    CatchSome (treeWith (CatchAll { containsVariables = False }) [])

                _ ->
                    CatchNone

        Expression.FunctionOrValue qualification name ->
            if not (stringStartsWithUpper name) then
                CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

            else
                -- variant without arguments or empty record type alias constructor
                case pattern of
                    Pattern.NamedPattern patternQualified [] ->
                        let
                            patternFullyQualified : ( ModuleName, String )
                            patternFullyQualified =
                                ( Maybe.withDefault patternQualified.moduleName (ModuleNameLookupTable.moduleNameAt lookupTable patternRange)
                                , patternQualified.name
                                )

                            fullyQualified : ( ModuleName, String )
                            fullyQualified =
                                ( Maybe.withDefault qualification (ModuleNameLookupTable.moduleNameAt lookupTable expressionRange)
                                , name
                                )
                        in
                        if patternFullyQualified == fullyQualified then
                            CatchSome (treeWith (CatchAll { containsVariables = False }) [])

                        else
                            CatchNone

                    _ ->
                        -- an empty record type alias constructor doesn't have a specific matching pattern
                        -- a curried variant constructor doesn't have a matching pattern either
                        CatchNone

        Expression.Hex int ->
            casePatternSpecificCatchForInt int pattern

        Expression.Integer int ->
            casePatternSpecificCatchForInt int pattern

        Expression.CharLiteral char ->
            case pattern of
                Pattern.CharPattern patternChar ->
                    catchAllIfElseNone (char == patternChar)

                _ ->
                    CatchNone

        -- compiler error
        Expression.Floatable _ ->
            CatchNone

        Expression.Literal string ->
            case pattern of
                Pattern.StringPattern patternString ->
                    catchAllIfElseNone (string == patternString)

                _ ->
                    CatchNone

        -- doesn't exist
        Expression.Application [] ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        -- doesn't exist
        Expression.Application (_ :: []) ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.Application ((Node fedRange fed) :: firstArg :: argsAfterFirst) ->
            case fed of
                Expression.FunctionOrValue qualification name ->
                    if not (stringStartsWithUpper name) then
                        CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

                    else
                        -- expression is (curried) variant or (curried) record type alias constructor
                        case pattern of
                            Pattern.NamedPattern patternQualified namedPatternArguments ->
                                let
                                    patternFullyQualified : ( ModuleName, String )
                                    patternFullyQualified =
                                        ( Maybe.withDefault patternQualified.moduleName (ModuleNameLookupTable.moduleNameAt lookupTable patternRange)
                                        , patternQualified.name
                                        )

                                    fedFullyQualified : ( ModuleName, String )
                                    fedFullyQualified =
                                        ( Maybe.withDefault qualification (ModuleNameLookupTable.moduleNameAt lookupTable fedRange)
                                        , name
                                        )
                                in
                                if patternFullyQualified /= fedFullyQualified then
                                    CatchNone

                                else if List.length (firstArg :: argsAfterFirst) < List.length namedPatternArguments then
                                    -- constructor is only partially applied
                                    CatchNone

                                else
                                    -- pattern with same variant
                                    catchForPairs (firstArg :: argsAfterFirst) namedPatternArguments

                            _ ->
                                -- expression is (curried) record type alias constructor
                                casePatternCatchForRecord ()

                _ ->
                    CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.OperatorApplication operatorName _ left right ->
            if List.member operatorName [ ">>", "<<", "|.", "|=", "<?>", "</>" ] then
                CatchNone

            else if operatorName == "::" then
                case getCollapsedPatternList pattern of
                    Nothing ->
                        CatchNone

                    Just collapsedPatternList ->
                        let
                            collapsedCons : { consed : List (Node Expression), tail : Node Expression }
                            collapsedCons =
                                case getCollapsedCons right of
                                    Just tailConsCollapsed ->
                                        { consed = left :: tailConsCollapsed.consed, tail = tailConsCollapsed.tail }

                                    Nothing ->
                                        { consed = [ left ], tail = right }

                            beginningCatch : Catch
                            beginningCatch =
                                catchForPairs collapsedCons.consed collapsedPatternList.beginning
                        in
                        case beginningCatch of
                            CatchSome beginningCatchSome ->
                                CatchSome
                                    (treeWith
                                        (case (Tree.element beginningCatchSome).patternInCase.catch of
                                            CatchSub ->
                                                CatchSub

                                            CatchAll beginningCatchAll ->
                                                if List.length collapsedPatternList.beginning <= List.length collapsedCons.consed then
                                                    CatchAll beginningCatchAll

                                                else
                                                    -- length collapsedPatternList.beginning > length collapsedCons.consedCatchSome beginningCatchSome
                                                    CatchSub
                                        )
                                        []
                                    )

                            CatchNone ->
                                CatchNone

            else
                CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.IfBlock _ _ _ ->
            -- simplify should clean the bool case up to a simple if
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.PrefixOperator _ ->
            CatchNone

        -- doesn't exist
        Expression.Operator _ ->
            CatchNone

        Expression.Negation _ ->
            CatchNone

        Expression.TupledExpression tupleParts ->
            -- TODO remove () parts
            case pattern of
                Pattern.TuplePattern patternParts ->
                    if List.length patternParts /= List.length tupleParts then
                        -- compiler error
                        CatchNone

                    else
                        -- length patternParts == length tupleParts
                        catchForPairs tupleParts patternParts

                _ ->
                    CatchNone

        Expression.LetExpression _ ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.CaseExpression _ ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        -- compiler error
        Expression.LambdaExpression _ ->
            CatchNone

        Expression.RecordExpr _ ->
            casePatternCatchForRecord ()

        Expression.RecordUpdateExpression _ _ ->
            casePatternCatchForRecord ()

        Expression.ListExpr elements ->
            case getCollapsedPatternList pattern of
                Just patternElements ->
                    case patternElements.tail of
                        Just _ ->
                            if List.length patternElements.beginning > List.length elements then
                                CatchNone

                            else
                                -- length patternElements.beginning <= List.length elements
                                catchForPairs elements patternElements.beginning

                        Nothing ->
                            if List.length patternElements.beginning /= List.length elements then
                                -- compiler error
                                CatchNone

                            else
                                -- length patternElements == length elements
                                catchForPairs elements patternElements.beginning

                Nothing ->
                    CatchNone

        Expression.RecordAccess _ _ ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.RecordAccessFunction _ ->
            CatchNone

        Expression.GLSLExpression _ ->
            CatchNone

        Expression.ParenthesizedExpression inParens ->
            casePatternSpecificCatchFor lookupTable inParens patternNode


stringStartsWithUpper : String -> Bool
stringStartsWithUpper string =
    case String.uncons string of
        Nothing ->
            False

        Just ( firstChar, _ ) ->
            Char.isUpper firstChar


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
