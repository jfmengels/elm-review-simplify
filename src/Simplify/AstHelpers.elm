module Simplify.AstHelpers exposing
    ( CaseCatchSomeTree
    , Catch
    , CatchNoneOrSome(..)
    , CatchNoneReason(..)
    , CatchSomeInfo
    , CatchSomeKind(..)
    , ConsTupleParts
    , ConsTupleSecondPart(..)
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
      CatchNone (List CatchNoneReason)


type CatchNoneReason
    = CatchNoneWithCompilerError String
    | CatchNoneDueToSpecificMismatch { kind : String, expression : String, pattern : String }


onCatchSome : (some -> mappedSome) -> CatchNoneOrSome some -> CatchNoneOrSome mappedSome
onCatchSome caseCatchSomeTreeChange catch =
    case catch of
        CatchNone catchNone ->
            CatchNone catchNone

        CatchSome catchSome ->
            CatchSome (caseCatchSomeTreeChange catchSome)


type alias CatchSomeInfo =
    { patternRange : Range
    , catch : CatchSomeKind
    }


type CatchSomeKind
    = -- CatchSub: the pattern matches only a narrower subset of possible values
      -- (like case result of Ok value -> or case list of [] ->)
      -- if the only case is CatchSub, there will be be a compiler error
      CatchSub
    | -- CatchAll: the pattern always partially matches the expression:
      -- (like case a of _ -> or case ( b, [ Ok a ] ) of ( 3, [ Ok 3 ] ) ->)
      -- they can all be simplified
      StructuralCatchAll StructuralCatchAllInfo


type alias StructuralCatchAllInfo =
    { -- if completeCatchAll is True, the pattern that always matches the expression:
      -- (like case () of () -> or case 'a' of 'a' -> or case ( 3, [] ) of ( b, [] ) ->)
      -- if a case is a complete catch-all, other patterns that exist later can be removed.
      -- if that pattern also isn't (partially) required, there will be be a compiler error
      -- if the patterns for the same expression from all cases are CatchAll,
      -- they are all simplifiable to ()
      completeCatchAll :
        Maybe
            { -- if containsVariables is True, then
              -- the case pattern contains at least 1 pattern variable there (like case Err err of Err error -> or case [ el ] of [ a ] ->)
              containsVariables : Bool
            }

    -- this is a bit ugly, but necessary
    -- to avoid check mixed cons (::) and literal ([...]) lists.
    -- whenever secondPart... is Just, its parts should be put in a tuple with the second part potentially put in a new list expression
    , toConsTuple :
        Maybe
            { expression : ConsTupleParts
            , pattern : ConsTupleParts
            }
    }


type alias ConsTupleParts =
    { firstPartRange : Range, secondPart : ConsTupleSecondPart }


type ConsTupleSecondPart
    = ConsTupleSecondPartToList (List Range)
    | ConsTupleSecondPartExisting Range


onStructuralCatchAll : (StructuralCatchAllInfo -> StructuralCatchAllInfo) -> CatchSomeKind -> CatchSomeKind
onStructuralCatchAll catchAllInfoChange catchSomeKind =
    case catchSomeKind of
        CatchSub ->
            CatchSub

        StructuralCatchAll catchAll ->
            StructuralCatchAll (catchAllInfoChange catchAll)


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
                (treeWith (StructuralCatchAll { completeCatchAll = Just { containsVariables = False }, toConsTuple = Nothing }) [])

        Pattern.VarPattern _ ->
            CatchSome
                (treeWith (StructuralCatchAll { completeCatchAll = Just { containsVariables = True }, toConsTuple = Nothing }) [])

        Pattern.AsPattern destructured _ ->
            onCatchSome
                (\destructuredCatchSome ->
                    Tree.elementAlter
                        (\el ->
                            patternInCaseAlter
                                (\catchSomeInfo ->
                                    { patternRange = Node.range pattern
                                    , catch =
                                        onStructuralCatchAll
                                            (\structuralCatchAllInfo ->
                                                { structuralCatchAllInfo
                                                    | completeCatchAll =
                                                        Maybe.map (\_ -> { containsVariables = True }) structuralCatchAllInfo.completeCatchAll
                                                }
                                            )
                                            catchSomeInfo.catch
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
            StructuralCatchAll { completeCatchAll = Just { containsVariables = False }, toConsTuple = Nothing }

        Pattern.AllPattern ->
            StructuralCatchAll { completeCatchAll = Just { containsVariables = False }, toConsTuple = Nothing }

        Pattern.VarPattern _ ->
            StructuralCatchAll { completeCatchAll = Just { containsVariables = True }, toConsTuple = Nothing }

        Pattern.AsPattern (Node _ destructured) _ ->
            onStructuralCatchAll
                (\structuralCatchAllInfo ->
                    { structuralCatchAllInfo
                        | completeCatchAll =
                            Maybe.map (\_ -> { containsVariables = True }) structuralCatchAllInfo.completeCatchAll
                    }
                )
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

                        StructuralCatchAll soFarCatchAll ->
                            case casePatternCatchForUnknown part of
                                CatchSub ->
                                    CatchSub

                                StructuralCatchAll catchAll ->
                                    StructuralCatchAll
                                        { completeCatchAll =
                                            Maybe.map2
                                                (\soFarCompleteCatchAll completeCatchAll ->
                                                    { containsVariables =
                                                        soFarCompleteCatchAll.containsVariables
                                                            || completeCatchAll.containsVariables
                                                    }
                                                )
                                                soFarCatchAll.completeCatchAll
                                                catchAll.completeCatchAll
                                        , toConsTuple = Nothing
                                        }
                )
                (StructuralCatchAll { completeCatchAll = Just { containsVariables = False }, toConsTuple = Nothing })
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
    Catch
    -> CatchNoneOrSome { catch : CatchSomeKind, parts : List CaseCatchSomeTree }
    -> CatchNoneOrSome { catch : CatchSomeKind, parts : List CaseCatchSomeTree }
catchMergeWith otherCatch structureCatch =
    case ( structureCatch, otherCatch ) of
        ( CatchNone catchNone, CatchNone otherCatchNone ) ->
            CatchNone (otherCatchNone ++ catchNone)

        ( CatchSome _, CatchNone otherCatchNone ) ->
            CatchNone otherCatchNone

        ( CatchNone catchNone, CatchSome _ ) ->
            CatchNone catchNone

        ( CatchSome catchSome, CatchSome otherCatchSomeTree ) ->
            CatchSome
                { catch =
                    case ( catchSome.catch, (Tree.element otherCatchSomeTree).patternInCase.catch ) of
                        ( StructuralCatchAll catchAll, StructuralCatchAll otherCatchAll ) ->
                            StructuralCatchAll
                                { completeCatchAll =
                                    Maybe.map2
                                        (\completeCatchAll otherCompleteCatchAll ->
                                            { containsVariables =
                                                completeCatchAll.containsVariables
                                                    || otherCompleteCatchAll.containsVariables
                                            }
                                        )
                                        catchAll.completeCatchAll
                                        otherCatchAll.completeCatchAll
                                , toConsTuple = Nothing
                                }

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

        catchAll : () -> Catch
        catchAll () =
            CatchSome
                (treeWith
                    (StructuralCatchAll
                        { completeCatchAll = Just { containsVariables = False }
                        , toConsTuple = Nothing
                        }
                    )
                    []
                )

        casePatternSpecificCatchForInt : Int -> Pattern -> Catch
        casePatternSpecificCatchForInt int patternForInt =
            case patternForInt of
                Pattern.HexPattern patternInt ->
                    if patternInt == int then
                        catchAll ()

                    else
                        CatchNone [ CatchNoneDueToSpecificMismatch { kind = "Int", expression = String.fromInt int, pattern = String.fromInt patternInt } ]

                Pattern.IntPattern patternInt ->
                    if patternInt == int then
                        catchAll ()

                    else
                        CatchNone [ CatchNoneDueToSpecificMismatch { kind = "Int", expression = String.fromInt int, pattern = String.fromInt patternInt } ]

                _ ->
                    CatchNone [ CatchNoneWithCompilerError "an Int must be matched by either an int or hex pattern" ]

        casePatternCatchForRecord : () -> Catch
        casePatternCatchForRecord () =
            case pattern of
                Pattern.RecordPattern _ ->
                    -- a record pattern without field variables is invalid syntax
                    catchAll ()

                _ ->
                    CatchNone [ CatchNoneWithCompilerError "a record must be matched by a record ({...}) pattern" ]

        catchForPairs : List (Node Expression) -> List (Node Pattern) -> Catch
        catchForPairs expressions patterns =
            case structureCatchForPairs expressions patterns of
                CatchNone catchNone ->
                    CatchNone catchNone

                CatchSome structureCatchSome ->
                    CatchSome (treeWith structureCatchSome.catch structureCatchSome.parts)

        structureCatchForPairs : List (Node Expression) -> List (Node Pattern) -> CatchNoneOrSome { catch : CatchSomeKind, parts : List CaseCatchSomeTree }
        structureCatchForPairs expressions patterns =
            case ( expressions, patterns ) of
                ( [], _ :: _ ) ->
                    CatchSome { parts = [], catch = StructuralCatchAll { completeCatchAll = Just { containsVariables = False }, toConsTuple = Nothing } }

                ( _ :: _, [] ) ->
                    CatchSome { parts = [], catch = StructuralCatchAll { completeCatchAll = Just { containsVariables = False }, toConsTuple = Nothing } }

                ( [], [] ) ->
                    CatchSome { parts = [], catch = StructuralCatchAll { completeCatchAll = Just { containsVariables = False }, toConsTuple = Nothing } }

                ( expressionPart :: expressionTail, patternPart :: patternTail ) ->
                    let
                        elementCatchEvaluated : Catch
                        elementCatchEvaluated =
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
                    catchAll ()

                _ ->
                    CatchNone [ CatchNoneWithCompilerError "a unit () value must be matched by a unit () pattern" ]

        Expression.Hex int ->
            casePatternSpecificCatchForInt int pattern

        Expression.Integer int ->
            casePatternSpecificCatchForInt int pattern

        Expression.CharLiteral char ->
            case pattern of
                Pattern.CharPattern patternChar ->
                    if char == patternChar then
                        catchAll ()

                    else
                        CatchNone [ CatchNoneDueToSpecificMismatch { kind = "Char", expression = "'" ++ String.fromChar char ++ "'", pattern = "'" ++ String.fromChar patternChar ++ "'" } ]

                _ ->
                    CatchNone [ CatchNoneWithCompilerError ("the Char '" ++ String.fromChar char ++ "' must be matched by a literal Char pattern") ]

        Expression.Floatable _ ->
            CatchNone [ CatchNoneWithCompilerError "a Float can never be matched" ]

        Expression.Literal string ->
            case pattern of
                Pattern.StringPattern patternString ->
                    if string == patternString then
                        catchAll ()

                    else
                        CatchNone [ CatchNoneDueToSpecificMismatch { kind = "String", expression = "\"" ++ string ++ "\"", pattern = "\"" ++ patternString ++ "\"" } ]

                _ ->
                    CatchNone [ CatchNoneWithCompilerError ("the String \"" ++ string ++ "\" must be matched by a literal String pattern") ]

        Expression.FunctionOrValue qualification name ->
            if not (stringStartsWithUpper name) then
                CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

            else
                -- variant without arguments or empty record type alias constructor
                let
                    fullyQualified : ( ModuleName, String )
                    fullyQualified =
                        ( Maybe.withDefault qualification (ModuleNameLookupTable.moduleNameAt lookupTable expressionRange)
                        , name
                        )
                in
                case pattern of
                    Pattern.NamedPattern patternQualified [] ->
                        let
                            patternFullyQualified : ( ModuleName, String )
                            patternFullyQualified =
                                ( Maybe.withDefault patternQualified.moduleName (ModuleNameLookupTable.moduleNameAt lookupTable patternRange)
                                , patternQualified.name
                                )
                        in
                        if patternFullyQualified == fullyQualified then
                            catchAll ()

                        else
                            CatchNone [ CatchNoneDueToSpecificMismatch { kind = "variant", expression = qualifiedToString fullyQualified, pattern = qualifiedToString patternFullyQualified } ]

                    _ ->
                        -- an empty record type alias constructor doesn't have a specific matching pattern
                        -- a curried variant constructor doesn't have a matching pattern either
                        CatchNone [ CatchNoneWithCompilerError ("the variant " ++ qualifiedToString fullyQualified ++ " must be matched by a variant pattern") ]

        -- doesn't exist
        Expression.Application [] ->
            CatchNone [ CatchNoneWithCompilerError "elm-syntax bug: empty Application was parsed as an Expression" ]

        -- doesn't exist
        Expression.Application (_ :: []) ->
            CatchNone [ CatchNoneWithCompilerError "elm-syntax bug: Application without arguments was parsed as an Expression" ]

        Expression.Application ((Node fedRange fed) :: firstArg :: argsAfterFirst) ->
            case fed of
                Expression.FunctionOrValue qualification name ->
                    if not (stringStartsWithUpper name) then
                        CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

                    else
                        let
                            fedFullyQualified : ( ModuleName, String )
                            fedFullyQualified =
                                ( Maybe.withDefault qualification (ModuleNameLookupTable.moduleNameAt lookupTable fedRange)
                                , name
                                )
                        in
                        -- expression is (curried) variant or (curried) record type alias constructor
                        case pattern of
                            Pattern.NamedPattern patternQualified namedPatternArguments ->
                                let
                                    patternFullyQualified : ( ModuleName, String )
                                    patternFullyQualified =
                                        ( Maybe.withDefault patternQualified.moduleName (ModuleNameLookupTable.moduleNameAt lookupTable patternRange)
                                        , patternQualified.name
                                        )
                                in
                                if patternFullyQualified /= fedFullyQualified then
                                    CatchNone [ CatchNoneDueToSpecificMismatch { kind = "variant", expression = qualifiedToString fedFullyQualified, pattern = qualifiedToString patternFullyQualified } ]

                                else if List.length (firstArg :: argsAfterFirst) < List.length namedPatternArguments then
                                    CatchNone [ CatchNoneWithCompilerError "a partially applied constructor (of a variant or record) can never be matched" ]

                                else
                                    -- pattern with same variant
                                    case structureCatchForPairs (firstArg :: argsAfterFirst) namedPatternArguments of
                                        CatchNone catchNone ->
                                            CatchNone catchNone

                                        CatchSome variantCatchSome ->
                                            CatchSome
                                                (treeWith
                                                    (case variantCatchSome.catch of
                                                        StructuralCatchAll structuralCatchAll ->
                                                            StructuralCatchAll structuralCatchAll

                                                        CatchSub ->
                                                            StructuralCatchAll { completeCatchAll = Nothing, toConsTuple = Nothing }
                                                    )
                                                    variantCatchSome.parts
                                                )

                            Pattern.RecordPattern _ ->
                                catchAll ()

                            _ ->
                                CatchNone [ CatchNoneWithCompilerError ("the variant or record " ++ qualifiedToString fedFullyQualified ++ " must be matched by a variant pattern") ]

                _ ->
                    CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.OperatorApplication "::" _ element0Expression elements1UpListExpression ->
            case patternToConsTuple pattern of
                Nothing ->
                    CatchNone [ CatchNoneWithCompilerError "a List with at least one element must be matched by a list ([..., ...]) or un-cons (...::...) pattern" ]

                Just patternConsTuple ->
                    CatchSome
                        (treeWith
                            (StructuralCatchAll
                                { completeCatchAll = Nothing
                                , toConsTuple =
                                    Just
                                        { expression =
                                            { firstPartRange = Node.range element0Expression
                                            , secondPart = ConsTupleSecondPartExisting (Node.range elements1UpListExpression)
                                            }
                                        , pattern = patternConsTuple
                                        }
                                }
                            )
                            []
                        )

        Expression.OperatorApplication operatorName _ _ _ ->
            if List.member operatorName [ ">>", "<<" ] then
                CatchNone [ CatchNoneWithCompilerError "composed functions can never be matched" ]

            else if List.member operatorName [ "|.", "|=" ] then
                CatchNone [ CatchNoneWithCompilerError "sequenced elm/parser Parsers are opaque and can therefore never be matched" ]

            else if List.member operatorName [ "<?>", "</>" ] then
                CatchNone [ CatchNoneWithCompilerError "sequenced elm/url Parsers are opaque and can therefore never be matched" ]

            else
                CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.ListExpr [] ->
            case pattern of
                Pattern.ListPattern [] ->
                    catchAll ()

                Pattern.ListPattern (_ :: _) ->
                    CatchNone [ CatchNoneDueToSpecificMismatch { kind = "List", expression = "[]", pattern = "a list pattern with at least one element ([..., ...]" } ]

                Pattern.UnConsPattern _ _ ->
                    CatchNone [ CatchNoneDueToSpecificMismatch { kind = "List", expression = "[]", pattern = "a cons pattern (... :: ...)" } ]

                _ ->
                    CatchNone [ CatchNoneWithCompilerError "an empty list must be matched with an empty list pattern ([])" ]

        Expression.ListExpr (element0Expression :: element1UpExpressions) ->
            case patternToConsTuple pattern of
                Nothing ->
                    CatchNone [ CatchNoneWithCompilerError "a List with at least one element must be matched by a list ([..., ...]) or un-cons (...::...) pattern" ]

                Just patternConsTuple ->
                    CatchSome
                        (treeWith
                            (StructuralCatchAll
                                { completeCatchAll = Nothing
                                , toConsTuple =
                                    Just
                                        { expression =
                                            { firstPartRange = Node.range element0Expression
                                            , secondPart = ConsTupleSecondPartToList (List.map Node.range element1UpExpressions)
                                            }
                                        , pattern = patternConsTuple
                                        }
                                }
                            )
                            []
                        )

        Expression.IfBlock _ _ _ ->
            -- simplify should clean the bool case up to a simple if
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.PrefixOperator operatorName ->
            CatchNone [ CatchNoneWithCompilerError ("a prefix operator function (" ++ operatorName ++ ") can never be matched (as any other function)") ]

        Expression.Operator _ ->
            CatchNone [ CatchNoneWithCompilerError "elm-syntax bug: intermediate Operator was parsed as an Expression" ]

        Expression.Negation _ ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.TupledExpression tupleParts ->
            let
                tuplePartCount : Int
                tuplePartCount =
                    List.length tupleParts
            in
            case pattern of
                Pattern.TuplePattern patternTupleParts ->
                    let
                        patternTuplePartCount : Int
                        patternTuplePartCount =
                            List.length tupleParts
                    in
                    if tuplePartCount == patternTuplePartCount then
                        catchForPairs tupleParts patternTupleParts

                    else
                        -- length patternParts == length tupleParts
                        CatchNone [ CatchNoneWithCompilerError (String.fromInt tuplePartCount ++ "-tuples can never be matched with a " ++ String.fromInt patternTuplePartCount ++ "-tuple pattern") ]

                _ ->
                    CatchNone [ CatchNoneWithCompilerError ("a " ++ String.fromInt tuplePartCount ++ "-tuple must be matched with a " ++ String.fromInt tuplePartCount ++ "-tuple pattern") ]

        Expression.LetExpression _ ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.CaseExpression _ ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        -- compiler error
        Expression.LambdaExpression _ ->
            CatchNone [ CatchNoneWithCompilerError "a lambda can never be matched (as any other function)" ]

        Expression.RecordExpr _ ->
            casePatternCatchForRecord ()

        Expression.RecordUpdateExpression _ _ ->
            casePatternCatchForRecord ()

        Expression.RecordAccess _ _ ->
            CatchSome (treeWith (casePatternCatchForUnknown pattern) [])

        Expression.RecordAccessFunction _ ->
            CatchNone [ CatchNoneWithCompilerError "a record accessor can never be matched (as any other function)" ]

        Expression.GLSLExpression _ ->
            CatchNone [ CatchNoneWithCompilerError "glsl can never be matched" ]

        Expression.ParenthesizedExpression inParens ->
            casePatternSpecificCatchFor lookupTable inParens patternNode


patternToConsTuple : Pattern -> Maybe { firstPartRange : Range, secondPart : ConsTupleSecondPart }
patternToConsTuple pattern =
    case pattern of
        Pattern.ListPattern [] ->
            Nothing

        Pattern.ListPattern ((Node headPatternRange _) :: element1UpPatterns) ->
            Just
                { firstPartRange = headPatternRange
                , secondPart = ConsTupleSecondPartToList (List.map Node.range element1UpPatterns)
                }

        Pattern.UnConsPattern (Node headPatternRange _) (Node tailPatternRange _) ->
            Just
                { firstPartRange = headPatternRange
                , secondPart = ConsTupleSecondPartExisting tailPatternRange
                }

        _ ->
            Nothing


test =
    { a0 =
        case [] of
            1 :: 2 :: [ 3, 4 ] ->
                1

            _ ->
                0
    , a1 =
        case Ok () of
            Err err ->
                "err " ++ err

            Ok () ->
                "eh "
    }


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
