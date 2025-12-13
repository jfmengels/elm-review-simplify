module Simplify.Evaluate exposing (getBoolean, getInt, getNumber)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer
import Simplify.Match exposing (Match(..))
import Simplify.Normalize as Normalize


getBoolean : Infer.Resources a -> Node Expression -> Match Bool
getBoolean resources baseNode =
    case AstHelpers.removeParens baseNode of
        Node referenceRange (Expression.FunctionOrValue _ name) ->
            case ModuleNameLookupTable.moduleNameAt resources.lookupTable referenceRange of
                Just moduleOrigin ->
                    case name of
                        "True" ->
                            case moduleOrigin of
                                [ "Basics" ] ->
                                    determinedTrue

                                _ ->
                                    Undetermined

                        "False" ->
                            case moduleOrigin of
                                [ "Basics" ] ->
                                    determinedFalse

                                _ ->
                                    Undetermined

                        _ ->
                            case
                                Infer.isBoolean (Expression.FunctionOrValue moduleOrigin name)
                                    (Tuple.first resources.inferredConstants)
                            of
                                Just bool ->
                                    Determined bool

                                Nothing ->
                                    Undetermined

                Nothing ->
                    Undetermined

        unparenthesizedNode ->
            case
                Infer.isBoolean
                    (Node.value (Normalize.normalize resources unparenthesizedNode))
                    (Tuple.first resources.inferredConstants)
            of
                Just bool ->
                    Determined bool

                Nothing ->
                    Undetermined


determinedTrue : Match Bool
determinedTrue =
    Determined True


determinedFalse : Match Bool
determinedFalse =
    Determined False


getInt : Infer.Resources a -> Node Expression -> Maybe Int
getInt resources baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.Integer n ->
            Just n

        Expression.Hex n ->
            Just n

        Expression.Negation expr ->
            Maybe.map negate (getInt resources expr)

        Expression.FunctionOrValue _ name ->
            case
                ModuleNameLookupTable.moduleNameFor resources.lookupTable node
                    |> Maybe.andThen (\moduleName -> Infer.get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
            of
                Just (Expression.Integer int) ->
                    Just int

                _ ->
                    Nothing

        _ ->
            Nothing


getNumber : Infer.Resources a -> Node Expression -> Maybe Float
getNumber resources baseNode =
    let
        unparenthesized : Node Expression
        unparenthesized =
            AstHelpers.removeParens baseNode
    in
    case getInt resources unparenthesized of
        Just int ->
            Just (Basics.toFloat int)

        Nothing ->
            case unparenthesized of
                Node _ (Expression.Floatable float) ->
                    Just float

                Node variableRange (Expression.FunctionOrValue _ name) ->
                    case
                        ModuleNameLookupTable.moduleNameAt resources.lookupTable variableRange
                            |> Maybe.andThen (\moduleName -> Infer.get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
                    of
                        Just (Expression.Floatable float) ->
                            Just float

                        _ ->
                            Nothing

                _ ->
                    Nothing
