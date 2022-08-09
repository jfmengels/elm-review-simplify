module Simplify.Evaluate exposing (..)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer
import Simplify.Match exposing (Match(..))


getBoolean : Infer.Resources a -> Node Expression -> Match Bool
getBoolean resources baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "True" ->
            case ModuleNameLookupTable.moduleNameFor resources.lookupTable node of
                Just [ "Basics" ] ->
                    Determined True

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ "False" ->
            case ModuleNameLookupTable.moduleNameFor resources.lookupTable node of
                Just [ "Basics" ] ->
                    Determined False

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ name ->
            case
                ModuleNameLookupTable.moduleNameFor resources.lookupTable node
                    |> Maybe.andThen (\moduleName -> Infer.get (Expression.FunctionOrValue moduleName name) (Tuple.first resources.inferredConstants))
            of
                Just (Expression.FunctionOrValue [ "Basics" ] "True") ->
                    Determined True

                Just (Expression.FunctionOrValue [ "Basics" ] "False") ->
                    Determined False

                Just _ ->
                    Undetermined

                Nothing ->
                    Undetermined

        Expression.OperatorApplication "/=" _ left _ ->
            case Infer.getConstraint (Node.value left) (Tuple.first resources.inferredConstants) of
                Just (Infer.Equals value) ->
                    Undetermined

                Just (Infer.NotEquals value) ->
                    Undetermined

                Nothing ->
                    Undetermined

        -- TODO Here is likely where we want to compare stuff
        --Debug.log "oo" (Determined False)
        _ ->
            Undetermined


isAlwaysBoolean : Infer.Resources a -> Node Expression -> Match Bool
isAlwaysBoolean resources node =
    case Node.value (AstHelpers.removeParens node) of
        Expression.Application ((Node alwaysRange (Expression.FunctionOrValue _ "always")) :: boolean :: []) ->
            case ModuleNameLookupTable.moduleNameAt resources.lookupTable alwaysRange of
                Just [ "Basics" ] ->
                    getBoolean resources boolean

                _ ->
                    Undetermined

        Expression.LambdaExpression { expression } ->
            getBoolean resources expression

        _ ->
            Undetermined
