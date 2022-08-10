module Simplify.Evaluate exposing (..)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers
import Simplify.Infer as Infer
import Simplify.Match as Match exposing (Match(..))
import Simplify.Normalize as Normalize


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

        Expression.OperatorApplication "==" _ left right ->
            -- TODO Handle constraints on the right side
            case Infer.getConstraint (Node.value left) (Tuple.first resources.inferredConstants) of
                Just Infer.IsTrue ->
                    getBoolean resources right

                Just Infer.IsFalse ->
                    getBoolean resources right
                        |> Match.map not

                Just (Infer.Equals value) ->
                    case Normalize.compare resources (Node Range.emptyRange value) right of
                        Normalize.ConfirmedEquality ->
                            Determined True

                        Normalize.ConfirmedInequality ->
                            Determined False

                        Normalize.Unconfirmed ->
                            Undetermined

                Just (Infer.NotEquals value) ->
                    case Normalize.compare resources (Node Range.emptyRange value) right of
                        Normalize.ConfirmedEquality ->
                            Determined False

                        Normalize.ConfirmedInequality ->
                            Undetermined

                        Normalize.Unconfirmed ->
                            Undetermined

                Nothing ->
                    Undetermined

        Expression.OperatorApplication "/=" _ left right ->
            -- TODO Handle constraints on the right side
            case Infer.getConstraint (Node.value left) (Tuple.first resources.inferredConstants) of
                Just Infer.IsTrue ->
                    getBoolean resources right
                        |> Match.map not

                Just Infer.IsFalse ->
                    getBoolean resources right

                Just (Infer.Equals value) ->
                    case Normalize.compare resources (Node Range.emptyRange value) right of
                        Normalize.ConfirmedEquality ->
                            Determined False

                        Normalize.ConfirmedInequality ->
                            Determined True

                        Normalize.Unconfirmed ->
                            Undetermined

                Just (Infer.NotEquals value) ->
                    case Normalize.compare resources (Node Range.emptyRange value) right of
                        Normalize.ConfirmedEquality ->
                            Determined True

                        Normalize.ConfirmedInequality ->
                            Determined False

                        Normalize.Unconfirmed ->
                            Undetermined

                Nothing ->
                    Undetermined

        _ ->
            let
                normalized : Expression
                normalized =
                    Node.value (Normalize.normalize resources node)
            in
            case Infer.getConstraint normalized (Tuple.first resources.inferredConstants) of
                Just (Infer.Equals value) ->
                    if value == Expression.FunctionOrValue [ "Basics" ] "True" then
                        Determined True

                    else if value == Expression.FunctionOrValue [ "Basics" ] "False" then
                        Determined False

                    else
                        Undetermined

                Just Infer.IsTrue ->
                    Determined True

                Just Infer.IsFalse ->
                    Determined False

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
