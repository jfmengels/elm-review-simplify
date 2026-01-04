module Simplify.Evaluate exposing (getBoolean, getInt, getNumber)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Simplify.Match exposing (Match(..))
import Simplify.Normalize as Normalize


getBoolean : Normalize.Resources a -> Node Expression -> Match Bool
getBoolean resources baseNode =
    case Node.value (Normalize.normalize resources baseNode) of
        Expression.FunctionOrValue [ "Basics" ] "True" ->
            determinedTrue

        Expression.FunctionOrValue [ "Basics" ] "False" ->
            determinedFalse

        _ ->
            Undetermined


determinedTrue : Match Bool
determinedTrue =
    Determined True


determinedFalse : Match Bool
determinedFalse =
    Determined False


getInt : Normalize.Resources a -> Node Expression -> Maybe Int
getInt resources expressionNode =
    case Node.value (Normalize.normalize resources expressionNode) of
        Expression.Integer int ->
            Just int

        _ ->
            Nothing


getNumber : Normalize.Resources a -> Node Expression -> Maybe Float
getNumber resources expressionNode =
    case Node.value (Normalize.normalize resources expressionNode) of
        Expression.Integer int ->
            Just (Basics.toFloat int)

        Expression.Floatable float ->
            Just float

        _ ->
            Nothing
