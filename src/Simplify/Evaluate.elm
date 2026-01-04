module Simplify.Evaluate exposing (getBool, getInt, getNumber)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Simplify.Normalize as Normalize


getBool : Normalize.Resources a -> Node Expression -> Maybe Bool
getBool resources baseNode =
    case Node.value (Normalize.normalize resources baseNode) of
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
