module Simplify.AstHelpers exposing (moduleHeaderExposing, removeParens, removeParensFromPattern)

import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)


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


moduleHeaderExposing : Elm.Syntax.Module.Module -> Node Exposing
moduleHeaderExposing moduleHeader =
    case moduleHeader of
        Elm.Syntax.Module.NormalModule info ->
            info.exposingList

        Elm.Syntax.Module.PortModule info ->
            info.exposingList

        Elm.Syntax.Module.EffectModule info ->
            info.exposingList
