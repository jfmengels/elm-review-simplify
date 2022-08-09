module Simplify.Infer exposing (..)

import AssocList
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Simplify.AstHelpers as AstHelpers
import Simplify.Match exposing (Match(..))


type Inferred
    = Inferred (AssocList.Dict Expression Expression)


type alias Resources a =
    { a
        | lookupTable : ModuleNameLookupTable
        , inferredConstants : Inferred
    }


empty : Inferred
empty =
    Inferred AssocList.empty


get : Expression -> Inferred -> Maybe Expression
get expr (Inferred inferred) =
    AssocList.get expr inferred


infer : List Expression -> Bool -> Inferred -> Inferred
infer nodes expressionValue dict =
    case nodes of
        [] ->
            dict

        first :: rest ->
            case first of
                Expression.FunctionOrValue _ _ ->
                    infer rest expressionValue (injectConstant first (booleanToConstant expressionValue) dict)

                Expression.Application [ Node _ (Expression.FunctionOrValue [ "Basics" ] "not"), expression ] ->
                    infer
                        rest
                        expressionValue
                        (infer [ Node.value expression ] (not expressionValue) dict)

                Expression.OperatorApplication "&&" _ left right ->
                    if expressionValue then
                        infer (Node.value left :: Node.value right :: rest) expressionValue dict

                    else
                        infer rest expressionValue dict

                Expression.OperatorApplication "||" _ left right ->
                    if not expressionValue then
                        infer (Node.value left :: Node.value right :: rest) expressionValue dict

                    else
                        infer rest expressionValue dict

                _ ->
                    infer rest expressionValue dict


booleanToConstant : Bool -> Expression
booleanToConstant expressionValue =
    Expression.FunctionOrValue [ "Basics" ]
        (if expressionValue then
            "True"

         else
            "False"
        )


injectConstant : Expression -> Expression -> Inferred -> Inferred
injectConstant expression value (Inferred inferred) =
    inferred
        |> AssocList.foldl
            (\expr v acc ->
                case expr of
                    _ ->
                        AssocList.insert expr v acc
            )
            AssocList.empty
        |> AssocList.insert expression value
        |> Inferred



--


getBoolean : Resources a -> Node Expression -> Match Bool
getBoolean inferMaterial baseNode =
    let
        node : Node Expression
        node =
            AstHelpers.removeParens baseNode
    in
    case Node.value node of
        Expression.FunctionOrValue _ "True" ->
            case ModuleNameLookupTable.moduleNameFor inferMaterial.lookupTable node of
                Just [ "Basics" ] ->
                    Determined True

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ "False" ->
            case ModuleNameLookupTable.moduleNameFor inferMaterial.lookupTable node of
                Just [ "Basics" ] ->
                    Determined False

                _ ->
                    Undetermined

        Expression.FunctionOrValue _ name ->
            case
                ModuleNameLookupTable.moduleNameFor inferMaterial.lookupTable node
                    |> Maybe.andThen (\moduleName -> get (Expression.FunctionOrValue moduleName name) inferMaterial.inferredConstants)
            of
                Just (Expression.FunctionOrValue [ "Basics" ] "True") ->
                    Determined True

                Just (Expression.FunctionOrValue [ "Basics" ] "False") ->
                    Determined False

                Just _ ->
                    Undetermined

                Nothing ->
                    Undetermined

        _ ->
            Undetermined
