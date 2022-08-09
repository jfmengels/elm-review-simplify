module Simplify.Infer exposing (..)

import AssocList
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


type alias Inferred =
    AssocList.Dict Expression Expression


type alias ConstantValue =
    Expression


type alias Resources a =
    { a
        | lookupTable : ModuleNameLookupTable
        , inferredConstants : Inferred
    }


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


booleanToConstant : Bool -> ConstantValue
booleanToConstant expressionValue =
    Expression.FunctionOrValue [ "Basics" ]
        (if expressionValue then
            "True"

         else
            "False"
        )


injectConstant : Expression -> ConstantValue -> Inferred -> Inferred
injectConstant expression value constants =
    constants
        |> AssocList.foldl
            (\expr v acc ->
                case expr of
                    _ ->
                        AssocList.insert expr v acc
            )
            AssocList.empty
        |> AssocList.insert expression value
