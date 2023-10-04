module ConvertQualifiedToFromFnModule exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location)
import Review.Fix exposing (Fix)
import Review.Rule exposing (Rule)


{-| Converts manual qualified fn references to `( [ "Module" ], "name" )` by `Fn.Module.name`

    config =
        [ ConvertQualifiedToFromFnModule.rule
            |> Rule.ignoreErrorsForDirectories [ "src/Fn" ]
        ]


## reported

    a =
        ( [ "Module" ], "name" )


## not reported

    import Fn.Module

    a =
        Fn.Module.name

-}
rule : Rule
rule =
    Review.Rule.newModuleRuleSchemaUsingContextCreator "ConvertQualifiedToFromFnModule" initialContext
        |> Review.Rule.withExpressionEnterVisitor
            (\expr context -> ( expressionVisitor expr context, context ))
        |> Review.Rule.providesFixesForModuleRule
        |> Review.Rule.fromModuleRuleSchema


type alias Context =
    { importLocation : Maybe Location }


initialContext : Review.Rule.ContextCreator () Context
initialContext =
    Review.Rule.initContextCreator
        (\moduleAst () ->
            case moduleAst.imports of
                [] ->
                    { importLocation = Nothing }

                firstImport :: _ ->
                    { importLocation = firstImport |> Elm.Syntax.Node.range |> .start |> Just }
        )
        |> Review.Rule.withFullAst


expressionVisitor : Node Expression -> Context -> List (Review.Rule.Error {})
expressionVisitor expressionNode context =
    case expressionNode of
        Node tupleRange (Elm.Syntax.Expression.TupledExpression ((Node _ (Elm.Syntax.Expression.ListExpr listLiteral)) :: (Node _ (Elm.Syntax.Expression.Literal name)) :: [])) ->
            case listLiteral |> allJustMap getStringLiteral of
                Just moduleName ->
                    let
                        moduleNameAsString : String
                        moduleNameAsString =
                            "Fn." ++ (moduleName |> String.join ".")

                        replacementAsString : String
                        replacementAsString =
                            moduleNameAsString ++ "." ++ (name |> stringFirstCharToLower)

                        importFix : List Fix
                        importFix =
                            case context.importLocation of
                                Nothing ->
                                    []

                                Just importLocation ->
                                    [ Review.Fix.insertAt importLocation ("import " ++ moduleNameAsString ++ "\n") ]
                    in
                    [ Review.Rule.errorWithFix
                        { message = "Manual qualified fn tuple can be replaced by " ++ replacementAsString
                        , details = [ "You can replace this tuple by " ++ replacementAsString ++ "." ]
                        }
                        tupleRange
                        (importFix
                            ++ [ Review.Fix.replaceRangeBy tupleRange
                                    replacementAsString
                               ]
                        )
                    ]

                Nothing ->
                    []

        _ ->
            []


getStringLiteral : Node Expression -> Maybe String
getStringLiteral expressionNode =
    case expressionNode of
        Node _ (Elm.Syntax.Expression.Literal stringLiteral) ->
            Just stringLiteral

        _ ->
            Nothing


stringFirstCharToLower : String -> String
stringFirstCharToLower string =
    case string |> String.uncons of
        Nothing ->
            ""

        Just ( firstChar, afterFirstChar ) ->
            String.cons (firstChar |> Char.toLower) afterFirstChar


allJustMap : (a -> Maybe b) -> List a -> Maybe (List b)
allJustMap f list =
    allJustMapHelp f list []


allJustMapHelp : (a -> Maybe b) -> List a -> List b -> Maybe (List b)
allJustMapHelp f list acc =
    case list of
        head :: tail ->
            case f head of
                Just a ->
                    allJustMapHelp f tail (a :: acc)

                Nothing ->
                    Nothing

        [] ->
            Just (List.reverse acc)
