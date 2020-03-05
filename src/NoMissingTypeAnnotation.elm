module NoMissingTypeAnnotation exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newSchema "NoMissingTypeAnnotation"
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromSchema


declarationVisitor : Node Declaration -> List Error
declarationVisitor declaration =
    case Node.value declaration of
        Declaration.FunctionDeclaration function ->
            case function.signature of
                Nothing ->
                    let
                        name : Node String
                        name =
                            function.declaration
                                |> Node.value
                                |> .name
                    in
                    [ Rule.error
                        { message = "Missing type annotation for `" ++ Node.value name ++ "`"
                        , details =
                            [ "Type annotations help you understand what happens in the code, and it will help the compiler give better error messages."
                            ]
                        }
                        (Node.range name)
                    ]

                Just _ ->
                    []

        _ ->
            []
