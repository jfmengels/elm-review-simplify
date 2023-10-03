module Generate exposing (main)

{-| Given a package's docs.json,
generate modules `Fn.Module` that expose each exposed function/value declaration name as

    name : ( Elm.Syntax.ModuleName.ModuleName, String )
    name =
        ( [ "Module" ], "name" )

You can download a package's docs.json using

    https://package.elm-lang.org/packages/{author}/{package name}/latest/docs.json

where you replace {author}/{package name} with your package name like elm/core

-}

import Elm
import Elm.Docs
import Gen.CodeGen.Generate
import Gen.Elm.Syntax.ModuleName
import Json.Decode


main : Program Json.Decode.Value () ()
main =
    Gen.CodeGen.Generate.fromJson
        (Json.Decode.list Elm.Docs.decoder)
        (\modulesDocs ->
            modulesDocs
                |> List.map moduleDocsToFile
        )


moduleDocsToFile : Elm.Docs.Module -> Elm.File
moduleDocsToFile moduleDocs =
    let
        moduleName : List String
        moduleName =
            moduleDocs.name |> String.split "."
    in
    Elm.file ("Fn" :: moduleName)
        (moduleDocs.values
            |> List.map
                (\fnDeclaration ->
                    Elm.declaration fnDeclaration.name
                        (Elm.tuple
                            (Elm.list (moduleName |> List.map Elm.string)
                                |> Elm.withType Gen.Elm.Syntax.ModuleName.annotation_.moduleName
                            )
                            (Elm.string fnDeclaration.name)
                        )
                )
        )
