module NoImportingEverything exposing (rule)

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newSchema "NoImportingEverything"
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromSchema


importVisitor : Node Import -> List Error
importVisitor moduleNode =
    case Node.value moduleNode |> .exposingList |> Maybe.map Node.value of
        Just (Exposing.All _) ->
            [ Rule.error
                { message = "TODO"
                , details = [ "TODO" ]
                }
                (Node.range moduleNode)
            ]

        _ ->
            []
