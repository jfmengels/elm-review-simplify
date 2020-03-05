module NoExposingEverything exposing (rule)

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newSchema "NoExposingEverything"
        |> Rule.withSimpleModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.fromSchema


moduleDefinitionVisitor : Node Module -> List Error
moduleDefinitionVisitor moduleNode =
    case Module.exposingList <| Node.value moduleNode of
        Exposing.All _ ->
            [ Rule.error
                { message = "Module exposes everything implicitly \"(..)\""
                , details =
                    [ "Modules should have hidden implementation details with an explicit API so that the module is used in a proper and controlled way. The users of this module should not have to know about what is inside a module it is using, and they shouldn't need to access it's internal details. Therefore, the API should be explicitly defined and ideally as small as possible." ]
                }
                (Node.range moduleNode)
            ]

        Exposing.Explicit _ ->
            []
