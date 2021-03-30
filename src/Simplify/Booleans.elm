module Simplify.Booleans exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import Review.Fix as Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Rule)


{-| Reports and fixes conditionals that can be made simpler.

    config =
        [ Simplify.Booleans.rule
        ]


## Fail

    a =
        -- Simplifiable as: True
        True || x

    b =
        -- Simplifiable as: x
        x || False


## Success

    a =
        x || y

    b =
        w && z


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template jfmengels/elm-review-simplification/example --rules Simplify.Booleans
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "Simplify.Booleans" initialContext
        |> Rule.withExpressionEnterVisitor (\node context -> ( expressionVisitor node context, context ))
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable }
        )
        |> Rule.withModuleNameLookupTable


expressionVisitor : Node Expression -> Context -> List (Rule.Error {})
expressionVisitor node { lookupTable } =
    case Node.value node of
        Expression.OperatorApplication "||" _ left right ->
            List.concat
                [ or_isLeftSimplifiableError node left right
                , or_isRightSimplifiableError node left right
                ]

        Expression.OperatorApplication "&&" _ left right ->
            List.concat
                [ and_isLeftSimplifiableError node left right
                , and_isRightSimplifiableError node left right
                ]

        Expression.OperatorApplication "==" _ left right ->
            if areTheSame lookupTable left right then
                [ Rule.errorWithFix
                    { message = "Condition is always True"
                    , details = sameThingOnBothSidesDetails True
                    }
                    (Node.range node)
                    [ Fix.replaceRangeBy (Node.range node) "True"
                    ]
                ]

            else
                []

        Expression.OperatorApplication "/=" _ left right ->
            if areTheSame lookupTable left right then
                [ Rule.errorWithFix
                    { message = "Condition is always False"
                    , details = sameThingOnBothSidesDetails False
                    }
                    (Node.range node)
                    [ Fix.replaceRangeBy (Node.range node) "False"
                    ]
                ]

            else
                []

        _ ->
            []


or_isLeftSimplifiableError : Node a -> Node Expression -> Node b -> List (Rule.Error {})
or_isLeftSimplifiableError node left right =
    if isTrue left then
        [ Rule.errorWithFix
            { message = "Condition is always True"
            , details = alwaysSameDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).end
                , end = (Node.range right).end
                }
            ]
        ]

    else if isFalse left then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).start
                , end = (Node.range right).start
                }
            ]
        ]

    else
        []


or_isRightSimplifiableError : Node a -> Node b -> Node Expression -> List (Rule.Error {})
or_isRightSimplifiableError node left right =
    if isTrue right then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).start
                , end = (Node.range right).start
                }
            ]
        ]

    else if isFalse right then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).end
                , end = (Node.range right).end
                }
            ]
        ]

    else
        []


and_isLeftSimplifiableError : Node a -> Node Expression -> Node b -> List (Rule.Error {})
and_isLeftSimplifiableError node left right =
    if isTrue left then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).start
                , end = (Node.range right).start
                }
            ]
        ]

    else if isFalse left then
        [ Rule.errorWithFix
            { message = "Condition is always False"
            , details = alwaysSameDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).end
                , end = (Node.range right).end
                }
            ]
        ]

    else
        []


and_isRightSimplifiableError : Node a -> Node b -> Node Expression -> List (Rule.Error {})
and_isRightSimplifiableError node left right =
    if isTrue right then
        [ Rule.errorWithFix
            { message = unnecessaryMessage
            , details = unnecessaryDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).end
                , end = (Node.range right).end
                }
            ]
        ]

    else if isFalse right then
        [ Rule.errorWithFix
            { message = "Condition is always False"
            , details = alwaysSameDetails
            }
            (Node.range node)
            [ Fix.removeRange
                { start = (Node.range left).start
                , end = (Node.range right).start
                }
            ]
        ]

    else
        []


isTrue : Node Expression -> Bool
isTrue node =
    case Node.value node of
        Expression.FunctionOrValue [] "True" ->
            True

        Expression.ParenthesizedExpression expr ->
            isTrue expr

        _ ->
            False


isFalse : Node Expression -> Bool
isFalse node =
    case Node.value node of
        Expression.FunctionOrValue [] "False" ->
            True

        Expression.ParenthesizedExpression expr ->
            isFalse expr

        _ ->
            False


alwaysSameDetails : List String
alwaysSameDetails =
    [ "This condition will always result in the same value. You may have hardcoded a value or mistyped a condition."
    ]


unnecessaryMessage : String
unnecessaryMessage =
    "Part of the expression is unnecessary"


unnecessaryDetails : List String
unnecessaryDetails =
    [ "A part of this condition is unnecessary. You can remove it and it would not impact the behavior of the program."
    ]


areTheSame : ModuleNameLookupTable -> Node Expression -> Node Expression -> Bool
areTheSame lookupTable left right =
    normalize lookupTable left == normalize lookupTable right


normalize : ModuleNameLookupTable -> Node Expression -> Node Expression
normalize lookupTable node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            normalize lookupTable expr

        Expression.Application nodes ->
            toNode (Expression.Application (List.map (normalize lookupTable) nodes))

        Expression.OperatorApplication string infixDirection left right ->
            toNode (Expression.OperatorApplication string infixDirection (normalize lookupTable left) (normalize lookupTable right))

        Expression.FunctionOrValue rawModuleName string ->
            let
                moduleName : ModuleName
                moduleName =
                    ModuleNameLookupTable.moduleNameFor lookupTable node
                        |> Maybe.withDefault rawModuleName
            in
            toNode (Expression.FunctionOrValue moduleName string)

        Expression.IfBlock cond then_ else_ ->
            toNode (Expression.IfBlock (normalize lookupTable cond) (normalize lookupTable then_) (normalize lookupTable else_))

        Expression.Negation expr ->
            toNode (Expression.Negation (normalize lookupTable expr))

        Expression.TupledExpression nodes ->
            toNode (Expression.TupledExpression (List.map (normalize lookupTable) nodes))

        Expression.LetExpression letBlock ->
            toNode
                (Expression.LetExpression
                    { declarations =
                        List.map
                            (\decl ->
                                case Node.value decl of
                                    Expression.LetFunction function ->
                                        let
                                            declaration : Expression.FunctionImplementation
                                            declaration =
                                                Node.value function.declaration
                                        in
                                        toNode
                                            (Expression.LetFunction
                                                { documentation = Nothing
                                                , signature = Nothing
                                                , declaration =
                                                    toNode
                                                        { name = toNode (Node.value declaration.name)
                                                        , arguments = List.map normalizePattern declaration.arguments
                                                        , expression = normalize lookupTable declaration.expression
                                                        }
                                                }
                                            )

                                    Expression.LetDestructuring pattern expr ->
                                        toNode (Expression.LetDestructuring (normalizePattern pattern) (normalize lookupTable expr))
                            )
                            letBlock.declarations
                    , expression = normalize lookupTable letBlock.expression
                    }
                )

        Expression.CaseExpression caseBlock ->
            toNode
                (Expression.CaseExpression
                    { cases = List.map (\( pattern, expr ) -> ( normalizePattern pattern, normalize lookupTable expr )) caseBlock.cases
                    , expression = toNode <| Node.value caseBlock.expression
                    }
                )

        Expression.LambdaExpression lambda ->
            toNode
                (Expression.LambdaExpression
                    { args = List.map normalizePattern lambda.args
                    , expression = normalize lookupTable lambda.expression
                    }
                )

        Expression.ListExpr nodes ->
            toNode (Expression.ListExpr (List.map (normalize lookupTable) nodes))

        Expression.RecordAccess expr (Node _ field) ->
            toNode (Expression.RecordAccess (normalize lookupTable expr) (toNode field))

        Expression.RecordExpr nodes ->
            nodes
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> toNode ( toNode fieldName, normalize lookupTable expr ))
                |> Expression.RecordExpr
                |> toNode

        Expression.RecordUpdateExpression value nodes ->
            -- TODO
            node

        expr ->
            toNode expr


normalizePattern : Node Pattern -> Node Pattern
normalizePattern node =
    case Node.value node of
        Pattern.TuplePattern patterns ->
            toNode (Pattern.TuplePattern (List.map normalizePattern patterns))

        Pattern.RecordPattern fields ->
            toNode (Pattern.RecordPattern (List.map (\(Node _ field) -> toNode field) fields))

        Pattern.UnConsPattern element list ->
            toNode (Pattern.UnConsPattern (normalizePattern element) (normalizePattern list))

        Pattern.ListPattern patterns ->
            toNode (Pattern.ListPattern (List.map normalizePattern patterns))

        Pattern.NamedPattern qualifiedNameRef patterns ->
            toNode (Pattern.NamedPattern qualifiedNameRef (List.map normalizePattern patterns))

        Pattern.AsPattern pattern (Node _ asName) ->
            toNode (Pattern.AsPattern (normalizePattern pattern) (toNode asName))

        Pattern.ParenthesizedPattern pattern ->
            normalizePattern pattern

        pattern ->
            toNode pattern


toNode : a -> Node a
toNode =
    Node Range.emptyRange


sameThingOnBothSidesDetails : Bool -> List String
sameThingOnBothSidesDetails computedResult =
    let
        computedResultString : String
        computedResultString =
            if computedResult then
                "True"

            else
                "False"
    in
    [ "The value on the left and on the right are the same. Therefore we can determine that the expression will always be " ++ computedResultString ++ "."
    ]
