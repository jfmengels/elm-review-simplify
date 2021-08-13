module Simplify.Normalize exposing (Comparison(..), areAllTheSame, areTheSame, compare)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range as Range
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


areTheSame : ModuleNameLookupTable -> Node Expression -> Node Expression -> Bool
areTheSame lookupTable left right =
    normalize lookupTable left == normalize lookupTable right


areAllTheSame : ModuleNameLookupTable -> Node Expression -> List (Node Expression) -> Bool
areAllTheSame lookupTable first rest =
    let
        normalizedFirst : Node Expression
        normalizedFirst =
            normalize lookupTable first
    in
    List.all (\node -> normalize lookupTable node == normalizedFirst) rest


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

        Expression.RecordUpdateExpression (Node _ value) nodes ->
            nodes
                |> List.map (\(Node _ ( Node _ fieldName, expr )) -> toNode ( toNode fieldName, normalize lookupTable expr ))
                |> Expression.RecordUpdateExpression (toNode value)
                |> toNode

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



-- COMPARE


type Comparison
    = ConfirmedEquality
    | ConfirmedInequality
    | Unconfirmed


compare : ModuleNameLookupTable -> Node Expression -> Node Expression -> Comparison
compare lookupTable left_ right_ =
    case ( Node.value left_, Node.value right_ ) of
        ( Expression.Literal left, Expression.Literal right ) ->
            fromEquality (left == right)

        _ ->
            Unconfirmed


fromEquality : Bool -> Comparison
fromEquality bool =
    if bool then
        ConfirmedEquality

    else
        ConfirmedInequality


removeParens : Node Expression -> Node Expression
removeParens node =
    case Node.value node of
        Expression.ParenthesizedExpression expr ->
            removeParens expr

        _ ->
            node
