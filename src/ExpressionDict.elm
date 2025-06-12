module ExpressionDict exposing (ExpressionDict, comparer, empty, get, insert)

import AVL.Dict
import Elm.Syntax.Expression as Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..))
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


type ExpressionDict a
    = ExpressionDict (AVL.Dict.Dict Expression a)


empty : ExpressionDict a
empty =
    ExpressionDict (AVL.Dict.emptyWith comparer)


insert : Expression -> a -> ExpressionDict a -> ExpressionDict a
insert k v (ExpressionDict dict) =
    ExpressionDict (AVL.Dict.insert k v dict)


get : Expression -> ExpressionDict a -> Maybe a
get k (ExpressionDict dict) =
    AVL.Dict.get k dict


comparer : Expression -> Expression -> Order
comparer l r =
    compareExpression (Node.empty l) (Node.empty r)


compareExpression : Node Expression -> Node Expression -> Order
compareExpression (Node _ l) (Node _ r) =
    case ( l, r ) of
        ( UnitExpr, UnitExpr ) ->
            EQ

        ( UnitExpr, _ ) ->
            LT

        ( _, UnitExpr ) ->
            GT

        ( TupledExpression lc, TupledExpression rc ) ->
            compareList compareExpression lc rc

        ( TupledExpression _, _ ) ->
            LT

        ( _, TupledExpression _ ) ->
            GT

        ( Application lc, Application rc ) ->
            compareList compareExpression lc rc

        ( Application _, _ ) ->
            LT

        ( _, Application _ ) ->
            GT

        ( OperatorApplication lo _ ll lr, OperatorApplication ro _ rl rr ) ->
            case compare lo ro of
                EQ ->
                    compareList compareExpression [ ll, lr ] [ rl, rr ]

                res ->
                    res

        ( OperatorApplication _ _ _ _, _ ) ->
            LT

        ( _, OperatorApplication _ _ _ _ ) ->
            GT

        ( FunctionOrValue lmod lname, FunctionOrValue rmod rname ) ->
            case compare lmod rmod of
                EQ ->
                    compare lname rname

                res ->
                    res

        ( FunctionOrValue _ _, _ ) ->
            LT

        ( _, FunctionOrValue _ _ ) ->
            GT

        ( IfBlock lc lt lf, IfBlock rc rt rf ) ->
            compareList compareExpression [ lc, lt, lf ] [ rc, rt, rf ]

        ( IfBlock _ _ _, _ ) ->
            LT

        ( _, IfBlock _ _ _ ) ->
            GT

        ( PrefixOperator lo, PrefixOperator ro ) ->
            compare lo ro

        ( PrefixOperator _, _ ) ->
            LT

        ( _, PrefixOperator _ ) ->
            GT

        ( Operator lo, Operator ro ) ->
            compare lo ro

        ( Operator _, _ ) ->
            LT

        ( _, Operator _ ) ->
            GT

        ( Integer li, Integer ri ) ->
            compare li ri

        ( Integer _, _ ) ->
            LT

        ( _, Integer _ ) ->
            GT

        ( Hex lh, Hex rh ) ->
            compare lh rh

        ( Hex _, _ ) ->
            LT

        ( _, Hex _ ) ->
            GT

        ( Floatable lf, Floatable rf ) ->
            compare lf rf

        ( Floatable _, _ ) ->
            LT

        ( _, Floatable _ ) ->
            GT

        ( Negation lc, Negation rc ) ->
            compareExpression lc rc

        ( Negation _, _ ) ->
            LT

        ( _, Negation _ ) ->
            GT

        ( Literal ll, Literal rl ) ->
            compare ll rl

        ( Literal _, _ ) ->
            LT

        ( _, Literal _ ) ->
            GT

        ( CharLiteral ll, CharLiteral rl ) ->
            compare ll rl

        ( CharLiteral _, _ ) ->
            LT

        ( _, CharLiteral _ ) ->
            GT

        ( ParenthesizedExpression lc, ParenthesizedExpression rc ) ->
            compareExpression lc rc

        ( ParenthesizedExpression _, _ ) ->
            LT

        ( _, ParenthesizedExpression _ ) ->
            GT

        ( ListExpr lc, ListExpr rc ) ->
            compareList compareExpression lc rc

        ( ListExpr _, _ ) ->
            LT

        ( _, ListExpr _ ) ->
            GT

        ( RecordExpr lc, RecordExpr rc ) ->
            compareList compareRecordSetter lc rc

        ( RecordExpr _, _ ) ->
            LT

        ( _, RecordExpr _ ) ->
            GT

        ( LetExpression lc, LetExpression rc ) ->
            compareLetBlocks lc rc

        ( LetExpression _, _ ) ->
            LT

        ( _, LetExpression _ ) ->
            GT

        ( CaseExpression lc, CaseExpression rc ) ->
            compareCaseBlocks lc rc

        ( CaseExpression _, _ ) ->
            LT

        ( _, CaseExpression _ ) ->
            GT

        ( LambdaExpression lc, LambdaExpression rc ) ->
            compareLambda lc rc

        ( LambdaExpression _, _ ) ->
            LT

        ( _, LambdaExpression _ ) ->
            GT

        ( RecordAccess ln (Node _ lf), RecordAccess rn (Node _ rf) ) ->
            case compareExpression ln rn of
                EQ ->
                    compare lf rf

                res ->
                    res

        ( RecordAccess _ _, _ ) ->
            LT

        ( _, RecordAccess _ _ ) ->
            GT

        ( RecordAccessFunction lf, RecordAccessFunction rf ) ->
            compare lf rf

        ( RecordAccessFunction _, _ ) ->
            LT

        ( _, RecordAccessFunction _ ) ->
            GT

        ( RecordUpdateExpression (Node _ ln) lc, RecordUpdateExpression (Node _ rn) rc ) ->
            case compare ln rn of
                EQ ->
                    compareList compareRecordSetter lc rc

                res ->
                    res

        ( RecordUpdateExpression _ _, _ ) ->
            LT

        ( _, RecordUpdateExpression _ _ ) ->
            GT

        ( GLSLExpression lc, GLSLExpression rc ) ->
            compare lc rc


compareLambda : Expression.Lambda -> Expression.Lambda -> Order
compareLambda l r =
    case compareList comparePattern l.args r.args of
        EQ ->
            compareExpression l.expression r.expression

        res ->
            res


comparePattern : Node Pattern -> Node Pattern -> Order
comparePattern (Node _ l) (Node _ r) =
    case ( l, r ) of
        ( AllPattern, AllPattern ) ->
            EQ

        ( AllPattern, _ ) ->
            LT

        ( _, AllPattern ) ->
            GT

        ( UnitPattern, UnitPattern ) ->
            EQ

        ( UnitPattern, _ ) ->
            LT

        ( _, UnitPattern ) ->
            GT

        ( CharPattern lc, CharPattern rc ) ->
            compare lc rc

        ( CharPattern _, _ ) ->
            LT

        ( _, CharPattern _ ) ->
            GT

        ( StringPattern ls, StringPattern rs ) ->
            compare ls rs

        ( StringPattern _, _ ) ->
            LT

        ( _, StringPattern _ ) ->
            GT

        ( IntPattern li, IntPattern ri ) ->
            compare li ri

        ( IntPattern _, _ ) ->
            LT

        ( _, IntPattern _ ) ->
            GT

        ( HexPattern li, HexPattern ri ) ->
            compare li ri

        ( HexPattern _, _ ) ->
            LT

        ( _, HexPattern _ ) ->
            GT

        ( FloatPattern lf, FloatPattern rf ) ->
            compare lf rf

        ( FloatPattern _, _ ) ->
            LT

        ( _, FloatPattern _ ) ->
            GT

        ( TuplePattern lc, TuplePattern rc ) ->
            compareList comparePattern lc rc

        ( TuplePattern _, _ ) ->
            LT

        ( _, TuplePattern _ ) ->
            GT

        ( RecordPattern lr, RecordPattern rr ) ->
            compareList compareNode lr rr

        ( RecordPattern _, _ ) ->
            LT

        ( _, RecordPattern _ ) ->
            GT

        ( UnConsPattern lh lt, UnConsPattern rh rt ) ->
            case comparePattern lh rh of
                EQ ->
                    comparePattern lt rt

                res ->
                    res

        ( UnConsPattern _ _, _ ) ->
            LT

        ( _, UnConsPattern _ _ ) ->
            GT

        ( ListPattern lp, ListPattern rp ) ->
            compareList comparePattern lp rp

        ( ListPattern _, _ ) ->
            LT

        ( _, ListPattern _ ) ->
            GT

        ( VarPattern lv, VarPattern rv ) ->
            compare lv rv

        ( VarPattern _, _ ) ->
            LT

        ( _, VarPattern _ ) ->
            GT

        ( NamedPattern lr lp, NamedPattern rr rp ) ->
            case compareQualifiedNameRef lr rr of
                EQ ->
                    compareList comparePattern lp rp

                res ->
                    res

        ( NamedPattern _ _, _ ) ->
            LT

        ( _, NamedPattern _ _ ) ->
            GT

        ( AsPattern lp (Node _ ln), AsPattern rp (Node _ rn) ) ->
            case comparePattern lp rp of
                EQ ->
                    compare ln rn

                res ->
                    res

        ( AsPattern _ _, _ ) ->
            LT

        ( _, AsPattern _ _ ) ->
            GT

        ( ParenthesizedPattern lc, ParenthesizedPattern rc ) ->
            comparePattern lc rc


compareQualifiedNameRef : Pattern.QualifiedNameRef -> Pattern.QualifiedNameRef -> Order
compareQualifiedNameRef lq rq =
    case compare lq.moduleName rq.moduleName of
        EQ ->
            compare lq.name rq.name

        res ->
            res


compareCaseBlocks : Expression.CaseBlock -> Expression.CaseBlock -> Order
compareCaseBlocks l r =
    case compareExpression l.expression r.expression of
        EQ ->
            compareList compareCase l.cases r.cases

        res ->
            res


compareCase : Expression.Case -> Expression.Case -> Order
compareCase ( lp, le ) ( rp, re ) =
    case comparePattern lp rp of
        EQ ->
            compareExpression le re

        res ->
            res


compareLetBlocks : Expression.LetBlock -> Expression.LetBlock -> Order
compareLetBlocks l r =
    case compareList compareDeclaration l.declarations r.declarations of
        EQ ->
            compareExpression l.expression r.expression

        res ->
            res


compareDeclaration : Node LetDeclaration -> Node LetDeclaration -> Order
compareDeclaration (Node _ l) (Node _ r) =
    case ( l, r ) of
        ( LetFunction lf, LetFunction rf ) ->
            compareFunction lf rf

        ( LetFunction _, _ ) ->
            LT

        ( _, LetFunction _ ) ->
            GT

        ( LetDestructuring lp le, LetDestructuring rp re ) ->
            case comparePattern lp rp of
                EQ ->
                    compareExpression le re

                res ->
                    res


compareFunction : Expression.Function -> Expression.Function -> Order
compareFunction l r =
    case compareMaybe compareNode l.documentation r.documentation of
        EQ ->
            case compareMaybe compareSignature l.signature r.signature of
                EQ ->
                    compareFunctionImplementation l.declaration r.declaration

                res ->
                    res

        res ->
            res


compareMaybe : (a -> a -> Order) -> Maybe a -> Maybe a -> Order
compareMaybe f l r =
    case ( l, r ) of
        ( Just lv, Just rv ) ->
            f lv rv

        ( Just _, _ ) ->
            LT

        ( _, Just _ ) ->
            GT

        ( Nothing, Nothing ) ->
            EQ


compareNode : Node comparable -> Node comparable -> Order
compareNode (Node _ l) (Node _ r) =
    compare l r


compareSignature : Node Signature -> Node Signature -> Order
compareSignature (Node _ l) (Node _ r) =
    case compareNode l.name r.name of
        EQ ->
            compareTypeAnnotation l.typeAnnotation r.typeAnnotation

        res ->
            res


compareTypeAnnotation : Node TypeAnnotation -> Node TypeAnnotation -> Order
compareTypeAnnotation (Node _ l) (Node _ r) =
    case ( l, r ) of
        ( TypeAnnotation.Unit, TypeAnnotation.Unit ) ->
            EQ

        ( TypeAnnotation.Unit, _ ) ->
            LT

        ( _, TypeAnnotation.Unit ) ->
            GT

        ( TypeAnnotation.GenericType lg, TypeAnnotation.GenericType rg ) ->
            compare lg rg

        ( TypeAnnotation.GenericType _, _ ) ->
            LT

        ( _, TypeAnnotation.GenericType _ ) ->
            GT

        ( TypeAnnotation.Typed (Node _ lm) lp, TypeAnnotation.Typed (Node _ rm) rp ) ->
            case compare lm rm of
                EQ ->
                    compareList compareTypeAnnotation lp rp

                res ->
                    res

        ( TypeAnnotation.Typed _ _, _ ) ->
            LT

        ( _, TypeAnnotation.Typed _ _ ) ->
            GT

        ( TypeAnnotation.Tupled lc, TypeAnnotation.Tupled rc ) ->
            compareList compareTypeAnnotation lc rc

        ( TypeAnnotation.Tupled _, _ ) ->
            LT

        ( _, TypeAnnotation.Tupled _ ) ->
            GT

        ( TypeAnnotation.Record lr, TypeAnnotation.Record rr ) ->
            compareList compareRecordFieldAnnotation lr rr

        ( TypeAnnotation.Record _, _ ) ->
            LT

        ( _, TypeAnnotation.Record _ ) ->
            GT

        ( TypeAnnotation.GenericRecord (Node _ ln) (Node _ lr), TypeAnnotation.GenericRecord (Node _ rn) (Node _ rr) ) ->
            case compare ln rn of
                EQ ->
                    compareList compareRecordFieldAnnotation lr rr

                res ->
                    res

        ( TypeAnnotation.GenericRecord _ _, _ ) ->
            LT

        ( _, TypeAnnotation.GenericRecord _ _ ) ->
            GT

        ( TypeAnnotation.FunctionTypeAnnotation lf lt, TypeAnnotation.FunctionTypeAnnotation rf rt ) ->
            case compareTypeAnnotation lf rf of
                EQ ->
                    compareTypeAnnotation lt rt

                res ->
                    res


compareRecordFieldAnnotation : Node TypeAnnotation.RecordField -> Node TypeAnnotation.RecordField -> Order
compareRecordFieldAnnotation (Node _ ( Node _ ln, lt )) (Node _ ( Node _ rn, rt )) =
    case compare ln rn of
        EQ ->
            compareTypeAnnotation lt rt

        res ->
            res


compareFunctionImplementation : Node Expression.FunctionImplementation -> Node Expression.FunctionImplementation -> Order
compareFunctionImplementation (Node _ l) (Node _ r) =
    case compareNode l.name r.name of
        EQ ->
            case compareList comparePattern l.arguments r.arguments of
                EQ ->
                    compareExpression l.expression r.expression

                res ->
                    res

        res ->
            res


compareRecordSetter : Node Expression.RecordSetter -> Node Expression.RecordSetter -> Order
compareRecordSetter (Node _ ( Node _ ln, le )) (Node _ ( Node _ rn, re )) =
    case compare ln rn of
        EQ ->
            compareExpression le re

        res ->
            res


compareList : (a -> a -> Order) -> List a -> List a -> Order
compareList f l r =
    case ( l, r ) of
        ( [], [] ) ->
            EQ

        ( [], _ ) ->
            LT

        ( _, [] ) ->
            GT

        ( lh :: lt, rh :: rt ) ->
            case f lh rh of
                EQ ->
                    compareList f lt rt

                res ->
                    res
