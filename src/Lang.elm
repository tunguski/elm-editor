module Lang exposing (Value(..), Expr(..), Pattern(..), Decl, Globals, Env)

{-| The data model of the interpreted Elm-like language: runtime values, the expression AST, the
pattern AST, and top-level declarations. Shared by the lexer/parser, the evaluator and the editor.
-}

import Dict exposing (Dict)


type Value
    = VNum Float
    | VBool Bool
    | VStr String
    | VChar Char
    | VList (List Value)
    | VCtor String (List Value)
    | VRecord (List ( String, Value ))
    | VTup (List Value)
    | VClosure (List String) Expr (List ( String, Value ))
    | VRec String (List String) Expr (List ( String, Value ))
    | VBuiltin String (List Value)


type Expr
    = Num Float
    | Str String
    | CharLit Char
    | Boolean Bool
    | ListE (List Expr)
    | Var String
    | Ctor String
    | Neg Expr
    | BinOp String Expr Expr
    | If Expr Expr Expr
    | Lam (List String) Expr
    | App Expr Expr
    | Let String Expr Expr
    | Case Expr (List ( Pattern, Expr ))
    | RecordLit (List ( String, Expr ))
    | RecordGet Expr String
    | RecordUpdate String (List ( String, Expr ))
    | Tup (List Expr)


type Pattern
    = PVar String
    | PWild
    | PInt Float
    | PBool Bool
    | PStr String
    | PChar Char
    | PCtor String (List Pattern)
    | PNil
    | PCons Pattern Pattern
    | PTup (List Pattern)
    | PRecord (List String)
    | PAlias Pattern String


{-| A top-level definition `name args = body`. -}
type alias Decl =
    { name : String
    , params : List String
    , body : Expr
    }


{-| All top-level definitions of a project, indexed by name for O(log n) lookup (a mutually-recursive
scope). The parser builds an assoc-list of decls in source order, then `parseProject` indexes it. -}
type alias Globals =
    Dict String Decl


{-| A local binding environment. -}
type alias Env =
    List ( String, Value )
