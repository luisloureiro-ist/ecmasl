open Expr

type t = Skip
       | Assign      of string * Expr.t
       | Seq         of t * t
       | If          of Expr.t * t * t
       | While       of Expr.t * t
       | Return      of Expr.t
       | FieldAssign of Expr.t * Field.t * Expr.t
       | FieldDelete of Expr.t * Field.t

let rec str (stmt : t) : string = match stmt with
    Skip                      -> ""
  | Assign (v, exp)           -> v ^ " = " ^ (Expr.str exp)
  | Seq (s1, s2)              -> (str s1) ^ "; " ^ (str s2)
  | If (exp, s1, s2)          -> "if (" ^ (Expr.str exp) ^ ") { " ^ (str s1) ^ " } else { " ^ (str s2) ^ "}"
  | While (exp, s)            -> "while (" ^ (Expr.str exp) ^ ") { " ^ (str s) ^ " }"
  | Return exp                -> "return " ^ (Expr.str exp) ^ ";"
  | FieldAssign (e_o, f, e_v) -> Expr.str e_o ^ "." ^ Field.str f ^ " = " ^ Expr.str e_v
  | FieldDelete (e, f)        -> "delete " ^ Expr.str e ^ "." ^ Field.str f
