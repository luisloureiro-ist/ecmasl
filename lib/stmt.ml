open Expr

type t = Skip
       | Assign of string * Expr.t
       | Seq    of t * t
       | If     of Expr.t * t * t
       | While  of Expr.t * t

let rec str (stmt : t) : string = match stmt with
    Skip             -> ""
  | Assign (v, exp)  -> v ^ " = " ^ (Expr.str exp)
  | Seq (s1, s2)     -> (str s1) ^ "; " ^ (str s2)
  | If (exp, s1, s2) -> "if (" ^ (Expr.str exp) ^ ") { " ^ (str s1) ^ " } else { " ^ (str s2) ^ "}"
  | While (exp, s)   -> "while (" ^ (Expr.str exp) ^ ") { " ^ (str s) ^ " }"

let rec eval (sto: Store.t) (s: t) : unit = match s with
    Skip           -> ()
  | Assign (x, e)  -> let v = Expr.eval sto e in Store.set_var sto x v
  | Seq (s1, s2)   -> eval sto s1; eval sto s2
  | If (e, s1, s2) -> let v = Expr.eval sto e in if (Val.is_true v) then eval sto s1 else eval sto s2
  | While (e, s)   -> eval sto (If (e, Seq (s, While (e, s)), Skip))
