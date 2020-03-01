type bopt = Plus | Minus | Times | Div | Equal | Gt | Lt | Egt | Elt

type uopt = Neg

type t = Val    of Val.t
       | Var    of string
       | BinOpt of (bopt * t * t)
       | UnOpt  of (uopt * t)

let str_of_unopt (op : uopt) : string = match op with
  | Neg   -> "-"

let str_of_binopt (op : bopt) : string = match op with
  | Plus  -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div   -> "/"
  | Equal -> "=="
  | Gt    -> ">"
  | Lt    -> "<"
  | Egt   -> ">="
  | Elt   -> "<="

let rec str (e : t) : string = match e with
  | Val n               -> Val.str n
  | Var x               -> x
  | UnOpt (op, e)       -> (str_of_unopt op) ^ (str e)
  | BinOpt (op, e1, e2) -> (str e1) ^ " " ^ (str_of_binopt op) ^ " " ^ (str e2)

let eval_unopt (op : uopt) (v : Val.t) : Val.t = match op with
  | Neg -> Val.neg v

let eval_binopt (op : bopt) (v1 : Val.t) (v2 : Val.t) : Val.t = match op with
  | Plus  -> Val.plus (v1, v2)
  | Minus -> Val.minus (v1, v2)
  | Times -> Val.times (v1, v2)
  | Div   -> Val.div (v1, v2)
  | Equal -> Val.equal (v1, v2)
  | Gt    -> Val.gt (v1, v2)
  | Lt    -> Val.lt (v1, v2)
  | Egt   -> Val.egt (v1, v2)
  | Elt   -> Val.elt (v1, v2)

let rec eval (sto : Store.t) (e : t) : Val.t = match e with
  | Val n                -> n
  | Var x                -> Store.get_var sto x
  | UnOpt (uop, e)       -> let v = eval sto e in eval_unopt uop v
  | BinOpt (bop, e1, e2) -> let v1 = eval sto e1 and v2 = eval sto e2 in eval_binopt bop v1 v2
