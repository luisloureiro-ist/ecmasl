type bopt = Plus
          | Minus
          | Times
          | Div
          | Equal
          | Gt
          | Lt
          | Egt
          | Elt
          | InObj

type uopt = Neg

type t = Val    of Val.t
       | Var    of string
       | BinOpt of (bopt * t * t)
       | UnOpt  of (uopt * t)
       | Call   of (t * t list)
       | NewObj of (Field.t * t) list
       | Access of t * Field.t

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
  | InObj -> "in"

let rec str (e : t) : string = match e with
  | Val n               -> Val.str n
  | Var x               -> x
  | UnOpt (op, e)       -> (str_of_unopt op) ^ (str e)
  | BinOpt (op, e1, e2) -> (str e1) ^ " " ^ (str_of_binopt op) ^ " " ^ (str e2)
  | Call (f, es)        -> (str f) ^ " (" ^ List.fold_left (fun acc ele -> acc ^ str ele) "" es ^ ")"
  | NewObj (fes)        -> "{ " ^ fields_list_to_string fes ^ " }"
  | Access (e, f)       -> str e ^ "." ^ Field.str f

and fields_list_to_string (fes : (Field.t * t) list) : string =
  let strs = List.map (fun (f, e) -> (Field.str f) ^ ": " ^ (str e)) fes in
  String.concat ", " strs
