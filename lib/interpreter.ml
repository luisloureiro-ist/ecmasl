let eval_unopt_expr (op : Expr.uopt) (v : Val.t) : Val.t = match op with
  | Neg -> Val.neg v

let eval_binopt_expr (op : Expr.bopt) (v1 : Val.t) (v2 : Val.t) : Val.t = match op with
  | Plus  -> Val.plus (v1, v2)
  | Minus -> Val.minus (v1, v2)
  | Times -> Val.times (v1, v2)
  | Div   -> Val.div (v1, v2)
  | Equal -> Val.equal (v1, v2)
  | Gt    -> Val.gt (v1, v2)
  | Lt    -> Val.lt (v1, v2)
  | Egt   -> Val.egt (v1, v2)
  | Elt   -> Val.elt (v1, v2)

let rec eval_expr (prog : Prog.t) (sto : Store.t) (e : Expr.t) : Val.t = match e with
  | Val n                -> n
  | Var x                -> Store.get sto x
  | UnOpt (uop, e)       -> let v = eval_expr prog sto e in eval_unopt_expr uop v
  | BinOpt (bop, e1, e2) -> let v1 = eval_expr prog sto e1 and v2 = eval_expr prog sto e2 in eval_binopt_expr bop v1 v2
  | Call (f, es)         -> let vs = List.map (eval_expr prog sto) es in fst (eval_proc prog f vs)

(* Syntax for mutually recursive functions *)
and eval_stmt (prog : Prog.t) (sto: Store.t) (s: Stmt.t) : Val.t option = match s with
    Skip           -> None
  | Assign (x, e)  -> let v = eval_expr prog sto e in Store.set sto x v; None
  | Seq (s1, s2)   -> (let v1 = eval_stmt prog sto s1 in
                       match v1 with
                         None -> eval_stmt prog sto s2
                       | Some v1 -> Some v1)
  | If (e, s1, s2) -> let v = eval_expr prog sto e in if (Val.is_true v) then eval_stmt prog sto s1 else eval_stmt prog sto s2
  | While (e, s)   -> eval_stmt prog sto (If (e, Seq (s, While (e, s)), Skip))
  | Return exp     -> let v = eval_expr prog sto exp in Some v

and eval_proc (prog : Prog.t) (pname : string) (args : Val.t list) : Val.t * Store.t =
  let func = Prog.get_func prog pname in
  let sto = Func.create_store func args in
  let s = Func.get_body func in
  let v0 = eval_stmt prog sto s in
  match v0 with
    None -> raise (Invalid_argument "Exception in Interpreter.eval_proc: statement didn't return")
  | Some v -> (v, sto)


let eval_prog (prog : Prog.t) : Val.t * Store.t =
  eval_proc prog "main" []
