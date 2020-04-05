let add_fields_to (obj : Object.t) (fes : (Field.t * Expr.t) list) (eval_e : (Expr.t -> Val.t)) : unit =
  List.iter (fun (f, e) -> let e' = eval_e e in Object.set obj f e') fes


let eval_inobj_expr (heap : Heap.t) (field : Val.t) (loc : Val.t) : Val.t =
  let b = match loc, field with
    | Loc l, Str f -> Heap.get_field heap l f
    | _            -> invalid_arg "Exception in Interpreter.eval_inobj_expr : \"loc\" is not a Loc value or \"field\" is not a string" in
  match b with
  | Some v -> Bool (true)
  | None -> Bool (false)


let eval_unopt_expr (op : Expr.uopt) (v : Val.t) : Val.t = match op with
  | Neg    -> Val.neg v
  | Not    -> Val.not v
  | Typeof -> Val.typeof v

let eval_binopt_expr (heap : Heap.t) (op : Expr.bopt) (v1 : Val.t) (v2 : Val.t) : Val.t = match op with
  | Plus    -> Val.plus (v1, v2)
  | Minus   -> Val.minus (v1, v2)
  | Times   -> Val.times (v1, v2)
  | Div     -> Val.div (v1, v2)
  | Equal   -> Val.equal (v1, v2)
  | Gt      -> Val.gt (v1, v2)
  | Lt      -> Val.lt (v1, v2)
  | Egt     -> Val.egt (v1, v2)
  | Elt     -> Val.elt (v1, v2)
  | Log_And -> Val.log_and (v1, v2)
  | Log_Or  -> Val.log_or (v1, v2)
  | InObj   -> eval_inobj_expr heap v1 v2

let rec eval_expr (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e : Expr.t) : Val.t = match e with
  | Val n                -> n
  | Var x                -> Store.get sto x
  | UnOpt (uop, e)       -> let v = eval_expr prog heap sto e in eval_unopt_expr uop v
  | BinOpt (bop, e1, e2) -> let v1 = eval_expr prog heap sto e1 and v2 = eval_expr prog heap sto e2 in eval_binopt_expr heap bop v1 v2
  | Call (f, es)         -> eval_call_expr prog heap sto f es
  | NewObj (fes)         -> let obj = Object.create() in add_fields_to obj fes (eval_expr prog heap sto); Loc (Heap.insert heap obj)
  | Access (e, f)        -> eval_access_expr prog heap sto e f


(* Syntax for mutually recursive functions *)
and eval_call_expr (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (f_name : Expr.t) (es : Expr.t list) : Val.t =
  let vs = List.map (eval_expr prog heap sto) es in
  try
    let fname = eval_expr prog heap sto f_name in
    match fname with
    | Str f -> fst (eval_proc prog heap f vs)
    | _     -> invalid_arg "Exception in Interpreter.eval_call_expr | value found in store is not a string."
  with Not_found -> fst (eval_proc prog heap (Expr.str f_name) vs)


and eval_access_expr (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e : Expr.t) (f : Expr.t) : Val.t =
  let loc = eval_expr prog heap sto e and field = eval_expr prog heap sto f in
  let loc' = (match loc with
      | Loc loc -> loc
      | _       -> invalid_arg "Exception in Interpreter.eval_access_expr : \"e\" didn't evaluate to Loc") in
  let field' = (match field with
      | Str field -> field
      | _         -> invalid_arg "Exception in Interpreter.eval_access_expr : \"f\" didn't evaluate to Str") in
  let v = Heap.get_field heap loc' field' in
  match v with
  | None    -> Undef
  | Some v' -> v'


and eval_stmt (prog : Prog.t) (heap : Heap.t) (sto: Store.t) (s: Stmt.t) : Val.t option = match s with
    Skip                      -> None
  | Assign (x, e)             -> let v = eval_expr prog heap sto e in Store.set sto x v; None
  | Seq (s1, s2)              -> (let v1 = eval_stmt prog heap sto s1 in
                                  match v1 with
                                    None -> eval_stmt prog heap sto s2
                                  | Some v1 -> Some v1)
  | If (exps_stmts)           -> eval_if_stmt prog heap sto exps_stmts
  | While (e, s)              -> eval_stmt prog heap sto (If ([Some e, Seq (s, While (e, s))]))
  | Return exp                -> let v = eval_expr prog heap sto exp in Some v
  | FieldAssign (e_o, f, e_v) -> (let (loc : Loc.t) =
                                    (let loc = eval_expr prog heap sto e_o in
                                     match loc with
                                     | Loc l -> l
                                     | _ -> invalid_arg "Exception in Interpreter.eval_stmt | FieldAssign : \"e_o\" is not a Loc value") in
                                  let v = eval_expr prog heap sto e_v in
                                  Heap.set_field heap loc f v; None
                                 )
  | FieldDelete (e, f)        -> eval_fielddelete_stmt prog heap sto e f
  | ExprStmt e                -> Some (eval_expr e)


and eval_if_stmt (prog : Prog.t) (heap : Heap.t) (sto: Store.t) (exps_stmts : (Expr.t option * Stmt.t) list) : Val.t option =
  if List.length exps_stmts = 0 then None
  else let e_s = List.hd exps_stmts and rest = List.tl exps_stmts in
    match e_s with
    | Some e, s -> (let v = eval_expr prog heap sto e in
                    if (Val.is_true v) then eval_stmt prog heap sto s
                    else eval_stmt prog heap sto (If rest))
    | None, s   -> eval_stmt prog heap sto s

and eval_fielddelete_stmt (prog : Prog.t) (heap : Heap.t) (sto : Store.t) (e : Expr.t) (f : Expr.t) =
  let loc = eval_expr prog heap sto e and field = eval_expr prog heap sto f in
  let loc' = (match loc with
      | Loc loc -> loc
      | _ -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"e\" is not a Loc value") in
  let field' = (match field with
      | Str field -> field
      | _         -> invalid_arg "Exception in Interpreter.eval_fielddelete_stmt : \"f\" didn't evaluate to Str") in
  Heap.delete_field heap loc' field'; None

and eval_proc (prog : Prog.t) (heap : Heap.t) (pname : string) (args : Val.t list) : Val.t * Store.t =
  let func = Prog.get_func prog pname in
  let sto = Func.create_store func args in
  let s = Func.get_body func in
  let v0 = eval_stmt prog heap sto s in
  match v0 with
    None -> raise (Invalid_argument "Exception in Interpreter.eval_proc: statement didn't return")
  | Some v -> (v, sto)


let eval_prog (prog : Prog.t) (heap : Heap.t) : Val.t * Store.t =
  eval_proc prog heap "main" []
