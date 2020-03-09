type t = {
  name   : string;
  params : string list;
  body   : Stmt.t;
}

let create_store (func : t) (vals : Val.t list) : Store.t =
  let varvals = List.combine func.params vals in
  Store.create varvals

let create (name : string) (params : string list) (body : Stmt.t) : t = { name; params; body }

let get_body (func : t) : Stmt.t = func.body

let str (func : t) : string = "function " ^ func.name ^ " (" ^ List.fold_left (fun acc ele -> acc ^ ele) ", " func.params ^ ") { " ^ Stmt.str func.body
