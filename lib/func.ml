type t = {
  name   : string;
  params : string list;
  body   : Stmt.t;
}

let create_store (func : t) (vals : Val.t list) : Store.t =
  let varvals = List.combine func.params vals in
  Store.create_store varvals

let get_body (func : t) : Stmt.t = func.body
