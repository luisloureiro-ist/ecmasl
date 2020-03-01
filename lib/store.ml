type t = (string, Val.t) Hashtbl.t

let create_store (varvals : (string * Val.t) list) : t =
  let sto : t = Hashtbl.create 511 in
  List.iter (fun (x, v) -> Hashtbl.add sto x v) varvals;
  sto

let get_var (sto : t) (name : string) : Val.t = Hashtbl.find sto name

let set_var (sto : t) (name : string) (value : Val.t) : unit = Hashtbl.replace sto name value

let to_string (sto : t) : string = Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ ", " else ac) ^ (Printf.sprintf "(%s -> %s)" n (Val.str v))) sto ""
