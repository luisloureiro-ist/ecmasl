type t = (string, Func.t) Hashtbl.t

let get_func (prog : t) (func : string) : Func.t = Hashtbl.find prog func

let to_string (prog : t) : string = Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ "\n" else ac) ^ (Printf.sprintf "(%s -> %s)" n (Func.str v))) prog ""

let create_prog (funcs : Func.t list) : t =
  let prog = Hashtbl.create 511 in
  List.iter (fun (f : Func.t) -> Hashtbl.add prog f.name f) funcs;
  match Hashtbl.find_opt prog "main" with
    None   -> invalid_arg "Missing main function"
  | Some _ -> prog
