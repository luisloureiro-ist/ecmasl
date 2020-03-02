type t = (string, Func.t) Hashtbl.t

let get_func (prog : t) (func : string) : Func.t = Hashtbl.find prog func

let to_string (prog : t) : string = Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ "\n" else ac) ^ (Printf.sprintf "(%s -> %s)" n (Func.str v))) prog ""
