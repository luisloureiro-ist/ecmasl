type t = (Field.t, Val.t) Hashtbl.t

let create () : t = Hashtbl.create 511

let get (obj : t) (f : Field.t) : Val.t option = Hashtbl.find_opt obj f

let set (obj : t) (f : Field.t) (v : Val.t) : unit = Hashtbl.add obj f v

let delete (obj : t) (f : Field.t) : unit = Hashtbl.remove obj f

let str (obj : t) : string = (Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ ", " else ac) ^ (Printf.sprintf "%s: %s" (Field.str n) (Val.str v))) obj "{ ") ^ " }"
