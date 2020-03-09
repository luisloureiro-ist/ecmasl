type t = (Loc.t, Object.t) Hashtbl.t

let create () : t = Hashtbl.create 511

let insert (heap : t) (obj : Object.t) : Loc.t =
  let loc = Loc.newloc() in Hashtbl.add heap loc obj; loc

let remove (heap : t) (loc : Loc.t) : unit = Hashtbl.remove heap loc

let update (heap : t) (loc : Loc.t) (obj : Object.t) : unit = Hashtbl.replace heap loc obj

let get (heap : t) (loc : Loc.t) : Object.t option = Hashtbl.find_opt heap loc

let str (heap : t) : string = (Hashtbl.fold (fun n v ac -> (if ac <> "" then ac ^ ", " else ac) ^ (Printf.sprintf "%s: %s" (Loc.str n) (Object.str v))) heap "{ ") ^ " }"
