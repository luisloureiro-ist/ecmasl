type t = (string, Func.t) Hashtbl.t

let main : t = Hashtbl.create 511

let getFunc (prog : t) (func : string) : Func.t = Hashtbl.find prog func
