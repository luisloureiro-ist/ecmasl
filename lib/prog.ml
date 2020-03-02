type t = (string, Func.t) Hashtbl.t

let get_func (prog : t) (func : string) : Func.t = Hashtbl.find prog func
