type t = string

let count = 0

let newloc () : t =
  "loc" ^ string_of_int (succ count)

let str (v : t) : string = "\"" ^ v ^ "\""
