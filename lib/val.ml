type t =
  | Flt  of float
  | Int  of int
  | Bool of bool
  | Str  of string
  | Loc  of Loc.t

let neg (v : t) : t = match v with
  | Flt v  -> Flt (-.v)
  | Int v  -> Int (-v)
  | Bool v -> Bool (v = false)
  | Str v  -> raise (Invalid_argument "Exception in Val.neg: this operation doesn't apply to string type argument")
  | Loc v  -> raise (Invalid_argument "Exception in Val.neg: this operation doesn't apply to Loc type argument")

let plus (v1, v2 : t * t) : t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 +. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 +. v2)
  | (Flt v1, Flt v2) -> Flt (v1 +. v2)
  | (Int v1, Int v2) -> Int (v1 + v2)
  | _ -> raise (Invalid_argument "Exception in Val.plus: this operation doesn't apply to boolean type arguments")

let minus (v1, v2 : t * t) : t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 -. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 -. v2)
  | (Flt v1, Flt v2) -> Flt (v1 -. v2)
  | (Int v1, Int v2) -> Int (v1 - v2)
  | _ -> raise (Invalid_argument "Exception in Val.minus: this operation doesn't apply to boolean type arguments")

let times (v1, v2 : t * t) : t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 *. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 *. v2)
  | (Flt v1, Flt v2) -> Flt (v1 *. v2)
  | (Int v1, Int v2) -> Int (v1 * v2)
  | _ -> raise (Invalid_argument "Exception in Val.times: this operation doesn't apply to boolean type arguments")

let div (v1, v2 : t * t) : t = match v1, v2 with
  | (Flt v1, Int v2) -> Flt (v1 /. float_of_int v2)
  | (Int v1, Flt v2) -> Flt (float_of_int v1 /. v2)
  | (Flt v1, Flt v2) -> Flt (v1 /. v2)
  | (Int v1, Int v2) -> Int (v1 / v2)
  | _ -> raise (Invalid_argument "Exception in Val.div: this operation doesn't apply to boolean type arguments")

let equal (v1, v2 : t * t) : t = Bool (v1 = v2)

let gt (v1, v2 : t * t) : t = Bool (v1 > v2)

let lt (v1, v2 : t * t) : t = Bool (v1 < v2)

let egt (v1, v2 : t * t) : t = Bool (v1 >= v2)

let elt (v1, v2 : t * t) : t = Bool (v1 <= v2)

let is_true (v : t) : bool = match v with
  | Bool v -> v
  | _ -> raise (Invalid_argument "Exception in Val.is_true: argument is not boolean")

let str (v : t) : string = match v with
  | Flt v  -> string_of_float v
  | Int v  -> string_of_int v
  | Bool v -> string_of_bool v
  | Str v  -> "\"" ^ v ^ "\""
  | Loc v  -> Loc.str v
