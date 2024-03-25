type dta =
  | Int of int
  | Bool of bool
  | Float of float
  | String of string

type t = {
  label : string;
  data : dta list;
}

let label c = c.label
let data c = c.data
