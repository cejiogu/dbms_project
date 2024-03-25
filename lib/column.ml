type elem =
  | Int of int
  | Bool of bool
  | Float of float
  | String of string

type t = {
  label : string;
  data : elem list;
}

let label t = t.label
let data t = t.data
