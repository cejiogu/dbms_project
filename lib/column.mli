(** [elem] represents the type of the data in a given column.  *)
type elem

(** [t] represents the record which holds [label] - the label of the column - and [data] - the data of the column which is of type elem list.  *)
type t

(** [label t] takes in a type t [t] and returns the label of t *)
val label : t -> string

(** [data t] takes in a type t [t] and returns the data of t *)
val data : t -> elem list