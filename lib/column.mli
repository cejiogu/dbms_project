type elem
(** [elem] represents the type of the data in a given column. *)

type t
(** [t] represents the record which holds [label] - the label of the column -
    and [data] - the data of the column which is of type elem list. *)

val label : t -> string
(** [label t] takes in a type t [t] and returns the label of t *)

val data : t -> elem list
(** [data t] takes in a type t [t] and returns the data of t *)
