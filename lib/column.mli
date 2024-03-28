type elem
(** [elem] represents the type of the data in a given column. *)

type t
(** [column] represents the record which holds [label] - the label of the column
    \- and [data] - the data of the column which is of type elem list. *)
val string_of_elem : elem -> string
(** [string_of_elem elem] returns [elem] as a string. *)

(* val label : t -> string *)
(* [label t] takes in a type t [t] and returns the label of [t] *)

val data : t -> elem list 
(** [data t] takes in a type t [t] and returns the data of [t] *)

val all_numbers : string -> bool
(** [all_numbers s] returns whether or not the string [s] contains only numbers. *)

val valid_date : elem -> bool
(** [is_valid_date d] returns whether or not the Date [d] is a valid Date. *)

val empty : t
(** [empty] returns an empty [t] *)

val elem_of_string : string -> elem
(** [elem_of_string s] takes in a string [s] and returns [s] as an [elem]. *)

val date_of_string : string -> elem
(** [string_to_elem s] takes in a string [s] and returns [s] as an elem of type
    [Date]. If [s] cannot be returned as a [Date] then [NULL] is returned. *)

val make_column : string -> string list -> t
(** [make_column s l] takes in a string [s] and a string list [l] and creates a
    column where the label is [s] and the data is [l] as an [elem list]. *)

val valid_data : elem list -> elem -> bool
(** [valid_data d h] takes in an elem list [d] which could be the data of a
    column. [h] is the elem type that [d] should be throughout. Returns true if
    [d] is all of type [h] otherwise returns false. *)

val valid_column : t -> bool
(** [valid_column col] returns if [col] is a valid [t]. *)

val elemlist_of_stringlist : string list -> elem list
(** [elemlist_of_stringlist s] returns the string [s] as an [elem list]. *)

val string_of_column : t -> string
(** [string_of_column c] takes in a column [c] of type [t] and returns it as a string. *)

val print : t -> unit
(** [print col] prints the contents of the column [col]. *)

val add_elem_to_column : elem -> t -> t
(** [add_elem_to_column elem col] returns a column with [elem] added to the
    beginning of [col.data]. *)
