type elem
(** [elem] represents the type of the data in a given column. *)

val elem_to_string : elem -> string
(** [elem_to_string elem] returns [elem] as a string. *)

val string_of_data : int * int * int -> string
(** [string_of_data d] takes in a tuple of three ints known as [d] and returns a
    string. [d] represents a [Date]. *)

type t
(** [t] represents the record which holds [label] - the label of the column -
    and [data] - the data of the column which is of type elem list. *)

val label : t -> string
(** [label t] takes in a type t [t] and returns the label of [t] *)

val data : t -> elem list
(** [data t] takes in a type t [t] and returns the data of [t] *)

val empty : t
(** [empty ()] returns an empty [t] *)

val make_column : string -> elem list -> t
(** [make_column s d] takes in a string [s] and an elem list [d] and creates a
    column where the label is [s] and the data is [d]. *)

val valid_column : t -> bool
(** [valid_column col] returns if [col] is a valid [t]. *)

val stringlist_to_elemlist : string list -> elem list
(** [stringlist_to_elemlist s] returns the string [s] as an [elem list]. *)

val print_data : elem list -> unit
(** [print_data d] prints the contents of the elem list [d]. *)

val print : t -> unit
(** [print col] prints the contents of the column [col]. *)

val add_elem_to_column : elem -> t -> t
(** [add_elem_to_column elem col] returns a column with [elem] added to the
    beginning of [col.data]. *)
