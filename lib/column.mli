type elem
(** [elem] represents the type of the data in a given column. *)

type t
(** [column] represents the record which holds [label] - the label of the column
    \- and [data] - the data of the column which is of type elem list. *)
val elem_to_string : elem -> string
(** [elem_to_string elem] returns [elem] as a string. *)

(* val string_of_data : int * int * int -> string *)
(** [string_of_data d] takes in a tuple of three ints known as [d] and returns a
    string. [d] represents a [Date]. *)

(* val label : t -> string *)
(** [label t] takes in a type t [t] and returns the label of [t] *)

(* val data : t -> elem list *)
(** [data t] takes in a type t [t] and returns the data of [t] *)

val all_numbers : string -> bool
(** [all_numbers s] returns whether or not the string [s] contains only numbers. *)

val is_valid_year : string -> bool
(** [is_valid_year s] returns whether or not the string [s] is a valid year. *)

val is_valid_month_or_day : string -> bool
(** [is_valid_month_or_day s] returns whether or not the string [s] is a valid
    month or day. *)

val is_valid_date : elem -> bool
(** [is_valid_date d] returns whether or not the Date [d] is a valid Date. *)

val empty : t
(** [empty] returns an empty [t] *)

val string_to_elem : string -> elem
(** [string_to_elem s] takes in a string [s] and returns [s] as an [elem]. *)

val date_of_string : string -> elem
(** [string_to_elem s] takes in a string [s] and returns [s] as an elem of type
    [Date]. If [s] cannot be returned as a [Date] then [NULL] is returned. *)

val make_column : string -> elem list -> t
(** [make_column s d] takes in a string [s] and an elem list [d] and creates a
    column where the label is [s] and the data is [d]. *)

val valid_data : elem list -> elem -> bool
(** [valid_data d h] takes in an elem list [d] which could be the data of a
    column. [h] is the elem type that [d] should be throughout. Returns true if
    [d] is all of type [h] otherwise returns false. *)

val valid_column : t -> bool
(** [valid_column col] returns if [col] is a valid [t]. *)

val stringlist_to_elemlist_aux : string list -> elem list -> elem list
(** [stringlist_to_elemlist_aux s acc] is a helper function for
    [stringlist_to_elemlist]. *)

val stringlist_to_elemlist : string list -> elem list
(** [stringlist_to_elemlist s] returns the string [s] as an [elem list]. *)

val string_of_date : int * int * int -> string
(** [string_of_data d] takes in a tuple of three ints known as [d] and returns a
    string. [d] represents a [Date]. *)

val print_data : elem list -> unit
(** [print_data d] prints the contents of the elem list [d]. *)

val print : t -> unit
(** [print col] prints the contents of the column [col]. *)

val add_elem_to_column : elem -> t -> t
(** [add_elem_to_column elem col] returns a column with [elem] added to the
    beginning of [col.data]. *)
