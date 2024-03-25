type elem
(** [elem] represents the type of the data in a given column. *)

type t
(** [t] represents the record which holds [label] - the label of the column -
    and [data] - the data of the column which is of type elem list. *)

val label : t -> string
(** [label t] takes in a type t [t] and returns the label of [t] *)

val data : t -> elem list
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
