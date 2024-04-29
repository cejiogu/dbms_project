type elem
(** The type representing the data contained within a column. Variants include
    integers, booleans, floats, strings, and dates. *)

type t
(** The type representing a column within a table, which consists of a record
    holding the fields of [title] the string label of the column and [data] the
    data entries of the column. [t] holds [elemtype] which is the type of elem. *)

val string_of_elem : elem -> string
(** [string_of_elem elem] Converts an element of type [elem] to its string
    representation.
    @param elem The element to be converted. *)

val title : t -> string
(**[title t] Gets the title of a given column [t]
   @param t The column.
   @return The title of a column*)

val data : t -> elem list
(** [data col] Retrieves the list of data entries from a column.
    @param col The column from which data is retrieved. *)

val col_size: t->int
(**[col_size t] is the number of elements in column [t]*) 

val empty : string -> string -> t
(** [empty n el] Produces an empty column with the title [n], elem of [el] and
    no data entries.
    @param n The string set to the [title] of the column.
    @param el The elem set to the [elemtype] of the column.
    @return An empty column instance. *)

val rename : t -> string -> t
(** [rename col new_title] Updates the title of a column [col] to [new_title].
    This function creates a new column with the updated title while preserving
    the original column's data and element type.

    @param col The column to rename.
    @param new_title The new title for the column.
    @return
      A new column instance with the updated title and original data and element
      type. *)

val date_of_string : string -> elem option
(** [date_of_string s] Attempts to parse a string into a [Date] option element.
    @param s The string representing a date.
    @return [Some Date] element if successful; otherwise, [None]. *)

val elem_of_string : string -> elem
(** [elem_of_string s] Parses a string to produce an element of type [elem].
    @param s The string to parse.
    @return The corresponding element. *)

val elemlist_of_stringlist : string list -> elem -> elem list
(** [elemlist_of_stringlist str_lst elt] Converts a list of strings into a list
    of elements of type [elem].
    @param str_lst The list of strings to convert.
    @param elt The type associated with the strings to convert.
    @return A list of data entries corresponding to the input strings. *)

val string_of_column : t -> string
(** [string_of_column col] Converts a column to its string representation,
    including its title and data entries.
    @param col The column to convert.
    @return The string representation of the column. *)

val stringlist_of_column : t -> string list
(** [stringlist_of_column col] Converts a column to a string list.
    @param col The column to convert.
    @return The string representation of the column. *)

val make : string -> string list -> t
(** [make t str_data_lst] Creates a column with a specified title and a list of
    data entries converted from strings.
    @param t The title of the column.
    @param str_data_lst The list of strings to be converted into data entries.
    @return A new column with the specified data. *)

val add : string -> t -> t
(** [add str_elem col] Adds an element, parsed from [str_elem], to the column
    [col], ensuring type consistency. If the parsed element's type matches the
    column's type, or if the column is uninitialized, the element is added;
    otherwise, a "All elements must be of the same type" exception is raised.

    @param str_elem The string representation of the element to be added.
    @param col The target column.
    @return The column with the new element added.
    @raise Failure
      if there's a type mismatch between the element and the column elemtype. *)

val print : t -> unit
(** [print col] Prints the contents of a column, including its title and data
    entries, to the console.
    @param col The column to print. *)

val make_raw : elem list -> string -> t
val col_type : t -> elem
val elemtype_of_stringparse : string -> string
val sqlstr_of_elm : elem -> string
val string_of_elmtyp : elem -> string
val filter_indx: t->int list->t
val filter_indicies:t->elem->int list

(* FUNCTION CEMETERY

   val valid_data : elem list -> elem -> bool (** [valid_data data type_elem]
   Verifies that all data entries in a list match a specified type. @param data
   The list of data entries to check. @param type_elem The type element used to
   check the data list elem type. @return [true] if all entries match the type;
   otherwise, [false]. *)

   val valid_column : t -> bool (** [valid_column col] Checks if a column's data
   entries are consistent and valid. @param col The column to validate. @return
   [true] if the column is valid; otherwise, [false]. *) *)

(* MISCELLANEOUS CEMETERY

   val all_numbers : string -> bool (** [all_numbers s] Checks if a given string
   consists entirely of digits. @param s The string to check. *)

   val valid_date : elem -> bool (** [valid_date d] Determines if a given
   element represents a valid date. @param d The element to validate. *) *)
