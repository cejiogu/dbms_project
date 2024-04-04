type elem
(** The type representing the data contained within a column. Variants include
    integers, booleans, floats, strings, and dates. *)

type t
(** The type representing a column within a table, which consists of a record
    holding the fields of [title] the string label of the column and [data] the
    data entries of the column. [t] holds [elem_type] which is an int corresponding to the 
    the type of elem. 
    - `-1` for NULL
    - `0` for Int
    - `1` for Bool
    - `2` for Float
    - `3` for String
    - `4` for Date. *)

val string_of_elem : elem -> string
(** [string_of_elem elem] Converts an element of type [elem] to its string
    representation.
    @param elem The element to be converted. *)

val data : t -> elem list
(** [data col] Retrieves the list of data entries from a column.
    @param col The column from which data is retrieved. *)

val all_numbers : string -> bool
(** [all_numbers s] Checks if a given string consists entirely of digits.
    @param s The string to check. *)

val valid_date : elem -> bool
(** [valid_date d] Determines if a given element represents a valid date.
    @param d The element to validate. *)

val empty : int -> string -> t
(** [empty elt n] Produces an empty column with the title [n], elemtype of [elt] and no data entries.
    @param elt The int set to the [elemtype] of the column.
    @param n The string set to the [title] of the column.
    @return An empty column instance. *)

val elem_of_string : string -> elem
(** [elem_of_string s] Parses a string to produce an element of type [elem].
    @param s The string to parse.
    @return The corresponding element. *)

val date_of_string : string -> elem
(** [date_of_string s] Attempts to parse a string into a [Date] element.
    @param s The string representing a date.
    @return A [Date] element if successful; otherwise, [NULL]. *)

val make : string -> string list -> t
(** [make t str_data_lst] Creates a column with a specified title and a list of
    data entries converted from strings.
    @param t The title of the column.
    @param str_data_lst The list of strings to be converted into data entries.
    @return A new column with the specified data. *)

val title : t -> string
(**[title t] Gets the title of a given column [t]
   @param t The column.
   @return The title of a column*)

val valid_data : elem list -> elem -> bool
(** [valid_data data type_elem] Verifies that all data entries in a list match a
    specified type.
    @param data The list of data entries to check.
    @param type_elem The type element used to check the data list elem type.
    @return [true] if all entries match the type; otherwise, [false]. *)

val valid_column : t -> bool
(** [valid_column col] Checks if a column's data entries are consistent and
    valid.
    @param col The column to validate.
    @return [true] if the column is valid; otherwise, [false]. *)

val elemlist_of_stringlist : string list -> elem list
(** [elemlist_of_stringlist str_lst] Converts a list of strings into a list of
    elements of type [elem].
    @param str_lst The list of strings to convert.
    @return A list of data entries corresponding to the input strings. *)

val string_of_column : t -> string
(** [string_of_column col] Converts a column to its string representation,
    including its title and data entries.
    @param col The column to convert.
    @return The string representation of the column. *)

val stringlist_of_data : elem list -> string list
(** [stringlist_of_data data] Converts an elem list to a string list.
    @param data The elem list to convert.
    @return The string representation of the data. *)

val stringlist_of_column : t -> string list
(** [stringlist_of_column col] Converts a column to a string list.
    @param col The column to convert.
    @return The string representation of the column. *)

val print : t -> unit
(** [print col] Prints the contents of a column, including its title and data
    entries, to the console.
    @param col The column to print. *)

val add_elem_to_column : elem -> t -> t
(** [add_elem_to_column elem col] Adds a new element to the beginning of a
    column's data list.
    @param elem The element to add.
    @param col The column to which the element will be added.
    @return The updated column with the new element added. *)
