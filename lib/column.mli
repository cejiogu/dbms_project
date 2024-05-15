exception EmptyColumn of string
(** [EmptyColumn s] is raised when an operation cannot be applied to an empty
    column, accompanied by explanatory error message [s]*)

exception InvalidQuery of string
(** [InvalidQuery s] is raised when an operation cannot be applied, accompanied
    by explanatory error message [s]*)

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

val col_size : t -> int
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
(**[make_raw data title] is the column with title [title] and data [data]*)

val col_type : t -> elem
(**[col_type t] is the element type of the column [t]*)

val elemtype_of_stringparse : string -> string
(**[elemtype_of_stringparse str] is the lowercase string version of the string
   [str] SQL command version of an element type (ex: elemtype_of_stringparse
   FLOAT -> Float))*)

val sqlstr_of_elm : elem -> string
(**[sqlstr_of_elm] is the uppercase SQL version of an elem type
   (ex:sqlstr_of_elm true -> "TRUE")*)

val string_of_elmtyp : elem -> string
(**[string_of_elmtyp] is the string elem type of the input (ex:
   string_of_elemtyp 6-> Int)*)

val filter_indx : t -> int list -> t
(**[filter_indx c indx_lst] is the column [c] containing only the indices in
   [indx_lst]*)

val filter_indicies : t -> elem -> int list
(**[filter_indicies c e] is the list containing the indicies of [c] which have a
   data element equal to [e]*)

val select_aux : t -> string -> string
(** [select_aux column specifier] Searches through the column [column] to find
    the the value that best fulfills the specifier. For example, if
    [select_aux column "max"] is passed on the specified column, the function
    will return the greatest value from the column. If [select_aux column "min"]
    is passed on the specified column, the function will return the least value
    from the column.
    @param column The column from which the value is retrieved
    @param specifier Keyword that specifies which value to be searched for
    @return
      The value, represented as a string, that best fulfills the specifier in
      column [column]
    @raise InvalidQuery
      if the datatype of the column is NULL, Date, or Bool, or if a string other
      than "max" or "min" is passed in as the specifier*)

val nth : t -> int -> elem
(** [nth column value location] is the elem located at the index [location] in
    the column [column]
    @param column The column being searched
    @param location The specific index to be searched
    @return The value at the index [location] in column [column]*)
