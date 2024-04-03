exception InvalidQuery of string

type column
(** The type representing a single column in a table. *)

type t
(** The type representing a table, which consists of a name [name] and a list of
    columns [columns]. *)

val name : t -> string
(** [name t] Returns the name of the table [t].
    @param t The table whose name you're retrieving. *)

val columns : t -> column list
(** [columns t] Returns the list of columns in the table [t].
    @param t The table whose name you're retrieving. *)

val empty : string -> t
(** [empty name] Creates an empty table with the specified name [name].
    @param name The name of the table to be created.
    @raise InvalidQuery if the provided [name] is an empty string.
    @return A new table instance with the specified name and no columns. *)

val make : string -> string list -> t
(** [make tab_name col_names] Creates a new table with the given [tab_name] and columns named according to [col_names].
    @param tab_name The name of the table to be created.
    @param col_names A list of names for each column in the table.
    @return A new table instance with the specified name and columns.
    @note This function is analogous to the SQL CREATE TABLE statement. *)

val insert_into : string -> string list -> string list -> t -> 'a
(** [insert_into tab_name col_names values table] Inserts a new row into [table], with values specified in [values] corresponding to the columns named in [column_names].
    @param tab_name The name of the table where values are to be inserted.
    @param col_names A list of column names indicating where each value in [values] should be inserted.
    @param values A list of values to be inserted into the table, corresponding to [col_names].
    @param table The table into which the new row is to be inserted.
    @raise InvalidQuery if the lengths of [col_names] and [values] do not match.
    @return An updated table with the new row of values inserted.
    @notes Requires implementation of functionality from the Column module to properly add values to columns. *)

val print : t -> string list list
(** [print t] Prints the table [t] to the terminal, showing its structure and content.
    @param t The table to be printed.
    @return The table converted to a 2-dimensional list of strings 
    @notes The actual implementation should iterate over the table's rows and columns, formatting the output for readability. *)
