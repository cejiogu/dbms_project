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

val make : string -> string list -> string list -> t
(** [make tab_name col_names col_types] Creates a new table with the given [tab_name] and columns named according to [col_names] with types according to [col_types].
    @param tab_name The name of the table to be created.
    @param col_names A list of names for each column in the table.
    @param col_types A list of data types for each column in the table.
    @return A new table instance with the specified name and columns.
    @note This function is analogous to the SQL CREATE TABLE statement. *)

val insert_into : t -> string list -> string list -> t
(** [insert_into table col_names values] Inserts a new row into [table], with values specified in [values] corresponding to the columns named in [column_names].
    @param table The table into which the new row is to be inserted.
    @param col_names A list of column names indicating where each value in [values] should be inserted.
    @param values A list of values to be inserted into the table, corresponding to [col_names].
    @raise InvalidQuery if the lengths of [col_names] and [values] do not match.
    @return An updated table with the new row of values inserted.
    @notes Requires implementation of functionality from the Column module to properly add values to columns. *)

val string_of_table : t -> string
(** [string_of_table t] Converts the table [t] into a string representation.
    @param t The table to convert to a string.
    @return A string that represents the table, including its name and a formatted list of columns and their data.
    The function formats the table name followed by each column's string representation, with each column separated by a newline. Each column's data is presented in a list format, and each data element within a column is also separated by newlines for clarity.
    @example Calling [string_of_table my_table] where [my_table] has a name "TestTable" and two columns named "ID" and "Name" with respective values ["1", "2"] and ["Alice", "Bob"] returns:
    "Table: TestTable\nID: [1, 2]\nName: [Alice, Bob]" *)

val print : t -> unit
(** [print t] Prints the table [t] to the terminal, showing its structure and content.
    @param t The table to be printed.
    @return The table is represented in the terminal
    @notes The actual implementation should iterate over the table's rows and columns, formatting the output for readability. *)

val exists_opt : string -> column list -> column option
(** [exists_opt name cols] Searches for a column titled [name] in [cols]
    @param name The name of the column being searched for
    @param cols A list of columns
    @return
      The column titled [name] if it exists in [cols], or [None] if the column
      does not exist in [cols]*)

val select_from : t -> string list -> t
(** [select_from tab names] Selects the columns whose titles are in [names] from
    table [tab]
    @param tab The table from which the columns are being selected
    @param names
      The names of the columns that are being selected from table [tab]
    @return *)
