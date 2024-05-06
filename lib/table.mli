exception InvalidQuery of string

(* type column *)
(** The type representing a single column in a table. *)

type t
(** The type representing a table, which consists of a name [name] and a list of
    columns [columns]. *)

val title : t -> string
(** [title t] Returns the name of the table [t].
    @param t The table whose name you're retrieving. *)

val columns : t -> Column.t list
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

val rename_column : string -> string -> t -> t
(** [rename_column pre_name post_name tab] Renames a column from [pre_name] to
    [post_name] in the table [tab]. If a column with [pre_name] exists, its name
    is updated to [post_name]; otherwise, the table remains unchanged.

    @param pre_name The current name of the column to be renamed.
    @param post_name The new name for the column.
    @param tab The table containing the column to be renamed.
    @return A new table instance with the column name updated, if applicable. *)

val remove : string -> t -> t
(** [remove col_name tab] returns a new table [t] that is identical to [tab] but
    with the column named [col_name] removed. If [col_name] does not exist in
    [tab], the table is returned unchanged.

    @param col_name The name of the column to be removed.
    @param tab The table from which the column will be removed.
    @return A new table with the specified column removed. *)

val string_of_table : t -> string
(** [string_of_table t] Converts the table [t] into a string representation.
    @param t The table to convert to a string.
    @return A string that represents the table, including its name and a formatted list of columns and their data.
    The function formats the table name followed by each column's string representation, with each column separated by a newline. 
    Each column's data is presented in a list format, and each data element within a column is also separated by newlines for clarity.
    @example Calling [string_of_table my_table] where [my_table] has a name "TestTable" and two columns named "ID" and "Name" 
with respective values ["1", "2"] and ["Alice", "Bob"] returns:
    "Table: TestTable\nID: [1, 2]\nName: [Alice, Bob]" *)

val print : t -> unit
(** [print t] Prints the table [t] to the terminal, showing its structure and content.
    @param t The table to be printed.
    @return The table is represented in the terminal
    @notes The actual implementation should iterate over the table's rows and columns, formatting the output for readability. *)

val exists_opt : string -> Column.t list -> Column.t option
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

val prt_des : t -> string
(**[prt_des t] prints a description the table [t] for use after [t] has been
   added to a databas*)

val str_cols : t -> string list
(**[str_cols t] is the list containing the names of each of the columns in table
   [t]*)

val str_coltyp : t -> string list
(**[str_coltyp] is the list containing the names of each of the types of each of
   the columns in table [t] *)

val insert_col : t -> Column.t -> t
(**[insert_col t c] is the table [t] with the column [c] added to the end of the
   table*)

val alter_table_add : t -> string -> string -> t
(**[alter_table_add t col_name typ] is the table [t] with a column added which
   has title [col_name] and column type [typ]*)

val get_col : t -> string -> Column.t
(**[get_col t name] is column with title [name] in table [t]. Requires: [name]
   is the title of a column in table [t]*)

val filtered_indx : t -> int list -> t
(* val col_size: t->column->int *)
(*[get_size t col] is size of column [col] in table [t]. Requires: [col] is a
  column in table [t] *)
