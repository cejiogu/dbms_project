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
(** [prt_des t] generates a description of the table [t] for display purposes.
    The description includes the table's name and the names and types of each
    column.

    @param t The table whose description is returned.
    @return
      A string representing the description of the table, including its name and
      a list of its columns with their respective types. *)

val str_cols : t -> string list
(** [str_cols t] retrieves the names of all columns in the table [t].

    @param t The table from which column names are retrieved.
    @return A list containing the names of each column in [t]. *)

val str_coltyp : t -> string list
(** [str_coltyp t] retrieves a list of the types of each column in the table [t], formatted as strings. This is useful for schema inspections and data type validations.

    @param t The table whose column types are being retrieved.
    @return A list of strings, each representing the type of one of the columns in [t].
    @example [str_coltyp my_table] might return ["INT", "STRING"] if 'my_table' has an 'INT' type column and a 'STRING' type column. *)

val insert_col : t -> Column.t -> t
(** [insert_col t c] adds a new column [c] to the end of the table [t].

    @param t The table to which the column added.
    @param c The column we are adding to [t].
    @return A new table instance with the column [c] added to it. *)

val alter_table_add : t -> string -> string -> t
(** [alter_table_add t col_name typ] adds a new column to the table [t] with the specified name [col_name] and data type [typ]. It mimics the functionality of SQL's ALTER TABLE ... ADD COLUMN statement.

    @param t The table to alter.
    @param col_name The name of the column to add.
    @param typ The data type of the new column, as a string.
    @return The altered table with the new column added.
    @notes This function checks if the type [typ] is valid and fails with an InvalidQuery exception if not. *)

val get_col : t -> string -> Column.t
(** [get_col t name] retrieves a column with the title [name] from the table
    [t].

    @param t The table used to retrive the column.
    @param name The name of the column to retrieve.
    @return The column with the specified name from the table [t].
    @raise InvalidQuery
      if a column with the specified [name] does not exist in the table [t]. *)

val truncate_table_aux : t -> t
(** [truncate_table_aux table] is a helper function to remove all the data from
    the table [table]
    @param table The table to be manipulated
    @return
      A new table table, sharing the name of the original table but lacking any
      data*)

val filtered_indx : t -> int list -> t
(** [filtered_indx t indices] creates a new table from [t] containing only the
    rows whose indices are specified in [indices].

    @param t The original table from which rows are being filtered.
    @param indices
      A list of indices representing the rows to include in the new table.
    @return
      A new table containing only the rows from [t] that are specified in
      [indices]. An InvalidQuery exception is raised if any index is out of
      range. *)

val equal : t -> t -> bool
(** [equal t1 t2] checks if two tables are equal.

    @param t1 The first table to compare.
    @param t2 The second table to compare.
    @return
      [true] if both tables have the same structural and data equality, [false]
      otherwise.

      Equality is defined
      by:
      - identical table names
      - same number of columns where columns appear in the same order
      - columns must have the same name, data type, and contain the same data in
        the same order *)

val inner_join : t -> t -> string -> t
(** [inner_join t1 t2 key] performs an inner join on [t1] and [t2] based on the [key] column. 

    @param t1 The first table involved in the join.
    @param t2 The second table involved in the join.
    @param key The column name on which the join is based. This column must exist in both tables and contain matching data for a join to occur.
    @return A new table instance that contains the combined columns and rows of [t1] and [t2] where their [key] column values match.
    @raise InvalidQuery if the [key] does not exist in either table or if there are discrepancies in the data types of the key column in both tables.
    @notes The returned table's name is a concatenation of the names of [table1] and [table2] with the prefix 'Joined_'. *)
