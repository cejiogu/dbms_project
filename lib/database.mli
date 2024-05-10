exception InvalidQuery of string

(* type table = Table.t *)
(** The type representing a table within a database. *)

type t
(** The type representing a database, which consists of a name and a list of
    tables. *)

val name : t -> string
(** [name db] Returns the name of the database [db].
    @param db The database instance whose name is to be retrieved. *)

val tables : t -> Table.t list
(** [tables db] Returns the list of tables in the database [db].
    @param db The database instance whose tables are to be retrieved. *)

val empty : string -> t
(** [empty_database name] Creates an empty database with the specified name [name].
      @param name The name of the database to be created.
      @raise InvalidQuery if the provided [name] is an empty string.
      @return A new database instance with the specified name and no tables.
      @notes This function is useful for initializing a new database before tables are added. *)

val table_exists : string -> t -> bool
(** [table_exists name db] Checks if a table with the specified name [name] exists in the database [db].
      @param name The name of the table to check for existence.
      @param db The database in which to look for the table.
      @return [true] if a table with the specified name exists in the database, [false] otherwise.
      @notes This function can be used to prevent duplicate tables in a database. *)

val insert_table : t -> string -> string list -> string list -> t
(** [insert_table db name column_names column_types] Creates a new table with
    the specified name [name] and columns [column_names] whos types are of
    [column_types] and inserts it into the database [db].
    @param db The database into which the new table is to be inserted.
    @param name The name of the new table.
    @param column_names A list of names for each column in the new table.
    @param column_types A list of types for each column in the new table.
    @return An updated database containing the new table.
    @raise Does
      not change the database if a table with the specified name already exists. *)

val get_table : t -> string -> Table.t
(** [get_table db name] Retrieves the table titled [name] from database [db]
    @param db The database from which the table is being retrieved
    @param name The title of the table being retrieved
    @return The table from the database with the name [name]
    @raise InvalidQuery if a table titled [name] does not exist in database [db]*)

val schema : Table.t list -> unit
(** [schema tables] Prints the names of all the names in a tables in [tables]
    @param tables The tables whose names are to be printed *)

val delete : t -> Table.t -> t
(**[delete db t] is the Database [db] with table [t] removed. Requires: [t] is a
   Table in Database [db]*)

val add : t -> Table.t -> t
(**[add d tabl] is the Database [d] with table [tabl] added. Requires: Table
   [tabl] is not alrady in Database [d]*)

val select_from_where : t -> string list -> string -> string * string -> Table.t
(**[select_from_where db col_lst table_name (col, valu)] is a table with columns
   in [col_lst] from table [table_name] containing only the rows where column
   [col] has value [valu]*)

val truncate_table : t -> string -> t
(** [truncate_table db table] is a database [db] that includes a table [tab] in
    which all the data in that table is removed
    @param db The name of the database in which table [table] resides
    @param table The name of the table from whom all the data is being removed
    @return An updated database, except with the updated table*)

val select_max_min : t -> string -> string -> string -> string
(** [select_max_min db tab col specifier] Searches through the column [col] of
    hte table [tab] of the database [db] to find the value that best fulfills
    the specifier [specifier]. For example, if "max" is passed as the specifier
    on a given column, table, and database, the function will return the
    greatest value from the aforementioned column. If "min" is passed as the
    specifier on a given column, table, and database, the function will return
    the least value from the aforementioned column.
    @param db The database in which the table being searched resides
    @param tab The table from which the column being searched resides
    @param col
      The column from which the maximum or minimum value being searched for
      resides
    @param specifier The keyword that specifies the value to be searched for
    @return The value, represented as a string, that best fulfills the specifier
    @raise InvalidQuery
      if the datatype of the column is NULL, Date, or Bool, or if a string other
      than "max" or "min" is passed in as the specifier *)
