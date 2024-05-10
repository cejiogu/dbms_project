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
(** [get_table db name] Retrieves the table titled [name] from database [db].
    @param db The database from which the table is being retrieved.
    @param name The title of the table being retrieved.
    @return The table from the database with the name [name].
    @raise InvalidQuery if a table titled [name] does not exist in database [db]. *)

val schema : Table.t list -> unit
(** [schema t] Prints the names of all the names in a tables in [t].
    @param t The table whose names are to be printed. *)

val delete : t -> Table.t -> t
(** [delete db t] is the Database [db] with table [t] removed. Requires: [t] is a
   Table in Database [db]
   @param db The Database we remove the specified table from.
   @param t The table we wish to remove from [db]. 
   @return [db] with without [t]. *)

val add : t -> Table.t -> t
(** [add db t] is the Database [db] with table [t] added. Requires: Table [t] is not alrady in Database [d].
    @param db
    @param t 
    @return  *)

val select_from_where : t -> string list -> string -> string * string -> Table.t
(** [select_from_where db col_lst table_name (col, valu)] is a table with columns
   in [col_lst] from table [table_name] containing only the rows where column
   [col] has value [valu]*)
