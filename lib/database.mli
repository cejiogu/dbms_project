exception InvalidQuery of string

type table
(** The type representing a table within a database. *)

type t
(** The type representing a database, which consists of a name and a list of tables. *)

val name : t -> string
(** [name db] Returns the name of the database [db].
      @param db The database instance whose name is to be retrieved. *)

val tables : t -> table list
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

val insert_table : t -> string -> string list -> t
(** [insert_table db name columns] Creates a new table with the specified name [name] and columns [columns] and inserts it into the database [db].
      @param db The database into which the new table is to be inserted.
      @param name The name of the new table.
      @param columns A list of names for each column in the new table.
      @return An updated database containing the new table.
      @raise Does not change the database if a table with the specified name already exists. *)
