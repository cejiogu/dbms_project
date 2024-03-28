exception InvalidQuery of string

type table
(**[table] is the type of database tables. *)

type t = {
  name : string;
  tables : table list;
}
(**[t] is an alias for the type of a database. *)

val name : t -> string
(**[name] is the name [string] of a database [t]*)

val tables : t -> table list
(**[tables] is the tables in a database [t]*)

val empty_database : string -> t
(**[empty_database] is an empty database, titled [string]. Raises [InvalidQuery]
   if [string] is empty. *)

val table_exists : string -> t -> bool
(**[table_exists] is whether or not a table of title [string] exists in [t]*)

val insert_table : t -> string -> string list -> t
(**[insert_table] is a [database] that now includes a new table titled [string]
   and with columns titled after each string in [string list] into [database].
   Does not change [database] if a table titled [string] already exists in
   [database]. This function corresponds to the SQL CREATE TABLE statement. *)

(* future implementation of insert_table below. Current implementation of
   insert_table above. *)
(* val insert_table : t -> string -> (string * string) list -> t *)

(* val select_from : string -> table -> table *)
(**[select_from] is a [table] representing the column, titled [string], selected
   from [table]. Raises [InvalidQuery] if the [string] title of the column is
   not found in [table]. This function corresponds to the SQL SELECT statement. *)

(* val insert_into : table -> string list -> string list -> table *)
(**[insert_into] is a [table] that inserts into the columns whose names are
   speficied in the first [string list] argument a new row of data, which is
   specified in the second [string list] argument. This function corresponds to
   the SQL INSERT INTO statement. *)