(**Implementation of Table module*)
module Table = struct
  open Column

  exception InvalidQuery of string

  type column = Column.t

  type table = {
    table_name : string;
    columns : column list;
  }

  let empty_table (name : string) : table =
    if name = "" then
      raise (InvalidQuery "You must enter the name of your table!")
    else
      let table = { table_name = name; columns = [] } in
      table

  let rec insert_into (_ : string) (_ : string array) (_ : string array) =
    failwith "TODO"

  (* let alter_add (_ : string) (_ : string) (_ : string) = failwith "TODO" let
     alter_drop (_ : string) (_ : string) = failwith "TODO" let alter_rename (_
     : string) (_ : string) (_ : string) = failwith "TODO" *)
  let print_table (_ : table) = failwith "TODO"
end

module type Table = sig
  exception InvalidQuery of string

  type column
  type table

  val empty_table : string -> table
  (**[empty_table] is an empty table of title [string]*)

  val create_table : string -> string list -> table
  (**[create_table] is a [table] titled [string] and with columns titled after
     each string in [string list]*)

  val insert_into : string -> string array -> string array -> table
  (**[insert_into] is a [table] containing a new row of values [string] that
     holds the string values in [string array] *)

  val print_table : table -> unit
  (**[print_table] represents the table as a string in the terminal*)
end

module Database = struct
  open Table
  open Column

  exception InvalidQuery of string

  type table = Table.table

  type database = {
    db_name : string;
    tables : table list;
  }

  let empty_database (name : string) : database =
    if name = "" then
      raise (InvalidQuery "You must enter the name of your table!")
    else
      let database = { db_name = name; tables = [] } in
      database

  let insert_table (name : string) (columns : (string * string) list) : database
      =
    failwith "TODO"
end

module type Database = sig
  exception InvalidQuery of string

  type table
  type database

  val empty_database : string -> database
  (**[empty_database] is an empty database of title [string] *)

  val insert_table : string -> (string * string) list -> database
  (**[add_table] is a [database] that now includes a new table titled [string]
     and with columns titled after each string in [string list] into [database].
     If a table titled [string] already existed in the database, raises
     [InvalidQuery]*)

  val select_from : string -> string -> table
  (**[select_from] is a [table] representing the column of title [string],
     selected from the table of title [string]*)
end
