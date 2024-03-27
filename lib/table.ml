module Table = struct
  exception InvalidQuery of string

  type column = Column.t

  type table = {
    table_name : string;
    columns : column list;
  }

  let rec create_table_aux (acc: column list) (column_names: string list) = match column_names with | [] -> acc | (h: string) :: (tail: string list) -> let col = Column.empty_column h in let columns = col :: acc in create_table_aux columns tail 

  let create_table (table_name: string) (column_names: string list) = let columns = (create_table_aux [] column_names) in let new_table = {table_name = table_name; columns = columns} in new_table
  let empty_table (name : string) : table =
    if name = "" then
      raise (InvalidQuery "You must enter the name of your table!")
    else
      let table = { table_name = name; columns = [] } in
      table

  let insert_into (_: string) (column_names : string list) (values : string list) (_ : table) =
    if List.length column_names <> List.length values then
      raise (InvalidQuery "Column names and values must have the same length")
    else
      failwith "TODO: Requires function that creates a column from Column module"
      (* let updated_columns = 
        List.map 
          (fun column ->
            if Array.mem column.column_name column_names then
              let value_index = Array.index_of column.column_name column_names in
              let value = values.(value_index) in
              (* Assuming you have a function to add a value to a column *)
              Column.add_value column value
            else
              column
          ) 
          table.columns
      in
      { table with columns = updated_columns } *)
  let print_table (_ : table) = failwith "TODO"
end

module type Table = sig
  exception InvalidQuery of string

  type column
  (**[column] is the type of table columns*)
  type table
  (**[table] is an alias for the type of a table*)

  val empty_table : string -> table
  (**[empty_table] is an empty table, titled [string]. Raises [InvalidQuery] if
  [string] is empty*)

  val create_table : string -> string list -> table
  (**[create_table] is a [table], titled [string] and containing columns titled 
  after each string in [string list]. This function is an auxiliary function that
  helps to implement the function that would correspond to the SQL CREATE TABLE
  statement *)

  val insert_into : string -> string array -> string array -> table
  (**[insert_into] is a [table] containing a new row of values [string] that
     holds the string values in [string array]*)

  val print_table : table -> unit
  (**[print_table] represents the table as a string in the terminal*)
end

module Database = struct
  open Table

  exception InvalidQuery of string

  type table = Table.table

  type database = {
    name : string;
    tables : table list;
  }

  let empty_database (name : string) : database =
    if name = "" then
      raise (InvalidQuery "You must enter the name of your table!")
    else
      let database = { name = name; tables = [] } in
      database

  let table_exists (name : string) (db : database) : bool =
      List.exists (fun t -> t.table_name = name) db.tables

  let insert_table (db : database) (name : string) (columns : string list) : database
      =
      if table_exists name db then
        db
      else
        let new_table = Table.create_table name columns in
        { db with tables = new_table :: db.tables }
end

module type Database = sig
  exception InvalidQuery of string

  type table
  (**[table] is the type of database tables*)
  type database
  (**[database] is an alias for the type of a database*)

  val empty_database : string -> database
  (**[empty_database] is an empty database, titled [string]. Raises
     [InvalidQuery] if [string] is empty*)

  val insert_table : database -> string -> (string * string) list -> database
  (**[insert_table] is a [database] that now includes a new table titled [string]
     and with columns titled after each string in [string list] into [database].
     Does not change [database] if a table titled [string] already exists in
     [database]. This function corresponds to the SQL CREATE TABLE statement*)

  val select_from : string -> table -> table
  (**[select_from] is a [table] representing the column, titled [string],
     selected from [table]. Raises [InvalidQuery] if the [string] title of the
     column is not found in [table]. This function corresponds to the SQL SELECT
     statement.*)

  val insert_into : table -> string list -> string list -> table
  (**[insert_into] is a [table] that inserts into the columns whose names are
     speficied in the first [string list] argument a new row of data, which is
     specified in the second [string list] argument. This function corresponds
     to the SQL INSERT INTO statement*)
end
