module type Table = sig
  exception InvalidQuery of string

  type column
  (**[column] is the type of table columns*)
  type table = {
    table_name : string;
    columns : column list;
  }
  (**[table] is an alias for the type of a table*)

  val empty_table : string -> table
  (**[empty_table] is an empty table, titled [string]. Raises [InvalidQuery] if
  [string] is empty*)

  val create_table : string -> string list -> table
  (**[create_table] is a [table], titled [string] and containing columns titled 
  after each string in [string list]. The columns created in this table are all empty, and only contain their titles.
  This function is a auxiliary function that helps to implement the function
  that would correspond to the SQL CREATE TABLE statement *)

  val insert_into : string -> string array -> string array -> table
  (**[insert_into] is a [table] containing a new row of values [string] that
     holds the string values in [string array]*)

  val print_table : table -> unit
  (**[print_table] represents the table as a string in the terminal*)
end

module Table: Table = struct
  open Column

  exception InvalidQuery of string

  type column = Column.column

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

  let rec create_table_aux (acc : column list) (data : string list) =
    match data with
    | [] -> List.rev acc
    | (head : string) :: (tail : string  list) -> let col : column = empty_column head in create_table_aux (col :: acc) tail

  let create_table (name : string) (data : string list) : table = let columns = create_table_aux [] data in 
    let result = {table_name = name; columns = columns} in result

  let insert_into (_ : string) (_ : string array) (_ : string array) =
    failwith "TODO"
    (**[TODO]: Implement INSERT INTO functionality for table
      This functionality is expected to insert a new row into the table. In SQL,
      INSERT INTO appears so: INSERT INTO table_name (column1, column2, column3,
       ...) VALUES (value1, value2, value3, ...);
      Note: The database module also has its own insert_into function, which
      will call this function.
  *)

  let print_table (_ : table) = failwith "TODO"
  (**[TODO]: Implement print table function
      Should actually serve as a to_string function for tables, and the actual
      printing to the terminal should be implemented in main*)

  (**[TODO]: Functions for future implementation; not needed for demo*)
  (* let alter_add (_ : string) (_ : string) (_ : string) = failwith "TODO" *)
  (* let alter_drop (_ : string) (_ : string) = failwith "TODO" *)
  (* let alter_rename (_ : string) (_ : string) (_ : string) = failwith "TODO" *)
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

  let rec get_table (name: string) (base: table list) = match base with | [] -> raise (InvalidQuery "Table not found!") | (head: table) :: (tail: table list) -> if head.table_name = name then head else get_table name tail

  let insert_table (base: database) (name : string) (columns : string list) : database
      = let new_table = create_table name columns in
      let new_database = { name = base.name; tables  = new_table :: base.tables} in
      new_database

  (* let select_from (name: string) (tab: table):table = failwith("TODO") *)
  (**[TODO]: Implement SELECT FROM functionality for table.

      This functionality is expected to return a table of only the columns that
      were mentioned in the string input. Note that, for our demo, we will only 
      implement the functionality for a simple select statement, and for that
      reason this function only accepts a single string input for the the title 
      of one (1) column, rather than a string list to return a table bearing 
      multiple columns. In short, this function for now should only return the 
      column from the table titled [string]. In SQL, SELECT FROM appears so: 
      SELECT column1 FROM table1

      Note: The column module should have a function to print out a column, so
      to implement this function, as we need is to search for the column titled
      [string] in [tab], and then call the Column function to print the column
  *)
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
  (**[add_table] is a [database] that now includes a new table titled [string]
     and with columns titled after each string in [string list] into [database].
     Does not change [database] if a table titled [string] already exists in
     [database]. This function corresponds to the SQL CREATE TABLE statement*)

  val select_from : string -> table -> table
  (**[select_from] is a [table] representing the column, titled [string],
     selected from [table]. Raises [InvalidQuery] if the [string] title of the
     column is not found in [table]. This function corresponds to the SQL SELECT
     statement.*)

  val insert_into : table -> string array -> string array -> table
  (**[insert_into] is a [table] that inserts into the columns whose names are
     speficied in the first [string array] argument a new row of data, which is
     specified in the second [string array] argument. This function corresponds
     to the SQL INSERT INTO statement*)
end
