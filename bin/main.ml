(** @author Ori Wachman (ow34), Chimdi Ejiogu (ce248), Ilan Klimberg (idk7)*)

open Interp
open Final_project

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let print_instructions () =
  print_endline
    "USER MANUAL: After being prompted to enter the name of your database,\n\
    \ you will have the opportunity to use SQL commands to modify your database.\n\
    \ The following are valid commands:\n\
    \  -CREATE TABLE <table_name> (<column_name> <column_type>,<column_name> \
     <column_type>,..)\n\
    \  -SCHEMA\n\
    \  -SELECT <column_name1>,<column_name2>,... FROM <table_name>\n\
    \  -ALTER TABLE <table_name> ADD <column_name> <column_type>\n\
    \  -INSERT INTO <table_name> (<col_name1>,<col_name1>,...) VALUES \
     (<value1>, <value2>,..)\n\
    \  -SELECT MIN(<col_name>) FROM <table_name>\n\
    \  -SELECT MAX(<col_name>) FROM <table_name>\n\
    \  -TRUNCATE TABLE <table_name>\n\
    \  -INNER JOIN <table_name1> <table_name2> ON <col_name>\n\
    \  -SELECT (<col_name1>,<col_name2>,...) FROM <table_name> WHERE \
     <column_name> = <value>\n\n\
     The following are valid column types:\n\
    \  -INT\n\
    \  -BOOL\n\
    \  -STRING\n\
    \  -DATE (Note date value format: YYYY-MM-DD)\n\
    \  -FLOAT\n\n\
    \ Note: The database, as well as any columns and tables, must be titled in \
     strictly alphabetical characters, and even may not white spaces. \n\n\n\
     Let's begin!\n\n"

let print_intro () : Database.t =
  let () = print_endline "Enter the name of your database: " in
  let name = read_line () in
  print_endline "";
  Database.empty name

let create_table_output (db : Database.t) (c : Table.t) loop : unit =
  let new_db =
    Database.insert_table db (Table.title c) (Table.str_cols c)
      (Table.str_coltyp c)
  in
  ();
  print_endline (Table.prt_des c);
  loop new_db ()

let schema_output (db : Database.t) loop : unit =
  let t = Database.tables db in
  if List.length t > 0 then (
    print_endline "DATABASE TABLES:";
    Database.schema t;
    loop db ())
  else print_endline "EMPTY DATABASE\n";
  loop db ()

let select_from_where_output (db : Database.t) loop (col_names : string list)
    (table_name : string) (col_nam : string) (elem_value : string) : unit =
  if Database.table_exists table_name db then (
    let t =
      Database.select_from_where db col_names table_name (col_nam, elem_value)
    in
    Table.print t;
    loop db ())
  else Printf.printf "TABLE %s is not in DB %s\n" table_name (Database.name db);
  loop db ()

let insert_into_output_aux (input : string list) : unit =
  Printf.printf "[";
  if List.length input > 1 then
    match input with
    | first :: rest ->
        Printf.printf "%s" first;
        List.iter (fun x -> Printf.printf ", %s" x) rest
    | _ -> failwith "Unreachable"
  else Printf.printf "%s" (List.nth input 0);
  Printf.printf "]\n"

let insert_into_output (db : Database.t) loop (table_name : string)
    (col_names : string list) (rw_values : string list) : unit =
  let tab = Database.get_table db table_name in
  let new_tab = Table.insert_into tab col_names rw_values in
  let new_db = Database.replace_table db new_tab in
  ();
  print_endline ("Row added to " ^ Table.title tab);
  insert_into_output_aux col_names;
  insert_into_output_aux rw_values;
  loop new_db ()

let select_min_output (db : Database.t) loop (col_name : string)
    (table_name : string) =
  if Database.table_exists table_name db then
    try
      let t = Database.select_max_min db table_name col_name "min" in
      print_endline t;
      loop db ()
    with Failure error -> Printf.printf "Error: %s" error
  else
    Printf.printf "Error: Table %s is not in database %s\n%!" table_name
      (Database.name db);
  loop db ()

let select_max_output (db : Database.t) loop (col_name : string)
    (table_name : string) =
  if Database.table_exists table_name db then
    try
      let t = Database.select_max_min db table_name col_name "max" in
      print_endline t;
      loop db ()
    with Failure error -> Printf.printf "Runtime Error: %s" error
  else
    Printf.printf "Runtime Error: Table %s is not in database %s\n%!" table_name
      (Database.name db);
  loop db ()

let alter_table_output (db : Database.t) loop (table_name : string)
    (col_name : string) (col_type : string) =
  if Database.table_exists table_name db then (
    let updated_table =
      Table.alter_table_add
        (Database.get_table db table_name)
        col_name
        (Column.elemtype_of_stringparse col_type)
    in
    let new_db = Database.replace_table db updated_table in
    ();
    print_endline (Table.prt_des updated_table);
    loop new_db ())
  else Printf.printf "TABLE %s is not in DB %s\n" table_name (Database.name db);
  loop db ()

let select_output (db : Database.t) loop (a : string list * string) : unit =
  Table.print (Table.select_from (Database.get_table db (snd a)) (fst a));
  loop db ()

let truncate_output (db : Database.t) loop (table_name : string) =
  if Database.table_exists table_name db then
    try
      let new_database = Database.truncate_table db table_name in
      ();
      Printf.printf "All data from table %s have been removed!\n" table_name;
      loop new_database ()
    with Failure error -> Printf.printf "Runtime Error: %s" error
  else
    Printf.printf "Table %s is not in database %s\n%!" table_name
      (Database.name db);
  loop db ()

let inner_join_output (db : Database.t) loop (table_name : string)
    (table_name2 : string) (key : string) =
  if Database.table_exists table_name db && Database.table_exists table_name2 db
  then
    try
      let t1 = Database.get_table db table_name in
      let t2 = Database.get_table db table_name2 in
      let new_table = Table.inner_join t1 t2 key in
      let new_database = Database.add db new_table in
      Printf.printf "%s and %s have been joined, and are now in %s\n" table_name
        table_name2 (Table.title new_table);
      loop new_database ()
    with Failure error -> Printf.printf "Runtime Error: %s" error
  else
    Printf.printf "Table %s or %s is not in database %s\n%!" table_name
      table_name2 (Database.name db);
  loop db ()

let exception_handler (message : string) loop (db : Database.t) =
  Printf.printf "%s" message;
  loop db ()

let rec loop db () =
  let () = print_endline "\nEnter an SQL command to modify your database: " in
  let command = read_line () in
  let () = print_endline "" in
  try
    let parsed = parse command in
    match parsed with
    | Ast.CreateTable c -> create_table_output db c loop
    | Ast.Schema -> schema_output db loop
    | Ast.Select a -> select_output db loop a
    | Ast.AlterTable (table_name, col_name, col_type) ->
        alter_table_output db loop table_name col_name col_type
    | Ast.SelectFromWhere (col_names, table_name, (col_nam, elem_value)) ->
        select_from_where_output db loop col_names table_name col_nam elem_value
    | Ast.InsertInto (table_name, col_names, rw_values) ->
        insert_into_output db loop table_name col_names rw_values
    | Ast.SelectMin (col_name, table_name) ->
        select_min_output db loop col_name table_name
    | Ast.SelectMax (col_name, table_name) ->
        select_max_output db loop col_name table_name
    | Ast.Truncate table_name -> truncate_output db loop table_name
    | Ast.InnerJoin (table_name, table_name2, key) ->
        inner_join_output db loop table_name table_name2 key
  with
  | Column.InvalidQuery error ->
      exception_handler ("InvalidQuery:" ^ error) loop db
  | Column.EmptyColumn error ->
      exception_handler ("EmptyColumn:" ^ error) loop db
  | Table.InvalidQuery error ->
      exception_handler ("InvalidQuery:" ^ error) loop db
  | Database.InvalidQuery error ->
      exception_handler ("InvalidQuery:" ^ error) loop db
  | Failure error -> exception_handler ("Failure:" ^ error) loop db
  | Parser.Error ->
      exception_handler "Parse error: Please enter an appropriate command\n"
        loop db

let main () =
  print_instructions ();
  let exit = [ "exit"; "quit" ] in
  let database = print_intro () in
  if List.mem (Database.name database) exit then
    print_endline "You have quit the program"
  else loop database ()

let () = main ()
