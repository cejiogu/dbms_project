open Interp
open Final_project

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : Ast.expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let main () =
  let exit = [ "exit"; "quit" ] in
  let () =
    print_endline
      "USER MANUAL: After being prompted to enter the name of your database,\n\
      \ you will have the opportunity to use SQL commands to modify your \
       database.\n\
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
      \  -SELECT (<col_name1>,<col_name1>,...) FROM <table_name> WHERE \
       <column_name> = <value>\n\n\
       The following are valid column types:\n\
      \  -INT\n\
      \  -BOOL\n\
      \  -STRING\n\
      \  -DATE (Note date value format: YYYY-MM-DD)\n\
      \  -FLOAT\n\n\
      \      \n\
       Let's begin!\n\n"
  in
  let () = print_endline "Enter the name of your database: " in
  let name = read_line () in
  let database = Database.empty name in
  let () = print_endline "" in
  if List.mem name exit then print_endline "You have quit the program"
  else
    let rec loop db () =
      let () =
        print_endline "\nEnter an SQL command to modify your database: "
      in
      let command = read_line () in
      (if List.mem name exit then print_endline "You have quit the program"
       else
         let () = print_endline "" in
         try
           let parsed = parse command in
           match parsed with
           | Ast.CreateTable c ->
               let new_db =
                 Database.insert_table db (Table.title c) (Table.str_cols c)
                   (Table.str_coltyp c)
               in
               ();
               print_endline (Table.prt_des c);
               loop new_db ()
           | Ast.Schema ->
               let t = Database.tables db in
               if List.length t > 0 then (
                 print_endline "DATABASE TABLES:";
                 Database.schema t;
                 loop db ())
               else print_endline "EMPTY DATABASE\n";
               loop db ()
           | Ast.Select a ->
               Table.print
                 (Table.select_from (Database.get_table db (snd a)) (fst a));
               loop db ()
           (*Can we make a better DB insert func that just takes in a table?*)
           | Ast.AlterTable (table_name, col_name, col_type) ->
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
               else
                 Printf.printf "TABLE %s is not in DB %s\n" table_name
                   (Database.name db);
               loop db ()
           | Ast.SelectFromWhere (col_names, table_name, (col_nam, elem_value))
             ->
               if Database.table_exists table_name db then (
                 let t =
                   Database.select_from_where db col_names table_name
                     (col_nam, elem_value)
                 in
                 print_endline (Table.string_of_table t);
                 loop db ())
               else
                 Printf.printf "TABLE %s is not in DB %s\n" table_name
                   (Database.name db);
               loop db ()
           | Ast.InsertInto (table_name, col_names, rw_values) ->
               let tab = Database.get_table db table_name in
               let new_tab = Table.insert_into tab col_names rw_values in
               let new_db = Database.replace_table db new_tab in
               ();
               print_endline ("Row added to " ^ Table.title tab);
               Printf.printf "[";
               if List.length col_names > 1 then
                 match col_names with
                 | first :: rest ->
                     Printf.printf "%s" first;
                     List.iter (fun x -> Printf.printf ", %s" x) rest
                 | _ -> failwith "Unreachable"
               else Printf.printf "%s" (List.nth col_names 0);
               Printf.printf "]\n";
               Printf.printf "[";
               if List.length rw_values > 1 then
                 match rw_values with
                 | first :: rest ->
                     Printf.printf "%s" first;
                     List.iter (fun x -> Printf.printf ", %s" x) rest
                 | _ -> failwith "Unreachable"
               else Printf.printf "%s" (List.nth rw_values 0);
               Printf.printf "]\n";
               loop new_db ()
           | Ast.SelectMin (col_name, table_name) ->
               if Database.table_exists table_name db then
                 try
                   let t =
                     Database.select_max_min db table_name col_name "min"
                   in
                   print_endline t;
                   loop db ()
                 with Failure error -> Printf.printf "Error: %s" error
               else
                 Printf.printf "Error: Table %s is not in database %s\n%!"
                   table_name (Database.name db);
               loop db ()
           | Ast.SelectMax (col_name, table_name) ->
               if Database.table_exists table_name db then
                 try
                   let t =
                     Database.select_max_min db table_name col_name "max"
                   in
                   print_endline t;
                   loop db ()
                 with Failure error -> Printf.printf "Runtime Error: %s" error
               else
                 Printf.printf
                   "Runtime Error: Table %s is not in database %s\n%!"
                   table_name (Database.name db);
               loop db ()
           | Ast.Truncate table_name ->
               if Database.table_exists table_name db then
                 try
                   let new_database = Database.truncate_table db table_name in
                   ();
                   Printf.printf "All data from table %s have been removed"
                     table_name;
                   loop new_database ()
                 with Failure error -> Printf.printf "Runtime Error: %s" error
               else
                 Printf.printf "Table %s is not in database %s\n%!" table_name
                   (Database.name db);
               loop db ()
           | Ast.InnerJoin (table_name, table_name2, key) ->
               if
                 Database.table_exists table_name db
                 && Database.table_exists table_name2 db
               then
                 try
                   let t1 = Database.get_table db table_name in
                   let t2 = Database.get_table db table_name2 in
                   let new_database =
                     Database.add db (Table.inner_join t1 t2 key)
                   in
                   Printf.printf "All data from table %s have been removed"
                     table_name;
                   loop new_database ()
                 with Failure error -> Printf.printf "Runtime Error: %s" error
               else
                 Printf.printf "Table %s or %s is not in database %s\n%!"
                   table_name table_name2 (Database.name db);
               loop db ()
         with Parser.Error ->
           Printf.printf "Parse error: Please enter an appropriate command\n");
      loop db ()
    in
    loop database ()

let () = main ()
