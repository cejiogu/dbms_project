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
  let d = ref database in
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
               d :=
                 Database.insert_table db (Table.title c) (Table.str_cols c)
                   (Table.str_coltyp c);
               print_endline (Table.prt_des c)
           | Ast.Schema ->
               let t = Database.tables db in
               if List.length t > 0 then (
                 print_endline "DATABASE TABLES:";
                 Database.schema t)
               else print_endline "EMPTY DATABASE\n"
           | Ast.Select a ->
               Table.print
                 (Table.select_from (Database.get_table db (snd a)) (fst a))
           (*Can we make a better DB insert func that just takes in a table?*)
           | Ast.AlterTable (table_name, col_name, col_type) ->
               if Database.table_exists table_name db then (
                 let t =
                   Table.alter_table_add
                     (Database.get_table db table_name)
                     col_name
                     (Column.elemtype_of_stringparse col_type)
                 in
                 d := Database.delete db (Database.get_table db table_name);
                 d :=
                   Database.insert_table db (Table.title t) (Table.str_cols t)
                     (Table.str_coltyp t);
                 print_endline (Table.prt_des t))
               else
                 Printf.printf "TABLE %s is not in DB %s\n" table_name
                   (Database.name db)
           | Ast.SelectFromWhere (col_names, table_name, (col_nam, elem_value))
             ->
               if Database.table_exists table_name db then
                 let t =
                   Database.select_from_where db col_names table_name
                     (col_nam, elem_value)
                 in
                 print_endline (Table.string_of_table t)
               else
                 Printf.printf "TABLE %s is not in DB %s\n" table_name
                   (Database.name db)
           | Ast.InsertInto (table_name, col_names, rw_values) ->
               let tab = Database.get_table db table_name in
               d := Database.delete db (Database.get_table db table_name);
               d := Database.add db (Table.insert_into tab col_names rw_values);
               print_endline ("Row added to " ^ Table.title tab);
               Printf.printf "[";
               List.iter (fun x -> Printf.printf "%s, " x) col_names;
               Printf.printf "]\n";
               Printf.printf "[";
               List.iter (fun x -> Printf.printf "%s, " x) rw_values;
               Printf.printf "]\n"
           (* | _->Printf.printf "Not a command\n" *)
         with Parser.Error -> Printf.printf "Parse error");
      (* | Failure msg -> Printf.printf "Error:%s\n" msg); *)
      loop !d ()
    in
    loop !d ()

let () = main ()

(* *Interface of SQL Database Management System

   module StringBuilder = struct exception ArgumentError of string

   let parse_words input = let words = Str.split (Str.regexp "[ \t\n]+") input
   in List.filter (fun x -> x <> "") words

   (** [demarker_aux] @param input @param finish @param acc @return *)

   let rec demarker_aux (input : string list) (finish : string) (acc : string
   list) : string list = match input with | [] -> List.rev acc | (h : string) ::
   (t : string list) -> if h = finish then acc else let new_acc = h :: acc in
   demarker_aux t finish new_acc

   (** [demarker input start finish] Selects a string list of all strings in
   [input] that occur between [start] and [finish], exclusive @param input The
   string list from which the specified string list is to be selected @param
   start The string, exclusive, at which the function begin selecting strings
   @param finish The string, exclusive, at which the function stops selecting
   strings @return A string list of all the words in between [start] and
   [finish], exclusive @raises ArgumentError if [start] is equal to "", if
   [start] does not exist in [input], or if [finish] does not exist in [input]
   and [finish] is not equal to "" @note If [finish] is equal to "", then the
   function selects all strings from [start] to the end of [input]*)

   let rec demarker (input : string list) (start : string) (finish : string) :
   string list option = if start = "" then raise (ArgumentError "Invalid Input:
   'start' argument cannot be an empty string") else if List.mem start input =
   false || ((not (List.mem finish input)) && finish <> "") then raise
   (ArgumentError "Invalid Input: One of the specified strings does not exist in
   the \ string list") else match input with | [] -> None | h :: t -> if h =
   start then Some (demarker_aux t finish []) else demarker t start finish end

   let cycler () = let () = print_endline "Enter an SQL command to modify your
   database: " in let command = read_line () in let () = print_endline "" in
   command

   let rec prompt_loop (exit : string list) (input : string) (database :
   Final_project.Database.t) = let open Final_project in if List.mem input exit
   then let () = print_endline "You have quit the program" in () else let
   command = StringBuilder.parse_words input in match command with | [] -> let
   () = print_endline "You did not provide any commands" in let () =
   print_endline "" in prompt_loop exit (cycler ()) database | word :: [] ->
   begin if word = "SCHEMA" then let tables = List.rev (Database.tables
   database) in let () = Database.schema tables in let () = print_endline "" in
   prompt_loop exit (cycler ()) database end | word1 :: word2 :: _ -> begin if
   word1 = "CREATE" && word2 = "TABLE" then let table_title = List.nth command 2
   in let columns = StringBuilder.demarker command "COLUMNS" "DATATYPES" in let
   col_values = StringBuilder.demarker command "DATATYPES" "" in match (columns,
   col_values) with | Some cols, Some vals -> begin let tab = Table.make
   table_title cols vals in let database = Database.insert_table database
   table_title cols vals in let () = Table.print tab in prompt_loop exit (cycler
   ()) database end | _ -> begin let () = print_endline "You did not enter the
   titles of the columns" in let () = print_endline "" in prompt_loop exit
   (cycler ()) database end else if word1 = "SELECT" then let columns =
   StringBuilder.demarker command "SELECT" "FROM" in let table_title = List.nth
   command (List.length command - 1) in match columns with | Some cols -> begin
   let tab = Database.get_table database table_title in let selected =
   Table.select_from tab cols in let () = Table.print selected in prompt_loop
   exit (cycler ()) database end | None -> begin let () = print_endline "You did
   not enter the titles of the columns" in let () = print_endline "" in
   prompt_loop exit (cycler ()) database end end

   let initial_prompt = let open Final_project in let exit = [ "exit"; "quit" ]
   in let () = print_endline "Enter the name of your database: " in let name =
   read_line () in let () = print_endline "" in if List.mem name exit then
   print_endline "You have quit the program" else let database = Database.empty
   name in let () = print_endline "Enter an SQL command to modify your database:
   " in let command = read_line () in let () = print_endline "" in prompt_loop
   exit command database

   let () = initial_prompt *)
