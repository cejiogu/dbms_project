(**Interface of SQL Database Management System*)

module StringBuilder = struct
  exception ArgumentError of string

  let parse_words input =
    let words = Str.split (Str.regexp "[ \t\n]+") input in
    List.filter (fun x -> x <> "") words

  let third_onward (input : string list) : string list =
    match input with
    | _ :: _ :: rest -> rest
    | _ -> raise (ArgumentError "You did not enter enough arguments")
end

let cycler () =
  let () = print_endline "Enter an SQL command to modify your database: " in
  let command = read_line () in
  command

let rec prompt_loop (exit : string list) (input : string)
    (database : Final_project.Table.Database.database) =
  if List.mem input exit then
    let () = print_endline "You have quit the program" in
    ()
  else
    let command = StringBuilder.parse_words input in
    match command with
    | [] ->
        let () = print_endline "You did not provide any commands" in
        prompt_loop exit (cycler ()) database
    | _ :: [] ->
        if command = [ "SCHEMA" ] then
          let () = print_endline database.name in
          prompt_loop exit (cycler ()) database
        else prompt_loop exit (cycler ()) database
    | [ _; _ ] ->
        let first_word = List.nth command 0 in
        let second_word = List.nth command 1 in
        if first_word = "CREATE" && second_word = "TABLE" then
          let columns = StringBuilder.third_onward command in
          let database =
            Final_project.Table.Database.insert_table database
              (List.nth command 2) columns
          in
          prompt_loop exit (cycler ()) database
        else print_endline "Invalid Command; Terminating Program"
    | _ -> prompt_loop exit (cycler ()) database

let initial_prompt =
  let exit = [ "exit"; "quit" ] in
  let () = print_endline "Enter the name of your database: " in
  let name = read_line () in
  if List.mem name exit then print_endline "You have quit the program"
  else
    let database = Final_project.Table.Database.empty_database name in
    let () = print_endline "Enter an SQL command to modify your database: " in
    let command = read_line () in
    prompt_loop exit command database

let () = initial_prompt
