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

let error_message () =
  let () = print_endline "Error: Invalid Command" in
  let () =
    print_endline "You did not enter a valid command. Please try again."
  in
  let () = print_endline "" in
  ()

let cycler () =
  let () = print_endline "Enter an SQL command to modify your database: " in
  let command = read_line () in
  let () = print_endline "" in
  command

let rec prompt_loop (exit : string list) (input : string)
    (database : Final_project.Database.t) =
  if List.mem input exit then
    let () = print_endline "You have quit the program" in
    ()
  else
    let command = StringBuilder.parse_words input in
    match command with
    | [] ->
        let () = print_endline "You did not provide any commands" in
        let () = print_endline "" in
        prompt_loop exit (cycler ()) database
    | word1 :: word2 :: _ ->
        if word1 = "CREATE" && word2 = "TABLE" then
          let columns = StringBuilder.third_onward command in
          let database =
            Final_project.Database.insert_table database (List.nth command 2)
              columns
          in
          prompt_loop exit (cycler ()) database
        else
          let () = error_message () in
          prompt_loop exit (cycler ()) database
    | _ ->
        let () = error_message () in
        prompt_loop exit (cycler ()) database

let initial_prompt =
  let exit = [ "exit"; "quit" ] in
  let () = print_endline "Enter the name of your database: " in
  let name = read_line () in
  let () = print_endline "" in
  if List.mem name exit then print_endline "You have quit the program"
  else
    let database = Final_project.Database.empty_database name in
    let () = print_endline "Enter an SQL command to modify your database: " in
    let command = read_line () in
    let () = print_endline "" in
    prompt_loop exit command database

let () = initial_prompt
