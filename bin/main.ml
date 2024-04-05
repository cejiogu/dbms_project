(**Interface of SQL Database Management System*)

module StringBuilder = struct
  exception ArgumentError of string

  let parse_words input =
    let words = Str.split (Str.regexp "[ \t\n]+") input in
    List.filter (fun x -> x <> "") words

  let rec demarker_aux (input : string list) (finish : string)
      (acc : string list) : string list =
    match input with
    | [] -> List.rev acc
    | (h : string) :: (t : string list) ->
        if h = finish then acc
        else
          let new_acc = h :: acc in
          demarker_aux t finish new_acc

  (** [demarker input start finish] Selects a string list of all strings in [input] that occur between [start] and [finish], exclusive
      @param input The string list from which the specified string list is to be selected 
      @param start The string, exclusive, at which the function begin selecting strings
      @param finish The string, exclusive, at which the function stops selecting strings
      @return A string list of all the words in between [start] and [finish], exclusive
      @raises ArgumentError if [start] is equal to ""
      @note If [finish] is equal to "", then the function selects all strings from [start] to the end of [input]*)

  let rec demarker (input : string list) (start : string) (finish : string) :
      string list option =
    if start = "" then
      raise
        (ArgumentError
           "Invalid Input: 'start' argument cannot be an empty string")
    else
      match input with
      | [] -> None
      | h :: t ->
          if h = start then Some (demarker_aux t finish [])
          else demarker t start finish
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
  let open Final_project in
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
          let table_title = List.nth command 2 in
          let columns = StringBuilder.demarker command "COLUMNS" "DATA TYPES" in
          let col_values = StringBuilder.demarker command "VALUES" "" in
          match (columns, col_values) with
          | Some cols, Some vals -> begin
              let database =
                Database.insert_table database table_title cols vals
              in
              prompt_loop exit (cycler ()) database
            end
          | _ ->
              let () =
                print_endline "You did not enter the titles of your columns"
              in
              let () = print_endline "" in
              prompt_loop exit (cycler ()) database
        else
          let () = error_message () in
          prompt_loop exit (cycler ()) database
    | _ ->
        let () = error_message () in
        prompt_loop exit (cycler ()) database

let initial_prompt =
  let open Final_project in
  let exit = [ "exit"; "quit" ] in
  let () = print_endline "Enter the name of your database: " in
  let name = read_line () in
  let () = print_endline "" in
  if List.mem name exit then print_endline "You have quit the program"
  else
    let database = Database.empty name in
    let () = print_endline "Enter an SQL command to modify your database: " in
    let command = read_line () in
    let () = print_endline "" in
    prompt_loop exit command database

let () = initial_prompt
