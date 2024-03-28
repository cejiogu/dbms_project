(* open Final_project.Column open Final_project.Table *)

(**Interface of SQL Database Management System*)

module StringBuilder = struct
  (* open Str *)

  let parse_words input =
    let words = Str.split (Str.regexp "[ \t\n]+") input in
    List.filter (fun x -> x <> "") words
end

let prompt_loop (keywords : string list) (input : string) =
  if List.mem input keywords then
    let () = print_endline "You have quit the program" in
    ()
  else
    let command = StringBuilder.parse_words input in
    let first_word = List.nth command 0 in
    let second_word = List.nth command 1 in
    if first_word = "CREATE" && second_word = "TABLE" then
      let new_table =
        Final_project.Table.Database.empty_database (List.nth command 2)
      in
      print_endline new_table.name

let prompt =
  let exit = [ "exit"; "quit" ] in
  let () = print_endline "Enter a SQL command: " in
  let command = read_line () in
  prompt_loop exit command

let () = prompt
