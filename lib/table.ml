exception InvalidQuery of string

type column = Column.t

type t = {
  name : string;
  columns : column list;
}

let name t = t.name
let columns t = t.columns

(** [make_aux acc col_names col_types] Makes a list of columns from [col_names] 
        and [col_types] and adds them to [acc].
    @param acc The accumulator for the columns.
    @param col_names A list of names for each column in the table.
    @param col_types A list of types of the data for each column in the table.
          The possible types are ["Int", "Bool", "Float", "String", and "Date"]
    @note This function is the helper function for [make tab_name col_names]. *)
let rec make_aux (acc : column list) (col_names : string list)
    (col_types : string list) =
  if List.length col_names <> List.length col_types then
    raise (InvalidQuery "Column names and values must have the same length")
  else
    match col_names with
    | [] -> List.rev acc
    | h :: t ->
        let h_col_types = List.hd col_types in
        let num =
          if h_col_types = "Int" then 0
          else if h_col_types = "Bool" then 1
          else if h_col_types = "Float" then 2
          else if h_col_types = "String" then 3
          else if h_col_types = "Date" then 4
          else raise (InvalidQuery "You included a nonexistent type!")
        in
        let col = Column.empty num h in
        let columns = col :: acc in
        make_aux columns t @@ List.tl col_types

let make (name : string) (column_names : string list)
    (column_types : string list) =
  let columns = make_aux [] column_names column_types in
  let new_table = { name; columns } in
  new_table

let empty (t_name : string) : t =
  if t_name = "" then
    raise (InvalidQuery "You must enter the name of your table!")
  else
    let table = { name = t_name; columns = [] } in
    table

(** [find_index_opt value lst] Searches for [value] within the list [lst] and 
    returns the index of the first occurrence.
    @param value The value to search for within [lst].
    @param lst The list to be searched.
    @return [Some index] where [index] is the zero-based position of [value] in [lst] if found; otherwise, [None].
    @note This function performs a linear search from the beginning of [lst], returning the index of the first match. *)
let find_index_opt value lst =
  let rec aux index = function
    | [] -> None
    | x :: xs -> if x = value then Some index else aux (index + 1) xs
  in
  aux 0 lst

let insert_into table column_names values =
  if List.length column_names <> List.length values then
    raise (InvalidQuery "Column names and values must have the same length")
  else
    let update_column col =
      match find_index_opt (Column.title col) column_names with
      | Some index ->
          let value = List.nth values index in
          Column.add value col
      | None -> col
    in
    let updated_columns = List.map update_column table.columns in
    { table with columns = updated_columns }

let rename_column pre_name post_name tab : t =
  let rec helper acc = function
    | [] -> List.rev acc
    | col :: cols ->
        if Column.title col = pre_name then
          let updated_col = Column.rename col post_name in
          helper (updated_col :: acc) cols
        else helper (col :: acc) cols
  in
  { tab with columns = helper [] tab.columns }

let remove_column col_name tab : t =
  let rec helper acc = function
    | [] -> List.rev acc
    | col :: cols ->
        if Column.title col = col_name then helper acc cols
        else helper (col :: acc) cols
  in
  { tab with columns = helper [] tab.columns }

let string_of_table t =
  let table_name = "Table: " ^ t.name ^ "\n" in
  let columns_to_string cols =
    List.fold_left
      (fun acc col -> acc ^ Column.string_of_column col ^ "\n")
      "" cols
  in
  table_name ^ columns_to_string t.columns

(** [print_aux cols acc] Recursively processes a list of columns to convert each
    column into a list of strings (representing its data) and accumulates these
    lists into a larger list of string lists.
    @param cols The list of columns to process.
    @param acc The accumulator where the resulting string lists are stored.
    @return
      The accumulated list of string lists after all columns have been
      processed. *)

let rec print_aux (cols : column list) (acc : string list list) :
    string list list =
  match cols with
  | [] -> acc
  | (head : column) :: (tail : column list) ->
      let lst = Column.stringlist_of_column head in
      let new_acc = lst :: acc in
      print_aux tail new_acc

(**[NOTE:] Csv.print_readable was used prior to Csv.print, and is currently
   being replaced because it does not provide the option of delineating columns
   with characters other than the whitespace (' '), whereas Csv.print allows the
   option to delineate columns with vertical bars ('|'). *)
let print (tab : t) : unit =
  let conversion = print_aux tab.columns [] in
  let transposition = Csv.transpose conversion in
  let () = Csv.print ~separator:'|' transposition in
  let _ = print_newline in
  ()
