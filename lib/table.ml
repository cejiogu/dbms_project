exception InvalidQuery of string

type column = Column.t

type t = {
  name : string;
  columns : column list;
}

let name t = t.name
let columns t = t.columns

(** [make_aux acc col_names] Makes a list of columns from [col_names] and adds them to [acc].
    @param acc The accumulator for the columns.
    @param col_names A list of names for each column in the table.
    @note This function is the helper function for [make tab_name col_names]. *)
let rec make_aux (acc : column list) (col_names : string list) =
  match col_names with
  | [] -> acc
  | _ :: t ->
      let col = Column.empty in
      let columns = col :: acc in
      make_aux columns t

let make (name : string) (column_names : string list) =
  let columns = make_aux [] column_names in
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

let insert_into column_names values table =
  if List.length column_names <> List.length values then
    raise (InvalidQuery "Column names and values must have the same length")
  else
    let update_column col =
      match find_index_opt (Column.title col) column_names with
      | Some index ->
          let value = List.nth values index in
          let elem = Column.elem_of_string value in
          Column.add_elem_to_column elem col
      | None -> col
    in
    let updated_columns = List.map update_column table.columns in
    { table with columns = updated_columns }

let rec print_aux (cols : column list) (acc : string list list) :
    string list list =
  match cols with
  | [] -> List.rev acc
  | (head : column) :: (tail : column list) ->
      let lst = Column.stringlist_of_column head in
      let new_acc = lst :: acc in
      print_aux tail new_acc

let print (tab : t) : string list list =
  let conversion = print_aux tab.columns [] in
  conversion
