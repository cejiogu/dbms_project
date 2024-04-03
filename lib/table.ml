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
  | h :: t ->
      let col = Column.empty h in
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

let insert_into (_ : string) (column_names : string list) (values : string list)
    (_ : t) =
  if List.length column_names <> List.length values then
    raise (InvalidQuery "Column names and values must have the same length")
  else
    failwith "TODO: Requires function that creates a column from Column module"
(* let updated_columns = List.map (fun column -> if Array.mem column.column_name
   column_names then let value_index = Array.index_of column.column_name
   column_names in let value = values.(value_index) in (* Assuming you have a
   function to add a value to a column *) Column.add_value column value else
   column ) table.columns in { table with columns = updated_columns } *)

(* let rec delineator (input : string list) (acc : string list) : string list =
   match input with | [] -> [] | head :: [] -> head :: [] | head :: tail -> let
   new_lst = head :: "|" :: acc in delineator tail new_lst *)

(**[print_aux cols acc] Makes a 2-dimensional list of strings from [cols] and adds them to [acc]
    @param cols The list of columns from a given table.
    @param acc The accumulator for the 2-dimensional list of strings.
    @note This function is the helper function for [print tab].*)
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
