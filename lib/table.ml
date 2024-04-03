exception InvalidQuery of string

type column = Column.t

type t = {
  name : string;
  columns : column list;
}

let name t = t.name
let columns t = t.columns

let rec create_table_aux (acc : column list) (column_names : string list) =
  match column_names with
  | [] -> acc
  | _ :: t ->
      let col = Column.empty in
      let columns = col :: acc in
      create_table_aux columns t

let make (name : string) (column_names : string list) =
  let columns = create_table_aux [] column_names in
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

let print (_ : t) = failwith "TODO"
