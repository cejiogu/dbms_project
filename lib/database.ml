exception InvalidQuery of string

(* type table = Table.t *)

type t = {
  name : string;
  tables : Table.t list;
}

let name t = t.name
let tables t = t.tables
let add d tabl = { name = name d; tables = tabl :: tables d }

let empty (name : string) : t =
  if name = "" then
    raise (InvalidQuery "You must enter the name of your table!")
  else
    let database = { name; tables = [] } in
    database

let table_exists (name : string) (db : t) : bool =
  List.exists (fun t -> Table.title t = name) db.tables

let insert_table (db : t) (name : string) (column_names : string list)
    (column_types : string list) : t =
  if table_exists name db then db
  else
    let new_table = Table.make name column_names column_types in
    { db with tables = new_table :: db.tables }

(** [get_table_aux tables name] Retrieves a table of title [name] from [tables]
    @param tables
      The list of tables from which the specified table is to be retrieved
    @param name The title of the table being searched for
    @return The table titled [name]*)

let rec get_table_aux (tables : Table.t list) (name : string) : Table.t =
  match tables with
  | [] -> failwith "Should never come to this case"
  | (h : Table.t) :: (t : Table.t list) ->
      if Table.title h = name then h else get_table_aux t name

let get_table (db : t) (name : string) : Table.t =
  if table_exists name db then get_table_aux db.tables name
  else raise (InvalidQuery ("The table titled " ^ name ^ " does not exist "))

let rec schema (tables : Table.t list) : unit =
  match tables with
  | [] -> ()
  | h :: t ->
      print_endline (Table.title h);
      schema t

let rec tab_names acc l cnt =
  if cnt <= List.length l - 1 then
    Table.title (List.nth l cnt) :: tab_names acc l (cnt + 1)
  else acc

let delete db t =
  let tab_lst = tables db in
  if List.mem (Table.title t) (tab_names [] tab_lst 0) then (
    let acc = ref [] in
    for x = 0 to List.length tab_lst - 1 do
      if Table.title (List.nth tab_lst x) <> Table.title t then
        acc := List.nth (tables db) x :: !acc
      else ()
    done;
    { name = name db; tables = !acc })
  else
    raise
      (Failure ("TABLE " ^ Table.title t ^ " is NOT in Database " ^ name db))

(*^CATCH FAILURE in main*)

let select_from_where db col_list table_name (col, value) =
  let org_tab = get_table db table_name in
  let cols = Table.select_from org_tab (col :: col_list) in
  let (c : Column.t) = (Table.get_col cols col : Column.t) in
  (*how can we avoid using Column.elem here? Do we need to?*)
  let i = Column.filter_indicies c (Column.elem_of_string value) in
  let tab = Table.filtered_indx (Table.remove col cols) i in
  tab

let rec replace_table_aux (db : Table.t list) (tab : Table.t) : Table.t list =
  match db with
  | [] -> db
  | (head : Table.t) :: (tail : Table.t list) ->
      if Table.title head = Table.title tab then
        tab :: replace_table_aux tail tab
      else head :: replace_table_aux tail tab

(* let delete_from_where (db : t) (tab : string) (col : string) (value : string)
   : t = try begin let table = get_table db tab in (* Raises "InvalidQuery"
   error if table is not in database*) let new_table =
   Table.delete_from_where_aux table col value in let new_tables = replace_table
   db.tables new_table in { name = db.name; tables = new_tables } end with
   InvalidQuery error -> raise (InvalidQuery error) *)

let replace_table (db : t) (tab : Table.t) : t =
  if table_exists (Table.title tab) db then
    match db with
    | { name; tables } ->
        let new_tables = replace_table_aux tables tab in
        let new_db = { name; tables = new_tables } in
        new_db
  else
    raise
      (InvalidQuery
         ("Table " ^ Table.title tab ^ " does not exist in database " ^ name db))

let truncate_table (db : t) (table : string) : t =
  let truncated_tab = get_table db table |> Table.truncate_table_aux in
  let new_tables = replace_table_aux db.tables truncated_tab in
  let new_db = { name = name db; tables = new_tables } in
  new_db

let select_max_min (db : t) (tab : string) (col : string) (specifier : string) :
    string =
  let table = get_table db tab in
  let column = Table.get_col table col in
  let value = Column.select_aux column specifier in
  value

let inner_join (table1 : t) (table2 : t) (key : string) : t =
  (* Retrieve a column safely, returning an option *)
  let safe_get_col table key =
    List.find_opt (fun col -> Column.title col = key) (columns table)
  in

  match (safe_get_col table1 key, safe_get_col table2 key) with
  | Some col1, Some col2 ->
      let data1 = Column.data col1 in
      let data2 = Column.data col2 in
      let common_data = List.filter (fun x -> List.mem x data2) data1 in
      let index_list1 =
        List.concat (List.map (Column.filter_indicies col1) common_data)
      in
      let index_list2 =
        List.concat (List.map (Column.filter_indicies col2) common_data)
      in

      let filtered_table1 = filtered_indx table1 index_list1 in
      let filtered_table2 = filtered_indx table2 index_list2 in

      (* Select only non-key columns from table2 to avoid duplication *)
      let non_key_columns_table2 =
        List.filter (fun col -> Column.title col <> key) filtered_table2.columns
      in
      (* Join columns making sure key column from table2 isn't included *)
      let new_columns = filtered_table1.columns @ non_key_columns_table2 in

      {
        name = "Joined_" ^ table1.name ^ "_" ^ table2.name;
        columns = new_columns;
      }
  | _ ->
      raise
        (InvalidQuery ("Column " ^ key ^ " not found in one or both tables"))

(* let d = delete db org_tab in add d tab *)

(* let _ = for x=0 to (Table.col_size cols c) -1 do if List.nth (Column.data c)
   x=valu then indx := x :: !indx else () done in print_endline (List.length
   !indx) *)
(*for each column in col_list, create a new column which contains all of the
  elements at the indicies from indx list, add the column to a new table*)
(* let _= for a=0 to (List.length cols) do for b=0 to (List.length (List.nth
   cols a)) *)

(*return the table*)
(*return the database with the table updated*)
