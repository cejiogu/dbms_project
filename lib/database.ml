include Table

exception InvalidQuery of string

type table = Table.t

type t = {
  name : string;
  tables : table list;
}

let name t = t.name
let tables t = t.tables

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

let rec get_table_aux (tables : table list) (name : string) : table =
  match tables with
  | [] -> failwith "Should never come to this case"
  | (h : table) :: (t : table list) ->
      if Table.title h = name then h else get_table_aux t name

let get_table (db : t) (name : string) : table =
  if table_exists name db then get_table_aux db.tables name
  else raise (InvalidQuery ("The table titled " ^ name ^ " does not exist "))

let rec schema_aux (tables : table list) : unit =
  match tables with
  | [] -> ()
  | h :: t ->
      let () = print_endline ("- " ^ Table.title h) in
      schema_aux t

let schema (tables : table list) : unit =
  print_endline "";
  print_endline "TABLES:";
  schema_aux tables

let insert_existing_table (db : t) (tab : table) : t =
  let new_db = { name = db.name; tables = List.rev (tab :: db.tables) } in
  new_db
