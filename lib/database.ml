include Table

exception InvalidQuery of string

type table = Table.t

type t = {
  name : string;
  tables : table list;
}

let empty_database (name : string) : t =
  if name = "" then
    raise (InvalidQuery "You must enter the name of your table!")
  else
    let database = { name; tables = [] } in
    database

let table_exists (name : string) (db : t) : bool =
  List.exists (fun t -> t.table_name = name) db.tables

let insert_table (db : t) (name : string) (columns : string list) : t =
  if table_exists name db then db
  else
    let new_table = Table.create_table name columns in
    { db with tables = new_table :: db.tables }
