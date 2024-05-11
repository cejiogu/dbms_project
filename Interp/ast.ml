open Final_project

type expr =
  | CreateTable of Table.t
  | Schema
  | Select of (string list * string)
  | AlterTable of (string * string * string)
  | SelectFromWhere of (string list * string * (string * string))
  | InsertInto of (string * string list * string list)
  | SelectMax of (string * string)
  | SelectMin of (string * string)
  | Truncate of string
  | InnerJoin of (string * string * string)
