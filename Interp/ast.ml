open Final_project

type expr =
  | CreateTable of Table.t
  | Schema
  | Select of (string list * string)
