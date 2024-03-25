type expr = unit

(*Defined using reference: https://www.w3schools.com/sql/sql_datatypes.asp*)
type sql_type =
  | Int of int
    (*SQL INT(size) where size is the display width, should we consider just
      using a default?*)
  | TinyInt of int
    (*SQL TINYINT(size) where size is the display width (TINYINT is used for
      Bool)*)
  | Float
    (*SQL FLOAT(size,d) size is # of digits, d is # of digits after decimal
      point*)
  | VarChar of int
    (*SQL VARCHAR(size) where size is max string length (VARCHAR is like
      string)*)
  | Date (*SQL DATE, format YYYY-MM-DD*)

type command =
  | CREATE_TABLE
  | INSERT
(* important SQL commands to add later --> | SELECT | DELETE | UPDATE |
   ALTER_TABLE | DROP_TABLE *)
