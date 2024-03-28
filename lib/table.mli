exception InvalidQuery of string
  
type column
(**[column] is the type of table columns*)

type t
(**[t] is an alias for the type of a table*)

val table_name : t -> string
(**[table_name t] returns the [table_name] field of [t]. *)

val columns : t -> column list
(**[columns t] returns the [columns] field of [t]. *)
  
val empty_table : string -> t
(**[empty_table] is an empty table, titled [string]. 
    Raises [InvalidQuery] if [string] is empty*)
  
val create_table : string -> string list -> t
(**[create_table] is a [table], titled [string] and containing columns titled 
    after each string in [string list]. This function is an auxiliary function that
    helps to implement the function that would correspond to the SQL CREATE TABLE
    statement *)
  
val insert_into : string -> string list -> string list -> t -> 'a
(**[insert_into] is a [table] containing a new row of values [string] that
       holds the string values in [string list]*)
  
val print_table : t -> unit
(**[print_table] represents the table as a string in the terminal*)