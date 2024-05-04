{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse
  | white { read lexbuf }
  | "CREATE" {CREATE}
  | "TABLE" {TABLE}
  | "SCHEMA" {SCHEMA}
  | "SELECT" {SELECT}
  | "FROM" {FROM}
  | "INTEGER" | "INT" {INT}
  (* | "VARCHAR" {VARCHAR} *)
  | "STRING" {STRING}
  | "BOOL" {BOOL}
  | "FLOAT" {FLOAT}
  | "DATE" {DATE}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT_ACT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }