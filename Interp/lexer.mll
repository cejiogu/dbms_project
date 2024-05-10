{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float='-'? digit+ '.' digit*
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
(* let str=(letter|white)+ *)
let date = digit digit digit digit '-' digit digit '-' digit digit

rule read = 
  parse
  | white { read lexbuf }
  | "CREATE" {CREATE}
  | "TABLE" {TABLE}
  | "SCHEMA" {SCHEMA}
  | "SELECT" {SELECT}
  | "FROM" {FROM}
  | "WHERE" {WHERE}
  | "INSERT" {INSERT}
  | "INTO" {INTO}
  | "VALUES" {VALUES}
  | "ALTER" {ALTER}
  | "ADD" {ADD}
  | "INTEGER" | "INT" {INT}
  (* | "VARCHAR" {VARCHAR} *)
  | "STRING" {STRING}
  | "BOOL" {BOOL}
  | "FLOAT" {FLOAT}
  | "DATE" {DATE}
  | "=" {EQUALS}
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "MIN" {MIN}
  | "MAX" {MAX}
  | "TRUNCATE" {TRUNCATE}
  | id { ID (Lexing.lexeme lexbuf) }
  (* | str {STR (Lexing.lexeme lexbuf) } *)
  | int { INT_ACT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT_ACT (float_of_string (Lexing.lexeme lexbuf)) }
  | date {DATE_ACT (Lexing.lexeme lexbuf)}
  | eof { EOF }