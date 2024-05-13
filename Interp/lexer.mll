{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float='-'? digit+ '.' digit*
let letter = ['a'-'z' 'A'-'Z' '_']
let id = letter+
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
  | "INNER" { INNER }
  | "JOIN" { JOIN }
  | "ON" { ON }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT_ACT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT_ACT (float_of_string (Lexing.lexeme lexbuf)) }
  | date {DATE_ACT (Lexing.lexeme lexbuf)}
  | eof { EOF }