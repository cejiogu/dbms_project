(* {
    open Parser
    open Ast
}

let space = [' ']
(* let empty_table = Table.empty_table  *)

rule read = 
    parse 
    | "CREATE" { CREATE (Ast.command) }
    (* for future reference: | "CREATE" { TABLE (Table.make (Lexing.lexeme lexbuf)) } *)
    | eof { EOF } *)