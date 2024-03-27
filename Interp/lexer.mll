{
    open Parser
}

let space = [' ']
let empty_table = table.empty_table 

rule read = 
    parse 
    | "CREATE" { command }
    (* for future reference: | "CREATE" { TABLE (table_of_string (Lexing.lexeme lexbuf)) } *)
    | eof { EOF }