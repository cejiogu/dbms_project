{
    open Parser
}

let empty_table = {name = ""; data = []}

rule read = 
    parse 
    | table { TABLE (table_of_string (Lexing.lexeme lexbuf)) }
    | eof { EOF }