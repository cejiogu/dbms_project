(* open Ast

   let parse (s : string) : expr = let lexbuf = Lexing.from_string s in let ast
   = Parser.prog Lexer.read lexbuf in ast

   let is_value : expr -> bool = function | Table _ -> true

   let string_of_val (e : expr) : string = match e with | Table t ->
   string_of_table t

   let rec step : expr -> expr = function | Table t -> failwith "Does not step"

   let eval (e : expr) : expr = if is_value e then e else e |> step |> eval let
   interp (s : string) : string = s |> parse |> eval |> string_of_val *)
