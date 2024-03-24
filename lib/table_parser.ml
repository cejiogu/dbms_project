open Ast

(** [parse s] parses [s] into an AST *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** [is_value e] is whether [e] is a value. *)
let is_value : expr -> bool = function
  | Table _ -> true

(** [string_of_val e] converts [e] to a string. Requires [e] is a value. *)
let string_of_val (e : expr) : string =
  match e with
  | Table t -> string_of_table t

(** [step e] takes a single step of evaluation of [e]. *)
let rec step : expr -> expr = function
  | Table t -> failwith "Does not step"

(** [eval e] fully evaluates [e] to a value [v]. *)
let eval (e : expr) : expr = if is_value e then e else e |> step |> eval

(** [interp s] interpres [s] by lexing and parsing it, evaluating it, and
    converting the result to a string. *)
let interp (s : string) : string = s |> parse |> eval |> string_of_val
