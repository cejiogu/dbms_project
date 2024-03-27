(* open Ast

val parse : string -> expr
(** [parse s] parses [s] into an AST *)

val is_value : expr -> bool
(** [is_value e] is whether [e] is a value. *)

val string_of_val : expr -> string
(** [string_of_val e] converts [e] to a string. Requires [e] is a value. *)

val step : expr -> expr
(** [step e] takes a single step of evaluation of [e]. *)

val eval : expr -> expr
(** [eval e] fully evaluates [e] to a value [v]. *)


val interp : string -> string
(** [interp s] interpres [s] by lexing and parsing it, evaluating it, and
    converting the result to a string. *) *)