open Ast
open Eval
open Typecheck

exception NonTerminal

(** parse s parses a program string s into an AST **)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

(** pretty_print expr is a formatted string representation of expr **)
let rec pretty_print = function
  | Unit -> "()"
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Matrix m ->
    "[" ^ String.concat ", " (List.map pretty_print m) ^ "]"

  | Space s->
      "{" ^ String.concat ", " (List.map pretty_print s) ^ "}"

  | _ -> raise NonTerminal

(* typecheck s parses s to an expression and typechecks it *)
let typecheck (s : string) = s |> parse |> typecheck []

(** interp s parses s to an expression, evaluates it and pretty-prints it **)
let interp (s : string) = s |> parse |> eval_expr [] |> pretty_print