open Ast

exception TypeError of string
exception UnboundVariable of string

(* Stores the environment mapping bindings to values *)
type ctx = (varname * typ) list

(** lookup_env var gamma finds the type of a variable in gamma **)
let rec lookup_ctx var = function
| [] -> None
| (v, t) :: _ when v = var -> Some t
| _ :: t -> lookup_ctx var t

(* eval_expr env expr evaluates expreessions given an env context *)
let typecheck gamma expr = match expr with
| Unit -> TUnit [1]
| Int _ -> TInt [1]
| Float _ -> TFloat [1]
| Bool _ -> TBool [1]

| Var v -> begin match lookup_ctx v gamma with
    | None -> raise (UnboundVariable v)
    | Some e -> e

end
| _ -> failwith "Unimplemented"