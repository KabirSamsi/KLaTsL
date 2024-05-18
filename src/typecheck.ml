open Ast

exception TypeError of string
exception InvalidOperation
exception UnboundVariable of string

(* Stores the environment mapping bindings to values *)
type ctx = (varname * typ) list

let extend_type entries = function
| TEmpty lst -> TEmpty (entries :: lst)
| TUnit lst -> TUnit (entries :: lst)
| TInt lst -> TInt (entries :: lst)
| TFloat lst -> TFloat (entries :: lst)
| TBool lst -> TBool (entries :: lst)
| _ -> raise InvalidOperation

let get_dimensions = function
| TEmpty lst | TUnit lst | TInt lst | TFloat lst | TBool lst -> lst
| _ -> raise InvalidOperation

(** lookup_ctx var context finds the type of a variable in a given context **)
let rec lookup_ctx var = function
| [] -> None
| (v, t) :: _ when v = var -> Some t
| _ :: t -> lookup_ctx var t

(* typecheck ctx expr typechecks an expression with a given context *)
let rec typecheck ctx expr = match expr with
| Unit -> TUnit []
| Int _ -> TInt []
| Float _ -> TFloat []
| Bool _ -> TBool []

| Var v -> begin
    match lookup_ctx v ctx with
    | None -> raise (UnboundVariable v)
    | Some e -> e
end

| Matrix m -> 
    if List.length m = 0
    then TEmpty []
    (* Ensure that all column matrices type the same *)
    else let type_acc = typecheck ctx (List.hd m) in
        if (List.fold_left (
            fun acc elem -> acc
            && (typecheck ctx elem) = type_acc
        ) true m)
        (* Extend dimension with number of new vectors *)
        then extend_type (List.length m) type_acc
        else raise (TypeError "Dimensions inconsistent across vectors")

| Space s -> 
    if List.length s = 0
        then TSpace(TEmpty [0], 0)
        else let type_acc = typecheck ctx (List.hd s) in
            (* Ensure all vectors type the same and all are column vectors *)
            if (
                List.fold_left (
                    fun acc elem -> acc
                    && (typecheck ctx elem) = type_acc
                ) true s
                && List.length (get_dimensions type_acc) = 1
            )
            then TSpace(type_acc, List.length s)
            else raise (TypeError "Dimensions inconsistent across vectors")

| Range (a, b) -> begin
    (* Ensure bounds are both integers *)
    match typecheck ctx a, typecheck ctx b with
    | TInt l1, TInt l2 when List.length l1 = 0 && List.length l2 = 0 -> TRange
    | _ -> raise (TypeError "Range bounds must be integers")
end

| If (e1, e2, e3) -> begin
    match (typecheck ctx e1), (typecheck ctx e2), (typecheck ctx e3) with
    | TBool _, t1, t2 when t1 = t2 -> t1
    | _, t1, t2 when t1 = t2 -> raise (TypeError "If expression expected guard of type bool")
    | _ -> raise (TypeError "If expression expected branches evaluate to the same type")
end

| While (e1, e2) -> begin
    match (typecheck ctx e1) with
    | TBool _ -> (typecheck ctx e2)
    | _ -> raise (TypeError "Loop expected guard of type bool")
end

| UExpr (op, e) ->
    typecheck_uop op (typecheck ctx e)

| BExpr (op, e1, e2) ->
    typecheck_bop op (typecheck ctx e1) (typecheck ctx e2)

| TExpr (op, e1, e2, e3) ->
    typecheck_top op (typecheck ctx e1) (typecheck ctx e2) (typecheck ctx e3)

| _ -> failwith "Unimplemented"

and typecheck_uop op e = match e with
| _ -> failwith "Unimplemented"

and typecheck_bop op e1 e2 = match e1 with
| _ -> failwith "Unimplemented"

and typecheck_top op e1 e2 e3 = match e1 with
| _ -> failwith "Unimplemented"