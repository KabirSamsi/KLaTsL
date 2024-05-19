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

let swap_dims new_lst = function
| TEmpty _ -> TEmpty new_lst
| TUnit _ -> TUnit new_lst
| TInt _ -> TInt new_lst
| TFloat _ -> TFloat new_lst
| TBool _ -> TBool new_lst
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

and typecheck_uop op t = match op with
| Not -> begin
    match t with
    | TBool lst -> TBool lst
    | TInt lst -> TInt lst
    | TFloat lst -> TFloat lst
    | _ -> raise (TypeError "Invalid structure for negation")
end

| Len ->
    if List.length (get_dimensions t) > 0 then TInt []
    else raise (TypeError "Cannot compute length of scalar")

| Det -> begin
    match t with
    | TInt [i] when i = 1 -> TFloat []
    | TFloat [i] when i = 1 -> TFloat []
    | TInt [i1; i2] when i1 = i2 -> TFloat []
    | TFloat [i1; i2] when i1 = i2 -> TFloat []
    | _ -> raise (TypeError "Invalid dimensions to take determinant")
end

| Dim -> TInt [List.length (get_dimensions t)]
| Square -> TBool []

| Inv -> begin
    match t with
    | TInt [i] when i = 1 -> TFloat [i]
    | TFloat [i] when i = 1 -> TFloat [i]
    | TInt [i1; i2] when i1 = i2 -> TFloat [i1; i2]
    | TFloat [i1; i2] when i1 = i2 -> TFloat [i1; i2]
    | _ -> raise (TypeError "Invalid dimensions to compute inverse")
end

| Transpose ->
    let dims = get_dimensions t in
    if List.length dims = 1 then swap_dims [1; (List.hd dims)] t
    else if List.length dims = 2 then swap_dims (List.rev dims) t
    else raise (TypeError "Invalid dimensions for transpose")

| Ref | Rref | Abs -> begin
    match t with
    | TInt lst | TFloat lst -> t
    | _ -> raise (TypeError "Type must be int or float matrix")
end

| Norm -> begin
    match t with
    | TInt lst | TFloat lst -> TFloat []
    | _ -> raise (TypeError "Type must be int or float matrix")
end

| _ -> failwith "Unimplemented"

and typecheck_bop op t1 t2 = match op with
| Plus | Minus -> begin
    match t1, t2 with
    | TInt d1, TInt d2 when d1 = d2 -> TInt d1
    | TBool d1, TBool d2 when d1 = d2 -> TBool d1
    | TUnit d1, TUnit d2 when d1 = d2 -> TUnit d1
    | TFloat d1, TFloat d2 when d1 = d2 -> TFloat d1
    | TFloat d1, TInt d2 when d1 = d2 -> TFloat d1
    | TInt d1, TFloat d2 when d1 = d2 -> TFloat d1
    | _ -> raise (TypeError "Dimensions or types do not match")
end

| Get -> let t1_dims = get_dimensions t1 in begin
    match t2 with
    | TInt [] ->
        if List.length t1_dims > 0
        then swap_dims (List.tl t1_dims) t1
        else raise (TypeError "Cannot access entry of scalar")
    | _ -> raise (TypeError "Get index must be integer")
end

| _ -> failwith "Unimplemented"

and typecheck_top op t1 t2 t3 = match op with
| _ -> failwith "Unimplemented"