open Ast
exception InvalidExpression

let eval_uexp (op : uop) (e : expr) = match op with
| Not -> begin match e with
  | Bool b -> Bool (not b)
  | _ -> raise InvalidExpression
end
| Dim -> begin match e with
  | Bool b -> Bool (not b)
  | _ -> raise InvalidExpression
end
| _ -> failwith "Unimplemented"