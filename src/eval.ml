open Ast

exception TypeError of string
exception DimensionError

(* Stores the environment mapping bindings to values *)
type env = (varname * expr) list

(* Evaluates an expression with the specified semantics*)
let rec eval_expr expr = match expr with
  | EEmpty
  | EInt(a)
  | EFloat(a) 
  | EBool(a) -> expr

  | EVector(h, t) -> EVector(eval_expr h, eval_expr t)
  | EMatrix(h, t) -> EMatrix(eval_expr h, eval_expr t)
  | ESpace(h, t) -> ESpace(eval_expr h, eval_expr t)

  | EBopExpr(b, e1, e2) -> eval_bop b e1 e2
  | EUopExpr(u, e) -> eval_uop u e
  | _ -> failwith "Unimplemented"

(* Evaluates a binary expression *)
and eval_bop op e1 e2 = 
  let v1, v2 = eval_expr e1, eval_expr e2 in match op with
  | Add -> add v1 v2
  | Subtract -> add v1 (multiply v2 (VInt(-1)))
  | Multiply -> multiply v1 v2
  | Divide -> divide v1 v2
  | Power -> power v1 v2

  | Eq -> VBool(eq v1 v2)
  | Neq -> VBool(neq v1 v2)
  | Gt -> VBool(gt v1 v2)
  | Geq -> VBool(geq v1 v2)
  | Lt -> VBool(lt v1 v2)
  | Leq -> VBool(leq v1 v2)

  | _  -> failwith "Unimplemented"

(* Evaluates a unary expression *)
and eval_uop op e = let v = eval_expr e in match op with
  | Neg -> multiply v (VInt(-1))
  | Square -> multiply v v
  | Transpose -> transpose v
  | Norm -> (match v with
    | VVector(h, t) -> power (multiply v v) (VFloat(0.5))
    | _ -> raise(TypeError "Norm can only be applied to vectors"))

  | _ -> failwith "Unimplemented"

(* Add two values*)
and add e1 e2 = match (eval_expr e1), (eval_expr e2) with
  | VEmpty, VEmpty -> VEmpty;
  | VEmpty, _ | _, VEmpty -> raise(DimensionError)
  
  (* Add two scalars *)
  | VInt(a), VInt(b) -> VInt(a + b);
  | VFloat(a), VFloat(b) -> VFloat(a +. b);
  | VInt(a), VFloat(b) 
  | VFloat(b), VInt(a) -> VFloat((float_of_int a) +. b);

  (* Add two vectors (if dimensions match) *)
  | VVector(h1, t1), VVector(h2, t2) -> VVector((add h1 h2), (add t1 t2))

  (* Add two matrices together, if dimensions match (vectors of vectors) *)
  | VMatrix(h1, t1), VMatrix(h2, t2) -> VMatrix((add h1 h2), (add t1 t2))

  | _, _ -> raise(TypeError "Invalid types")

(* Check the equality of two values*)
and eq v1 v2 = match v1, v2 with
  | VEmpty, VEmpty -> true
  | VInt(a), VInt(b) -> a = b
  | VBool(a), VBool(b) -> a = b
  | VFloat(a), VFloat(b) -> a = b
  | VInt(a), VFloat(b) -> float_of_int a = b
  | VFloat(b), VInt(a) -> float_of_int a = b

  | VVector(h1, t1), VVector(h2, t2)
  | VMatrix(h1, t1), VMatrix(h2, t2)
  | VSpace(h1, t1), VSpace(h2, t2) -> (eq h1 h2) && (eq t1 t2)
  | _, _ -> failwith "Unimplemented"

(* Check that one expression is greater than the other*)
and gt v1 v2 = match v1, v2 with
  | VEmpty, VEmpty -> false
  | VInt(a), VInt(b) -> a > b
  | VBool(a), VBool(b) -> a > b
  | VFloat(a), VFloat(b) -> a > b
  | VInt(a), VFloat(b) -> float_of_int a > b
  | VFloat(b), VInt(a) -> float_of_int a > b

  | VVector(h1, t1), VVector(h2, t2)
  | VMatrix(h1, t1), VMatrix(h2, t2)
  | VSpace(h1, t1), VSpace(h2, t2) -> (gt h1 h2) && (gt t1 t2)
  | _, _ -> failwith "Unimplemented"

(* Remaining equality/inequality expressions *)
and neq v1 v2 = ((eq v1 v2) = false)
and geq v1 v2 = (gt v1 v2) || (eq v1 v2)
and lt v1 v2 = (geq v1 v2) = false
and leq v1 v2 = (gt v1 v2) = false

(* Create a vector of specified dimensions filled with a specific value *)
and kvector k size =
  if eq size (VInt(0)) then VEmpty
  else VVector(k, kvector k (add size (VInt(-1))))

and zerovector size = kvector (VInt(0)) size (* Create the zero vector *)

(* Compute the scalar multiple of a vector/matrix and int/float *)
and scalarmultiple k v = match k, v with
  | _, VEmpty -> VEmpty

  (* Vector scalar multiplication *)
  | i, VVector(h, t)
  | VVector(h, t), i -> VVector((multiply i h), (scalarmultiple k t))

  (* Matrix scalar multiplication *)
  | i, VMatrix(h, t)
  | VMatrix(h, t), i -> VMatrix((multiply i h), (scalarmultiple k t))

  | _, _ -> raise(TypeError "Invalid types");

(* Multiply two values *)
and multiply v1 v2 = match v1, v2 with
  | _, VEmpty -> VEmpty

  (* Multiply two scalars *)
  | VInt(a), VInt(b) -> VInt(a * b)
  | VFloat(a), VFloat(b) -> VFloat(a *. b)
  | VInt(a), VFloat(b) | VFloat(b), VInt(a)
    -> VFloat((float_of_int a) *. b)

  (* Multiply a vector/matrix and scalar *)
  | VInt(_), VVector(_, _) | VVector(_, _), VInt(_)
  | VFloat(_), VVector(_, _) | VVector(_, _), VFloat(_)
  | VInt(_), VMatrix(_, _) | VMatrix(_, _), VInt(_)
  | VFloat(_), VMatrix(_, _) | VMatrix(_, _), VFloat(_)
    -> scalarmultiple v1 v2

  (* Dot product a vector with itself *)
  | VVector(h1, t1), VVector(h2, t2) -> add (multiply h1 h2) (multiply t1 t2)

  (* Multiply matrix with vector*)
  | VMatrix(_, _), VVector(_, _) -> mvp v1 v2
  
  (* Multiply matrix with matrix *)
  | VMatrix(_, _), VMatrix(_, _) -> mmp v1 v2

  | _, _ -> failwith "Unimplemented" (* All other cases *)

(* Divide two values *)
and divide v1 v2 = match v1, v2 with
  | _, VEmpty -> VEmpty

  (* Divide two scalars *)
  | VInt(a), VInt(b) -> VFloat(float_of_int a /. float_of_int b)
  | VFloat(a), VFloat(b) -> VFloat(a /. b)
  | VInt(a), VFloat(b) | VFloat(b), VInt(a)
    -> VFloat((float_of_int a) /. b)

  (* Divide a vector/matrix by a scalar *)
  | VVector(_, _), VInt(_) | VVector(_, _), VFloat(_)
  | VMatrix(_, _), VInt(_) | VMatrix(_, _), VFloat(_)
    -> scalarmultiple v1 (divide (VInt(1)) v2)

  | _, _ -> raise(TypeError "Invalid types") (* All other cases *)

(* Raise a value to a power *)
and power v1 v2 = match v1, v2 with
  | VInt(a), VInt(b) -> VFloat((float_of_int a) ** (float_of_int b))
  | VFloat(a), VFloat(b) -> VFloat(a ** b)
  | VInt(a), VFloat(b) -> VFloat((float_of_int a) ** b)
  | VFloat(a), VInt(b) -> VFloat(a ** float_of_int b)
  | _, _ -> failwith "Unimplemented"

and num_rows v = match v with
  (* For a vector, the number of rows is the number of entries *)
  | VEmpty -> VInt(0)
  | VVector(h, t) -> add (num_rows t) (VInt(1))

  (* For both matrix and vector space,
     num. rows is just number of entries per vector *)
  | VMatrix(h, t) | VSpace(h, t) -> num_rows h

  | _ ->
    raise(
      TypeError "Can only be applied to vectors, matrices and vector spaces")

and num_cols v = match v with
  | VEmpty -> VInt(0)
  | VMatrix(h, t) | VSpace(h, t) -> add (num_cols t) (VInt(1))
  | _ -> raise(TypeError "Only matrices and vector spaces have countable columns")

(** Extract a certain entry from a vector, or column from matrix
  v1 is the position/index of the entry
  v2 is the vector/matrix
  v3 is our current position (accumulator) in the vector/matrix
**)
and extract_entry v1 v2 v3 = match v1, v2, v3 with
  (* Extracting an entry out of a vector*)
  | VInt(k), VVector(h, t), VInt(current)
    (* Check that the dimensions and query are accurate *)
    when k > 0 && leq v1 (num_rows v2) && k <= current ->
    if current = k then h (*Return the current entry if index matches*)
    (* Recurse through the vector otherwise *)
    else extract_entry v1 t (add v1 (VInt(1))) 

  (* Extracting a column out of a matrix or out of a basis for vector space*)
  | VInt(col), VMatrix(h, t), VInt(current)
    (* Check that the dimensions and query are accurate *)
    when col > 0 && leq v1 (num_cols v2) && col <= current ->
    if current = col then h (*Return the current column if index matches*)
    (* Recurse through the vector otherwise *)
    else extract_entry v1 t (add v1 (VInt(1))) 

  (* Extracting a column out of a matrix or out of a basis for vector space*)
  | VInt(vector), VSpace(h, t), VInt(current)
    (* Check that the dimensions and query are accurate *)
    when vector > 0 && leq v1 (num_cols v2) && vector <= current ->
    if current = vector then h (*Return the current column if index matches*)
    (* Recurse through the vector otherwise *)
    else extract_entry v1 t (add v1 (VInt(1))) 

  | _, _, _ -> raise(TypeError "Types do not match")

(** Extracts the specified row from a given matrix**)
and extract_row v1 matrix = match v1, matrix with
  | _, VEmpty -> VEmpty (*Have reached empty matrix*)

  (*Tracks the current row index and current matrix*)
  | VInt(row_index), VMatrix(h, t)
    when row_index > 0 && leq v1 (num_rows matrix) ->
    (* Extracts the corresponding entry from the current column,
      recurses over remainder *)
    VVector(extract_entry v1 h (VInt(0)), extract_row v1 t)

  | VInt(row_index), VMatrix(h, t) -> VEmpty
  | _, _ -> raise(TypeError "Invalid types, can only be applied to matrix")

(* Compute the product of a matrix (m) and vector (v) *)
and mvp m v =
  let rec mvp_helper matrix vector current = match matrix, vector with
    | _, VEmpty | VEmpty, _ -> VEmpty

    | VMatrix(h1, t1), VVector(h2, t2)
      when eq (num_cols matrix) (num_rows vector) ->
      (* Base case – current row is out of range of the number of rows*)
      if gt current (num_rows matrix) then VEmpty else

      (* Extract the current row as a vector and dot it with the vector*)
      let current_row = extract_row current matrix in
      let product = multiply current_row vector
      (* Create the final vector and recursively multiply the remaining rows by vector *)
      in VVector(product, mvp_helper matrix vector (add current (VInt(1))))

    (* If the dimensions are inaccurate or we try and other form of multiplication,
       raise an error *)
    | VMatrix(h1, t1), VVector(h2, t2) -> raise(DimensionError)
    | _, _ -> raise(TypeError "Must multiply matrix by vector, in that order")
  in mvp_helper m v (VInt(0))

(* Compute the product of two matrices with appropriate dimensions*)
and mmp m1 m2 = match m1, m2 with
  | _, VEmpty -> VEmpty (*Base case – all cols for m2 have been processed*)

  (* If dimensions match, recursively find the matrix-vector product
    of m1 with each column of m2*)
  | VMatrix(h1, t1), VMatrix(h2, t2) when eq (num_cols m1) (num_rows m2) ->
    VMatrix(mvp m1 h2, mmp m1 t2)

  | VMatrix(h1, t1), VMatrix(h2, t2) -> raise(DimensionError)
  | _, _ -> raise(TypeError "Must take in two matrices to multiply")

(* Generate a matrix with one constant value *)
and kmatrix dim k = 
  let rec kmatrix_helper dim k current =
    if eq k current then VEmpty (*Base case – we have filled all columns*)
    (* Generate a vector and then recursively populate the remaining columns *)
    else VMatrix(kvector k dim, kmatrix_helper dim k (add current (VInt(1))))
  in kmatrix_helper dim k (VInt(0))

(* Generate a square identity matrix of the specified columns *)
and identity e = let dim = eval_expr e in
  (* Generate the kth column of a size x size identity matrix
    size is the dimension (square), k is the position of the pivot, current is acc*)
  let rec identity_vector size k current = 
    if eq size current then VEmpty
    (* If this is the pivot entry, insert a 1 *)
    else if (eq current k) then VVector(VInt(1), identity_vector size k (add current(VInt(1))))
    (* Otherwise, insert a 0 *)
    else VVector(VInt(0), identity_vector size k (add current(VInt(1))))
  
  (* Helper function for developing identity matrix *)
  in let rec identity_helper size current =
    if eq size current then VEmpty (*Base case – we have populated all columns*)
    else VMatrix( (*Otherwise, build the current identity vector and recurse over the rest*)
      identity_vector size current (VInt(1)),
      identity_helper size (add current (VInt(1)))
    )
  in identity_helper dim (VInt(0))

(* Build the transpose of a matrix *)
and transpose v = match v with
  | VEmpty -> VEmpty

  (* Extract each row of the matrix and insert it as a new column *)
  | VMatrix(h, t) -> let rec transpose_helper row_index matrix =
      VMatrix(extract_row row_index matrix,
      transpose_helper (add row_index (VInt(1))) matrix)
    in transpose_helper (VInt(0)) v

  | _ -> raise(TypeError "Types do not match")

and ref e1 e2 = failwith "Unimplemented"
and rref e1 e2 = failwith "Unimplemented"
and independent v = failwith "Unimplemented"
and inverse v = failwith "Unimplemented"
and det v = failwith "Unimplemented"
and orth v = failwith "Unimplemented"
and eigenvalues v = failwith "Unimplemented"
and eigenvectors v = failwith "Unimplemented"
and factor f e = failwith "Unimplemented"