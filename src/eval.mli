open Ast

type value

val eval_expr : expr -> value

(* Creates an identity matrix with the specified dimension*)
val identity : expr -> value

(* Row reduces a matrix of specified dimensions to reduced echelon form*)
val ref: expr -> expr -> value

(* Row reduces a matrix of specified dimensions to row reduced echelon form*)
val rref: expr -> expr -> value

(* Adds two expressions (matrices, vectors, ints, floats) *)
val add : expr -> expr -> value

(* Checks dimensions and multiplies two matrices, vectors or integers/floats*)
val multiply : expr -> expr -> value

(* Checks if a matrix/vector space contains linearly independent col vectors *)
val independent : expr -> value

(* Transposes a matrix or vector *)
val transpose: expr -> value

(* Inverts a matrix if possible *)
val inverse : expr -> value

(* Computes the determinant of a square matrix*)
val det : expr -> expr -> value

(* Computes the eigenvalues of a matrix *)
val eigenvalues : expr -> value

(* Computes the eigenvectors of a matrix *)
val eigenvectors : expr -> value

(* Computes an orthonormal basis of a matrix or vector space*)
val orth : expr -> value

(* Produces a matrix factorization with the specified rules *)
val factor : factorization -> expr -> value