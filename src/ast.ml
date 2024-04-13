type varname = string

type factorization = LU | CR | PDP | QR | SVD;;

type varname = string

type expr =
(* Baseline Types *)
| EEmpty
| EInt of int
| EFloat of float
| EBool of bool

| EVector of (expr * expr)
| EMatrix of (expr * expr)
| ESpace of (expr * expr)
| ERange of (expr * expr)

(* Operators *)
| EUopExpr of uop * expr
| EBopExpr of bop * expr * expr

(* NOTE: Turn into unary operator *)
| EFactorization of factorization * expr

(* Definitions And Functions *)
| Let of varname * expr * expr

and uop =
  Neg | Square | Transpose | Norm | Det | Inverse | REF | RREF | Eigenvalues | Eigenvectors | Orth

and bop = Add | Subtract | Multiply | Divide | Eq | Neq | Gt | Lt | Geq | Leq | Power | Solve