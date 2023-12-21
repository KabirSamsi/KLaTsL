type varname = string

type factorization = LU | CR | PDP | QR | SVD;;

type expr =
| EEmpty
| EInt of int
| EFloat of float
| EBool of bool
| EVector of (expr * expr)
| EMatrix of (expr * expr)
| ESpace of (expr * expr)
| ERange of (expr * expr)
| EUopExpr of uop * expr
| EBopExpr of bop * expr * expr
| EFactorization of factorization * expr

and uop =
  Neg | Square | Norm | Det | Inverse | REF | RREF | Transpose | Eigenvalues | Eigenvectors | Orth

and bop = Add | Subtract | Multiply | Divide | Eq | Neq | Gt | Lt | Geq | Leq | Power | Solve