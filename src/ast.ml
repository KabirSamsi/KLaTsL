type factorization = LU | CR | PDP | QR | SVD;;

type expr =
| Empty
| Int of int
| Float of float
| Size of expr
| Vector of expr * expr * expr
| Matrix of expr * expr * expr
| VSpace of expr * expr
| UopExpr of uop * expr
| BopExpr of bop * expr * expr
| Factorization of factorization * expr

and uop =Det | Dimensions | Inverse | REF | RREF | Transpose | Eigenvalues | Eigenvectors

and bop = Add | Multiply | ScalarMultiply | Solve;;