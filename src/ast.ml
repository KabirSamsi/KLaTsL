type factorization = LU | CR | PDP | QR | SVD;;

type varname = string

type uop =
| Read | Not | Len | Id | Det | Dim | Square | Inv
| Ref | Rref | Span | Transpose | Abs | Norm

type bop =
| Plus | Minus | Times | Divide | Pow | Cross
| Diff | Range | Get | Eq | Gt | Geq | Lt | Leq
| Implies | Iff | Solve | Change | Fac

type top =
| Set | Gen | Squeeze

type expr =
(* Terminals Types *)
| Unit
| Int of int
| Float of float
| Bool of bool

(* Variables *)
| Var of varname

(* Linear Algebra structures *)
| Vector of expr list
| Space of expr list
| Range of expr * expr

(* Operators *)
| UExpr of uop * expr
| BExpr of bop * expr * expr
| TExpr of top * expr * expr * expr

(* Ifs and Loops *)
| If of expr * expr * expr
| While of expr * expr
| For of varname * expr * expr * expr

(* Definitions And Functions *)
| Assgn of varname * expr
| Let of varname * expr * expr
| Fun of expr list * expr