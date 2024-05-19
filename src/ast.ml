type varname = string

type typ = 
| TEmpty of int list
| TUnit of int list
| TInt of int list
| TFloat of int list
| TBool of int list
| TFun of typ list * typ
| TSpace of typ * int
| TRange

type uop =
| Not | Len | Id | Det | Dim | Square | Inv
| Transpose | Ref | Rref | Abs | Norm

type bop =
| Plus | Minus | Times | Divide | Pow | Diff
| Range | Get | Eq | Gt | Geq | Lt | Leq
| Implies | Iff | Solve | Change

type top =
| Set | Gen | Squeeze

type factorization =
| QR | SVD | PDP | LU | CR

type expr =
(* Terminals Types *)
| Unit
| Int of int
| Float of float
| Bool of bool

(* Variables *)
| Var of varname

(* Linear Algebra structures *)
| Matrix of expr list
| Space of expr list
| Range of expr * expr

(* Operators and Factorizations *)
| UExpr of uop * expr
| BExpr of bop * expr * expr
| TExpr of top * expr * expr * expr
| Fac of factorization * expr

(* Ifs and Loops *)
| If of expr * expr * expr
| While of expr * expr
| For of varname * expr * expr * expr

(* Definitions And Functions *)
| Assgn of varname * expr
| Let of varname * expr * expr
| Fun of expr list * expr
| App of expr * expr list

(* Expression sequencing *)
| Seq of expr * expr