{open Parser}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+'.'digit+
let letter = ['a'-'z' 'A'-'Z']
let var = letter+
let sep = ',' ' '?

rule read = parse
| white { read lexbuf }

(* Binary Operators *)
| "^" { POW }
| "*" { TIMES }
| "/" { DIVIDE }
| "+" { PLUS }
| "-" { MINUS }
| "\\" { DIFF }
| "range" { RANGE }
| "get" { GET }
| "(" { LPAREN }
| ")" { RPAREN }
| "=>" { IMPLIES }
| "<=>" { IFF }
| "=" { EQUALS }
| ">" { GT }
| ">=" { GEQ }
| "<" { LT }
| "<=" { LEQ }
| "solve" { SOLVE }
| "change" { CHANGE }
| "factor" { FAC }

(* Matrix & Space Declarations *)
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LBRACE }
| "}" { RBRACE }
| sep { SEP }

(* Unary Operators *)
| "not" { NOT }
| "len" { LEN }
| "id" { ID }
| "det" { DET }
| "inv" { INV }
| "dim" { DIM }
| "square" { SQUARE }
| "ref" { REF }
| "rref" { RREF }
| "^tsp" { TRANSPOSE }
| "|" { ABS }
| "||" { NORM }

(* Ternary Operators *)
| "set" { SET }
| "generate" { GEN }
| "squeeze" { SQUEEZE }

(* Factorization *)
| "fac" { FAC }

(* Let & Functions *)
| "let" { LET }
| "in" { IN }
| "fun" { FUN }
| "->" { ARROW }

(* If-Else *)
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }

(* Loops *)
| "while" { WHILE }
| "do" { DO }
| "for" { FOR }

(* Terminals and conjunction *)
| "true" { TRUE }
| "false" { FALSE }
| "()" { UNIT }
| ":" { TYP }
| ";" { SEQ }
| "QR" { QR }
| "SVD" { SVD }
| "PDP" { PDP }
| "LU" { LU }
| "CR" { CR }
| var { VAR (Lexing.lexeme lexbuf)}
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| eof { EOF }