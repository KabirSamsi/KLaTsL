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
| "x" { CROSS }
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

(* Vector & Space Declarations *)
| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LBRACE }
| "}" { RBRACE }
| sep { SEP }

(* Unary Operators *)
| "read" { READ }
| "not" { NOT }
| "len" { LEN }
| "id" { ID }
| "det" { DET }
| "inv" { INV }
| "dim" { DIM }
| "square" { SQUARE }
| "ref" { REF }
| "rref" { RREF }
| "span" { SPAN }
| "^tsp" { TRANSPOSE }
| "|" { ABS }
| "||" { NORM }

(* Ternary Operators *)
| "set" { SET }
| "generate" { GEN }
| "squeeze" { SQUEEZE }

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

(* Factorizations *)
| "QR" { QR }
| "SVD" { SVD }
| "PDP" { PDP }
| "CR" { CR }
| "LU" { LU }

(* Terminals *)
| "true" { TRUE }
| "false" { FALSE }
| "()" { UNIT }
| ":" { TYP }
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| var { VAR (Lexing.lexeme lexbuf)}
| eof { EOF }