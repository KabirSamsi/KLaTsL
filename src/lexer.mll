{open Parser}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+'.'digit+
let letter = ['a'-'z' 'A'-'Z']
let var = letter+
let sep = ',' ', '?

rule read = parse
| white { read lexbuf }
| "*" { TIMES }
| "x" { CROSS }
| "+" { PLUS }
| "-" { MINUS }
| "range" { RANGE }
| "get" { GET }
| "(" { LPAREN }
| ")" { RPAREN }
| "->" { ARROW }
| "=>" { IMPLIES }
| "<=>" { IFF }
| "=" { EQUALS }
| ">" { GT }
| ">=" { GEQ }
| "<" { LT }
| "<=" { LEQ }
| "let" { LET }
| "in" { IN }
| "solve" { SOLVE }
| "change" { CHANGE }

| "[" { LBRACKET }
| "]" { RBRACKET }
| "{" { LBRACE }
| "}" { RBRACE }
| sep { SEP }

| "not" { NOT }
| "len" { LEN }
| "id" { ID }
| "det" { DET }
| "inv" { INV }
| "ref" { REF }
| "rref" { RREF }
| "factor" { FAC }
| "span" { SPAN }
| "^tsp" { TRANSPOSE }

| "set" { SET }
| "generate" { GEN }

| "true" { TRUE }
| "false" { FALSE }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }

| "while" { WHILE }
| "do" { DO }
| "for" { FOR }

| "fun" { FUN }

| "QR" { QR }
| "SVD" { SVD }
| "PDP" { PDP }
| "CR" { CR }
| "LU" { LU }

| "()" { UNIT }
| var { VAR (Lexing.lexeme lexbuf)}
| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
| float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
| eof { EOF }