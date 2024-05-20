%{
open Ast
%}

// Terminals
%token UNIT
%token <int> INT
%token <float> FLOAT
%token <string> VAR
%token TRUE
%token FALSE

// Binary Operators
%token POW
%token TIMES
%token DIVIDE
%token PLUS
%token MINUS
%token DIFF
%token RANGE
%token GET
%token LPAREN
%token RPAREN
%token ARROW
%token IMPLIES
%token IFF
%token EQUALS
%token GT
%token GEQ
%token LT
%token LEQ
%token SOLVE
%token CHANGE

// Matrix & Space Declarations
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token SEP

// Unary Operators
%token NOT
%token LEN
%token DET
%token DIM
%token SQUARE
%token INV
%token REF
%token RREF
%token SPAN
%token TRANSPOSE
%token ABS
%token NORM

// Ternary Operators
%token SET
%token GEN
%token SQUEEZE

//Factorization
%token FAC
%token QR
%token SVD
%token PDP
%token LU
%token CR

// If-Else
%token IF
%token THEN
%token ELSE

// Loops
%token WHILE
%token DO
%token FOR

// Let & Functions
%token LET
%token IN
%token FUN

// Sequencing
%token SEQ

// Type annotations
%token TYP

// EOF
%token EOF

// Associativity declarations
%right SEQ
%left EQUALS
%left GT
%left GEQ
%left LT
%left LEQ
%left PLUS
%left MINUS
%left DIFF
%left TIMES
%left DIVIDE

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e };

expr:
	// Terminal expressions
    | UNIT { Unit }
    | i = INT { Int i }
    | f = FLOAT { Float f }
	| TRUE { Bool true }
	| FALSE { Bool false }
    
    // Vars
    | x = VAR { Var x }

    // Matrix and Spaces
    | LBRACKET; elements = separated_list(SEP, expr); RBRACKET
    { Matrix(elements) }

    | LBRACE; elements = separated_list(SEP, expr); RBRACE
    { Space(elements) }

	// Binary expressions
    | e1 = expr; POW; e2 = expr { BExpr (Pow, e1, e2) } 
	| e1 = expr; TIMES; e2 = expr { BExpr (Times, e1, e2) } 
    | e1 = expr; DIVIDE; e2 = expr { BExpr (Divide, e1, e2) } 
	| e1 = expr; PLUS; e2 = expr { BExpr (Plus, e1, e2) }
    | e1 = expr; MINUS; e2 = expr { BExpr (Minus, e1, e2) }
    | e1 = expr; DIFF; e2 = expr { BExpr (Diff, e1, e2) }
    | RANGE; LBRACE; e1 = expr; e2 = expr; RBRACE { BExpr(Get, e1, e2) }
    | GET; e1 = expr; e2 = expr { BExpr(Get, e1, e2) }
    | e1 = expr; IMPLIES; e2 = expr { BExpr (Implies, e1, e2) }
    | e1 = expr; IFF; e2 = expr { BExpr (Iff, e1, e2) }
    | e1 = expr; EQUALS; e2 = expr { BExpr (Eq, e1, e2) }
    | e1 = expr; GT; e2 = expr { BExpr (Gt, e1, e2) }
    | e1 = expr; GEQ; e2 = expr { BExpr (Geq, e1, e2) }
    | e1 = expr; LT; e2 = expr { BExpr (Lt, e1, e2) }
    | e1 = expr; LEQ; e2 = expr { BExpr (Leq, e1, e2) }
    | SOLVE; e1 = expr; e2 = expr {BExpr(Solve, e1, e2)}
    | CHANGE; e1 = expr; e2 = expr {BExpr(Change, e1, e2)}
    
    // Unary expressions
    | NOT; e = expr { UExpr(Not, e) }
    | LEN; e = expr { UExpr(Len, e) }
    | DET; e = expr { UExpr(Det, e) }
    | DIM; e = expr { UExpr(Dim, e) }
    | SQUARE; e = expr { UExpr(Square, e) }
    | INV; e = expr { UExpr(Inv, e) }
    | REF; e = expr { UExpr(Ref, e) }
    | RREF; e = expr { UExpr(Rref, e) }
    | SPAN; e = expr { UExpr(Span, e) }
    | TRANSPOSE; e = expr { UExpr(Transpose, e) }
    | ABS; e = expr; ABS { UExpr(Abs, e) }
    | NORM; e = expr; NORM { UExpr(Norm, e) }

    // Ternary expressions
    | SET; e1 = expr; e2 = expr; e3 = expr { TExpr(Set, e1, e2, e3) }
    | GEN; e1 = expr; e2 = expr; e3 = expr { TExpr(Gen, e1, e2, e3) }
    | SQUEEZE; e1 = expr; e2 = expr; e3 = expr { TExpr(Squeeze, e1, e2, e3) }

    // Factorizations
    | FAC; QR; e2 = expr { Fac(QR, e2) }
    | FAC; SVD; e2 = expr { Fac(SVD, e2) }
    | FAC; PDP; e2 = expr { Fac(PDP, e2) }
    | FAC; LU; e2 = expr { Fac(LU, e2) }
    | FAC; CR; e2 = expr { Fac(CR, e2) }

	// If and loops
    | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
    | WHILE; e1 = expr; DO; e2 = expr { While (e1, e2) }
    | FOR; x = VAR; IN; RANGE; LBRACE; e1 = expr; SEP; e2 = expr; RBRACE; e3 = expr { For (x, e1, e2, e3) }

    // Let and functions
    | LET; x = VAR; EQUALS; e1 = expr; IN; e2 = expr { Let (x, e1, e2) }
    | LET; x = VAR; EQUALS; e = expr { Assgn (x, e) }
    | FUN; LBRACE; args = separated_list(SEP, expr); RBRACE; ARROW; e = expr { Fun (args, e) }

    // Sequencing and application
    | e1 = expr; SEQ; e2 = expr; { Seq (e1, e2) }
    | e1 = expr; LBRACE; args = separated_list(SEP, expr); RBRACE {App (e1, args) }

	| LPAREN; e=expr; RPAREN {e} 
	;
