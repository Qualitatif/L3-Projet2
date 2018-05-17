%{
    open Expr
%}

%token <int> INT
%token <string> VAR
%token PLUS TIMES MINUS SLASH
%token LPAREN RPAREN
%token SEMICOLONS
%token LET EQUAL IN UNDERSCORE
%token IF THEN ELSE
%token NEQ LEQ LT GEQ GT
%token NOT PIPES AMPERSANDS
%token PRINT
%token FUN ARROW REC
%token EOF

%nonassoc IN
%nonassoc ELSE
%left PLUS MINUS            /* a+b+c means (a+b)+c, etc. */
%left TIMES SLASH           /* a+b+c means (a+b)+c, etc. */
%nonassoc UMINUS            /* un "faux token", correspondant au "-" unaire */
                            /* cf. son usage plus bas : il sert � "marquer" une
                            r�gle pour lui donner la pr�c�dence maximale */

%left PIPES
%left AMPERSANDS
%nonassoc NOT
%nonassoc RPAREN
%nonassoc PRINT
%nonassoc VAR INT LPAREN    /* f g x c'est (f g) x */

%start main                 /* "start" signale le point d'entr�e: */
                            /* c'est ici main, qui est d�fini plus bas */
%type <Expr.expr> main      /* on _doit_ donner le type associ� au point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */


main:                       /* <- le point d'entr�e (cf. + haut, "start") */
    top_level_expr EOF                      { $1 }
;

/* It is probably possible to do the same thing as I did without detailing things
as much as I did, but I prefered doing this to insure the parser wouldn't accept
things which shouldn't be */

top_level_expr: /* this matching assures that we cannot use semicolons anywhere */
    | lett                                  { $1 }
    | expr                                  { $1 }
/*    | top_level_expr SEMICOLONS top_level_expr { Let_anon ($1, $3) } */
;

/* brouillon */

expr:
    | VAR                                   { Var $1 }
    | arith                                 { $1 }
    | PRINT expr                            { PrInt $2 }
    | LPAREN expr RPAREN                    { $2 }
    | letin                                 { $1 }
    | IF condition THEN expr ELSE expr      { Ite ($2, $4, $6) }
/*    | appexpr                             { $1 } */
/*    | FUN VAR ARROW expr                  {  } */
;
/*
appexpr :
    | sexpr                                 { $1 }
    | appexpr sexpr                         { App($1,$2) }
; */

arith:
    | INT                                   { Const $1 }
    | expr PLUS expr                        { Add ($1, $3) }
    | expr TIMES expr                       { Mul ($1, $3) }
    | expr MINUS expr                       { Sub ($1, $3) }
    | expr SLASH expr					    { Div ($1, $3) }
    | MINUS expr %prec UMINUS               { Sub (Const 0, $2) }
;

/* plusBrouillon */
/*
expr:
    | arith                                 { $1 }
    | letin                                 { $1 }
    | IF condition THEN expr ELSE expr      { Ite ($2, $4, $6) }
    | PRINT input        				    { PrInt $2 }
    | expr input                            { App ($1, $2) }
    | input								    { $1 }
;
*/
lett:
    | LET VAR EQUAL expr lett               { Let (Var $2, $4, $5) }
    | LET VAR EQUAL expr SEMICOLONS         { Let (Var $2, $4, Const 0) }
    | LET VAR EQUAL expr SEMICOLONS top_level_expr    { Let (Var $2, $4, $6) }
/*    | LET UNDERSCORE EQUAL expr lett      { Let_anon ($4, $5) }
    | LET UNDERSCORE EQUAL expr             { Let_anon ($4, Const 0) } */
;

letin:
    | LET VAR EQUAL expr IN                 { Let (Var $2, $4, Const 0) }
/*    | LET UNDERSCORE EQUAL expr IN        { Let_anon ($4, Const 0) } */
    | LET VAR EQUAL expr IN expr            { Let (Var $2, $4, $6) }
/*    | LET UNDERSCORE EQUAL expr IN expr   { Let_anon ($4, $6) }
    | LET VAR EQUAL func IN expr            { Let (Var $2, $4, $6) } */
;

condition:
    | expr EQUAL expr                       { Eq ($1, $3) }
    | expr NEQ expr                         { Neq ($1, $3) }
    | expr LEQ expr                         { Leq ($1, $3) }
    | expr LT expr                          { Lt ($1, $3) }
    | expr GEQ expr                         { Geq ($1, $3) }
    | expr GT expr                          { Gt ($1, $3) }
    | NOT condition                         { Not $2 }
    | condition PIPES condition             { Or ($1, $3) }
    | condition AMPERSANDS condition        { And ($1, $3) }
;
/*
func:
    | FUN VAR ARROW expr                    { Fun ($2, $4) }
    | IF condition THEN func ELSE func      { Ite ($2, $4, $6) }
;

sexpr:
    | VAR                                   { Var $1 }
    | INT                                   { Const $1 }
    | LPAREN expr RPAREN                    { $2 }
;

*/
