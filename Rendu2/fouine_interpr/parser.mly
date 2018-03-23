%{
    open Expr
%}

%token <int> INT            /* le lex�me INT a un attribut entier */
%token <string> VAR         /* le lex�me INT a un attribut entier */
%token PLUS TIMES MINUS
%token LPAREN RPAREN
%token SEMICOLONS
%token LET EQUAL IN UNDERSCORE
%token IF THEN ELSE
%token NEQ LEQ LT GEQ GT
%token NOT OR AND
%token PRINT
%token FUN ARROW REC


%nonassoc IN
%nonassoc ELSE
%left PLUS MINUS            /* associativit� gauche: a+b+c, c'est (a+b)+c */
%left TIMES                 /* associativit� gauche: a*b*c, c'est (a*b)*c */
%nonassoc UMINUS            /* un "faux token", correspondant au "-" unaire */
                            /* cf. son usage plus bas : il sert � "marquer" une
                            r�gle pour lui donner la pr�c�dence maximale */

%left OR
%left AND
%nonassoc NOT

%nonassoc RPAREN
%nonassoc VAR               /* f g x c'est (f g) x */

%start main                 /* "start" signale le point d'entr�e: */
                            /* c'est ici main, qui est d�fini plus bas */
%type <Expr.expr> main      /* on _doit_ donner le type associ� au point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */


main:                       /* <- le point d'entr�e (cf. + haut, "start") */
    top_level_expr                      { $1 }
;

/* It is probably possible to do the same thing as I did without detailing things
as much as I did, but I prefered doing this to insure the parser wouldn't accept
things which shouldn't be */

top_level_expr: /* this matching assures that we cannot use semicolons anywhere */
    | lett SEMICOLONS top_level_expr    { Semis ($1, $3) }
    | expr                              { $1 }
;

expr:
    | VAR                               { Var $1 }
    | arith                             { $1 }
    | letin                             { $1 }
    | IF condition THEN expr ELSE expr  { Ite ($2, $4, $6) }
    | PRINT LPAREN expr RPAREN          { PrInt $3 }
    | op input                          { App ($1, $2) }
;

arith:
    | INT                               { Const $1 }
    | expr PLUS expr                    { Add ($1, $3) }
    | expr TIMES expr                   { Mul ($1, $3) }
    | expr MINUS expr                   { Sub ($1, $3) }
    | MINUS expr %prec UMINUS           { Sub (Const 0, $2) }
;

lett:
    | LET VAR EQUAL expr lett           { Let (Var $2, $4, $5) }
    | LET VAR EQUAL expr                { Let (Var $2, $4, Const 0) }
    | LET UNDERSCORE EQUAL expr lett    { Let_anon ($4, $5) }
    | LET UNDERSCORE EQUAL expr         { Let_anon ($4, Const 0) }
;

letin:
    | LET VAR EQUAL expr IN expr        { Let (Var $2, $4, $6) }
    | LET UNDERSCORE EQUAL expr IN expr { Let_anon ($4, $6) }
    | LET VAR EQUAL func IN expr        { Let (Var $2, $4, $6) }
;

condition:
    | expr EQUAL expr                   { Eq ($1, $3) }
    | expr NEQ expr                     { Neq ($1, $3) }
    | expr LEQ expr                     { Le ($1, $3) }
    | expr LT expr                      { Lt ($1, $3) }
    | expr GEQ expr                     { Ge ($1, $3) }
    | expr GT expr                      { Gt ($1, $3) }
    | NOT condition                     { Not $2 }
    | condition OR condition            { Or ($1, $3) }
    | condition AND condition           { And ($1, $3) }
;

func:
    | FUN VAR ARROW expr                { Fun ($2, $4) }
    | IF condition THEN func ELSE func  { Ite ($2, $4, $6) }
;

op:
    | VAR                               { Var $1 }
    | LPAREN func RPAREN                { $2 }
    | op input                          { App ($1, $2) }
    | LPAREN op RPAREN                  { $2 }
;

input:
    | VAR                               { Var $1 }
    | INT                               { Const $1 }
    | LPAREN expr RPAREN                { $2 }
;
