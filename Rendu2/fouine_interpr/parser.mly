%{
    open Types
%}

%token <int> INT
%token <string> VAR
%token PLUS TIMES MINUS SLASH
%token LPAREN RPAREN
%token SEMICOLONS
%token LET EQUAL IN UNDERSCORE
%token IF THEN ELSE
%token NEQ LEQ LT GEQ GT
%token TRUE FALSE
%token NOT PIPES AMPERSANDS
%token PRINT
%token FUN ARROW REC MATCH WITH PIPE
%token SEMICOLON REF COLONEQ BANG
%token COMMA FST SND
%token E RAISE TRY
%token EOF

%left SEMICOLONS
%nonassoc IN
%left SEMICOLON
%nonassoc COLONEQ
%nonassoc ELSE
%nonassoc ARROW
%nonassoc COMMA
%left PLUS MINUS            /* a+b+c means (a+b)+c, etc. */
%left TIMES SLASH           /* a+b+c means (a+b)+c, etc. */
%nonassoc UMINUS            /* un "faux token", correspondant au "-" unaire */
                            /* cf. son usage plus bas : il sert � "marquer" une
                            r�gle pour lui donner la pr�c�dence maximale */
%nonassoc FST SND

%left PIPES
%left AMPERSANDS
%nonassoc NOT
%nonassoc RPAREN
%nonassoc PRINT
%nonassoc VAR INT LPAREN    /* f g x c'est (f g) x */

%start main                 /* "start" signale le point d'entr�e: */
                            /* c'est ici main, qui est d�fini plus bas */
%type <Types.expr> main      /* on _doit_ donner le type associ� au point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */


main:                       /* <- le point d'entr�e (cf. + haut, "start") */
    | top_level_expr EOF                { $1 }
;

top_level_expr:
    | expr                                      { $1 }
    | top_let                                   { $1 }
    | top_level_expr SEMICOLONS                 { $1 }
    | top_level_expr SEMICOLONS top_level_expr  { Seq ($1, $3) }
;

/* Core */

expr:
    | PRINT sexpr                       { PrInt $2 }
    | arith                             { $1 }
    | letin                             { $1 }
    | IF condition THEN expr ELSE expr  { Ite ($2, $4, $6) }
    | appexpr                           { $1 }
    | FUN args ARROW expr               { set_f $2 $4 }
    | expr SEMICOLON expr               { Seq ($1, $3) }
    | REF sexpr                         { Ref $2 }
    | VAR COLONEQ expr                  { Update (Var $1, $3) }
    | expr COMMA expr                   { Pair ($1, $3) }
    | FST expr                          { Fst $2 }
    | SND expr                          { Snd $2 }
    | E sexpr                           { Excn $2 }
    | RAISE sexpr                       { Raise $2 }
    | TRY expr WITH E VAR ARROW expr    { TryWith ($2, Var $5, $7) }
;

arith:
    | expr PLUS expr                    { Add ($1, $3) }
    | expr TIMES expr                   { Mul ($1, $3) }
    | expr MINUS expr                   { Sub ($1, $3) }
    | expr SLASH expr                   { Div ($1, $3) }
    | MINUS expr %prec UMINUS           { Sub (Const 0, $2) }
;

/* Valid arguments of functions and pseudo-functions */

sexpr:
    | INT                               { Const $1 }
    | VAR                               { Var $1 }
    | LPAREN RPAREN                     { Skip }
    | bang                              { $1 }
    | LPAREN expr RPAREN                { $2 }
;

bang:
    | BANG VAR                          { Bang (Var $2) }
    | LPAREN BANG VAR RPAREN            { Bang (Var $3) }
    | BANG bang                         { Bang $2 }
;

/* Let */

top_let:
    | sub_let                           { set_let $1 None }
    | sub_let top_let                   { Seq (set_let $1 None, $2) }
;

letin:
    | sub_let IN expr                   { set_let $1 (Some $3) }
;

sub_let:
    | LET var EQUAL expr                { $2, $4 }
    | LET VAR args EQUAL expr           { Var $2, set_f $3 $5 }
;

var:
    | UNDERSCORE                        { Nil }
    | VAR                               { Var $1 }
    | VAR COMMA VAR                     { Pair (Var $1, Var $3) }
    | LPAREN var RPAREN                 { $2 }
;

args:
    | VAR                               { [Var $1] }
    | UNDERSCORE                        { [Nil] }
    | VAR args                          { (Var $1)::$2 }
    | UNDERSCORE args                   { Nil::$2 }
;

/* Functions */

appexpr:
    | sexpr                             { $1 }
    | appexpr sexpr                     { App ($1, $2) }
;

/* Conditions for Ite */

condition:
    | TRUE                              { True }
    | FALSE                             { False }
    | expr EQUAL expr                   { Eq ($1, $3) }
    | expr NEQ expr                     { Neq ($1, $3) }
    | expr LEQ expr                     { Leq ($1, $3) }
    | expr LT expr                      { Lt ($1, $3) }
    | expr GEQ expr                     { Geq ($1, $3) }
    | expr GT expr                      { Gt ($1, $3) }
    | NOT condition                     { Not $2 }
    | condition PIPES condition         { Or ($1, $3) }
    | condition AMPERSANDS condition    { And ($1, $3) }
;
