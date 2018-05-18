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
%token TRUE FALSE
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
    | top_level_expr EOF                    { $1 }
    | expr SEMICOLONS EOF                   { $1 }
;

/* It is probably possible to do the same thing as I did without detailing things
as much as I did, but I prefered doing this to insure the parser wouldn't accept
things which shouldn't be */

top_level_expr: /* this matching assures that we cannot use semicolons anywhere */
    | top_let                               { $1 }
    | expr                                  { $1 }
;

expr:
    | PRINT expr                            { PrInt $2 }
    | letin                                 { $1 }
    | IF condition THEN expr ELSE expr      { Ite ($2, $4, $6) }
    | appexpr                               { $1 }
/*    | func                                  { $1 }
;
/* brouillon */
appexpr:
    | sexpr                                 { $1 }
/*    | appexpr sexpr                         { App ($1,$2) }
;

func:
    | LET VAR EQUAL FUN VAR ARROW expr      { Fun (Var $2, Var $5, $7) }
    | LET VAR args EQUAL expr               { let rec set_f ar e = match ar with
                                                | [] -> e
                                                | v::a -> Anon_fun (v, set_f a e)
                                              in Fun (Var $2, List.hd $3, set_f (List.tl $3) $5) }
;

args:
    | VAR                                   { [Var $1] }
    | VAR args                              { (Var $1)::$2 }
;
/* re-propre */
sexpr:
    | VAR                                   { Var $1 }
    | arith                                 { $1 }
    | LPAREN expr RPAREN                    { $2 }
;

arith:
    | INT                                   { Const $1 }
    | expr PLUS expr                        { Add ($1, $3) }
    | expr TIMES expr                       { Mul ($1, $3) }
    | expr MINUS expr                       { Sub ($1, $3) }
    | expr SLASH expr					    { Div ($1, $3) }
    | MINUS expr %prec UMINUS               { Sub (Const 0, $2) }
;

top_let:
    | sub_let                               { Let (fst $1, snd $1, Const 0) }
    | lett                                  { $1 }
;

lett:
    | sub_let lett                          { Let (fst $1, snd $1, $2) }
    | sub_let SEMICOLONS                    { Let (fst $1, snd $1, Const 0) }
    | sub_let SEMICOLONS top_level_expr     { Let (fst $1, snd $1, $3) }
;

letin:
    | sub_let IN expr                       { Let (fst $1, snd $1, $3) }
;

sub_let:
    | LET VAR EQUAL expr                    { (Var $2, $4) }
    | LET UNDERSCORE EQUAL expr             { (Nil, $4) }
;

condition:
    | TRUE                                  { True }
    | FALSE                                 { False }
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
anon_func:
    | FUN VAR ARROW expr                            { Fun ($2, $4) }
    | IF condition THEN anon_func ELSE anon_func    { Ite ($2, $4, $6) }
;
*/
