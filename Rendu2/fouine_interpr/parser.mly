%{
    open Expr
%}

%token <int> INT            /* le lex�me INT a un attribut entier */
%token <string> VAR         /* le lex�me INT a un attribut entier */
%token PLUS TIMES MINUS
%token LPAREN RPAREN
%token SEMICOLONS
%token LET ASS IN ANON
%token IF THEN ELSE
%token EQ NEQ LEQ LT GEQ GT
%token NOT OR AND
%token EOL                  /* retour � la ligne */

%nonassoc IN
%left PLUS MINUS            /* associativit� gauche: a+b+c, c'est (a+b)+c */
%left TIMES                 /* associativit� gauche: a*b*c, c'est (a*b)*c */
%nonassoc UMINUS            /* un "faux token", correspondant au "-" unaire */
                            /* cf. son usage plus bas : il sert � "marquer" une
                            r�gle pour lui donner la pr�c�dence maximale */

%left OR
%left AND

%start main                 /* "start" signale le point d'entr�e: */
                            /* c'est ici main, qui est d�fini plus bas */
%type <Expr.expr> main      /* on _doit_ donner le type associ� au point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */


main:                       /* <- le point d'entr�e (cf. + haut, "start") */
    expression                  { $1 }
;

/* It is probably possible to do the same thing as I did without detailing things
as much as I did, but I prefered doing this to insure the parser wouldn't accept
things which shouldn't be */

expression: /* this matching assures that we cannot use semicolons anywhere */
    | let SEMICOLONS expression     { Iter($1,$3) }
    | expr                          { $1 }
;

expr:
    | arith                         { $1 }
    | letin                         { $1 }
/*    | IF condition THEN expr ELSE expr      { Ite(2$,$4,$6) } */
;

arith:
    | INT                           { Const $1 }
    | LPAREN arith RPAREN           { $2 }
    | arith PLUS arith              { Add($1,$3) }
    | arith TIMES arith             { Mul($1,$3) }
    | arith MINUS arith             { Min($1,$3) }
    | MINUS arith %prec UMINUS      { Min(Const 0, $2) }
;

let:
    | LET VAR ASS expr let          { Iter(Let(Var $2,$4,Const 0),$5) }
    | LET VAR ASS expr              { Let(Var $2,$4,Const 0) }
    | LET ANON ASS expr let          { Iter(Let(Anon,$4,Const 0),$5) }
    | LET ANON ASS expr              { Let(Anon,$4,Const 0) }
;

letin:
    | LET VAR ASS expr IN expr      { Let(Var $2,$4,$6) }
    | LET ANON ASS expr IN expr     { Let(Anon,$4,$6) }
/*
condition:
    | arith EQ arith                { Eq($1,$3) }
    | arith NEQ arith               { Neq($1,$3) }
    | arith LEQ arith               { Le($1,$3) }
    | arith LT arith                { Lt($1,$3) }
    | arith GEQ arith               { Ge($1,$3) }
    | arith GT arith                { Gt($1,$3) }
    | NOT condition                 { Not $2 }
    | condition OR condition        { Or($1,$3) }
    | condition AND condition       { And($1,$3) }
;
*/
