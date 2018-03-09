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
    expression EOL                              { $1 }
                                    /* on veut reconna�tre une expression */
;

/* It is probably possible to do the same thing as I did without detailing things
as much as I did, but I prefered doing this to insure the parser wouldn't accept
things which shouldn't be */

expression: /* this matching assures that we cannot use semicolons anywhere */
    | expression SEMICOLONS expression      { Iter($1,$3) }
    /* Ocaml does accept the syntax `expr ;; ;; expr`, so it's no problem that
    the following case can occur inside the previous one */
    /* Voir avec Jérémy pour Var */
    | LET VAR ASS expr SEMICOLONS           { Let(Var $2,$4,Const 0) }
    /* Voir avec Jérémy pour Anon */
    | LET ANON ASS expr SEMICOLONS          { Let(Anon,$4,Const 0) }
    | expr                                  { $1 }
;

expr:
    | arith                                 { $1 }
    | let                                   { $1 }
    | letin                                 { $1 }
    | IF condition THEN expr ELSE expr      { Ite(2$,$4,$6) }
;

arith:
    | INT                                   { Const $1 }
    | LPAREN arith RPAREN                   { $2 }
    | arith PLUS arith                      { Add($1,$3) }
    | arith TIMES arith                     { Mul($1,$3) }
    | arith MINUS arith                     { Min($1,$3) }
    | MINUS arith %prec UMINUS              { Min(Const 0, $2) }
;

let:
    | let let                               { Iter($1,$2) }
    | LET VAR ASS expr                      { Let(Var $2,$4,Const 0) }
;

letin:
    | LET VAR ASS expr IN expr              { Let(Var $2,$4,$6) }
    | LET ANON ASS expression IN expression { Let(Anon,$4,$6) }

condition:
    | arith EQ arith                        { Eq($1,$3) }
    | arith NEQ arith                       { Neq($1,$3) }
    | arith LEQ arith                       { Leq($1,$3) }
    | arith LT arith                        { Lt($1,$3) }
    | arith GEQ arith                       { Geq($1,$3) }
    | arith GT arith                        { Gt($1,$3) }
    | NOT condition                         { Not $2 }
    | condition OR condition                { Or($1,$3) }
    | condition AND condition               { And($1,$3) }
;
