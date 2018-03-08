%{
    open Expr
%}

%token <int> INT       /* le lex�me INT a un attribut entier */
%token PLUS TIMES MINUS
%token LPAREN RPAREN
%token EOL             /* retour � la ligne */

%left PLUS MINUS  /* associativit� gauche: a+b+c, c'est (a+b)+c */
%left TIMES  /* associativit� gauche: a*b*c, c'est (a*b)*c */
%nonassoc UMINUS  /* un "faux token", correspondant au "-" unaire */
                  /* cf. son usage plus bas : il sert � "marquer" une r�gle pour lui donner la pr�c�dence maximale */


%start main             /* "start" signale le point d'entr�e: */
                        /* c'est ici main, qui est d�fini plus bas */
%type <Expr.expr> main     /* on _doit_ donner le type associ� au point d'entr�e */

%%
    /* --- d�but des r�gles de grammaire --- */
                            /* � droite, les valeurs associ�es */


main:                       /* <- le point d'entr�e (cf. + haut, "start") */
expression EOL                { $1 }  /* on veut reconna�tre une expression */
  ;

expression:			    /* r�gles de grammaire pour les expressions */
    | INT                                               { Const $1 }
    | LPAREN expression RPAREN                          { $2 } /* on r�cup�re le deuxi�me �l�ment */
    | expression PLUS expression                        { Add($1,$3) }
    | expression TIMES expression                       { Mul($1,$3) }
    | expression MINUS expression                       { Min($1,$3) }
    | MINUS expression %prec UMINUS                     { Min(Const 0, $2) }
    | let                                               { $1 }

    | IF condition THEN expression ELSE expression      { Ite(2$,$4,$6) }
;

let:
    | LET expression ASS expression let                 { Let($2,$4,$5) }
    | LET expression ASS expression IN expression       { Let($2,$4,$6) }
    | LET expression ASS expression ENDLET              { Let($2,$4,Const 0) }
    | LET expression ASS expression ENDLET expression   { Let($2,$4,$6) }
    /* Voir avec Jérémy */
    | LET ANON ASS expression IN expression             { Let(Anon,$4,$6) }
    | LET ANON ASS expression ENDLET              { Let(Anon,$4,Const 0) }
    | LET ANON ASS expression ENDLET expression   { Let(Anon,$4,$6) }

;

condition:
  | expression EQ expression { Eq($2,$3) }
  | expression NEQ expression { Neq($2,$3) }
  | expression LEQ expression { Leq($2,$3) }
  | expression LT expression { Lt($2,$3) }
  | expression GEQ expression { Geq($2,$3) }
  | expression GT expression { Gt($2,$3) }
  | NOT condition { Not $2 }
  | condition OR condition { Or($2,$3) }
  | condition AND condition { And($2,$3) }
;
