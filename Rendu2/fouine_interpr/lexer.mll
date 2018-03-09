{
    open Parser;;
exception Eof;;
}

rule token = parse
    | [' ' '\t' '\n']                           { token lexbuf } (* for blanks and tabs *)

    | '+'                                       { PLUS }
    | '*'                                       { TIMES }
    | '-'                                       { MINUS }
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | ['0'-'9']+ as s                           { INT (int_of_string s) }

    | "let"                                     { LET }
    | '='                                       { ASS }
    | "in"                                      { IN }
    | ";;"                                      { SEMICOLONS }
    | '_'                                       { ANON }

    | "if"                                      { IF }
    | "then"                                    { THEN }
    | "else"                                    { ELSE }
    | "=="                                      { EQ }
    | "<>"                                      { NEQ }
    | "<="                                      { LEQ }
    | '<'                                       { LT }
    | ">="                                      { GEQ }
    | '>'                                       { GT }
    | "not"                                     { NOT }
    | "||"                                      { OR }
    | "&&"                                      { AND }

    | "prInt"                                   { PRINT }

    | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as s  { VAR s }

    | eof                                       { raise Eof }
