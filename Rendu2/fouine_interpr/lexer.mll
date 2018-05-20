{
    open Parser;;
exception EOF;;
}

rule token = parse
    | [' ' '\t' '\n']                           { token lexbuf } (* for blanks and tabs *)

    | '+'                                       { PLUS }
    | '*'                                       { TIMES }
    | '-'                                       { MINUS }
    | '/'										{ SLASH }
    | '('                                       { LPAREN }
    | ')'                                       { RPAREN }
    | ['0'-'9']+ as s                           { INT (int_of_string s) }

    | "let"                                     { LET }
    | '='                                       { EQUAL }
    | "in"                                      { IN }
    | ";;"                                      { SEMICOLONS }
    | '_'                                       { UNDERSCORE }

    | "if"                                      { IF }
    | "then"                                    { THEN }
    | "else"                                    { ELSE }
    | "<>"                                      { NEQ }
    | "<="                                      { LEQ }
    | '<'                                       { LT }
    | ">="                                      { GEQ }
    | '>'                                       { GT }
    | "not"                                     { NOT }
    | "||"                                      { PIPES }
    | "&&"                                      { AMPERSANDS }
    | "true"                                    { TRUE }
    | "false"                                   { FALSE }

    | "prInt"                                   { PRINT }

    | "fun"                                     { FUN }
    | "->"                                      { ARROW }
    | "rec"                                     { REC }
    | "match"                                   { MATCH }
    | "with"                                    { WITH }
    | '|'                                       { PIPE }

    | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as s  { VAR s }

    | eof                                       { EOF }
