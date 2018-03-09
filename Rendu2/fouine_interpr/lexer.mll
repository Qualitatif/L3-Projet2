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
    | "||"                                      { OR }
    | "&&"                                      { AND }

    | "prInt"                                   { PRINT }

    | "fun"                                     { FUN }
    | "->"                                      { ARROW }
    | "rec"                                     { REC }

    | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* as s  { VAR s }

    | eof                                       { raise Eof }
