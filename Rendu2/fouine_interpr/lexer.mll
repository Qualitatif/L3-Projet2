{
    open Parser;;
exception Eof;;
}

rule token = parse
    | [' ' '\t']        { token lexbuf } (* for blanks and tabs *)
    | '\n'              { EOL }

    | '+'               { PLUS }
    | '*'               { TIMES }
    | '-'               { MINUS }
    | '('               { LPAREN }
    | ')'               { RPAREN }
    | ['0'-'9']+ as s   { INT (int_of_string s) }

    | ['a'-'z']['a'-'z' 'A'-'Z']* as s   { VAR (s) } (* TODO : Check compatibility with other rules *)
    | "let"             { LET }
    | '='               { ASS }
    | "in"              { IN }
    | ";;"              { ENDLET }
    | '_'               { ANON }

    | "if"              { IF }
    | "then"            { THEN }
    | "else"            { ELSE }
    | "=="              { EQ }
    | "<>"              { NEQ }
    | "<="              { LEQ }
    | '<'               { LT }
    | ">="              { GEQ }
    | '>'               { GT }
    | "||"              { OR }
    | "&&"              { AND }

    | "prInt"           { PRINT }

    | eof               { raise Eof }
