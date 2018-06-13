open Types

let caps = ref false (* for the -shout option *)

(* display function for the debug mode *)

let rec display_expr e =
let aff_aux s ar =
(* I use a pair of lists here because the number of arguments varies between 1 and 3 and they can be either of type cond or of type expr *)
    let rec give_args ar = match ar with
        | [],[]     ->  print_string ")"
        | [],[e]    ->  display_expr e; print_string ")"
        | [],h::t   ->  display_expr h;
                        print_string ", ";
                        give_args ([],t)
        | [cd],[]   ->  display_cond cd; print_string ")"
        | h::t,l    ->  display_cond h;
                        print_string ", ";
                        give_args (t,l)

in begin
   print_string s;
   give_args ar;
end

in match e with
    | Const k       -> print_int k
    | Var v         -> print_string v
    | Add(e1,e2)    -> aff_aux (if !caps then "ADD(" else "Add(") ([],[e1;e2])
    | Mul(e1,e2)    -> aff_aux (if !caps then "MUL(" else "Mul(") ([],[e1;e2])
    | Sub(e1,e2)    -> aff_aux (if !caps then "SUB(" else "Sub(") ([],[e1;e2])
    | Div(e1,e2)    -> aff_aux (if !caps then "DIV(" else "Div(") ([],[e1;e2])
    | PrInt e       -> aff_aux (if !caps then "PRINT(" else "PrInt(") ([],[e])
    | Let(e1,e2,o)  -> (match o with
                            | None      -> (aff_aux (if !caps then "LET(" else "Let(") ([],[e1;e2]))
                            | Some e3    -> (aff_aux (if !caps then "LET(" else "Let(") ([],[e1;e2;e3])))
    | Ite(cd,e1,e2) -> aff_aux (if !caps then "ITE(" else "Ite(") ([cd],[e1;e2])
    | Fun(e1,e2)    -> aff_aux (if !caps then "FUN(" else "Fun(") ([],[e1;e2])
    | App(e1,e2)    -> aff_aux (if !caps then "APP(" else "App(") ([],[e1;e2])
    | Skip          -> print_string (if !caps then "SKIP" else "Skip")
    | Seq(e1,e2)    -> aff_aux (if !caps then "SEQ(" else "Seq(") ([],[e1;e2])
    | Ref r         -> aff_aux (if !caps then "REF(" else "Ref(") ([],[r])
    | Bang e        -> aff_aux (if !caps then "BANG(" else "Bang(") ([],[e])
    | Update(e1,e2) -> aff_aux (if !caps then "UPDATE(" else "Update(") ([],[e1;e2])
    | Pair(e1,e2)   -> aff_aux (if !caps then "PAIR(" else "Pair(") ([],[e1;e2])
    | Fst e         -> aff_aux (if !caps then "FST(" else "Fst(") ([],[e])
    | Snd e         -> aff_aux (if !caps then "SND(" else "Snd(") ([],[e])
    | Excn e        -> aff_aux (if !caps then "BANG(" else "Bang(") ([],[e])
    | Raise e       -> aff_aux (if !caps then "RAISE(" else "Raise(") ([],[e])
    | TryWith(e1,e2,e3) -> aff_aux (if !caps then "TRYWITH(" else "TryWith(") ([],[e1;e2;e3])
    | _ -> ()

(* exactly the same thing, but for conditions *)
and display_cond cd =
let aff_aux s ar =
    let rec give_args ar = match ar with
        | [],[]     ->  print_string ")"
        | [],[e]    ->  display_expr e; print_string ")"
        | [],h::t   ->  display_expr h;
                        print_string ", ";
                        give_args ([],t)
        | [cd],[]   ->  display_cond cd; print_string ")"
        | h::t,l    ->  display_cond h;
                        print_string ", ";
                        give_args (t,l)

in begin
   print_string s;
   give_args ar;
end

in match cd with
    | True          -> print_string (if !caps then "TRUE" else "True")
    | False         -> print_string (if !caps then "FALSE" else "False")
    | Eq(e1,e2)     -> aff_aux (if !caps then "EQ(" else "Eq(") ([],[e1;e2])
    | Neq(e1,e2)    -> aff_aux (if !caps then "NEQ(" else "Neq(") ([],[e1;e2])
    | Leq(e1,e2)    -> aff_aux (if !caps then "LEQ(" else "Leq(") ([],[e1;e2])
    | Lt(e1,e2)     -> aff_aux (if !caps then "LT(" else "Lt(") ([],[e1;e2])
    | Geq(e1,e2)    -> aff_aux (if !caps then "GEQ(" else "Geq(") ([],[e1;e2])
    | Gt(e1,e2)     -> aff_aux (if !caps then "GT(" else "Gt(") ([],[e1;e2])
    | Not(cd)       -> aff_aux (if !caps then "NOT(" else "Not(") ([cd],[])
    | Or(c1,c2)     -> aff_aux (if !caps then "OR(" else "Or(") ([c1;c2],[])
    | And(c1,c2)    -> aff_aux (if !caps then "AND(" else "And(") ([c1;c2],[])
