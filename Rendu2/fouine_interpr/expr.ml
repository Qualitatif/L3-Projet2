(* We have two recursive types working together. *)

type expr =
    | Nil
    | Const of int
    | Var of string
    | Add of expr * expr
    | Mul of expr * expr
    | Sub of expr * expr
    | Div of expr * expr
    | Let of expr * expr * expr
    | Ite of cond * expr * expr
    | PrInt of expr
    | Semis of expr * expr
    | Fun of expr * expr * expr
    | Anon_fun of expr * expr
    | App of expr * expr

and cond =
    | True
    | False
    | Leq of expr * expr
    | Lt of expr * expr
    | Geq of expr * expr
    | Gt of expr * expr
    | Eq of expr * expr
    | Neq of expr * expr
    | And of cond * cond
    | Or of cond * cond
    | Not of cond

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
        | PrInt(e)      -> aff_aux (if !caps then "PRINT(" else "PrInt(") ([],[e])
        | Let(e1,e2,e3) -> aff_aux (if !caps then "LET(" else "Let(") ([],[e1;e2;e3])
        | Ite(cd,e1,e2) -> aff_aux (if !caps then "ITE(" else "Ite(") ([cd],[e1;e2])
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
