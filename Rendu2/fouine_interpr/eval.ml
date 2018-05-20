open Expr

type closure = expr * expr * env
and env = (string * value) list
and value = I of int | Cl of closure

(* sémantique opérationnelle à grands pas *)
let verbose = ref false (* for the -debug option *)
let maxVerbose = ref false (* for the -debug+ option *)
let toPrInt = ref false (* for the "function" prInt *)
let giveEnv = ref false (* for the (yet unfunctional) -env option *)
(*
let rec print_env ev = (* -env option, but it doesn't work yet *)
    match ev with
    | []         -> print_string "]\n"
    | [(vr,vl)]  -> (match vl with
                        | I(x)          ->  print_string ("(" ^ vr ^ ", " ^ (string_of_int x) ^ ")]\n")
                        | Cl(x, ex, ev) ->  (match x with
                                                | Nil   -> print_string ("(" ^ vr ^ ", (" ^ Nil)
                                                | Var v -> print_string ("(" ^ vr ^ ", (" ^ v)
                                                | _     -> failwith "Not a correct closure"
                                                (* should not happen *));
                                            display_expr ex;
                                            print_env ev;
                                            print_string ")]\n")
    | (vr,vl)::q -> (match vl with
                        | I(x)          ->  print_string ("(" ^ vr ^ ", " ^ (string_of_int x) ^ ")]\n")
                        | Cl(f, ex, ev) ->  print_string ("(" ^ vr ^ ", (" ^ f);
                                            display_expr ex;
                                            print_env ev;
                                            print_string ");\n")
*)

(* I modified the following function from what I took on the website. This is for the purposes of implementing the -debug+ and -env options *)
let p_verb s ev =   if !maxVerbose
                        then ( (* if !giveEnv
                            then (Format.printf "%s in [" s; print_env ev)
                            else *) Format.printf "%s\n" s)
                    else ()

(* Not much to say here. I just added additional operators to the list given on the website's examples. *)
let rec eval ex ev = match ex with
    | Nil               -> I(0) (* Just for the pattern matching to be exhaustive so that compilation give no warning about that matter. This case actually cannot happen. *)
    | Const k           -> I(k)
    | Var v             -> (p_verb "Reading var" ev;
                        try
                            (if String.compare (fst (List.hd ev)) v == 0
                                then snd (List.hd ev)
                                else eval (Var v) (List.tl ev))
                        with _ -> failwith ("Unbound value " ^ v))
    | Add(e1,e2)        -> (p_verb "Adding" ev;
                            match eval e1 ev, eval e2 ev with
                                | I(x), I(y)    -> I(x + y)
                                | _             -> failwith "Typing error on addition")
    | Mul(e1,e2)        -> (p_verb "Multipliying" ev;
                            match eval e1 ev, eval e2 ev with
                                | I(x), I(y)    -> I(x * y)
                                | _             -> failwith "Typing error on multiplication")
    | Sub(e1,e2)        -> (p_verb "Substracting" ev;
                            match eval e1 ev, eval e2 ev with
                                | I(x), I(y)    -> I(x - y)
                                | _             -> failwith "Typing error on substraction")
    | Div(e1,e2)        -> (p_verb "Dividing" ev;
                            match eval e1 ev, eval e2 ev with
                                | I(x), I(y)    -> I(x / y)
                                | _             -> failwith "Typing error on division")
    | PrInt(e)          -> (p_verb "PrInting" ev;
                        match eval e ev with
                            | I(x)          -> (print_int x; print_newline();
                                                I(x))
                            | _             -> failwith "Invalid argument for prInt");
    | Let(e1,e2,e3)     -> (p_verb "Declaring" ev;
                            match e1 with
                                | Nil   -> let _ = eval e2 ev
                                    in eval e3 ev
                                | Var v -> let vl = eval e2 ev
                                    in eval e3 ((v,vl)::ev)
                                | _     -> failwith "Typing error: tryed to assign an expression to a non-valid term")
                                    (* Nota: Such a case actually should not pass the parser. *)
    | Ite(cd,e1,e2)     -> (p_verb "If Then Else" ev;
                            let cnd = (eval_cond cd ev) in if cnd
                                then (eval e1 ev)
                                else (eval e2 ev))
    | Fun(e1,e2)        -> (p_verb "Function" ev;
                            Cl(e1,e2,ev))
    | App(e1,e2)        -> (p_verb "Applying" ev;
                            let f = eval e1 ev
                            in match f with
                            | Cl(e3,e4,ev1) -> (match e3 with
                                                | Nil   -> eval e4 ev1 (* Such a function's expression doesn't depend on its argument *)
                                                | Var v -> eval e4 ((v, eval e2 ev)::ev1)
                                                | _     -> failwith "Invalid argument")
                            | _             -> failwith "Typing error: tryed to apply what is no function")

(* This is wholly new and follows exactly the same scheme as the previous function, except it works for conditions instead of expressions *)
and eval_cond cd ev = match cd with
    | True -> true
    | False -> false
    | Leq(e1,e2) -> (let x = eval e1 ev
                        in let y = eval e2 ev
                            in x <= y)
    | Lt(e1,e2) -> (let x = eval e1 ev
                        in let y = eval e2 ev
                            in x < y)
    | Geq(e1,e2) -> (let x = eval e1 ev
                        in let y = eval e2 ev
                            in x >= y)
    | Gt(e1,e2) -> (let x = eval e1 ev
                        in let y = eval e2 ev
                            in x > y)
    | Eq(e1,e2) -> (let x = eval e1 ev
                        in let y = eval e2 ev
                            in x == y)
    | Neq(e1,e2) -> (let x = eval e1 ev
                        in let y = eval e2 ev
                            in x <> y)
    | And(c1,c2) -> (let b1 = eval_cond c1 ev
                        in let b2 = eval_cond c2 ev
                            in b1 && b2)
    | Or(c1,c2) -> (let b1 = eval_cond c1 ev
                        in let b2 = eval_cond c2 ev
                            in b1 || b2)
    | Not(cd) -> not (eval_cond cd ev)
