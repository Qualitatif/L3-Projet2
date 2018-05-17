open Expr

type closure = string * expr * env
and env = (string * value) list
and value = I of int | Cl of closure

(* sémantique opérationnelle à grands pas *)
let maxVerbose = ref false
let toPrInt = ref false
let giveEnv = ref false

let rec print_env ev = (* this option doesn't work yet *)
    match ev with
    | []         -> print_string "]\n"
    | [(vr,vl)]  -> (match vl with
                        | I(x)          ->  print_string ("(" ^ vr ^ ", " ^ (string_of_int x) ^ ")]\n")
                        | Cl(f, ex, ev) ->  print_string ("(" ^ vr ^ ", (" ^ f);
                                            display_expr ex;
                                            print_env ev;
                                            print_string ")]\n")
    | (vr,vl)::q -> (match vl with
                        | I(x)          ->  print_string ("(" ^ vr ^ ", " ^ (string_of_int x) ^ ")]\n")
                        | Cl(f, ex, ev) ->  print_string ("(" ^ vr ^ ", (" ^ f);
                                            display_expr ex;
                                            print_env ev;
                                            print_string ");\n")

let p_verb s ev =   if !maxVerbose
                        then (if !giveEnv
                            then (Format.printf "%s in [" s; print_env ev)
                            else Format.printf "%s\n" s)
                    else ()

let rec eval ex ev = match ex with
    | Const k           -> k
    | Var v             -> (p_verb "Reading var" ev;
                        try
                            (if String.compare (fst (List.hd ev)) v == 0
                                then match snd (List.hd ev) with
                                    | I(x)  -> x
                                    | _     -> 0 (* case to consider later on
                                                    when there will be functions *)
                                else eval (Var v) (List.tl ev))
                        with _ -> failwith ("Unbound value " ^ v))
    | Add(e1,e2)        -> (p_verb "Adding" ev;
                        eval e1 ev + eval e2 ev)
    | Mul(e1,e2)        -> (p_verb "Multipliying" ev;
                        eval e1 ev * eval e2 ev)
    | Sub(e1,e2)        -> (p_verb "Substracting" ev;
                        eval e1 ev - eval e2 ev)
    | Div(e1,e2)        -> (p_verb "Dividing" ev;
                        eval e1 ev / eval e2 ev)
    | PrInt(e)          -> (p_verb "PrInting" ev;
                        let x = eval e ev in (print_int x; print_newline(); x))
    | Let(e1,e2,e3)     -> (p_verb "Declaring" ev;
                            match e1 with
                                | Var v -> let x = eval e2 ev
                                    in eval e3 ((v,I(x))::ev)
                                | _     -> failwith "How did you trick the parser?!")
                                    (* This case shouldn't happen *)
    | Let_anon(e1,e2)   -> (p_verb "Anonymous declaration" ev;
                            let _ = eval e1 ev in eval e2 ev)
    | Ite(cd,e1,e2)     -> (p_verb "If Then Else" ev;
                            let cnd = (eval_cond cd ev) in if cnd
                                then (eval e1 ev)
                                else (eval e2 ev))
    | _ -> 0

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
