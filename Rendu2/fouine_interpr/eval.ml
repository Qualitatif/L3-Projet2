open Types
(* open Display *)

let size = 1000
let ref_tab = Array.make size (I 0) ;;
for i = 0 to 999
    do ref_tab.(i) <- (I 0)
done ;;
(* This is the memory for references. To each reference we associate an id which tells where to look for its value in the memory. *)

let verbose = ref false (* for the -debug option *)
let maxVerbose = ref false (* for the -debug+ option *)
let toPrInt = ref false (* for the "function" prInt *)

(* I modified the following function from what I took on the website. This is for the purposes of implementing the -debug+ and -env options *)
let p_verb s =  if !maxVerbose
                    then (Format.printf "%s\n" s)
                    else ()

(* Not much to say here. I just added additional operators to the list given on the website's examples. *)
let glob_ev = ref [] (* for imperative programs *)

let free_id = ref 0 (* Indicates the first free emplacement in the memory table *)

let rec typeMatch a b = match a, b with
    | I x, I y      -> true
    | Id j, Id k    -> typeMatch ref_tab.(j) ref_tab.(k)
    | Cl c1, Cl c2  -> true
    | E x, E y      -> true
    | _             -> false
(* Type check for references updates. *)

let rec eval ex ev = match ex with
    | Nil               -> I 0 (* Just for pattern matching completeness *)
    | Skip              -> (p_verb "Skipping"; S)
    | Const k           -> I k
    | Var v             -> (p_verb "Reading var";
                        try
                            (if String.compare (fst (List.hd ev)) v == 0
                                then snd (List.hd ev)
                                else eval (Var v) (List.tl ev))
                        with _ ->
                                try (eval (Var v) !glob_ev)
                                with _ -> failwith ("Unbound value " ^ v))
    | Ref r             ->  (if !free_id < 999
                                then let e = eval r ev in
                                        let i = !free_id in
                                            (ref_tab.(i) <- e ;
                                            free_id := i + 1 ;
                                            Id i)
                                else failwith "Full memory")
    | Bang r            -> (p_verb "Bang";
                            let vl = eval r ev in match vl with
                                | Id i  -> ref_tab.(i)
                                | _     -> failwith "Expected 'a ref on content call")
    | Update (e1, e2)   -> (p_verb "Updating";
                            match eval e2 ev with
                                | E x   -> E x
                                | vl    -> (match eval e1 ev with
                                    | Id i  -> (let ct = ref_tab.(i)
                                        in if typeMatch ct vl
                                            then (ref_tab.(i) <- vl ; S)
                                            else failwith "Typing error on update")
                                    | _     -> failwith "Expected 'a ref on update"))
    | Add (e1, e2)      -> (p_verb "Adding";
                            match eval e1 ev, eval e2 ev with
                                | I x, I y      -> I (x + y)
                                | _, E y        -> E y (* I checked with OCaml, in case we have sum two exceptions the one which is raised is the right one *)
                                | E x, I y      -> E x
                                | _             -> failwith "Expected int on addition")
    | Mul (e1, e2)      -> (p_verb "Multipliying";
                            match eval e1 ev, eval e2 ev with
                                | I x, I y      -> I (x * y)
                                | _, E y        -> E y
                                | E x, I y      -> E x
                                | _             -> failwith "Expected int on multiplication")
    | Sub (e1, e2)      -> (p_verb "Substracting";
                            match eval e1 ev, eval e2 ev with
                                | I x, I y      -> I (x - y)
                                | _, E y        -> E y
                                | E x, I y      -> E x
                                | _             -> failwith "Expected int on substraction")
    | Div (e1, e2)      -> (p_verb "Dividing";
                            match eval e1 ev, eval e2 ev with
                                | I x, I y      -> I (x / y)
                                | _, E y        -> E y
                                | E x, I y      -> E x
                                | _             -> failwith "Expected int on division")
    | PrInt e           -> (p_verb "PrInting";
                            match eval e ev with
                                | I x           -> (print_int x; print_newline();
                                                I x)
                                | E x           -> E x
                                | _             -> failwith "Expected int on prInt");
    | Let (e1, e2, o)   -> (p_verb "Declaring";
                            match eval e2 ev with
                                | E x   -> E x
                                | vl    -> (match e1,o with
                                    | Nil, None         -> vl
                                    | Nil, Some e3      -> eval e3 ev
                                    | Var v, None       -> glob_ev := (v,vl)::(!glob_ev) ; S
                                    (* This case would have unit type if S wasn't put at the end *)
                                    | Var v, Some e3    -> eval e3 ((v,vl)::ev)
                                    | _                 -> failwith "Invalid variable assignment"))
                                    (* Nota: This case can only happen while desconstructing pairs. *)
    | Ite (cd, e1, e2)  -> (p_verb "If Then Else";
                            match eval_cond cd ev with
                                | E x   -> E x
                                | C cnd -> if cnd
                                                then (eval e1 ev)
                                                else (eval e2 ev)
                                | _     -> failwith "Expected bool on if")
                                (* pattern-matching completion, shouldn't happen *)
    | Fun (e1, e2)      -> (p_verb "Function";
                            Cl(e1,e2,ev))
    | App (e1, e2)      -> (p_verb "Applying";
                            match eval e2 ev with
                                | E x   -> E x
                                | vl    -> let f = eval e1 ev
                                    in (match f with
                                        | Cl(e3,e4,ev1) -> (match e3 with
                                            | Nil   -> eval e4 ev (* Such a function's expression doesn't depend on its argument *)
                                            | Var v -> eval e4 ((v, vl)::ev1)
                                            | _     -> failwith "Invalid argument")
                                            (* actually shouldn't happen *)
                                        | _             -> failwith "Typing error: tried to apply what is no function"))
    | Seq (e1, e2)      -> (p_verb "Segueing";
                            match eval e1 ev with
                                | E x   -> E x
                                | a     -> eval e2 ev)
    | Pair (e1, e2)     -> (p_verb "Pair";
                            match eval e2 ev with
                                | E y   -> E y
                                | r     -> (match eval e1 ev with
                                            | E x   -> E x
                                            | l     -> P (l, r)))
                            (* as for arith, snd is prioritary when it comes to exceptions *)
    | Fst e             -> (p_verb "Fst";
                            match eval e ev with
                                | P (e1, e2)    -> e1
                                | E x           -> E x
                                | _ -> failwith "Expected 'a * 'b on fst")
    | Snd e             -> (p_verb "Fst";
                            match eval e ev with
                                | P (e1, e2) -> e2
                                | E y           -> E y
                                | _ -> failwith "Expected 'a * 'b on snd")
    | Excn e            -> (p_verb "Exception";
                            match eval e ev with
                                | I k   -> E k
                                | _     -> failwith "Expected int on E")
    | Raise e           -> (p_verb "Raising";
                            match eval e ev with
                                | E x   -> E x
                                | _     -> failwith "Expected exception on raise")
    | TryWith(e1,e2,e3) -> (p_verb "TryWith";
                            match eval e1 ev, e2 with
                                | E x, Var v    -> eval e3 ((v, I x)::ev)
                                | vl, Var v     -> vl
                                | _             -> failwith "Expected E var on with")

(* This is wholly new and follows exactly the same scheme as the previous function, except it works for conditions instead of expressions *)
and eval_cond cd ev = match cd with
    | True -> C true
    | False -> C false
    | Leq (e1, e2)  -> (match eval e2 ev with
                        | E y       -> E y
                        | I y       -> (match eval e1 ev with
                                        | E x   -> E x
                                        | I x   -> C (x <= y)
                                        | _     -> failwith "Expected int on <=")
                        | _         -> failwith "Expected int on <=")
    | Lt (e1, e2)   -> (match eval e2 ev with
                        | E y       -> E y
                        | I y       -> (match eval e1 ev with
                                        | E x   -> E x
                                        | I x   -> C (x < y)
                                        | _     -> failwith "Expected int on <")
                        | _         -> failwith "Expected int on <")
    | Geq (e1, e2)  -> (match eval e2 ev with
                        | E y       -> E y
                        | I y       -> (match eval e1 ev with
                                        | E x   -> E x
                                        | I x   -> C (x >= y)
                                        | _     -> failwith "Expected int on >=")
                        | _         -> failwith "Expected int on >=")
    | Gt (e1, e2)   -> (match eval e2 ev with
                        | E y       -> E y
                        | I y       -> (match eval e1 ev with
                                        | E x   -> E x
                                        | I x   -> C (x > y)
                                        | _     -> failwith "Expected int on >")
                        | _         -> failwith "Expected int on >")
    | Eq (e1, e2)   -> (match eval e2 ev with
                        | E y       -> E y
                        | I y       -> (match eval e1 ev with
                                        | E x   -> E x
                                        | I x   -> C (x = y)
                                        | _     -> failwith "Expected int on =")
                        | _         -> failwith "Expected int on =")
    | Neq (e1, e2)  -> (match eval e2 ev with
                        | E y       -> E y
                        | I y       -> (match eval e1 ev with
                                        | E x   -> E x
                                        | I x   -> C (x <> y)
                                        | _     -> failwith "Expected int on <>")
                        | _         -> failwith "Expected int on <>")
    | And (c1, c2)  -> (match eval_cond c1 ev, eval_cond c2 ev with
                            | C b1, C b2    -> C (b1 && b2)
                            | _     -> failwith "Expected bool on &&")
                            (* pattern-matching completion, shouldn't happen *)
    | Or (c1, c2)   -> (match eval_cond c1 ev, eval_cond c2 ev with
                            | C b1, C b2    -> C (b1 || b2)
                            | _     -> failwith "Expected bool on ||")
                            (* pattern-matching completion, shouldn't happen *)
    | Not cd        -> (match eval_cond cd ev with
                            | C b   -> C (not b)
                            | _     -> failwith "Expected bool on not")
                            (* pattern-matching completion, shouldn't happen *)
