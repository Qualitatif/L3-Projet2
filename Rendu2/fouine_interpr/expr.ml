(* un type pour des expressions arithmétiques simples *)
type expr =
    Const of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Let of expr * expr * expr
  | Ite of cond * expr * expr

type cond =
	True 
  | False
  | Le of expr * expr
  | Lt of expr * expr
  | Ge of expr * expr
  | Gt of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | And of cond * cond
  | Or of cond * cond
  | Not of cond
  

(* fonction d'affichage *)
let rec affiche_expr e =
	let aff_aux s a b =
		begin
		print_string s;
		affiche_expr a;
		print_string ", ";
		affiche_expr b;
		print_string ")"
		end
	in
	match e with
	| Const k -> print_int k
	| Var(str) -> print_string str
	| Add(e1,e2) -> aff_aux "Add(" e1 e2
	| Mul(e1,e2) -> aff_aux "Mul(" e1 e2
	| Sub(e1,e2) -> aff_aux "Sub(" e1 e2
	| Let(var,e1,e2) -> (print_string "Let(";print_string var;aff_aux "," e1 e2)
	| Ite(c,e1,e2) -> (print_string "IfThenElse(";affiche_cond c;aff_aux "," e1 e2)
	
let rec affiche_cond c =
	let aff_aux_log logic_op a b =
		begin
		print_string logic_op;
		affiche_cond a;
		print_string ", ";
		affiche_cond b;
		print_string ")";
		end
	in
	let aff_aux_comp comp e1 e2 =
		begin
		print_string logic_op;
		affiche_expr e1;
		print_string ", ";
		affiche_expr e2;
		print_string ")";
		end
	in
	match c with
	| True -> print_string "true"
	| False -> print_string "false"
	| Le(e1,e2) -> aff_aux_comp "Le(" e1 e2
	| Lt(e1,e2) -> aff_aux_comp "Lt(" e1 e2
	| Ge(e1,e2) -> aff_aux_comp "Ge(" e1 e2
	| Gt(e1,e2) -> aff_aux_comp "Gt(" e1 e2
	| Eq(e1,e2) -> aff_aux_comp "Eq(" e1 e2
	| Neq(e1,e2) -> aff_aux_comp "Neq(" e1 e2
	| And(c1,c2) -> aff_aux_log "And(" e1 e2
	| Or(c1,c2) -> aff_aux_log "Or(" e1 e2
	| Not(c) -> (print_string "Not(";affiche_cond c;print_string ")")
  

(* s�mantique op�rationnelle � grands pas *)
let rec eval_cond = function
  | True -> true
  | False -> false
  | Le(e1,e2) -> (eval e1) <= (eval e2) 
  | Lt(e1,e2) -> (eval e1) < (eval e2)
  | Ge(e1,e2) -> (eval e1) >= (eval e2)
  | Gt(e1,e2) -> (eval e1) > (eval e2)
  | Eq(e1,e2) -> (eval e1) == (eval e2)
  | Neq(e1,e2) -> (eval e1) != (eval e2)
  | And(c1,c2) -> (eval_cond c1) && (eval_cond c2)
  | Or(c1,c2) -> (eval_cond c1) || (eval_cond c2)
  | Not(c) -> not (eval_cond c)
  
let rec replace var_name var_value e = 
	match e with
	| Const k -> k
  	| Var(str) -> if (str==var_name) then var_value else Var(str)  
  	| Add(e1,e2) -> (eval e1) + (eval e2)
  	| Mul(e1,e2) -> (eval e1) * (eval e2)
  	| Sub(e1,e2) -> (eval e1) - (eval e2)
  	| Let(var,e1,e2) -> (match var with 
  					  |Var(str) -> replace var_name var_value (replace str e1 e2)
  					  |_ -> failwith "Error: Incorrect name of variable")
  	| Ite(c,e1,e2) -> (match (eval_cond c) with
  					| true -> e1
  					| false -> e2)
  
let rec eval = function
  | Const k -> k
  | Var(str) -> failwith "Error: variable not declared" 
  | Add(e1,e2) -> (eval e1) + (eval e2)
  | Mul(e1,e2) -> (eval e1) * (eval e2)
  | Sub(e1,e2) -> (eval e1) - (eval e2)
  | Let(var,e1,e2) -> (match var with 
  					  |Var(str) -> eval (replace str e1 e2)
  					  |_ -> failwith "Error: Incorrect name of variable")
  | Ite(c,e1,e2) -> (match (eval_cond c) with
  					| true -> e1
  					| false -> e2)
