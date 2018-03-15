(* un type pour des expressions arithmétiques simples *)
type expr =
    Const of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Let of expr * expr * expr
  | Let_anon of expr * expr
  | Ite of cond * expr * expr
  | PrInt of expr
  | Semis of expr * expr  

and cond =
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
	| Const(k) -> print_int k
	| Var(str) -> print_string str
    | Add(e1,e2) -> aff_aux "Add(" e1 e2
	| Mul(e1,e2) -> aff_aux "Mul(" e1 e2
	| Sub(e1,e2) -> aff_aux "Sub(" e1 e2
	| Let(var,e1,e2) -> (print_string "Let(";affiche_expr var;aff_aux "," e1 e2)
	| Let_anon(e1,e2) -> aff_aux "Let(_," e1 e2
	| Ite(c,e1,e2) -> (print_string "IfThenElse(";affiche_cond c;aff_aux "," e1 e2)
	| PrInt(e) -> (print_string "PrInt(";affiche_expr e;print_string ")")
	
and affiche_cond c =
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
		print_string comp;
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
	| And(c1,c2) -> aff_aux_log "And(" c1 c2
	| Or(c1,c2) -> aff_aux_log "Or(" c1 c2
	| Not(c) -> (print_string "Not(";affiche_cond c;print_string ")")
