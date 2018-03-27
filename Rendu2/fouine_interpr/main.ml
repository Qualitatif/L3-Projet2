open Expr
open Eval;;

(*
eval (Add(Const(7),Const(2))) [];;
eval (Mul(Const(7),Const(2))) [];;
eval (Sub(Const(7),Const(2))) [];;
eval (Let(Var("x"),Sub(Const(7),Const(2)),Add(Var("x"),Const(2)))) [];;
eval (PrInt(Let(Var("x"),Const(3),Let(Var("x"),Const(2),Var("x"))))) [];;
eval (Ite(True,Const(1),Const(0))) [];;
eval (Ite(False,Const(1),Const(0))) [];;
eval (Ite(Le(Const(3),Let(Var("x"),Add(Const(1),Const(2)),Var("x"))),Const(1),Const(0))) [];;
affiche_expr (Ite(Lt(Const(3),Let(Var("x"),Add(Const(1),Const(2)),Var("x"))),Const(1),Const(0)));;
eval (Ite(Ge(Const(3),Let(Var("x"),Add(Const(1),Const(2)),Var("x"))),Const(1),Const(0))) [];;
eval (Ite(Gt(Const(3),Let(Var("x"),Add(Const(1),Const(2)),Var("x"))),Const(1),Const(0))) [];;
eval (Ite(Eq(Const(3),Let(Var("x"),Add(Const(1),Const(2)),Var("x"))),Const(1),Const(0))) [];;
eval (Ite(Neq(Const(3),Let(Var("x"),Add(Const(1),Const(2)),Var("x"))),Const(1),Const(0))) [];;
eval (Ite(And(True,False),Const(1),Const(0))) [];;
eval (Ite(Or(True,False),Const(1),Const(0))) [];;	
eval (PrInt(Semis(Ite(Not(False),Const(1),Const(0)),(Ite(Or(True,False),Const(1),Const(0)))))) [];;
eval (PrInt(App(Fun("x",Add(Var("x"),Const(1))),Const(3)))) [];;
*)

let compile e =
  begin
    affiche_expr e;
    print_newline();
    (let v =eval e [] in
    	match v with 
    	| VI(k) -> print_int(k)
    	| VF(x,e) -> print_string("fonction..."));
    print_newline()
  end

(* stdin d�signe l'entr�e standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on encha�ne les tuyaux: lexbuf est pass� � Lexer.token,
   et le r�sultat est donn� � Parser.main *)

let parse () = Parser.main Lexer.token lexbuf

(* la fonction que l'on lance ci-dessous *)
let calc () =
  try
      let result = parse () in
      (* Expr.affiche_expr result; print_newline (); flush stdout *)
	compile result; flush stdout
  with _ -> (print_string "erreur de saisie\n");;

let _ = calc();;
