open Expr
open Eval

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
  with _ -> (print_string "erreur de saisie\n")
;;

let _ = calc()
