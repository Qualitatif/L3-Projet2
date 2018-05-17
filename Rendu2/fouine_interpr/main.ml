open Expr
open Eval

let compile e =
    begin
        if !verbose || !maxVerbose then
        (* debug mode *)
            (display_expr e;
            print_newline());
        eval e []
    end

(* stdin désigne l'entrée standard (le clavier) *)
(* lexbuf est un canal ouvert sur stdin *)

let lexbuf = Lexing.from_channel stdin

(* on enchaîne les tuyaux: lexbuf est passé à Lexer.token,
   et le résultat est donné à Parser.main *)

let nom_fichier = ref ""

let fonction_principale () =
  (* on decrit les diverses options possibles ; Arg.Set dit de mettre
     un bool ref a vrai si on active l'option.
     Voir la librairie "Arg" de Caml pour davantage de details *)
  let optlist = [
    ("-debug", Arg.Set verbose, " Activates light debug mode" );
    ("-debug+", Arg.Set maxVerbose, " Activates full debug mode" );
    ("-shout", Arg.Set caps, " Types operators in capitals in both debug modes");
    ("-env", Arg.Set giveEnv, " Displays current environments in full debug mode [currently ill functional]")
  ] in
  let usage = "Help menu for fouine:" in  (* message d'accueil *)
  Arg.parse (* ci-dessous les 3 arguments de Arg.parse : *)
    optlist (* la liste des options definie ci-dessus *)

    (fun s -> nom_fichier := s) (* la fonction a declencher lorsqu'on recupere un string qui n'est pas une option : ici c'est le nom du fichier, et on stocke cette information dans la reference nom_fichier *)
    usage; (* le message d'accueil *)
  (*try*)
    (* les incantations d'usage pour aller lire dans le fichier et
       faire les analyses lexicale et syntaxique *)
    let in_file = open_in !nom_fichier in
    let lexbuf_file = Lexing.from_channel in_file in
    let parse () = Parser.main Lexer.token lexbuf_file in
    let result = parse () in
    (* on a recupere une expressions dans result, on peut lancer la
       fonction compile, definie plus haut *)
    compile result; flush stdout
  (*with _ -> (print_string "erreur de saisie\n")*)


let _ = fonction_principale()
