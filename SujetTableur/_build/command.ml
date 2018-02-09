open Debug
open Cell
open Sheet

(* commandes: ce que l'utilisateur peut saisir dans un fichier.
 - La modification d'une cellule avec une nouvelle formule,
 - l'affichage d'une cellule,
 - l'affichage de toute la feuille *)
type comm = Upd of cellname * form | Show of cellname | ShowAll
let notUpToDate = ref false

(************ affichage **************)
let show_comm c =
  match c with
  | Upd (c,f) ->
     begin
       print_string (cell_name2string c);
       print_string "=";
       show_form f
     end
  | Show c ->
     begin
       print_string "Show(";
       print_string (cell_name2string c);
       print_string ")"
     end
  | ShowAll -> print_string "ShowAll"

(************ faire tourner les commandes **************)

(* exécuter une commande *)
let run_command c = match c with
  | Show cn ->
    begin
      if !notUpToDate
      then
        (recompute_sheet() ;
        notUpToDate := false) ;
      let co = cellname_to_coord cn in
      eval_p_debug (fun () ->
          "Showing cell "
          ^ cell_name2string cn
        );
      print_string (cell_val2string (read_cell co)); (* <- ici print_string, et pas p_debug, car on veut afficher au moins cela *)
      print_newline()
    end
  | ShowAll ->
    begin
      eval_p_debug (fun () -> "Show All\n");
      if !notUpToDate
      then
        (recompute_sheet();
        notUpToDate := false);
      show_sheet ()
    end
  | Upd(cn,f) ->
     let co = cellname_to_coord cn in
     eval_p_debug (fun () -> "Update cell " ^ cell_name2string cn ^ "\n");
     update_cell_formula co f;
     notUpToDate := true

(* exécuter une liste de commandes *)
let run_script cs = List.iter run_command cs
