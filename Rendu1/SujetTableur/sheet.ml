(* tableau de cellules *)
open Cell

let size = (20,10) (* lignes, colonnes *)

(* le tableau que l'on manipule dans le programme *)
(* tapez "fst" et "snd" dans un interprete Caml pour connaître leur type *)
let thesheet = Array.make_matrix (fst size) (snd size) default_cell

let read_cell co = thesheet.(fst co).(snd co)

let update_cell_formula co f = thesheet.(fst co).(snd co).formula <- f
let update_cell_value co v = thesheet.(fst co).(snd co).value <- v


(* exécuter une fonction (f) sur tout le tableau *)
let sheet_iter f =
  for i = 0 to (fst size -1) do
    for j = 0 to (snd size -1) do
      f i j
    done;
  done



(* initialisation du tableau : questions un peu subtiles de partage,
 * demandez autour de vous si vous ne comprenez pas pourquoi cela est
 * nécessaire.  Vous pouvez ne pas appeler la fonction ci-dessous,
 * modifier une case du tableau à l'aide de update_cell_formula, et
 * regarder ce que ça donne sur le tableau : cela devrait vous donner
 * une piste *)
(* Array.make_matrix creates a matrix whose cells all point toward the same
content. Thus we must initialise the matrix by creating a different content
for each cell. *)
let init_sheet () =
  let init_cell i j =
    let c = { value = None; formula = Cst 0. } in
    thesheet.(i).(j) <- c
  in
  sheet_iter init_cell

(* on y va, on initialise *)
let _ = init_sheet ()


(* affichage rudimentaire du tableau *)

let show_sheet () =
  let g i j =
    begin
       (* aller à la ligne en fin de ligne *)
      if j = 0 then print_newline() else ();
      let c = read_cell (i,j) in
      print_string (cell_val2string c);
      print_string " "
    end
  in
  sheet_iter g;
  print_newline()




(********** calculer les valeurs à partir des formules *************)

(* on marque qu'on doit tout recalculer en remplissant le tableau de "None"
 * N.B. : on pourrait être moins naïf *)
let invalidate_sheet () =
  let f i j = update_cell_value (i,j) None in
  sheet_iter f


(* à faire : le cœur du programme *)
let rec eval_form fo = match fo with
  | Cst n -> n
  | Cell (p,q) -> eval_cell p q
  | Op(o,fs) -> match o, fs with
                  | S, [] -> 0.
                  | S, t::q -> (eval_form t) +. (eval_form (Op(S, q)))
                  | M, [] -> 1.
                  | M, t::q -> (eval_form t) *. (eval_form (Op(M, q)))
                  | A, [] -> 0. (* la moyenne *)
                  | A, l -> let n = float_of_int (List.length l)
                            in let rec avg n a l = match l with
                              | [] -> a /. n
                              | t::q -> avg n (a +. (eval_form t)) q
                            in (avg n) 0. l
                  | MAX, [] -> neg_infinity
                  | MAX, t::q -> let rec max t q = let t0 = (eval_form t)
                                  in let t1 = eval_form (Op(MAX, q))
                      					  in if t0 > t1 then t0 else t1
                      					  in max t q

(* ici un "and", car eval_formula et eval_cell sont a priori
   deux fonctions mutuellement récursives *)
and eval_cell i j =
  let f = eval_form (thesheet.(i).(j).formula) in
  thesheet.(i).(j).value <- Some(f) ;
  f

let recompute_sheet () =
  invalidate_sheet ();
  sheet_iter eval_cell
