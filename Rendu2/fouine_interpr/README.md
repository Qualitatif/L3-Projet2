Beginner level version
----------------------
* expr.ml et eval.ml fonctionnent correctement:
-let x=3 in let x=2 in x;; renvoie bien 2
-les conditions fonctionnent
-le prInt fonctionne

* il semble qu'il y ait une erreur dans le parser.mly puisque l'execution du main.native sur une entrée test valide renvoie une "erreur de saisie"

Intermediary level version
--------------------------
* Les fonctions marchent
* Les séparateurs marchent
* Le let_anon ne marche pas encore (*à voir*)

---------------------------------------------------------------------------

pour tester les fonctions, mettre en commentaire tout ce qui suit les importations de Expr et Eval
ecrire une expression dans le main et lancer la fonction eval dessus (exemples donnés en commentaire dans le main)
fabriquer main.native avec make.
executer main.native
