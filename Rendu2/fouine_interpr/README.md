Beginner level version
----------------------
* L'intégralité de la partie débutant fonctionne:

* expr.ml et eval.ml fonctionnent correctement:
-let x=3 in let x=2 in x;; renvoie bien 2
-les conditions fonctionnent
-le prInt fonctionne

* parser.mly et lexer.mll fonctionnent pour la partie débutant

* l'option -debug fonctionne

Intermediary level version
--------------------------

* Il y a conflict entre la manière dont les fonctions sont vues dans le parser et dans l'interpréteur
* Les séparateurs marchent //Encore une fois voir pour l'interprétation
* Le let_anon fonctionne //À priori pas de conflict

---------------------------------------------------------------------------

pour (re)compiler, lancer
make

On a alors fabirqué un executable main.native

On peut executer main.native de diverses facons, que vous pouvez tester en
tapant :

./main.native test1.txt

./main.native -debug test1.txt 

./main.native -shout test1.txt //L'option -shout ne fait rien pour l'instant

./calc -shout -debug test1.txt
