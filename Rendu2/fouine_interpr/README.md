# Assessment of the whole work

Doing Rendu 2 as a 'beginner' was fairly easy. Difficulty mostly went up from Rendu 2 to Rendu 3 yet not from Rendu 3 to Rendu 4 contrarily to my expectations: Rendu 4 was actually much more of a long work than a difficult one. This is probably beacause I had a better understanding of the tools I was using (especially ocamlyacc) and that I could guess how to design this or that and correct bugs both much more easily.

In the end, I think what was worst surely was Rendu 3 which I couldn't fully do due to a lack of time and external factors which interfered in my ability to work well and fast. Though I'd would have been glad to implement recursive functions during Rendu 4 instead, I didn't have time to do it either.

Also I briefly had a hope to implement comparisons on refs and pairs (which is allowed by Caml) when I updated `eval_cond` to handle exceptions. This actually sounds possible, but it would require a trick to handle `value`, e.g. in `ref 0 <= ref 1`, 0 and 1 are actually in shape `I 0` and `I 1`, which I supposedly cannot directly compare [1] directly.

However, these aren't `expr` but `value` thus I can't compare them using `eval_cond` recursively either and I would have been obligated to implement an auxiliary function to handle this. Given I'm currently doing an internship and I'm trying *not* to spend my whole time working on Projet2, this would have been unadvisable.

[1] Actually this is doable in Caml, but if I implemented a direct comparison it would also be possible to compare e.g. `I 0` and `Id 0`, which is not advisable.

# Rendu 2

Solo work: I've done everything except the core elements found on the website (most have been upgraded however).

Everything works (unless I've utterly missed something).

Expressions can both end with or without a pair of semicolons `;;`.

No shift/reduce or reduce/reduce conflicts.

# Rendu 3

Non-recusive functions have been fully implemented, including curryed functions. All sets of arguments can be used in the declaration of a function, including a nondescript number of anonymous arguments in a random order (e.g. in func5.ml).

Anonymous functions are implemented using `Nil` as a fictive name.

All combinations of writings work, using `fun` and `->` or not.

# Rendu 4

## Modifications

* Corrected equality test, which was using `x == y` instead of `x = y`.
* Added systematic evaluation of a function's argument before its application.
* Gave up on implementing `-env` (I lacked enough time to do that properly). I thus deleted everything relating to keep the code clean.
* Modified the constructor Let from `expr * expr * expr` to `expr * expr * (expr option)` to distinguish recursive and imperative declarations and modified the way it is processed in *parser.mly* for the same purpose and for the purpose of accepting parentheses in declarations.
* Modified the definition of `top_let` in *parser.mly* in order to adapt to the newly added sequences feature of which it is now a special case.
* Moved definitions of `closure`, `env` and `value` from *eval.ml* to the newly renamed *types.ml* (formerly *expr.ml*, which was a fairly poor name).
* `prInt` now takes `sexpr` as arguments instead of `expr`.

## New features

* I made a difference between global and local variables in my implementation of imperative aspects. Thus eval now take two distinct environments as arguments : first is the local environment (which is prioritary) and second is the global one (in shape of a reference).
* Implemented unit and both sequences with `;` and `;;`.
* Implemented references. The number of memory slots has been arbitrarily set to 1000. It can be changed by modifying the value of `size` in *eval.ml*. Content can be either `int`, `'a -> 'b`, `'a * 'b`, `ref` or `exception`, though update isn't allowed in that last case (see the comment on *excn-ref.ml* for the explanation).
* Fully implemented pairs.
* Fully implemented exceptions. That frightened me at first but despite it requires to update most patterns in `eval` the updates are actually fairly simple. Thus it was more of a long work than a difficult one in the end.
* Nota: in Rendu 3, `eval_cond` actually kept working without issue despite the fact it then worked on `value` instead of `int`. That is due to the fact that Caml automatically sets an order on types made of constructors on ordered types (e.g. `I 0 < I 1`).

## Encountered bugs

* At first got a bug with *misuse-unit.ml* which didn't fail as it should. That was because the output for `eval Skip ev` was `(p_verb "Skipping"; I 0)`: I needed to put a value so it had the right typing. That was easily patched by adding the constructor `S` to `value`.
* *basic-tuples.ml* first didn't work because I didn't allow the use of parentheses in *parser.mly*. Thus `let a,b = bla` was okay but not `let (a,b) = bla`.
* *basic-chaining.ml* first didn't work because I had put `()` in `expr` and not in `sexpr`. That made me realize I should put bangs in it too.

# Options

* `-debug`;
* `-shout`;
* `-debug+`;
* Descriptions of the options above are available using `./fouine -help` or `./fouine --help`.

# Test files

* Base test files from the website are available in ~/tests/Base
* Self-made test files are available in ~/tests. *anon.ml* tests anonymous declaration, which is not available among the base test files. *bangs.ml* tests functions references. *elaborate-excn.ml* mixes references and exceptions. *elaborate-excn.ml* mixes references and pairs.
* Included two test files in ~/tests/AbnormalBehaviour which don't work in Fouine as they should in Caml.

## Comments on test files which include errors and thus fail

* *scoping.ml:* the final y is unbound.
* *missing-arg.ml:* tries to apply prInt to a non-integer argument.
* *func4.ml:* shows that the unary minus is treated correctly.
* *ref-types.ml:* tries to assign a function to an int ref.
* *missing-deref.ml*, *misuse-unit.ml* and *not-a-ref* fail as expected.

## Comments on test files which have abnormal behaviour compared to Caml

* *bangbang.ml:* Caml allows `! !a`, but not `!!a`. Thus *bangbang.ml* works in Fouine but not in Caml.
Replacing
```
| LPAREN BANG VAR RPAREN            { Bang (Var $3) }
| BANG bang                         { Bang $2 }
```
with
```
| BANG LPAREN bang RPAREN           { Bang $3 }
```
will remove both the valid and invalid syntaxes.

* *excn-ref.ml:* In Fouine, this program will lead to raising E 1, though in Caml it would rather update a. Actually, I would have been simple to allow the updating of exceptions references, but due to the lack of typing it would have meant allowing things like `let g x = if x < 0 then -x else raise (E 1) in let a = ref (E 0) in a := (g 1) ;;` too. However, examples like this one are not allowed by Caml given that e.g. here (g 1) is supposed to be an int, not an exception.

# Bonuses

* `if then else` syntax allows the use of `true`, `false` (both fancy yet fully useless in practice), `or`, `and` and `not`.
