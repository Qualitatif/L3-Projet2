# Rendu 2

Solo work: I've done everything except the core elements found on the website (most have been upgraded however).

Everything works (unless I've utterly missed something).

Expressions can both end with or without a pair of semicolons `;;`.

No shift/reduce or reduce/reduce conflicts.

# Rendu 3

Non-recusive functions have been fully implemented, including curryed functions. All sets of arguments can be used in the declaration of a function, including a nondescript number of anonymous arguments in a random order (e.g. in func5.ml).

Anonymous functions are implemented using `Nil` as a fictive name.

All combinations of writings work, using `fun` and `->` or not.

# Options

* `-debug`;
* `-shout`;
* `-debug+`;
* `-env` (yet unfunctional);
* Descriptions of the options above are available using `./fouine -help` or `./fouine --help`.

## Test files

* Base test files from the website are available in ~/tests/Base
* Nine other test files are available in ~/tests, including one for testing anonymous declaration, which is not available among the base test files.

### Comments on test files which include errors and thus

* scoping.ml: the final y is unbound.
* missing-arg.ml: tries to apply prInt to a non-integer argument.
* func4.ml: shows that the unary minus is treated correctly.

## Bonuses

* `if then else` syntax allows the use of `true`, `false` (both fancy yet fully useless in practice), `or`, `and` and `not`.
