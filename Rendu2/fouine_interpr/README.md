# Rendu 2

Solo work: I've done everything except the core elements found on the website.

Everything works unless I've utterly missed something.

Expressions can both end with or without a pair of semicolons `;;`.

No shift/reduce or reduce/reduce conflicts.

## Options

* `-debug`;
* `-shout`;
* `-debug+`;
* `-env` (yet unfunctional);
* Descriptions of the options above are available using `./fouine -help` or `./fouine --help`.

## Test files

* Base test files from the website are available in ~/tests/Base
* Three other test files are available in ~/tests, including one for testing anonymous declarations, which is not available among the base test files.

## Bonuses

* `if then else` syntax allows the use of `true`, `false` (both fancy yet fully useless in practice), `or`, `and` and `not`.

# Rendu 3

Type closure has been implemented.

A bit of work on functions has been done in the parser.
