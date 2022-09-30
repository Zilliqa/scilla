# Tests that ensure AST stays the same after formatting

This is [cram](https://dune.readthedocs.io/en/stable/tests.html#cram-tests-1) tests
integrated into the Dune build system.

AST comparison tests are run on all `.scilexp` and `.scilla` files in this directory.

*Note*: Scilla file names in this directory cannot start with `FORMATTED-` and
cannot end with `.sexp`.

The `run.t` script which calls `scilla-fmt` on Scilla source files, saves them
as new, dumps ASTs of the old and new files in the form of S-expressions
and compares them.

