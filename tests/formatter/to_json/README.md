# To JSON tests

These are [cram](https://dune.readthedocs.io/en/stable/tests.html#cram-tests-1) tests integrated into the Dune build system. Each directory is a separate test and the directory name must end with `.t`.
Also, each directory contains one contract or expression to format and the `run.t` script which calls `scilla-fmt --json --deannot` on its contract or expression and shows the expected output on the next line.

To run cram tests use the `dune runtests` command.
To update tests when the formatter behavior changes run `dune promote`.
