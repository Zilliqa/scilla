# Contributing to Scilla

## Building Scilla

You could build the source code according to installation instructions in the [README](./README.md#building-scilla) and the [INSTALL.md](./INSTALL.md), which contains OS-specific information about build dependencies.

## Running tests

### Running the testsuite

The testsuite is based on the `OUnit2` framework and is driven by the
main module in `tests/Testsuite.ml`. There are several types of
tests run in the testsuite. For instance, `contracts` tests run a full transition on
a contract with all input data provided, and `eval` tests only test
expression evaluation. To add more tests of either of these kinds,
look for the corresponding `.ml` files in their tests/directory and add
accordingly.

To run the testsuite:

```shell
make test
```

(this makes the Dune build system invoke `testsuite` executable with `-print-diff true` option to print colored diffs between error messages).

To run the `testsuite` executable manually using `dune exec tests/testsuite.exe`,
you have to provide the parameters `-bin-dir` and `-tests-dir`, which must be absolute paths to
the directory containing `eval-runner`, `type-checker`, `scilla-runner`, `scilla-checker` and
the `tests/` directory containing the tests.
Relative paths may not work.
Parameters to `testsuite` executable can be passed like so:
```shell
dune exec tests/testsuite.exe -- <space-separated-parameters>
```

To obtain a list of tests available:

```shell
dune exec tests/testsuite.exe -- -list-test
```

### Running an individual test
To run an individual test(s), for example
`tests:4:checker:0:good:1:exptests:5:one-accept.scilla`,
(it's one of the tests from the list obtained via `dune exec -- tests/testsuite.exe -list-test`,
this needs to be run from the project's root):

```shell
dune exec tests/testsuite.exe -- -only-test tests:4:checker:0:good:1:exptests:5:one-accept.scilla -print-cli true
```

The optional `-print-cli true` argument is to produce the command line
that has been used to run the test.

### Running a group of tests
If you'd like to run a group of tests, for instance, the typechecking tests
which are assigned the name `checker`, execute the following from the project's
root:

```shell
dune exec -- tests/testsuite.exe -only-test tests:4:checker
```

If you need to update the so-called `gold`-files which keep the expected output
for the `checker` tests, run the following command:

```shell
dune exec -- tests/testsuite.exe -only-test tests:4:checker -update-gold true
```

## Formatting and linting the codebase
Our CI checks that the source code is formatted properly. Use
```shell
make fmt
```
to ensure your code adheres to the style guide.
Note that the command will automatically change ("promote") your source code.
You will need the `ocamlformat` opam package for the command above to work.

To make sure you are good to go, before sending PR run
```shell
make lint
```
to check if there are any issues with your contribution.
In addition to the `ocamlformat` package, `make lint` uses `opam` and
[`shellcheck`](https://www.shellcheck.net).

## Debugging
To debug scilla-checker or scilla-runner, you must build `make debug`, which will generate
the OCaml bytecode versions of the binaries. These can be debugged using `ocamldebug`.
Executing a bytecode executable also requires the environment variable `LD_LIBRARY_PATH` to
be set to `_build/default/src/base/cpp`. An example debug command line is provided below.


```shell
LD_LIBRARY_PATH=${PWD}/_build/default/src/base/cpp ocamldebug _build/default/src/runners/scilla_checker.bc -libdir src/stdlib -gaslimit 10000 tests/contracts/helloworld.scilla
```

## Using OCaml with Emacs

Please, read the instructions in [INSTALL.md](./INSTALL.md) if you intend to hack on Scilla implementation.

<details><summary>Instructions</summary>

Scilla is written in [OCaml](https://ocaml.org/).
You can read about how to setup your OCaml development environment [here](https://dev.realworldocaml.org/install.html).
The following extensions would be useful for working on this codebase:

* [tuareg](https://github.com/ocaml/tuareg) for syntax highlighting
* [merlin](https://github.com/ocaml/merlin/wiki/emacs-from-scratch) for auto-completion
* [ocp-indent](https://github.com/OCamlPro/ocp-indent) for smart indentation

All those libraries can be installed via [opam-user-setup](https://github.com/OCamlPro/opam-user-setup):
```shell
opam install user-setup
```

Additionally, you might want to install a nice OCaml REPL called [utop](https://github.com/ocaml-community/utop).

To enable flycheck mode (integration of `scilla-checker` with Emacs for editing Scilla files), install
flycheck for Emacs. See installation instructions [here](http://www.flycheck.org/en/latest/user/installation.html).

</details>
