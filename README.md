# Scilla: A Smart Contract Intermediate Level Language

## Project structure

* [`docs`](./docs) -- specification and other documents 
* [`src/lang`](./src/lang) -- language definition
* [`src/runners`](./src/runners) -- interpreters

## Building and Running

### Build requirements

Install the folowing dependencies for OCaml:

* `opam`, package manager for OCaml, version >= 1.2'
* `jbuilder` build tool, can be installed via `opam install jbuilder`
* `ocamlc`, version >= 4.05

Setup OCaml using the instructions [here](https://github.com/realworldocaml/book/wiki/Installation-Instructions). Make sure you have switched (using `opam switch`) to a version not older than the one specified above.

The package dependencies can be installed via `opam` as follows:

```
opam install ocaml-migrate-parsetree
opam install core cryptokit ppx_sexp_conv yojson batteries
opam install angstrom hex ppx_deriving ppx_deriving_yojson
opam install menhir oUnit
```

Alternatively, run "ubuntu-setup.sh" to setup Ubuntu for building Scilla.

* `./ubuntu-setup.sh`

### Compiling and Running

To build the project, run `make clean; make` from the root folder.

Once the probject is built you can try the following things:

#### Evaluating a standalone closed expression:

From the project root, execute

```
./bin/eval-runner tests/eval/exp/let.scilla 
```

Instead of `let.scilla` you might want to try any idfferent file in `tests/eval/exp`.

#### Executing a simple transition

From the project root, execute

```
./bin/scilla-runner -init tests/contracts/crowdfunding/init.json -istate tests/contracts/crowdfunding/state_4.json -iblockchain tests/contracts/crowdfunding/blockchain_4.json -imessage tests/contracts/crowdfunding/message_4.json -o tests/contracts/crowdfunding/output_4.json -i tests/contracts/crowdfunding/contract
```
  or
```
./bin/scilla-runner -init tests/contracts/zil-game/init.json -istate tests/contracts/zil-game/state_5.json -iblockchain tests/contracts/zil-game/blockchain_5.json -imessage tests/contracts/zil-game/message_5.json -o tests/contracts/zil-game/output_5.json -i tests/contracts/zil-game/contract
```

Alternatively, use the easyrun script as below:

```
./easyrun.sh crowdfunding 1
```

where `n` is a number `0-5` for the number of "steps" to execute the
protocol (the messages and blockchain states are provided for only so
many steps in the simulation).

### Where to find binaries

* The runnables are put into the folder

```
$PROJECT_DIR/_build/install/default/bin
```

### Running the testsuite

The testsuite is based on the `OUnit2` framework and is driven by the
main module in `tests/Testsuite.ml`. Currently there are two types of
tests run in the testsuite. `contracts` tests run a full transition on
a contract with all input data provided. `eval` tests only test
expression evaluation. To add more tests of either of these kinds,
look for the corresponding `.ml` files in their tests/directory and add
accordingly.

To run the testsuite:

```
make test
```

To run the testsuite executable manually from bin/testsuite, you have to provide
the parameters "-bin-dir" and "-tests-dir", which must be absolute paths to
the directory containing scilla-runner, eval-runner and the tests/directory
containng the tests. Relative paths may  not work.

To obtain a list of tests available:

```
./bin/testsuite -list-test
```

To run an individual test(s), for example
`all_tests:1:exptests:14:let.scilla`
(one of the tests from the list obtained via `./bin/testsuite -list-test`):

```
./bin/testsuite -only-test all_tests:1:exptests:14:let.scilla -print-cli true 
```

The optional `-print-cli true` argument is to produce the command line
that has been used to run the test.

## Using Ocaml with Emacs

The following extensions would be useful:

* [tuareg](https://github.com/ocaml/tuareg) for syntax highlighting
* [merlin](https://github.com/ocaml/merlin/wiki/emacs-from-scratch) for auto-completion
* [ocp-indent](https://github.com/OCamlPro/ocp-indent) for smart indentation

All those libraries can be installed via [opem-user-setup](https://github.com/OCamlPro/opam-user-setup):

```
opam install user-setup
```

## Roadmap

Check the working [Notes](./ROADMAP.md)

