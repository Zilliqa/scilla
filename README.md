# Scilla: A Smart Contract Intermediate Level Language

[![Build Status](https://travis-ci.com/Zilliqa/scilla.svg?token=7qzjATfZuxTQvRjMHPVQ&branch=master)](https://travis-ci.com/Zilliqa/scilla)
[![License](https://img.shields.io/cran/l/devtools.svg)](https://github.com/Zilliqa/scilla/blob/master/LICENSE)
[![Gitter chat](http://img.shields.io/badge/chat-on%20gitter-077a8f.svg)](https://gitter.im/Zilliqa/SmartContract)
[![Coverage Status](https://coveralls.io/repos/github/Zilliqa/scilla/badge.svg?branch=master)](https://coveralls.io/github/Zilliqa/scilla?branch=master)

<p align="center">
  <a href="https://scilla-lang.org/"><img src="https://github.com/Zilliqa/scilla/blob/master/imgs/scilla-logo-color.jpg" width="200" height="200"></a>
</p>

## Building and Running

### Source code

We suggest users to use the latest release of Scilla available [here](https://github.com/Zilliqa/scilla/releases).

### Build requirements

Platform specific instructions for setting up your system for building Scilla can be
found in [INSTALL.md](./INSTALL.md).

### Compiling and Running

To build the project, run `make clean; make` from the root folder.

Once the project is built you can try the following things:

#### Evaluating a standalone closed expression:

From the project root, execute

```
./bin/eval-runner -libdir src/stdlib tests/eval/exp/good/let.scilla
```

Instead of `let.scilla` you might want to try any dfferent file in
`tests/eval/exp`. The second argument, which is a path to the Scilla
standard library can alternatively be specified in the environment
variable `SCILLA_STDLIB_PATH`. This must be an absolute path (or a
list of `;` separated paths.

#### Type-checking a contract

From the project root, execute

```
./bin/scilla-checker -libdir src/stdlib tests/checker/good/auction.scilla
```

Instead of `auction.scilla` you might want to try any dfferent file in
`tests/checker` with a complete implementation of a contract, or your
own contract code. The second argument, which is a path to the Scilla
standard library can alternatively be specified in the environment
variable `SCILLA_STDLIB_PATH`. As above, this must be an absolute
path(s).

If the checker only returns the contract structure in JSON format, it
means that the contract has no type errors. Otherwise, a type error
trace is provided.

The checker can be run with the following optional flags:

- `-cf` to enable the cashflow checker and print its results.


#### Executing a simple transition

From the project root, execute

```
./bin/scilla-runner -init tests/contracts/crowdfunding/init.json -istate tests/contracts/crowdfunding/state_4.json -iblockchain tests/contracts/crowdfunding/blockchain_4.json -imessage tests/contracts/crowdfunding/message_4.json -o tests/contracts/crowdfunding/output_4.json -i tests/contracts/crowdfunding/contract -libdir src/stdlib
```
  or
```
./bin/scilla-runner -init tests/contracts/zil-game/init.json -istate tests/contracts/zil-game/state_5.json -iblockchain tests/contracts/zil-game/blockchain_5.json -imessage tests/contracts/zil-game/message_5.json -o tests/contracts/zil-game/output_5.json -i tests/contracts/zil-game/contract -libdir src/stdlib
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

## Developer Tools
### Emacs mode for Scilla

An emacs major mode for editing Scilla contracts is [provided](./misc/emacs-mode/scilla-mode.el).
Add the following line to your `~/.emacs` file to load this mode for files ending with `.scilla`.
For enabling flycheck mode for Scilla (see [INSTALL.md](./INSTALL.md)).

```
;; For enabling flycheck mode for Scilla.
(setq scilla-root "/path/to/scilla/root")
;; Scilla mode
(load-file "/path/to/scilla-mode.el")
```
### Vim plugin for Scilla

A vim plugin for editing Scilla contracts is [provided](./misc/vim-plugin).

You can install the vim config files through Pathogen by:
```
git clone https://github.com/edisonljh/vim-scilla.git ~/.vim/bundle/vim-scilla
```

Or through Vundle by adding the following line to your `~/.vimrc`:
```
Plugin 'edisonljh/vim-scilla'
```

Mirror Repo: [vim-scilla](https://github.com/edisonljh/vim-scilla).
