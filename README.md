# Scilla: A Smart Contract Intermediate Level Language

[![Build Status](https://travis-ci.com/Zilliqa/scilla.svg?token=7qzjATfZuxTQvRjMHPVQ&branch=master)](https://travis-ci.com/Zilliqa/scilla)
[![License](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://github.com/Zilliqa/scilla/blob/master/LICENSE)
[![Gitter chat](http://img.shields.io/badge/chat-on%20gitter-077a8f.svg)](https://gitter.im/Zilliqa/SmartContract)
[![Coverage Status](https://coveralls.io/repos/github/Zilliqa/scilla/badge.svg?branch=master)](https://coveralls.io/github/Zilliqa/scilla?branch=master)

<p align="center">
  <a href="https://scilla-lang.org/"><img src="https://github.com/Zilliqa/scilla/blob/master/imgs/scilla-logo-color.jpg" width="200" height="200"></a>
</p>

## Introduction
Scilla short for Smart Contract Intermediate-Level LAnguage is an intermediate-level smart contract language being developed for Zilliqa. Scilla has been designed as a principled language with smart contract safety in mind.

Scilla imposes a structure on smart contracts that will make applications less vulnerable to attacks by eliminating certain known vulnerabilities directly at the language-level. Furthermore, the principled structure of Scilla will make applications inherently more secure and amenable to formal verification.

Zilliqa - the underlying blockchain platform on which Scilla contracts are run, has been designed to be scalable. It employs the idea of sharding to validate transactions in parallel. Zilliqa has an intrinsic token named Zilling, ZIL for short that are required to run smart contracts on Zilliqa.

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
./bin/eval-runner -libdir src/stdlib tests/eval/exp/good/let.scilexp
```

Instead of `let.scilla` you might want to try any different file in
`tests/eval/exp`. The second argument, which is a path to the Scilla
standard library can alternatively be specified in the environment
variable `SCILLA_STDLIB_PATH`. This must be an absolute path (or a
list of paths separated with `:` (or `;` on Windows).

#### Type-checking a contract

From the project root, execute

```
./bin/scilla-checker -libdir src/stdlib tests/contracts/auction.scilla
```

Instead of `auction.scilla` you might want to try any different file in
`tests/contracts` with a complete implementation of a contract, or your
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
./bin/scilla-runner -init tests/runner/crowdfunding/init.json -istate tests/runner/crowdfunding/state_4.json -iblockchain tests/runner/crowdfunding/blockchain_4.json -imessage tests/runner/crowdfunding/message_4.json -o tests/runner/crowdfunding/output_4.json -i tests/contracts/crowdfunding.scilla -libdir src/stdlib -gaslimit 8000
```
  or
```
./bin/scilla-runner -init tests/runner/zil-game/init.json -istate tests/runner/zil-game/state_5.json -iblockchain tests/runner/zil-game/blockchain_5.json -imessage tests/runner/zil-game/message_5.json -o tests/runner/zil-game/output_5.json -i tests/contracts/zil-game.scilla -libdir src/stdlib -gaslimit 8000
```

If you'd like to see the output produced by the aforementioned commands,
check the file specified by `-o path/to/file.json` argument.

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
$PROJECT_DIR/bin
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
dune exec tests/testsuite.exe -- <space-separate-parameters>
```

To obtain a list of tests available:

```shell
dune exec tests/testsuite.exe -- -list-test
```

To run an individual test(s), for example
`all_tests:1:exptests:14:let.scilla`
(one of the tests from the list obtained via `./bin/testsuite -list-test`):

```shell
dune exec tests/testsuite.exe -- -only-test all_tests:1:exptests:14:let.scilla -print-cli true 
```

The optional `-print-cli true` argument is to produce the command line
that has been used to run the test.

## Developer Tools
### Emacs mode for Scilla

An emacs major mode for editing Scilla contracts is [provided](./misc/emacs-mode/scilla-mode.el).
Add the following line to your `.emacs` file to load this mode for files ending with `.scilla` and `.scillib`.
For enabling flycheck mode for Scilla (see [INSTALL.md](./INSTALL.md)).

```
;; For enabling flycheck mode for Scilla.
(setq scilla-root "/path/to/scilla/root")
;; Scilla mode
(load-file "/path/to/scilla-mode.el")
```
### Vim plugin for Scilla

A vim plugin for editing Scilla contracts is provided.

You can install the vim config files through Pathogen by:
```
git clone https://github.com/edisonljh/vim-scilla.git ~/.vim/bundle/vim-scilla
```

Or through Vundle by adding the following line to your `~/.vimrc`:
```
Plugin 'edisonljh/vim-scilla'
```

Repo: [vim-scilla](https://github.com/edisonljh/vim-scilla).
