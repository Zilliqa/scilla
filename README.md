# Scilla: A Smart Contract Intermediate Level Language

[![Build Status](https://travis-ci.com/Zilliqa/scilla.svg?token=7qzjATfZuxTQvRjMHPVQ&branch=master)](https://travis-ci.com/Zilliqa/scilla)
[![License](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://github.com/Zilliqa/scilla/blob/master/LICENSE)
[![Discord chat](https://img.shields.io/discord/370992535725932544.svg)](https://discord.gg/mWp9HdR)
[![Coverage Status](https://coveralls.io/repos/github/Zilliqa/scilla/badge.svg?branch=master)](https://coveralls.io/github/Zilliqa/scilla?branch=master)

<p align="center">
  <a href="https://scilla-lang.org/"><img src="https://github.com/Zilliqa/scilla/blob/master/imgs/scilla-logo-color.jpg" width="200" height="200"></a>
</p>

## Introduction
Scilla short for Smart Contract Intermediate-Level LAnguage is an intermediate-level smart contract language being developed for Zilliqa. Scilla has been designed as a principled language with smart contract safety in mind.

Scilla imposes a structure on smart contracts that will make applications less vulnerable to attacks by eliminating certain known vulnerabilities directly at the language-level. Furthermore, the principled structure of Scilla will make applications inherently more secure and amenable to formal verification.

Zilliqa - the underlying blockchain platform on which Scilla contracts are run, has been designed to be scalable. It employs the idea of sharding to validate transactions in parallel. Zilliqa has an intrinsic token named Zilling, ZIL for short that are required to run smart contracts on Zilliqa.

### Language Reference

A comprehensive documentation on Scilla, its features and constructs can be found [here](https://scilla.readthedocs.io/en/latest/)

## Building Scilla

If you don't want to setup and build Scilla from source, skip this section to follow the opam installation instructions.

### 1. Cloning source code

We suggest users to use the latest release of Scilla available [here](https://github.com/Zilliqa/scilla/releases).

If you'd like to hack on Scilla, clone it with all of its submodules:
```shell
git clone --jobs 4 --recurse-submodules https://github.com/Zilliqa/scilla/
```

### 2. Build prerequisites

Platform specific instructions for setting up your system for building Scilla can be
found in [INSTALL.md](./INSTALL.md).

### 3. Compiling

To build the project from the root folder:
```
make
```

### Installation

Scilla can be installed into your opam switch as

```
make install
```

and can similarly be uninstalled as

```
make uninstall
```

## Installing Scilla with opam
Scilla can be installed using OCaml's package manager `opam`.

### Installing Scilla from GitHub

To install the development version of Scilla package make sure you are using
the correct opam switch and execute the following

```shell
opam pin add scilla git+https://github.com/Zilliqa/scilla#master --yes
```

### Installing Scilla from your local repo

```shell
cd <scilla-repo>
# It is important to pick the right git branch because opam pins the package to the current branch
git checkout master
opam install ./scilla.opam
```

If you are using a local opam switch (see [here](https://github.com/Zilliqa/scilla/blob/master/INSTALL.md#installing-opam-packages))
in you local Scilla repo (`~/path/to/scilla`), then most likely you will want to reuse the same local switch for your Scilla-based project.
To do that create a symlink `_opam` as follows:

```shell
cd <scilla-based-project-repo>
ln -s ~/path/to/scilla/_opam _opam
```

## Running the binary

Once the project is built you can try the following things:

#### Evaluating a standalone closed expression:

From the project root, execute

```
eval-runner -gaslimit 10000 -libdir src/stdlib tests/eval/good/let.scilexp
```

Instead of `let.scilla` you might want to try any different file in
`tests/eval`. The second argument, which is a path to the Scilla
standard library can alternatively be specified in the environment
variable `SCILLA_STDLIB_PATH`. This must be an absolute path (or a
list of paths separated with `:` (or `;` on Windows).

#### Type-checking a contract

From the project root, execute

```
scilla-checker -gaslimit 10000 -libdir src/stdlib tests/contracts/auction.scilla
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
scilla-runner -init tests/runner/crowdfunding/init.json -istate tests/runner/crowdfunding/state_4.json -iblockchain tests/runner/crowdfunding/blockchain_4.json -imessage tests/runner/crowdfunding/message_4.json -o tests/runner/crowdfunding/output_4.json -i tests/contracts/crowdfunding.scilla -libdir src/stdlib -gaslimit 8000
```
  or
```
scilla-runner -init tests/runner/zil-game/init.json -istate tests/runner/zil-game/state_5.json -iblockchain tests/runner/zil-game/blockchain_5.json -imessage tests/runner/zil-game/message_5.json -o tests/runner/zil-game/output_5.json -i tests/contracts/zil-game.scilla -libdir src/stdlib -gaslimit 8000
```

If you'd like to see the output produced by the aforementioned commands,
check the file specified by `-o path/to/file.json` argument.

Alternatively, use the `easyrun.sh` script as below:

```
./easyrun.sh crowdfunding 1
```

where `n` is a number `0-5` for the number of "steps" to execute the
protocol (the messages and blockchain states are provided for only so
many steps in the simulation).

#### Using Scilla as a service
A `scilla-server` is provided that provides the functionality of `scilla-runner`
and `scilla-checker` as a JSON-RPC server. The `scilla-server` process accepts
contract execution requests and executes the contract, providing a JSON output
within the server process itself.

More details on the protocol can be found [here](https://github.com/Zilliqa/scilla/wiki/scilla-server-API).

For local testing and experiments, a `scilla-client` is also provided on development
builds (`make dev`). This can interact with `scilla-server`, achieving the same effect
as `scilla-runner` and `scilla-client`.

Start `scilla-server` without any arguments. Examples for checking a contract
and running a transition via `scilla-server` are provided below. They are to be
run on a separate shell (while `scilla-server` continues to run).

```shell
scilla-client run -argv " -init tests/runner/crowdfunding/init.json -istate tests/runner/crowdfunding/state_4.json -iblockchain tests/runner/crowdfunding/blockchain_4.json -imessage tests/runner/crowdfunding/message_4.json -o tests/runner/crowdfunding/output_4.json -i tests/contracts/crowdfunding.scilla -libdir src/stdlib -gaslimit 8000"

scilla-client check -argv " -libdir src/stdlib -gaslimit 8000 tests/contracts/helloWorld.scilla"
```


### Where to find binaries

* The runnables are put into the folder

```
$PROJECT_DIR/bin
```

## Developer Tools

### Emacs mode

An emacs major mode for editing Scilla contracts is [provided](./misc/emacs-mode/scilla-mode.el).
Add the following line to your `.emacs` file to load this mode for files ending with `.scilla` and `.scillib`.
For enabling flycheck mode for Scilla (see [INSTALL.md](./INSTALL.md)). When `scilla-checker` is available,
type reporting is also supported. The key binding `C-c C-t` will print the type of the variable on which
the cursor currently is.

```
;; For enabling flycheck mode for Scilla.
(setq scilla-root "/path/to/scilla/root")
;; Scilla mode
(load-file "/path/to/scilla-mode.el")
```
### VSCode Plugin

Visual Studio Code support for Scilla is avaiable. [Github Source](https://github.com/as1ndu/vscode-scilla)
You can install it through:https://marketplace.visualstudio.com/items?itemName=as1ndu.scilla

Credits: [as1ndu](https://github.com/as1ndu)
