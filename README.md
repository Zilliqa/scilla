# Scilla: A smart contract intermediate level language

## Introduction

Scilla is a smart contract language used by the Zilliqa blockchain.

A language reference can be found [here](https://scilla.readthedocs.io/en/latest/)

## Building Scilla

Here are some instructions for building Scilla natively. A dockerfile
is provided in `docker/`, as is a
`Dockerfile.test-modern-ubuntu-build` which we use to test these
instructions.

### 1. Cloning source code

```shell
git clone --jobs 4 --recurse-submodules https://github.com/Zilliqa/scilla/
```

### 2. Build prerequisites

There are packages you'll need - run:

```shell
make install-from-apt
```

To install them, or see the list in the `Makefile`.

### 3. Compiling

You'll need to install `vcpkg` and set `VCPKG_ROOT` to the root of your `vcpkg` installation, following the instructions at <https://github.com/microsoft/vcpkg>.

```sh
export VCPKG_ROOT=/my/directory/vcpkg
export SCILLA_REPO_ROOT=/where/you/checked/out/scilla
apt install libgmp-dev patchelf
```

Now install the opam dependencies:

```sh
make opamdep
eval $(opam env)
```

Now install packages and try to build the first time:

```shl
make
```

If `vcpkg` installation fails, you'll need to set:

```sh
export VCPKG_ALWAYS_INSTALL=true
```

and run `make` again.

The first build will fail, because `Snark.h` doesn't include `<cstdio>` properly. You now need to fix this:

```
sed -i '1s;^;#include <cstdint>\n;' vcpkg_installed/x64-linux-dynamic/include/Snark/Snark.h
```

And you may well need to:

```
touch scilla/_build/default/vcpkg-ocaml/vcpkg-secp256k1/src/c_flags.exp
```

And retry to persuade `secp256k1` to rebuild.

Now build again:

```
make
```

and this time the build should succeed!

### Installation

Scilla can be installed into your opam switch as

```
make install
```

and can similarly be uninstalled as

```
make uninstall
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

Visual Studio Code support for Scilla is available. [Github Source](https://github.com/as1ndu/vscode-scilla)
You can install it through:https://marketplace.visualstudio.com/items?itemName=as1ndu.scilla

Credits: [as1ndu](https://github.com/as1ndu)

