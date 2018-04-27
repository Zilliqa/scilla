Scilla: A Smart Contract Intermediate Level Language

## Building and Running

### Build requirements

Install the folowing dependencies for OCaml:

* `opam`, package manager for OCaml, version >= 1.2'
* `jbuilder` build tool, can be installed via `opam install jbuilder`
* `ocamlc`, version >= 4.05

The package dependencies can be installed via `opam` as follows:

```
opam install ocaml-migrate-parsetree
opam install core cryptokit ppx_sexp_conv yojson
opam install menhir 
```

### Compiling the project

Just run `make clean; make` from the root folder

To invoke a simple runner (subject to
[ongoing implementation](./ROADMAP.md)), execute from the project
root:

```
./_build/install/default/bin/scilla-module-parser examples/contracts/zil-game.scilla 
```

### Where to find binaries

* The runnables are put into the folder

```
$PROJECT_DIR/_build/install/default/bin
```

## Using Ocaml with Emacs

The following extensions would be useful:

* [tuareg](https://github.com/ocaml/tuareg) for syntax highlighting
* [merlin](https://github.com/ocaml/merlin/wiki/emacs-from-scratch) for auto-completion
* [ocp-indent](https://github.com/OCamlPro/ocp-indent) for smart indentation

All thos libraries can be installed via OPAM, e.g.,

```
opam install merlin
```
