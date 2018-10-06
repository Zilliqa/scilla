# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: default all utop test clean docker zilliqa-docker

default: all

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
all:
	dune build @install
	@test -L bin || ln -s _build/install/default/bin .

# Launch utop such that it finds the libraroes.
utop: all
	OCAMLPATH=_build/install/default/lib:$(OCAMLPATH) utop

# Build and run tests
test: all
	./bin/testsuite

# Clean up
clean:
# Remove files produced by dune.
	dune clean
# Remove remaining files/folders ignored by git as defined in .gitignore (-X).
	git clean -dfXq

# Build a standalone scilla docker
docker:
	docker build .

# Build a zilliqa-plus-scilla docker based on from zilliqa image ZILLIQA_IMAGE
zilliqa-docker:
	@if [ -z "$(ZILLIQA_IMAGE)" ]; \
	then \
		echo "ZILLIQA_IMAGE not specified" && \
		echo "Usage:\n\tmake zilliqa-docker ZILLIQA_IMAGE=zilliqa:zilliqa" && \
		echo "" && \
		exit 1; \
	fi
	docker build --build-arg BASE_IMAGE=$(ZILLIQA_IMAGE) .

opamdep:
	opam init -y
	opam switch -y 4.06.1
	opam install -y ocaml-migrate-parsetree core cryptokit ppx_sexp_conv yojson batteries angstrom hex ppx_deriving ppx_deriving_yojson menhir oUnit dune stdint fileutils ctypes ctypes-foreign
