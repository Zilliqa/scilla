# Invoke `make` to build, `make clean` to clean up, etc.

OCAML_VERSION_RECOMMENDED=4.08.1
# In case of upgrading ocamlformat version:
# package.json also needs updating
OCAMLFORMAT_VERSION=0.18.0
IPC_SOCK_PATH="/tmp/zilliqa.sock"
CPPLIB_DIR=${PWD}/_build/default/src/base/cpp

# Dependencies useful for developing Scilla
OPAM_DEV_DEPS := \
merlin \
ocamlformat.$(OCAMLFORMAT_VERSION) \
ocp-indent \
utop

.PHONY: default release utop dev clean docker zilliqa-docker

default: release

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
release:
	./scripts/build_deps.sh
	dune build --profile release @install
	@test -L bin || ln -s _build/install/default/bin .

# Build only scilla-checker and scilla-runner
slim:
	./scripts/build_deps.sh
	dune build --profile release src/runners/scilla_runner.exe @install
	dune build --profile release src/runners/scilla_checker.exe @install
	@test -L bin || ln -s _build/install/default/bin .

dev:
	./scripts/build_deps.sh
	dune build --profile dev @install
	dune build --profile dev tests/scilla_client.exe
	@test -L bin || ln -s _build/install/default/bin .
	ln -s ../../../default/tests/scilla_client.exe _build/install/default/bin/scilla-client

# Update src/base/ParserFaults.messages
parser-messages:
	menhir --list-errors src/base/ScillaParser.mly >src/base/NewParserFaultsStubs.messages
	menhir --merge-errors src/base/ParserFaults.messages \
		   --merge-errors src/base/NewParserFaultsStubs.messages \
		   src/base/ScillaParser.mly  >src/base/NewParserFaults.messages
	mv src/base/NewParserFaults.messages src/base/ParserFaults.messages
	rm src/base/NewParserFaultsStubs.messages

# Launch utop such that it finds the libraroes.
utop: release
	OCAMLPATH=_build/install/default/lib:$(OCAMLPATH) utop

fmt:
	dune build @fmt --auto-promote

# Lint OCaml and dune source files, all the opam files in the project root, and the shell scripts
lint:
	dune build @fmt
	opam lint .
	shellcheck scripts/*.sh && shellcheck easyrun.sh && shellcheck tests/runner/pingpong.sh

# Installer, uninstaller and test the installation
install : release
	dune install

# This is different from the target "test" which runs on dev builds.
test_install : install
	ulimit -n 1024; dune exec -- tests/polynomials/testsuite_polynomials.exe
	ulimit -n 1024; dune exec -- tests/base/testsuite_base.exe -print-diff true
	ulimit -n 1024; dune exec -- tests/testsuite.exe -print-diff true

uninstall : release
	dune uninstall

# Debug with ocamldebug: Build byte code instead of native code.
debug :
	dune build --profile dev src/runners/scilla_runner.bc
	dune build --profile dev src/runners/scilla_checker.bc
	dune build --profile dev src/runners/type_checker.bc
	dune build --profile dev src/runners/eval_runner.bc
	@echo "Note: LD_LIBRARY_PATH must be set to ${CPPLIB_DIR} before execution"
	@echo "Example: LD_LIBRARY_PATH=${CPPLIB_DIR} ocamldebug _build/default/src/runners/scilla_checker.bc -libdir src/stdlib -gaslimit 10000 tests/contracts/helloworld.scilla"

# === TESTS (begin) ===========================================================
# Build and run tests

testbase: dev
  # This effectively adds all the runners into PATH variable
	ulimit -n 1024; dune exec -- tests/base/testsuite_base.exe -print-diff true

goldbase: dev
	ulimit -n 4096; dune exec tests/base/testsuite_base.exe -- -update-gold true

# Run all tests for all packages in the repo: scilla-base, polynomials, scilla
test: dev
	ulimit -n 1024; dune exec -- tests/polynomials/testsuite_polynomials.exe
	ulimit -n 1024; dune exec -- tests/base/testsuite_base.exe -print-diff true
	ulimit -n 1024; dune exec -- tests/testsuite.exe -print-diff true

gold: dev
	ulimit -n 4096; dune exec -- tests/base/testsuite_base.exe -update-gold true
	ulimit -n 4096; dune exec -- tests/testsuite.exe -update-gold true

# This must be run only if there is an external IPC server available
# that can handle access requests. It is important to use the sequential runner here as we
# don't want multiple threads of the testsuite connecting to the same server concurrently.
test_extipcserver: dev
	dune exec -- tests/testsuite.exe -print-diff true -runner sequential \
	-ext-ipc-server $(IPC_SOCK_PATH) \
	-only-test "tests:0:contract_tests:0:these_tests_must_SUCCEED"

# Run tests in server-mode
test_server: dev
	dune build src/runners/scilla_server.exe
	./_build/default/src/runners/scilla_server.exe &
	dune exec tests/testsuite.exe -- -print-diff true -runner sequential \
  -server true \
	-only-test "tests:0:contract_tests:0:these_tests_must_SUCCEED"

# === TESTS (end) =============================================================


# Clean up
clean:
# Remove files produced by dune.
	dune clean
# Remove remaining files/folders ignored by git as defined in .gitignore (-X)
# but keeping a local opam switch and other dependencies built.
	git clean -dfXq --exclude=\!deps/** --exclude=\!_opam/** --exclude=\!_esy/**

# Clean up libff installation
cleanall: clean
	rm -rf deps/cryptoutils/{build,install} deps/schnorr/{build,install}

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

# Create an opam-based development environment
.PHONY : opamdep
opamdep:
	opam init --compiler=$(OCAML_VERSION_RECOMMENDED) --yes
	eval $$(opam env)
	opam install ./scilla.opam --deps-only --with-test --yes
	opam install --yes $(OPAM_DEV_DEPS)

.PHONY : dev-env
dev-deps:
	opam install --yes $(OPAM_DEV_DEPS)

.PHONY : opamdep-ci
opamdep-ci:
	opam init --disable-sandboxing --compiler=$(OCAML_VERSION) --yes
	eval $$(opam env)
	opam install ./scilla.opam --deps-only --with-test --yes
	opam install ocamlformat.$(OCAMLFORMAT_VERSION) --yes

.PHONY : coverage
coverage :
	make clean
	mkdir -p _build/coverage
	./scripts/build_deps.sh
	BISECT_ENABLE=YES make
	dune build @install
	ulimit -n 1024; dune exec -- tests/testsuite.exe
	bisect-ppx-report html
	make clean
	-find . -type f -name 'bisect*.coverage' | xargs rm

.PHONY : coveralls
coveralls:
	make clean
	mkdir -p _build/coverage
	./scripts/build_deps.sh
	BISECT_ENABLE=YES make
	dune build @install
	ulimit -n 1024; dune exec -- tests/testsuite.exe
	bisect-ppx-report coveralls coverage.json --ignore-missing-files --service-name travis-ci --service-job-id ${TRAVIS_JOB_ID}
	curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
	make clean
	-find . -type f -name 'bisect*.coverage' | xargs rm


# Diagnostic builds

verbose:
	dune build --profile dev @install --verbose

# sequential build
verbose-j1:
	dune build -j1 --profile dev @install --verbose
