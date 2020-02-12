# Invoke `make` to build, `make clean` to clean up, etc.

OCAML_VERSION_RECOMMENDED=4.07.1
IPC_SOCK_PATH="/tmp/zilliqa.sock"

.PHONY: default release utop dev clean docker zilliqa-docker

default: release

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
release:
	./scripts/libff.sh
	dune build --profile release @install

# Build only scilla-checker and scilla-runner
slim:
	./scripts/libff.sh
	dune build --profile release src/runners/scilla_runner.exe
	dune build --profile release src/runners/scilla_checker.exe

dev:
	./scripts/libff.sh
	dune build --profile dev @install
	@test -L bin || ln -s _build/install/default/bin .

# Launch utop such that it finds the libraroes.
utop: release
	OCAMLPATH=_build/install/default/lib:$(OCAMLPATH) utop

fmt:
	dune build @fmt --auto-promote

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

# === TESTS (begin) ===========================================================
# Build and run tests

testbase: dev
  # This effectively adds all the runners into PATH variable
	ulimit -n 1024; dune exec -- tests/base/testsuite_base.exe -print-diff true

goldbase: dev
	ulimit -n 1024; dune exec tests/base/testsuite_base.exe -- -update-gold true

# Run all tests for all packages in the repo: scilla-base, polynomials, scilla
test: dev
	ulimit -n 1024; dune exec -- tests/polynomials/testsuite_polynomials.exe
	ulimit -n 1024; dune exec -- tests/base/testsuite_base.exe -print-diff true
	ulimit -n 1024; dune exec -- tests/testsuite.exe -print-diff true

gold: dev
	ulimit -n 1024; dune exec -- tests/base/testsuite_base.exe -update-gold true
	ulimit -n 1024; dune exec -- tests/testsuite.exe -update-gold true

# This must be run only if there is an external IPC server available
# that can handle access requests. It is important to use the sequential runner here as we
# don't want multiple threads of the testsuite connecting to the same server concurrently.
test_extipcserver: dev
	dune exec -- tests/testsuite.exe -print-diff true -runner sequential \
	-ext-ipc-server $(IPC_SOCK_PATH) \
	-only-test "all_tests:0:contract_tests:0:these_tests_must_SUCCEED"

# Run tests in server-mode
test_server: dev
	ln -sr _build/default/tests/scilla_client.exe bin/scilla-client
	dune exec src/runners/scilla_server.exe &
	dune exec tests/testsuite.exe -- -print-diff true -runner sequential \
  -server true \
	-only-test "all_tests:0:contract_tests:0:these_tests_must_SUCCEED"

# === TESTS (end) =============================================================


# Clean up
clean:
# Remove files produced by dune.
	dune clean
# Remove remaining files/folders ignored by git as defined in .gitignore (-X)
# but keeping a local opam switch and other dependencies built.
	git clean -dfXq --exclude=\!deps/** --exclude=\!_opam/**

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

.PHONY : opamdep
opamdep:
	opam init --compiler=$(OCAML_VERSION_RECOMMENDED) --yes
	eval $$(opam env)
	opam install ./scilla.opam --deps-only --with-test --yes

.PHONY : opamdep-ci
opamdep-ci:
	opam init --disable-sandboxing --compiler=$(OCAML_VERSION) --yes
	eval $$(opam env)
	opam install ./scilla.opam --deps-only --with-test --yes

.PHONY : coverage
coverage :
	make clean
	mkdir -p _build/coverage
	./scripts/libff.sh
	BISECT_ENABLE=YES make
	dune build @install
	dune exec -- tests/testsuite.exe
	bisect-ppx-report -I _build/default/ -html _coverage/ `find . -name 'bisect*.out'`
	make clean
	-find . -name 'bisect*.out' | xargs rm

.PHONY : coveralls
coveralls:
	make clean
	mkdir -p _build/coverage
	./scripts/libff.sh
	BISECT_ENABLE=YES make
	dune build @install
	dune exec -- tests/testsuite.exe
	bisect-ppx-report -ignore-missing-files -I _build/ -coveralls coverage.json -service-name travis-ci -service-job-id ${TRAVIS_JOB_ID} `find . -name 'bisect*.out'`
	curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
	make clean
	-find . -name 'bisect*.out' | xargs rm


# Diagnostic builds

verbose:
	dune build --profile dev @install --verbose

# sequential build
verbose-j1:
	dune build -j1 --profile dev @install --verbose
