# Invoke `make` to build, `make clean` to clean up, etc.

.PHONY: default all utop dev clean docker zilliqa-docker

default: all

# Build one library and one standalone executable that implements
# multiple subcommands and uses the library.
# The library can be loaded in utop for interactive testing.
all:
	dune build --profile release @install
	@test -L bin || ln -s _build/install/default/bin .

# Build only scilla-checker and scilla-runner
slim:
	dune build --profile release src/runners/scilla_runner.exe
	dune build --profile release src/runners/scilla_checker.exe
	@test -L bin || mkdir bin; ln -s _build/default/src/runners/*.exe bin/

dev:
	dune build --profile dev @install
	@test -L bin || ln -s _build/install/default/bin .

# Launch utop such that it finds the libraroes.
utop: all
	OCAMLPATH=_build/install/default/lib:$(OCAMLPATH) utop

# Build and run tests
test: dev
	./bin/testsuite -print-diff true

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
	opam init --disable-sandboxing -y --compiler=4.06.1
	opam install ./scilla.opam --deps-only --with-test --yes


.PHONY : coverage
coverage :
	make clean
	mkdir -p _build/coverage
	BISECT_ENABLE=YES make
	./bin/testsuite
	bisect-ppx-report -I _build/default/ -html _coverage/ `find . -name 'bisect*.out'`
	make clean
	-find . -name 'bisect*.out' | xargs rm

.PHONY : coveralls
coveralls:
	make clean
	mkdir -p _build/coverage
	BISECT_ENABLE=YES make
	./bin/testsuite
	bisect-ppx-report -ignore-missing-files -I _build/ -coveralls coverage.json -service-name travis-ci -service-job-id ${TRAVIS_JOB_ID} `find . -name 'bisect*.out'`
	curl -L -F json_file=@./coverage.json https://coveralls.io/api/v1/jobs
	make clean
	-find . -name 'bisect*.out' | xargs rm
