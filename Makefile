all:
	dune build @install @runtest

clean:
	rm -rf _build
