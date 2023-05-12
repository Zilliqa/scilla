all:
	dune build @install @runtest @doc 
	rm -rf docs/* && cp -r _build/default/_doc/_html/* docs/
clean:
	rm -rf _build
