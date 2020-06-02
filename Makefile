all:
	dune build @install @runtest @doc 
	rm -rf docs/* && cp -r _build/default/_doc/_html/secp256k1/* docs/
clean:
	rm -rf _build
