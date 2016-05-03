secp256k1_wrap.o: secp256k1_wrap.c
	ocamlc -c $<

secp256k1_ml.so: secp256k1_wrap.o
	ocamlmklib  -o  secp256k1_ml  $<

secp256k1.mli: secp256k1.ml
	ocamlc -i $< > $@

secp256k1.cmi: secp256k1.mli
	ocamlc -c $<

secp256k1.cmo: secp256k1.ml secp256k1.cmi
	ocamlc -c $<

secp256k1.cma:  secp256k1.cmo  secp256k1_ml.so
	ocamlc -a  -o $@  $<  -dllib -lsecp256k1_ml

secp256k1.cmx: secp256k1.ml secp256k1.cmi
	ocamlopt -c $<

secp256k1.cmxa:  secp256k1.cmx  secp256k1_ml.so
	ocamlopt -a  -o $@  $<  -cclib -lsecp256k1_ml

clean:
	rm -f *.[oa] *.so *.cm[ixoa] *.cmxa
	
all:
	make secp256k1.cmxa