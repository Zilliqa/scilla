  $ for f in *.scilla *.scilexp
  > do
  >   scilla-fmt "$f" > FORMATTED-"$f" &&
  >   scilla-fmt -h --sexp --deannot "$f" > "$f".sexp &&
  >   scilla-fmt -h --sexp --deannot FORMATTED-"$f" > FORMATTED-"$f".sexp &&
  >   diff "$f".sexp FORMATTED-"$f".sexp
  >   if [ $? -ne 0 ]; then
  >     break
  >   fi
  > done
