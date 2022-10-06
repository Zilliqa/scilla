  $ for f in *.scilla
  > do
  >   scilla-fmt "$f" > FORMATTED-"$f" &&
  >   diff "$f".gold FORMATTED-"$f"
  >   if [ $? -ne 0 ]; then
  >     break
  >   fi
  > done
