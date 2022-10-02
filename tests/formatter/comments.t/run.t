  $ for f in *.scilla
  > do
  >   scilla-fmt "$f" > FORMATTED-"$f" &&
  >   diff "$f".gold FORMATTED-"$f"
  >   if [ $? -ne 0 ]; then
  >     break
  >   fi
  > done
  4a5
  > (* A comment *)
  11a13,21
  > 
  > (* B comment *)
  > type B =
  > | B1
  > | B2 (* Multiline B2
  >     comment *)
  > 
  > type C =
  > | C1
