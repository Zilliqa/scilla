  $ for f in *.scilla
  > do
  >   scilla-checker -gaslimit 8000 -cf -libdir ../src/stdlib "$f" 2> CHECK-"$f" &&
  >   diff CHECK-"$f" "$f".gold
  >   if [ $? -ne 0 ]; then
  >     break
  >   fi
  > done

