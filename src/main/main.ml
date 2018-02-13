open Printf
open Pervasives

let () =
  let foo z =
    for i = 1 to Array.length Sys.argv - 1 do
      printf "[%i] %d\n" i (int_of_string (Sys.argv.(i)))
    done 
  in foo 5

 
