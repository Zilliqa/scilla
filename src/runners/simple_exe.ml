open Printf
open Sexplib.Std

type int_pair = (int * int) [@@deriving sexp]

let () =
  printf "Arguments: %i\n" (Array.length Sys.argv);
  let foo z =
    for i = 1 to Array.length Sys.argv - 1 do
      printf "[%i] %d\n" i ((Sys.argv.(i) |> int_of_string) + z)
    done
  in
  foo 5;
  printf "%s \n" (sexp_of_int_pair (42, 2) |> Sexplib.Sexp.to_string)


