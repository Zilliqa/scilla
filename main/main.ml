open Printf
open Scilla.ScillaParser
open Scilla.Syntax
open Sexplib.Std

(* let file = ref "" *)
(* let args = [] *)
(* let usage = "Usage: ./main <options> [file] (stdin by default)" *)

(* let () = *)
(*   let filename = Sys.argv.(1) in *)
(*   match parse_file filename with *)
(*     | Some exprs ->  *)
(*       List.iter (fun e -> printf "%s \n" (sexp_of_expr e |> Sexplib.Sexp.to_string)) exprs *)
(*     | None -> *)
(*       printf "%s\n" "Failed to parse" *)
  
type int_pair = (int * int) [@@deriving sexp]

let () =
  let foo z =
    for i = 1 to Array.length Sys.argv - 1 do
      printf "[%i] %d\n" i (Sys.argv.(i) |> int_of_string)
    done
  in
  foo 5;
  printf "%s \n" "aaa"

    (* (sexp_of_int_pair (42, 2) |> Sexplib.Sexp.to_string) *)

 
