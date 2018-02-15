open Printf
open Sexplib.Std
open Syntax

(* let file = ref "" *)
(* let args = [] *)
(* let usage = "Usage: ./main <options> [file] (stdin by default)" *)

let () =
  let filename = Sys.argv.(1) in
  match FrontEndParser.parse_file filename with
    | Some exprs ->
      List.iter (fun e -> printf "%s \n" (sexp_of_expr e |> Sexplib.Sexp.to_string)) exprs
    | None ->
      printf "%s\n" "Failed to parse input file."
  


