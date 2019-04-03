(*
  This file is part of scilla.

  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.

  scilla is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  scilla.  If not, see <http://www.gnu.org/licenses/>.
*)


open Core
open Syntax
open Yojson
open PrimTypes
open ErrorUtils
open Stdint

(****************************************************************)
(*                    Exception wrappers                        *)
(****************************************************************)

let lookup_constructor_exn cn =
  let t = Datatypes.DataTypeDictionary.lookup_constructor cn in
  match t with
  | Error emsg -> raise (Utils.InternalError (emsg))
  | Ok s-> s

(****************************************************************)
(*                    JSON printing                             *)
(****************************************************************)


let kjson_counter = ref 0.0
let vjson_counter = ref 0.0
let kvjson_counter = ref 0.0
let concat_counter = ref 0.0
let fold_counter = ref 0.0

let rec mapvalues_to_json ms =
  let x = Caml.Hashtbl.fold (fun k v a ->
      (* let foldstart = Unix.gettimeofday() in *)
      (* let tstart = Unix.gettimeofday() in *)
      let kjson = "key", (literal_to_json k) in
      (* let kjson = "key", (`String "0x44345678901234567890123456789012345678cd") in *)
      (* let tend = Unix.gettimeofday() in *)
      (* kjson_counter := !kjson_counter +. (tend -. tstart); *)
      (* let tstart = Unix.gettimeofday() in *)
      (* let vjson = "val", (`String "100") in *)
      let vjson = "val", (literal_to_json v) in
      (* let tend = Unix.gettimeofday() in *)
      (* vjson_counter := !vjson_counter +. (tend -. tstart); *)
      (* let tstart = Unix.gettimeofday() in *)
      let kv_json = `Assoc (kjson :: vjson :: []) in
      (* let tend = Unix.gettimeofday() in *)
      (* kvjson_counter := !kvjson_counter +. (tend -. tstart); *)
      (* let _ = Printf.printf "kvjson:%f\n" (Core.Float.sub tend tstart) in *)
      (* let tstart = Unix.gettimeofday() in *)
      let concat = kv_json :: a in
      (* let tend = Unix.gettimeofday() in *)
      (* concat_counter := !concat_counter +. (tend -. tstart); *)
      (* let _ = Printf.printf "concat:%f\n" (Core.Float.sub tend tstart) in *)
      (* let foldend = Unix.gettimeofday() in *)
      (* fold_counter := !fold_counter +. (foldend -. foldstart); *)
      (* fold_counter := !fold_counter +. 1.0; *)
      concat) ms [] in
  x

and adtargs_to_json vlist =
  match vlist with
  | v1 :: vn ->
    let j2 = literal_to_json v1 in
    let jvn= adtargs_to_json vn in
    (j2 :: jvn)
  | _ -> []

and adttyps_to_json tlist =
  match tlist with
  | t1 :: tn ->
    let j1 = `String (pp_typ t1) in
    let jtn = adttyps_to_json tn in
    (j1 :: jtn)
  | _ -> []

and literal_to_json lit =
  match lit with
  | StringLit (x) | BNum (x) | ByStr(x)
  | ByStrX(_, x) -> `String (x)
  | IntLit x  -> `String (string_of_int_lit x)
  | UintLit x ->
    `String (string_of_uint_lit x)
    (* `String "100" *)
  | Map ((_, _), kvs) ->
    (* let tstart = Unix.gettimeofday() in *)
    let ls = `List (mapvalues_to_json kvs) in
    (* let tend = Unix.gettimeofday() in
       let _ = Printf.printf "map:%f\n" (Core.Float.sub tend tstart) in *)
    ls
  | ADTValue (n, t, v) as ls ->
    let open Datatypes in
    let (a, _) = lookup_constructor_exn n in
    if a.tname = "List"
    then
      (* We make an exception for Lists and print them as a JSON array. *)
      (match Datatypes.scilla_list_to_ocaml_rev ls with
       | Ok ls' -> 
         let ls'' = List.rev_map ls' ~f:(fun a -> literal_to_json a) in
         let lsa = `List ls'' in
         (* let tend = Unix.gettimeofday() in *)
         (* let _ = Printf.printf "map:%f\n" (Core.Float.sub tend tstart) in *)
         lsa
       | Error emsg -> raise (Utils.InternalError emsg))
    else
      let argtl = adttyps_to_json t in
      let argl = adtargs_to_json v in
      let lsa = `Assoc [
          ("constructor", `String n);
          ("argtypes", `List argtl);
          ("arguments", `List argl)
        ] in
      lsa
  | _ -> `Null

let literal_to_jstring ?(pp = false) lit =
  let j = literal_to_json lit in
  if pp then Basic.pretty_to_string j
  else Basic.to_string j

let loc_to_json (l : loc) =
  `Assoc [
    ("file", `String l.fname);
    ("line", `Int l.lnum);
    ("column", `Int l.cnum);
  ]

let scilla_error_to_json elist =
  let err_to_json (e : scilla_error) =
    `Assoc [
      ("error_message", `String e.emsg);
      ("start_location", loc_to_json e.startl);
      ("end_location", loc_to_json e.endl);
    ] in
  let ejl = List.fold_right elist ~init:[] ~f:(fun e acc -> (err_to_json e) :: acc) in
  `List ejl

let scilla_warning_to_json wlist =
  let warning_to_json (w : scilla_warning) =
    `Assoc [
      ("warning_message", `String w.wmsg);
      ("start_location", loc_to_json w.wstartl);
      ("end_location", loc_to_json w.wendl);
      ("warning_id", `Int w.wid);
    ] in
  let ejl = List.fold_right wlist ~init:[] ~f:(fun e acc -> (warning_to_json e) :: acc) in
  `List ejl

let scilla_error_to_jstring ?(pp = true) elist =
  let j' = scilla_error_to_json elist in
  let k' = scilla_warning_to_json (get_warnings()) in
  let j = `Assoc [
      ("errors", j');
      ("warnings", k');
    ] in
  if pp then Basic.pretty_to_string j
  else Basic.to_string j

let scilla_error_to_sstring elist =
  let strip_nl s = Str.global_replace (Str.regexp "[\n]") " " s in
  let pp e =
    let msg = strip_nl e.emsg in
    (sprintf "%s:%d:%d: error: %s" e.startl.fname e.startl.lnum e.startl.cnum msg)
  in
  (List.fold elist ~init:"" ~f:(fun acc e -> acc ^ "\n" ^ (pp e))) ^ "\n"

let scilla_warning_to_sstring wlist =
  let strip_nl s = Str.global_replace (Str.regexp "[\n]") " " s in
  let pp w =
    let msg = strip_nl w.wmsg in
    (sprintf "%s:%d:%d: warning: [%d] %s" w.wstartl.fname w.wstartl.lnum w.wstartl.cnum w.wid msg)
  in
  (List.fold wlist ~init:"" ~f:(fun acc e -> acc ^ "\n" ^ (pp e))) ^ "\n"

let scilla_error_to_string elist  =
  if GlobalConfig.use_json_errors()
  then scilla_error_to_jstring elist
  else (scilla_error_to_sstring elist) ^
       (scilla_warning_to_sstring (get_warnings()))

let scilla_error_gas_jstring ?(pp = true) gas_remaining elist =
  let j' = scilla_error_to_json elist in
  let k' = scilla_warning_to_json (get_warnings ()) in
  let j = `Assoc [
      ("gas_remaining", `String (Uint64.to_string gas_remaining));
      ("errors", j');
      ("warnings", k');
    ] in
  if pp then Basic.pretty_to_string j
  else Basic.to_string j

let scilla_error_gas_string gas_remaining elist  =
  if GlobalConfig.use_json_errors()
  then scilla_error_gas_jstring gas_remaining elist
  else
    (scilla_error_to_sstring elist) ^
    (scilla_warning_to_sstring (get_warnings())) ^
    (sprintf "Gas remaining: %s\n" (Uint64.to_string gas_remaining))

(*****************************************************)
(*                Pretty Printers                    *)
(*****************************************************)

let scilla_version_string =
  let (major, minor, patch) = scilla_version in
  sprintf "%d.%d.%d" major minor patch

let rec pp_literal_simplified l =
  let open Int in
  match l with
  | StringLit s -> "(String " ^ "\"" ^ s ^ "\"" ^ ")"
  (* (bit-width, value) *)
  | IntLit i -> "(Int" ^ (Int.to_string (int_lit_width i))^ " " ^ (string_of_int_lit i) ^ ")"
  (* (bit-width, value) *)
  | UintLit i -> "(Uint" ^ (Int.to_string (uint_lit_width i))^ " " ^ (string_of_uint_lit i) ^ ")"
  | BNum b -> "(BNum " ^ b ^ ")"
  | ByStr s -> "(ByStr " ^ s ^ ")"
  | ByStrX (i, s) -> "(ByStr" ^ (to_string i) ^ " " ^ s ^ ")"
  | Msg m ->
    let items = "[" ^
                List.fold_left m ~init:"" ~f:(fun a (s, l') ->
                    let t = "(" ^ s ^ " : " ^ (pp_literal_simplified l') ^ ")" in
                    if String.is_empty a then t else a ^ " ; " ^ t
                  ) ^ "]" in
    ("(Message " ^ items ^ ")")
  | Map ((_, _), kv) ->
    (* we don't print mtype as that's printed for every entry. *)
    let items = "[" ^
                (Caml.Hashtbl.fold (fun k v a ->
                     let t = "(" ^ (pp_literal_simplified k) ^ " => " ^ (pp_literal_simplified v) ^ ")" in
                     if String.is_empty a then t else a ^ "; " ^ t
                   ) kv "")  ^ "]" in
    ("(Map " ^ items ^ ")")
  | ADTValue (cn, _, al) ->
    (match cn with
     | "Cons" ->
       (* Print non-empty lists in a readable way. *)
       let rec pcons largs =
         if List.length largs = 0 then "(Nil)" else
           let this = (pp_literal_simplified (List.nth_exn largs 0)) ^ ", " in
           let next =
             if List.length largs <> 2 then "(Malformed List)" else
               (match (List.nth_exn largs 1) with
                | ADTValue(_, _, al') ->
                  pcons al'
                | _ -> "(Malformed List") in
           (this ^ next)
       in
       "(List " ^ pcons al ^ ")"
     | "Zero" | "Succ" ->
       let rec counter largs =
         if List.length largs = 0 then "0" else
         if List.length largs <> 1 then "(Malformed Nat)" else
           (match (List.nth_exn largs 0) with
            | ADTValue (_, _, al') ->
              Int.to_string ((Int.of_string (counter al')) + 1)
            | _ -> "(Malformed Nat)")
       in
       "(Nat "^ (counter al) ^ ")"
     | _ ->
       (* Generic printing for other ADTs. *)
       "(" ^ cn ^
       List.fold_left al ~init:"" ~f:(fun a l' -> a ^ " " ^ (pp_literal_simplified l'))
       ^ ")"
    )
  | Clo _ -> "<closure>"
  | TAbs _ -> "<type_closure>"


let pp_literal_json l =
  literal_to_jstring l

let pp_literal l =
  if GlobalConfig.get_pp_lit ()
  then pp_literal_simplified l
  else pp_literal_json l

let pp_literal_map s =
  let ps = List.map s
      ~f:(fun (k, v) -> sprintf " [%s -> %s]" k (pp_literal v)) in
  let cs = String.concat ~sep:",\n " ps in
  sprintf "{%s }" cs

let pp_literal_list ls =
  let ps = List.map ls
      ~f:(fun l -> sprintf " %s" (pp_literal l)) in
  let cs = String.concat ~sep:",\n " ps in
  sprintf "[ %s]" cs
