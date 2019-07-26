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

let rec mapvalues_to_json ms =
  Caml.Hashtbl.fold (fun k v a ->
    let kjson = "key", (literal_to_json k) in
    let vjson = "val", (literal_to_json v) in
    let kv_json = `Assoc (kjson :: vjson :: []) in
      kv_json :: a) ms []

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
  | StringLit x | BNum x -> `String x
  | ByStr bs -> `String (Bystr.hex_encoding bs)
  | ByStrX bs -> `String (Bystrx.hex_encoding bs)
  | IntLit x  -> `String (string_of_int_lit x)
  | UintLit x -> `String (string_of_uint_lit x)
  | Map ((_, _), kvs) ->
      `List (mapvalues_to_json kvs)
  | ADTValue (n, t, v) as ls ->
    let open Datatypes in
    let (a, _) = lookup_constructor_exn n in
     if a.tname = "List"
    then
      (* We make an exception for Lists and print them as a JSON array. *)
      (match Datatypes.scilla_list_to_ocaml_rev ls with
      | Ok ls' -> 
        let ls'' = List.rev_map ls' ~f:(fun a -> literal_to_json a) in
        `List ls''
      | Error emsg -> raise (Utils.InternalError emsg))
    else
      let argtl = adttyps_to_json t in
      let argl = adtargs_to_json v in
        `Assoc [
          ("constructor", `String n);
          ("argtypes", `List argtl);
          ("arguments", `List argl)
        ]
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

(* Print a message with location info. *)
let located_msg msg loc =
  let open ErrorUtils in
  (sprintf "%s:%d:%d: %s" loc.fname loc.lnum loc.cnum msg)

let fatal_error err =
  DebugMessage.perr @@ scilla_error_to_string err; exit 1

let fatal_error_gas err gas_remaining =
  DebugMessage.perr @@ scilla_error_gas_string gas_remaining err; exit 1

let fatal_error_noformat err =
  DebugMessage.perr err; exit 1

(*****************************************************)
(*                Pretty Printers                    *)
(*****************************************************)

let scilla_version_string =
  let (major, minor, patch) = scilla_version in
  sprintf "%d.%d.%d" major minor patch

let rec pp_literal_simplified l =
    match l with
    | StringLit s -> "(String " ^ "\"" ^ s ^ "\"" ^ ")"
    (* (bit-width, value) *)
    | IntLit i -> "(Int" ^ (Int.to_string (int_lit_width i))^ " " ^ (string_of_int_lit i) ^ ")"
    (* (bit-width, value) *)
    | UintLit i -> "(Uint" ^ (Int.to_string (uint_lit_width i))^ " " ^ (string_of_uint_lit i) ^ ")"
    | BNum b -> "(BNum " ^ b ^ ")"
    | ByStr bs -> "(ByStr " ^ Bystr.hex_encoding bs ^ ")"
    | ByStrX bsx -> "(ByStr" ^ (Int.to_string (Bystrx.width bsx)) ^ " " ^ Bystrx.hex_encoding bsx ^ ")"
    | Msg m ->
      let items = "[" ^
        List.fold_left m ~init:"" ~f:(fun a (s, l') ->
          let t = "(" ^ s ^ " : " ^ (pp_literal_simplified l') ^ ")" in
            if String.is_empty a then t else a ^ " ; " ^ t
          ) ^ "]" in
      ("(Message " ^ items ^ ")")
    | Map ((kt, vt), kv) ->
      let items = "[" ^
        (Caml.Hashtbl.fold (fun k v a ->
          let t = "(" ^ (pp_literal_simplified k) ^ " => " ^ (pp_literal_simplified v) ^ ")" in
            if String.is_empty a then t else a ^ "; " ^ t
          ) kv "")  ^ "]" in
      ("(Map " ^ pp_typ kt ^ " " ^ pp_typ vt ^ " "  ^ items ^ ")")
    | ADTValue (cn, _, al) ->
        (match cn with
        | "Cons" ->
          (* Print non-empty lists in a readable way. *)
          let list_buffer = Buffer.create 1024 in
          let rec plist = function
            | ADTValue ("Nil", _, []) -> Buffer.add_string list_buffer "(Nil)"
            | ADTValue ("Cons", _, [head; tail]) ->
                let head_str = (pp_literal_simplified head) ^ ", " in
                Buffer.add_string list_buffer head_str;
                plist tail
            | _ -> Buffer.clear list_buffer; Buffer.add_string list_buffer "(Malformed List)"
          in
          plist l;
          "(List " ^ Buffer.contents list_buffer ^ ")"
        | "Zero" | "Succ" ->
            let rec counter nat acc =
              match nat with
              | ADTValue ("Zero", _, []) -> Some acc
              | ADTValue ("Succ", _, [pred]) -> counter pred (Uint32.succ acc)
              | _ -> None
            in
            let res = Option.map (counter l Uint32.zero) ~f:Uint32.to_string in
            "(Nat " ^ Option.value res ~default:"(Malformed Nat)" ^ ")"
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

let pp_typ_map s =
  let ps = List.map s
    ~f:(fun (k, v) -> sprintf " [%s : %s]" k (pp_typ v)) in
  let cs = String.concat ~sep:",\n"  ps in
  sprintf "{%s }" cs
