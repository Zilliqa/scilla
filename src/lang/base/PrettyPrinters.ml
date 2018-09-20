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


(*****************************************************)
(*                Pretty Printers                    *)
(*****************************************************)

let rec pp_literal_simplified l =
    let open Int in
    match l with
    | StringLit s -> "(String " ^ "\"" ^ s ^ "\"" ^ ")"
    (* (bit-width, value) *)
    | IntLit (b, i) -> "(Int" ^ (to_string b) ^ " " ^ i ^ ")"
    (* (bit-width, value) *)
    | UintLit (b, ui) -> "(Int" ^ (to_string b) ^ " " ^ ui ^ ")"
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
        List.fold_left kv ~init:"" ~f:(fun a (k, v) ->
          let t = "(" ^ (pp_literal_simplified k) ^ " => " ^ (pp_literal_simplified v) ^ ")" in
            if String.is_empty a then t else a ^ "; " ^ t
          ) ^ "]" in
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

let pp_literal_json l =
  JSON_pp.literal_to_jstring l

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
