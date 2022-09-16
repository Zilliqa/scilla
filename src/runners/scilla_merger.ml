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
open Scilla_base
open Scilla_merge
open Merge
open JSONMerge
open ErrorUtils
module MMerger = ScillaMerger (Checker.TCSRep) (Checker.TCERep)

module Fmt =
  Formatter.Format (Checker.TCSRep) (Checker.TCERep) (MMerger.PLiteral)

let parse_args args =
  let startswith s c =
    String.index s c |> Option.value_map ~default:false ~f:(phys_equal 0)
  in
  let rec aux (acc_args, acc_files, config_path, json_info) = function
    | [] -> (acc_args, acc_files, config_path, json_info)
    | x :: y :: xs when String.equal x "--config" || String.equal x "-c" ->
        aux (acc_args, acc_files, Some y, json_info) xs
    | x :: y :: z :: xs when String.equal x "--json" ->
        let j = { JSONMerge.file = y; JSONMerge.contract_name = z } in
        aux (acc_args, acc_files, config_path, json_info @ [ j ]) xs
    | x :: y :: xs when startswith x '-' ->
        aux (acc_args @ [ x ] @ [ y ], acc_files, config_path, json_info) xs
    | x :: xs -> aux (acc_args, acc_files @ [ x ], config_path, json_info) xs
  in
  aux ([], [], None, []) args

(** Merges smart-contract files. *)
let merge_contracts files args config =
  List.fold_left files ~init:[] ~f:(fun acc file ->
      let _, (cmod, rlibs, elibs) =
        Checker.check_cmod
          (Some (args @ [ file ]))
          ~exe_name:(Sys.get_argv ()).(0)
      in
      acc @ [ (cmod, rlibs, elibs) ])
  |> MMerger.run config
  |> fun (output, warnings) ->
  DebugMessage.perr warnings;
  Option.value_map output ~default:""
    ~f:(fun ((cmod : MMerger.PSyntax.cmodule), _rlibs) ->
      Fmt.contract_to_string cmod)
  |> DebugMessage.pout

(** Merges [init.json] files. *)
let merge_init_jsons files config =
  ScillaJSONMerger.run config files |> fun (output, warnings) ->
  DebugMessage.perr warnings;
  DebugMessage.pout output

let run args =
  try
    let args, files, config_path, jsons = parse_args args in
    let config =
      Option.value_map config_path ~default:None ~f:(fun path ->
          match Config.from_file path with
          | Ok cfg -> Some cfg
          | Error err ->
              exit_with_error (err ^ "\n");
              None)
    in
    if List.is_empty jsons then merge_contracts files args config
    else merge_init_jsons jsons config
  with FatalError msg -> exit_with_error msg

let () =
  run (Sys.get_argv () |> Array.to_list |> List.tl |> Option.value ~default:[])
