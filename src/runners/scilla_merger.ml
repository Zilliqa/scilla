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
open Scilla_product
open Product
open ErrorUtils
module MProduct = ScillaProduct (Checker.TCSRep) (Checker.TCERep)

module Fmt =
  Formatter.Format (Checker.TCSRep) (Checker.TCERep) (MProduct.PLiteral)

let parse_args args =
  let startswith s c =
    String.index s c |> Option.value_map ~default:false ~f:(phys_equal 0)
  in
  let rec aux (acc_args, acc_files, config_path) = function
    | [] -> (acc_args, acc_files, config_path)
    | x :: y :: xs when String.equal x "--config" || String.equal x "-c" ->
        aux (acc_args, acc_files, Some y) xs
    | x :: y :: xs when startswith x '-' ->
        aux (acc_args @ [ x ] @ [ y ], acc_files, config_path) xs
    | x :: xs -> aux (acc_args, acc_files @ [ x ], config_path) xs
  in
  aux ([], [], None) args

let run args =
  try
    let args, files, config_path = parse_args args in
    let config =
      Option.value_map config_path ~default:None ~f:(fun path ->
          match Config.from_file path with
          | Ok cfg -> Some cfg
          | Error err ->
              exit_with_error (err ^ "\n");
              None)
    in
    List.fold_left files ~init:[] ~f:(fun acc file ->
        let _, (cmod, rlibs, elibs) =
          Checker.check_cmod
            (Some (args @ [ file ]))
            ~exe_name:(Sys.get_argv ()).(0)
        in
        acc @ [ (cmod, rlibs, elibs) ])
    |> MProduct.run config
    |> fun (output, warnings) ->
    DebugMessage.perr warnings;
    Option.value_map output ~default:""
      ~f:(fun ((cmod : MProduct.PSyntax.cmodule), _rlibs) ->
        Fmt.contract_to_string cmod)
    |> DebugMessage.pout
  with FatalError msg -> exit_with_error msg

let () =
  run (Sys.get_argv () |> Array.to_list |> List.tl |> Option.value ~default:[])
