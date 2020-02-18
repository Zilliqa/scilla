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
open Core_bench

let result_to_option = function
  | Error err ->
      eprintf "Error %s\n%!" (Error.to_string_hum err);
      None
  | Ok r -> Some r

(* Run the [B.Analysis.analyze] for each
   measurement and get back the results *)
let analyze meas =
  (* Usually, we only interested in the number of nano secs taken
     and the relative execution time as a percentage *)
  let analysis_configs = [ Analysis_config.nanos_vs_runs ] in
  let analyze_one m = Analysis.analyze m analysis_configs in
  meas |> List.map ~f:analyze_one |> List.filter_map ~f:result_to_option
