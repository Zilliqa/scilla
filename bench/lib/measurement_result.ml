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
open ScillaUtil.FilePathInfix

module R = Analysis_result.Regression
module C = Analysis_result.Coefficient

type t =
  { benchmark_name : string;
    machine_id : string;
    ocaml_version : string;

    time_r_square : float;
    time_per_run_nanos : float;
    ci95_upper_bound : float;
    ci95_lower_bound : float;
    minor_words_per_run : float;
    major_words_per_run : float;
    promoted_words_per_run : float;
  } [@@deriving sexp]

let mk res =
  let open Analysis_result in
  let estimate regr = C.estimate (R.coefficients regr).(0) in
  let get_ci regr = C.ci95 (R.coefficients regr).(0) in
  let check_time_preds regr =
    let preds = R.predictors regr in
    Array.length preds = 1 && Array.exists preds ~f:((=) `Runs)
  in
  let check_overhead_preds regr =
    let preds = R.predictors regr in
    Array.length preds = 2 && Array.exists preds ~f:((=) `One)
    && Array.exists preds ~f:((=) `Runs)
  in
  let init =
    { benchmark_name = name res;
      machine_id = Unix.gethostname ();
      ocaml_version = Version_util.ocaml_version;

      time_r_square = 0.;
      time_per_run_nanos = 0.;
      ci95_upper_bound = 0.;
      ci95_lower_bound = 0.;
      minor_words_per_run = 0.;
      major_words_per_run = 0.;
      promoted_words_per_run = 0.;
    } in
  Array.fold (regressions res) ~init ~f:(fun acc regr ->
      let acc = match R.r_square regr with
      | Some rsq -> { acc with time_r_square = rsq }
      | None -> acc
      in
      let value = estimate regr in
      match (R.responder regr) with
      | `Nanos ->
        if check_time_preds regr
        then
          begin match get_ci regr with
          | None -> { acc with time_per_run_nanos = value }
          | Some ci ->
            let (ci_minus, ci_plus) = Ci95.ci95_rel_err ci ~estimate:value
            in { acc with
                 time_per_run_nanos = value;
                 ci95_upper_bound = ci_plus;
                 ci95_lower_bound = ci_minus
               }
          end
        else acc
      | `Minor_allocated ->
        if check_overhead_preds regr
        then { acc with minor_words_per_run = value }
        else acc
      | `Major_allocated ->
        if check_overhead_preds regr
        then { acc with major_words_per_run = value }
        else acc
      | `Promoted ->
        if check_overhead_preds regr
        then { acc with promoted_words_per_run = value }
        else acc
      | _ -> acc
    )

let save res ~path =
  let name = Util.sanitize res.benchmark_name in
  let filename = path ^/ name ^. "sexp" in
  let data = res |> sexp_of_t |> Sexp.to_string in
  Out_channel.write_all filename ~data

let load path =
  path
  |> In_channel.read_all
  |> Sexp.of_string
  |> t_of_sexp
