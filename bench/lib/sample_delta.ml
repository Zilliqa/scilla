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

let assert_length x y ~name =
  let xl = Array.length x in
  let yl = Array.length y in
  if xl <> yl then failwith @@ sprintf
      "Measurement \"%s\" have different number \
       of samples: %d <> %d" name xl yl

let calc x y =
  let open Measurement_sample in
  (* Actually, we're only interested in the [runs] and [nanos].
     But let's just calculate everything for consistency.
     Maybe we'll need other numbers later *)
  { runs              = abs (x.runs - y.runs);
    cycles            = abs (x.cycles - y.cycles);
    nanos             = abs (x.nanos - y.nanos);
    compactions       = abs (x.compactions - y.compactions);
    minor_allocated   = abs (x.minor_allocated - y.minor_allocated);
    major_allocated   = abs (x.major_allocated - y.major_allocated);
    promoted          = abs (x.promoted - y.promoted);
    major_collections = abs (x.major_collections - y.major_collections);
    minor_collections = abs (x.minor_collections - y.minor_collections);
  }
