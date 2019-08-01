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
open Datatypes
open Syntax
open EvalUtil
open MonadUtil
open Result.Let_syntax
open EvalSyntax

let rec match_with_pattern v p = match p with
  | Wildcard -> pure []
  | Binder x -> pure @@ [(x, v)]
  | Constructor (cn, ps) ->
      let%bind (_, ctr) =
        DataTypeDictionary.lookup_constructor cn in
      (* Check that the pattern is well-formed *)
      if ctr.arity <> List.length ps
      then fail0 @@
        sprintf "Constructor %s requires %d parameters, but %d are provided."
          ctr.cname ctr.arity (List.length ps)
      (* Pattern is well-formed, processing the value *)    
      else (match v with
          | ADTValue (cn', _, ls')
            when cn' = ctr.cname &&
                 (List.length ls') = ctr.arity  ->
              (* The value structure matches the pattern *)
              (match List.zip ls' ps with
               | None -> fail0 "Pattern and value lists have different length"
               | Some sub_matches ->
                   let%bind res_list =
                     mapM sub_matches
                       ~f:(fun (w, q) -> match_with_pattern w q) in
                   (* Careful: there might be duplicate bindings! *)
                   (* We will need to catch this statically. *)
                   pure @@ ListLabels.flatten res_list)

          | _ -> fail0 @@
              sprintf "Cannot match value %s againts pattern %s."
                (Env.pp_value v)
                (sexp_of_pattern p |> Sexplib.Sexp.to_string))
