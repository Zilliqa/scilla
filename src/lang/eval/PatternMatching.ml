(*
 * Copyright (c) 2018 - present , Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Core
open Sexplib.Std
open Datatypes
open Syntax
open EvalUtil
open Result.Let_syntax
open MonadUtil

let rec match_with_pattern v p = match p with
  | Wildcard -> pure @@ []
  | Binder x -> (match v with
      | Env.ValClosure _ | Env.ValFix _ ->
          fail @@ sprintf "Cannot pattern match a function:\n%s"
            (Env.pp_value v)
      | Env.ValLit _ ->
          (* Bound a plain literal *)
          pure @@ [(x, v)]
    )
  | Constructor (cn, ps) ->
      let%bind (_, ctr) =
        DataTypeDictionary.lookup_constructor cn in
      (* Check that the pattern is well-formed *)
      if ctr.arity <> List.length ps
      then fail @@
        sprintf "Constructor %s requires %d parameters, but %d are provided."
          ctr.cname ctr.arity (List.length ps)
      (* Pattern is well-formed, processing the value *)    
      else (match v with
          | Env.ValLit (ADTValue (cn', ts', ls'))
            when cn' = ctr.cname &&
                 (List.length ls') = ctr.arity  ->
              (* The value structure matches the pattern *)
              let vs = List.map ls' ~f:(fun l -> Env.ValLit l) in
              (match List.zip vs ps with
               | None -> fail
                           "Pattern and value lists have different length"
               | Some sub_matches ->
                   let%bind res_list =
                     mapM sub_matches
                       ~f:(fun (w, q) -> match_with_pattern w q) in
                   (* Careful: there might be duplicate bindings! *)
                   (* We will need to catch this statically. *)
                   pure @@ ListLabels.flatten res_list)

          | _ -> fail @@
              sprintf "Cannot match value %s againts pattern %s."
                (Env.pp_value v)
                (sexp_of_pattern sexp_of_loc p |> Sexplib.Sexp.to_string))
