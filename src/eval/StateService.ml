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

open Core_kernel
open! Int.Replace_polymorphic_compare
open Result.Let_syntax
open MonadUtil
open TypeUtil
open Syntax
module ER = ParserRep
module SR = ParserRep
module SSTypeUtil = TypeUtilities
module EvalSyntax = ScillaSyntax (SR) (ER)
open EvalSyntax

type ss_field = {
  fname : string;
  ftyp : typ;
  fval : literal option; (* We may or may not have the value in memory. *)
}

type service_mode =
  | IPC of string
  (* Socket address for IPC *)
  | Local

type ss_state = Uninitialized | SS of service_mode * ss_field list

module MakeStateService () = struct
  (* Internal state for the state service. *)
  let ss_cur_state = ref Uninitialized

  (* Sets up the state service object. Should be called before any queries. *)
  let initialize ~sm ~fields = ss_cur_state := SS (sm, fields)

  (* Finalize: no more queries. *)
  let finalize () = pure ()

  let assert_init () =
    match !ss_cur_state with
    | Uninitialized -> fail0 "StateService: Uninitialized"
    | SS (sm, fields) -> pure (sm, fields)

  let field_type fields fname =
    match List.find fields ~f:(fun z -> String.(z.fname = get_id fname)) with
    | Some f -> pure @@ f.ftyp
    | None ->
        fail1
          (sprintf "StateService: Unable to determine the type of field %s."
             (get_id fname))
          (ER.get_loc (get_rep fname))

  let fetch_local ~fname ~keys fields =
    let s = fields in
    match List.find s ~f:(fun z -> String.(z.fname = get_id fname)) with
    | Some { fname = _; ftyp = MapType _; fval = Some (Map ((kt, vt), mlit)) }
      when not @@ List.is_empty keys ->
        let%bind ret_val_type =
          SSTypeUtil.map_access_type (MapType (kt, vt)) (List.length keys)
        in
        (* Recursively, index with each key and provide the indexed value. *)
        let rec recurser mlit' klist' vt' =
          match klist' with
          | [ k ] ->
              (* Just an assert. *)
              if not @@ [%equal: typ] vt' ret_val_type then
                fail1
                  (sprintf
                     "StateService: Failed indexing into map %s. Internal \
                      error."
                     (get_id fname))
                  (ER.get_loc (get_rep fname))
              else
                let res = Caml.Hashtbl.find_opt mlit' k in
                pure @@ (res, G_MapGet (List.length keys, res))
          | k :: krest -> (
              (* we have more nested maps *)
              match Caml.Hashtbl.find_opt mlit' k with
              | Some (Map ((_, vt''), mlit'')) -> recurser mlit'' krest vt''
              | None ->
                  (* No element found. Return none. *)
                  pure @@ (None, G_MapGet (List.length keys, None))
              (* The remaining keys cannot be used for indexing as
                 we ran out of nested maps. *)
              | _ ->
                  fail1
                    (sprintf
                       "StateService: Cannot index into map %s. Too many index \
                        keys."
                       (get_id fname))
                    (ER.get_loc (get_rep fname)) )
          (* this cannot occur. *)
          | [] ->
              fail1
                (sprintf
                   "StateService: Internal error in retriving from map %s."
                   (get_id fname))
                (ER.get_loc (get_rep fname))
        in
        recurser mlit keys vt
    | Some { fname = _; ftyp = _; fval = Some l } -> pure @@ (Some l, G_Load l)
    | _ ->
        fail1
          (sprintf "StateService: field \"%s\" not found.\n" (get_id fname))
          (ER.get_loc (get_rep fname))

  let fetch ~fname ~keys =
    let%bind sm, fields = assert_init () in
    match sm with
    | IPC socket_addr -> (
        let%bind tp = field_type fields fname in
        let%bind res = StateIPCClient.fetch ~socket_addr ~fname ~keys ~tp in
        if not @@ List.is_empty keys then
          pure @@ (res, G_MapGet (List.length keys, res))
        else
          match res with
          | None ->
              fail1
                (sprintf "StateService: Field %s not found on IPC server."
                   (get_id fname))
                (ER.get_loc (get_rep fname))
          | Some res' -> pure @@ (res, G_Load res') )
    | Local -> fetch_local ~fname ~keys fields

  let update_local ~fname ~keys vopt fields =
    let s = fields in
    match List.find s ~f:(fun z -> String.(z.fname = get_id fname)) with
    | Some { fname = _; ftyp = _; fval = Some (Map ((_, vt), mlit)) }
      when not @@ List.is_empty keys ->
        let rec recurser mlit' klist' vt' =
          match klist' with
          (* we're at the last key, update literal. *)
          | [ k ] -> (
              match vopt with
              | Some v ->
                  Caml.Hashtbl.replace mlit' k v;
                  pure @@ (s, G_MapUpdate (List.length keys, Some v))
              | None ->
                  Caml.Hashtbl.remove mlit' k;
                  pure @@ (s, G_MapUpdate (List.length keys, None)) )
          | k :: krest -> (
              (* we have more nested maps *)
              match Caml.Hashtbl.find_opt mlit' k with
              | Some (Map ((_, vt''), mlit'')) -> recurser mlit'' krest vt''
              | None ->
                  if is_some vopt then (
                    (* not a delete operation. *)
                    (* We have more keys remaining, but no entry for "k".
                       So create an empty map for "k" and then proceed. *)
                    let mlit'' = Caml.Hashtbl.create 4 in
                    let%bind kt'', vt'' =
                      match vt' with
                      | MapType (keytype, valtype) -> pure (keytype, valtype)
                      | _ ->
                          fail1
                            (sprintf
                               "StateService: Cannot index into map %s due to \
                                non-map type"
                               (get_id fname))
                            (ER.get_loc (get_rep fname))
                    in
                    Caml.Hashtbl.replace mlit' k (Map ((kt'', vt''), mlit''));
                    recurser mlit'' krest vt'' )
                  else
                    (* No point removing a key that doesn't exist. *)
                    pure @@ (s, G_MapUpdate (List.length keys, None))
              (* The remaining keys cannot be used for indexing as
                 we ran out of nested maps. *)
              | _ ->
                  fail1
                    (sprintf
                       "StateService: Cannot index into map %s. Too many index \
                        keys."
                       (get_id fname))
                    (ER.get_loc (get_rep fname)) )
          (* this cannot occur. *)
          | [] ->
              fail1
                (sprintf "StateService: Internal error in updating map %s."
                   (get_id fname))
                (ER.get_loc (get_rep fname))
        in
        recurser mlit keys vt
    | Some { fname = f; ftyp = t; fval = Some _ } -> (
        match vopt with
        | Some fval' ->
            let fields' =
              List.filter fields ~f:(fun f -> String.(f.fname <> get_id fname))
            in
            pure
              ( { fname = f; ftyp = t; fval = Some fval' } :: fields',
                G_Store fval' )
        | None ->
            fail1
              (sprintf "StateService: Cannot remove non-map value %s from state"
                 (get_id fname))
              (ER.get_loc (get_rep fname)) )
    | _ ->
        fail1
          (sprintf "StateService: Field \"%s\" not found.\n" (get_id fname))
          (ER.get_loc (get_rep fname))

  let update ~fname ~keys ~value =
    let%bind sm, fields = assert_init () in
    match sm with
    | IPC socket_addr ->
        let%bind tp = field_type fields fname in
        let%bind _ =
          StateIPCClient.update ~socket_addr ~fname ~keys ~value ~tp
        in
        if not @@ List.is_empty keys then
          pure @@ G_MapUpdate (List.length keys, Some value)
        else pure @@ G_Store value
    | Local ->
        let%bind fields', g = update_local ~fname ~keys (Some value) fields in
        let _ = ss_cur_state := SS (sm, fields') in
        pure g

  (* Is a key in a map. keys must be non-empty. *)
  let is_member ~fname ~keys =
    let%bind sm, fields = assert_init () in
    match sm with
    | IPC socket_addr ->
        let%bind tp = field_type fields fname in
        let%bind res = StateIPCClient.is_member ~socket_addr ~fname ~keys ~tp in
        pure @@ (res, G_MapGet (List.length keys, None))
    | Local ->
        let%bind v, _ = fetch_local ~fname ~keys fields in
        pure @@ (Option.is_some v, G_MapGet (List.length keys, None))

  (* Remove a key from a map. keys must be non-empty. *)
  let remove ~fname ~keys =
    let%bind sm, fields = assert_init () in
    match sm with
    | IPC socket_addr ->
        let%bind tp = field_type fields fname in
        let%bind _ = StateIPCClient.remove ~socket_addr ~fname ~keys ~tp in
        pure @@ G_MapUpdate (List.length keys, None)
    | Local ->
        let%bind _, g = update_local ~fname ~keys None fields in
        (* We don't need to update ss_cur_state because only map keys can be removed, and that's stateful. *)
        pure @@ g

  (* Expensive operation, use with care. *)
  let get_full_state () =
    match !ss_cur_state with
    | Uninitialized -> fail0 "StateService: Uninitialized"
    | SS (Local, fl) ->
        mapM fl ~f:(fun f ->
            match f.fval with
            | None ->
                fail0
                  (sprintf "StateService: Field %s's value is not known"
                     f.fname)
            | Some l -> pure (f.fname, l))
    | SS (IPC _, fl) ->
        let%bind sl =
          mapM fl ~f:(fun f ->
              let%bind vopt, _ = fetch ~fname:(asId f.fname) ~keys:[] in
              match vopt with
              | Some v -> pure (f.fname, v)
              | None ->
                  fail0
                    (sprintf
                       "StateService: Field %s's value not found on server"
                       f.fname))
        in
        pure sl
end

(* module MakeStateService *)

module StateServiceInstance = MakeStateService ()

include StateServiceInstance
