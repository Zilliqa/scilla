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

(* This file aids Testcontracts.ml in setting up a state server
 * and initializing it with some initial data. *)

open OUnit2
open Core
open Yojson
open Scilla_base
open Scilla_eval
open Literal
module IPCTestType = StateIPCTestClient.Type
module IPCTestName = IPCTestType.TIdentifier.Name

(* Use GlobalLiteral for compatibility with TypeUtil *)
module FEParser = FrontEndParser.ScillaFrontEndParser (GlobalLiteral)

let parse_typ_wrapper t =
  match FEParser.parse_type t with
  | Error _ ->
      assert_failure (sprintf "StateIPCTest: Invalid type in json: %s\n" t)
  | Ok s -> s

let json_exn_wrapper thunk =
  try thunk () with
  | Json_error s | Basic.Util.Undefined (s, _) | Basic.Util.Type_error (s, _) ->
      assert_failure s
  | e -> assert_failure (Exn.to_string e)

let json_from_file f =
  let thunk () = Basic.from_file f in
  json_exn_wrapper thunk

let json_from_string s =
  let thunk () = Basic.from_string s in
  json_exn_wrapper thunk

let json_to_assoc j =
  let thunk () = Basic.Util.to_assoc j in
  json_exn_wrapper thunk

let json_member m j =
  let thunk () = Basic.Util.member m j in
  json_exn_wrapper thunk

let json_to_list j =
  let thunk () = Basic.Util.to_list j in
  json_exn_wrapper thunk

let json_to_string j =
  let thunk () = Basic.Util.to_string j in
  json_exn_wrapper thunk

let rec json_to_pb t j =
  let open IPCTestType in
  match t with
  | MapType (_, vt) ->
      let kvlist = json_to_list j in
      let kvlist' =
        List.map kvlist ~f:(fun kvj ->
            let kj = json_member "key" kvj in
            let vj = json_member "val" kvj in
            let kpb = Basic.pretty_to_string kj in
            let vpb = json_to_pb vt vj in
            (kpb, vpb))
      in
      Ipcmessage_types.Mval { m = kvlist' }
  | _ -> Ipcmessage_types.Bval (Bytes.of_string (Basic.pretty_to_string j))

let rec pb_to_json pb =
  match pb with
  | Ipcmessage_types.Mval pbm ->
      `List
        (List.map pbm.m ~f:(fun (k, vpb) ->
             let k' = json_from_string k |> json_to_string in
             `Assoc [ ("key", `String k'); ("val", pb_to_json vpb) ]))
  | Ipcmessage_types.Bval s -> json_from_string (Bytes.to_string s)

(* Parse a state JSON file into (fname, ftyp, fval) where fval is protobuf encoded. *)
type json_parsed_field =
  (* A field mutable belonging to this contract. *)
  | ThisContr of string * IPCTestType.t * Ipcmessage_types.proto_scilla_val
  (* External contracts and their mutable or immutable fields. *)
  | ExtrContrs of (string * (string * bool * IPCTestType.t * Ipcmessage_types.proto_scilla_val) list) list

let json_file_to_state path =
  let j = json_from_file path |> json_to_list in

  let rec recurser js =
    let fname = json_member "vname" js |> json_to_string in
    let jval = json_member "value" js in
    if String.equal fname "_external" then
      let exts = json_to_list jval in
      let addr_states =
        List.map exts ~f:(fun addr_states ->
            let addr =
              json_member "address" addr_states |> json_to_string
            in
            let cparams = json_member "cparams" addr_states |> json_to_list in
            let cparams' = List.map cparams ~f:(recurser_mapper false) in
            let state = json_member "state" addr_states |> json_to_list in
            let all_fields = List.rev_map_append state cparams' ~f:(recurser_mapper true) in
            (addr, all_fields))
      in
      ExtrContrs addr_states
    else 
      let ftyp =
        json_member "type" js |> json_to_string |> parse_typ_wrapper
      in
      let fval = json_to_pb ftyp jval in
      ThisContr (fname, ftyp, fval)
  and recurser_mapper is_mutable json =
    match recurser json with
    | ThisContr (n, t, l) -> (n, is_mutable, t, l)
    | _ ->
        assert_failure
          "External state cannot contain nested external states"
  in
  List.map j ~f:recurser
      
let state_to_json s =
  let open IPCTestType in
  `List
    (List.map s ~f:(fun (fname, ftyp, fval) ->
         `Assoc
           [
             ("vname", `String fname);
             ("type", `String (pp_typ ftyp));
             ("value", pb_to_json fval);
           ]))

(* Given two output JSONs, sort map keys in the second one w.r.t the first.
 * The order of variables in the "state" member is asserted to be the same. *)
let sort_mapkeys goldj outj =
  let goldstates = json_to_list @@ json_member "states" goldj in
  let outstates = json_to_list @@ json_member "states" outj in
  let rec map_dumper outmap t =
    let open IPCTestType in
    match t with
    | MapType (_, vt) ->
        let outlist = json_to_list outmap in
        let outlist_complete =
          List.map outlist ~f:(fun out ->
              let outkey = json_member "key" out in
              let outval = json_member "val" out in
              let outval' = map_dumper outval vt in
              `Assoc [ ("key", outkey); ("val", outval') ])
        in
        `List outlist_complete
    | _ -> outmap
  in
  let rec map_sorter goldmap outmap t =
    let open IPCTestType in
    match t with
    | MapType (_, vt) ->
        let goldlist = json_to_list goldmap in
        let outlist = json_to_list outmap in
        let outlist_goldkeys =
          List.fold_right goldlist
            ~f:(fun gold outacc ->
              let goldkey = json_member "key" gold |> json_to_string in
              let goldval = json_member "val" gold in
              let corresponding_out =
                List.find outlist ~f:(fun outelm ->
                    let outkey = json_member "key" outelm |> json_to_string in
                    String.(goldkey = outkey))
              in
              match corresponding_out with
              | Some out ->
                  let outkey = json_member "key" out in
                  let outval = json_member "val" out in
                  let outval' = map_sorter goldval outval vt in
                  let outj = `Assoc [ ("key", outkey); ("val", outval') ] in
                  outj :: outacc
              | None -> outacc)
            ~init:[]
        in
        let outlist_complete =
          List.fold_left outlist ~init:outlist_goldkeys ~f:(fun outacc out ->
              let outkey = json_member "key" out in
              match
                List.find outlist_goldkeys ~f:(fun other_out ->
                    let other_outkey =
                      json_member "key" other_out |> json_to_string
                    in
                    String.(json_to_string outkey = other_outkey))
              with
              | Some _ ->
                  (* Ignore - already sorted *)
                  outacc
              | None ->
                  (* New key not present in the gold file *)
                  let outval = json_member "val" out in
                  let outval' = map_dumper outval vt in
                  let outj = `Assoc [ ("key", outkey); ("val", outval') ] in
                  outj :: outacc)
        in
        `List outlist_complete
    | _ -> outmap
  in
  let outstates' =
    `List
      (List.map2_exn goldstates outstates ~f:(fun goldstate outstate ->
           let vname = json_member "vname" goldstate in
           let t =
             json_member "type" outstate |> json_to_string |> parse_typ_wrapper
           in
           assert_bool
             "sort_mapkeys: order of gold states and out states mismatch"
             String.(
               vname |> json_to_string
               = (json_member "vname" outstate |> json_to_string));
           let outval =
             map_sorter
               (json_member "value" goldstate)
               (json_member "value" outstate)
               t
           in
           `Assoc
             [
               ("vname", vname);
               ("type", json_member "type" outstate);
               ("value", outval);
             ]))
  in
  (* Replace outstates with outstates' in outj. *)
  `Assoc
    (List.fold_right (json_to_assoc outj)
       ~f:(fun (s, j) acc ->
         if String.(s = "states") then (s, outstates') :: acc else (s, j) :: acc)
       ~init:[])

(* Start a mock server (if set) at ~sock_addr and initialize server
 * (external or mock server) state with ~state_json_path. *)
let setup_and_initialize ~start_mock_server ~sock_addr ~state_json_path
    ~blockchain_json_path =
  let open ContractUtil in
  let state = json_file_to_state state_json_path in
  let bcinfo = JSON.BlockChainState.get_json_data blockchain_json_path in

  (* Setup a mock server within the testsuite? *)
  if start_mock_server then StateIPCTestServer.start_server ~sock_addr;

  let fields =
    List.filter_map state ~f:(function
        | ThisContr (s, t, _) when not String.(s = CUName.as_string balance_label) ->
            Some (s, t)
        | _ -> None)
  in
  let () = StateIPCTestClient.initialize ~fields ~sock_addr in

  (* Update the blockchain info into the IPC server. *)
  Caml.Hashtbl.iter
    (fun query_name subm ->
      Caml.Hashtbl.iter
        (fun query_args value ->
          StateIPCTestClient.set_bcinfo ~query_name ~query_args value)
        subm)
    bcinfo;

  (* Update the server (via the test client) with the state values we want. *)
  List.iter state ~f:(function
      | ThisContr (fname, _tp, value) when String.(fname <> CUName.as_string balance_label) -> 
          StateIPCTestClient.update ~fname ~value
      | ExtrContrs extrs ->
          List.iter extrs ~f:(fun (caddr, fields) ->
              List.iter fields ~f:(function
                  | (fname, is_mutable, tp, value) ->
                      StateIPCTestClient.update_ext ~caddr ~fname ~is_mutable ~value ~tp))
      | _ -> ());
  (* Find the balance from state and return it. *)
  match
    List.find state ~f:(function
        | ThisContr (fname, _, _) when String.(fname = CUName.as_string balance_label) -> true
        | _ -> false)
  with
  | Some (ThisContr (_, _, balpb)) -> (
      match balpb with
      | Ipcmessage_types.Bval bal ->
          json_from_string (Bytes.to_string bal) |> json_to_string
      | _ ->
          assert_failure
            ("Incorrect type of "
            ^ CUName.as_error_string balance_label
            ^ " in state.json"))
  | _ ->
      assert_failure
        ("Unable to find "
        ^ CUName.as_error_string balance_label
        ^ " in state.json")

(* Get full state, and if a server was started in ~setup_and_initialize, shut it down. *)
let get_final_finish ~sock_addr =
  let state = StateIPCTestClient.fetch_all () in
  StateIPCTestServer.stop_server ~sock_addr;
  state

(* Given the interpreter's output, parse the JSON, append svars to it and print out new JSON.
 * The gold output is used to re-order state variables and map keys from StateIPCTestServer. *)
let append_full_state ~goldoutput_file ~interpreter_output svars =
  (* Let's first re-order variables based on gold. *)
  let goldj = json_from_file goldoutput_file in
  let goldjs = json_to_list @@ json_member "states" goldj in
  let svars' =
    List.fold_right goldjs ~init:svars ~f:(fun goldv acc ->
        let golds = json_member "vname" goldv |> json_to_string in
        if
          String.(
            golds = ContractUtil.CUName.as_string ContractUtil.balance_label)
        then acc
        else
          let s', rest =
            List.partition_tf acc ~f:(fun (s, _, _) -> String.(s = golds))
          in
          s' @ rest)
  in
  (* Now we go about generating an appended output JSON. *)
  let j = json_from_string interpreter_output in
  let items = json_to_assoc j in
  let unsorted_output_j =
    `Assoc
      (List.map items ~f:(fun (s, j) ->
           if String.(s <> "states") then (s, j)
           else
             (* Just add our states to "_balance" that's in the output JSON. *)
             let svars_j = state_to_json svars' in
             (s, `List (json_to_list j @ json_to_list svars_j))))
  in
  (* Let's now sort within each state variable (for maps). *)
  let sorted_output_j = sort_mapkeys goldj unsorted_output_j in
  Basic.pretty_to_string sorted_output_j
