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
*)

(* Check each transition for code paths which may have more than one
   accept statement.  We can't be sure of duplicates through static
   analysis alone, since the code path chosen can depend on run-time
   state.  Therefore we only warn, rather than error, when there is
   the *possibility* of duplicates.

   Also check for contracts which have no transitions with accept
   statements.  There might be valid reasons for writing such contracts,
   so again we only generate warnings not errors.
   An exception to this rule is the case when there are only [send] statements
   that send zero amount of tokens. *)

open Core
open Literal
open TypeUtil
open ErrorUtils
open Syntax

module ScillaAcceptChecker
    (SR : Rep) (ER : sig
      include Rep

      val get_type : rep -> PlainTypes.t inferred_type [@@warning "-32"]
    end) =
struct
  module ACLiteral = GlobalLiteral
  module ACType = ACLiteral.LType
  module ACIdentifier = ACType.TIdentifier

  module ACIdentifierComp = struct
    include ACIdentifier.Name
    include Comparable.Make (ACIdentifier.Name)
  end

  module ACIdentifierSet = Set.Make (ACIdentifierComp)
  module ACSyntax = ScillaSyntax (SR) (ER) (ACLiteral)
  open ACSyntax

  let emp_idset = ACIdentifierSet.empty

  (* Warning level to use when contract has code paths with potentially
   * no accept statement. *)
  let warning_level_missing_accept = 1

  (* Warning level to use when contract has code paths with potentially
   * multiple accept statements. *)
  let warning_level_duplicate_accepts = 1

  let is_uint128_zero v =
    Stdint.Uint128.compare Stdint.Uint128.zero v |> phys_equal 0

  (** Collects a set of names of constants that unfold to [Uint128 0]. *)
  let collect_zero_constants (cmod : cmodule) =
    Option.value_map cmod.libs ~default:emp_idset ~f:(fun lib ->
        List.fold_left lib.lentries ~init:emp_idset ~f:(fun s -> function
          | LibVar (name, _ty, (expr, _annot)) -> (
              match expr with
              | Literal (UintLit (Uint128L v)) when is_uint128_zero v ->
                  ACIdentifier.get_id name |> ACIdentifierSet.add s
              | _ -> s)
          | LibTyp _ -> s))

  (** Returns [true] iff the given message payload is a zero constant. *)
  let is_zero_payload zero_constants = function
    | MLit (UintLit (Uint128L v)) when is_uint128_zero v -> true
    | MVar id when ACIdentifier.get_id id |> ACIdentifierSet.mem zero_constants
      ->
        true
    | _ -> false

  (** Collects a list of the [_amount] fields from [Message] definitions. *)
  let collect_msg_amounts (cmod : cmodule) =
    let walk_comp (comp : component) =
      let rec walk_expr (e, _ann) =
        match e with
        | Literal _ -> []
        | Var _id -> []
        | Let (_id, _ty, lhs, rhs) -> walk_expr lhs @ walk_expr rhs
        | Message fields ->
            List.fold_left fields ~init:[] ~f:(fun acc (f, pld) ->
                if String.equal f "_amount" then acc @ [ pld ] else acc)
        | Fun (_id, _ty, body) -> walk_expr body
        | App (_id, _args) -> []
        | Constr _ -> []
        | MatchExpr (_id, arms) ->
            List.fold_left arms ~init:[] ~f:(fun acc (_pattern, ea) ->
                acc @ walk_expr ea)
        | Builtin _ -> []
        | TFun (_id, body) -> walk_expr body
        | TApp _ -> []
        | Fixpoint (_id, _ty, ea) -> walk_expr ea
        | GasExpr (_, ea) -> walk_expr ea
      in
      let rec walk_stmt (s, _ann) =
        match s with
        | Bind (_id, ea) -> walk_expr ea
        | MatchStmt (_id, arms) ->
            List.fold_left arms ~init:[] ~f:(fun acc (_pattern, stmts) ->
                acc
                @ List.fold_left stmts ~init:[] ~f:(fun acc s ->
                      acc @ walk_stmt s))
        | Load _ | RemoteLoad _ | Store _ | MapUpdate _ | MapGet _
        | RemoteMapGet _ | ReadFromBC _ | TypeCast _ | AcceptPayment | Return _
        | Iterate _ | SendMsgs _ | CreateEvnt _ | CallProc _ | Throw _
        | GasStmt _ ->
            []
      in
      List.fold_left comp.comp_body ~init:[] ~f:(fun acc s -> acc @ walk_stmt s)
    in
    List.fold_left cmod.contr.ccomps ~init:[] ~f:(fun acc c ->
        acc @ walk_comp c)

  (** Returns true if there are no message definitions in [cmod] or at least one
      of them has non-zero [_amount]. *)
  let check_msg_amounts cmod =
    let amounts = collect_msg_amounts cmod in
    if not @@ List.is_empty amounts then
      let zero_amounts =
        let zero_constants = collect_zero_constants cmod in
        List.filter amounts ~f:(fun msg -> is_zero_payload zero_constants msg)
      in
      not @@ phys_equal (List.length amounts) (List.length zero_amounts)
    else true (* There are no messages *)

  let find_accept_groups (stmts : stmt_annot list) : loc list list =
    let rec walk (seen : loc list list) (stmts : stmt_annot list) :
        loc list list =
      (* Walk the syntax tree looking for code paths containing zero or
         more accept statements.

         seen is a List with an item for each code path reaching
         the point in the syntax tree just before stmts.  Each item in
         the List is List of all the accepts seen along that path to
         this point.

         Returns an updated version of the List of Lists which
         includes any accepts seen in stmts.

         Note that we have to track any given code path even before an
         accept is discovered in it, because it might later branch and
         maybe some but not all of the branches will include accepts.
         So we need an entry on the seen list for each path, because
         we don't know what will happen on each in advance. *)
      List.fold_left stmts ~init:seen
        ~f:(fun (seen2 : loc list list) (stmt : stmt_annot) ->
          match fst stmt with
          | AcceptPayment ->
              (* Add this accept statement to the list of accepts
               * already seen on each code path reaching this point. *)
              let loc = stmt_loc stmt in
              List.map seen2 ~f:(fun accepts -> loc :: accepts)
          | MatchStmt (_ident, branches) ->
              (* For each branch in the match statement we have a
                  new code path to "multiply" with the code paths
                  which already reached this point, so walk each
                  branch and build all the results into a new list.
              *)
              List.concat_map branches ~f:(fun (_pattern, branchstmts) ->
                  walk seen2 branchstmts)
          | _ -> seen2)
    in
    walk [ [] ] stmts

  let check_accepts (cmod : cmodule) =
    let contr = cmod.contr in
    let check_transition_accepts (transition : component) =
      let transition_accept_groups =
        List.map (find_accept_groups transition.comp_body) ~f:List.rev
      in

      let accept_loc_end (l : loc) = { l with cnum = l.cnum + 6 } in

      let dup_accept_warning (group : loc list) : unit =
        warn2
          (sprintf
             "transition %s has a potential code path with duplicate accept \
              statements:\n"
             (ACIdentifier.as_error_string transition.comp_name)
          ^ String.concat ~sep:""
              (List.map group ~f:(fun loc ->
                   sprintf "  Accept at %s\n" (get_loc_str loc))))
          warning_level_duplicate_accepts (List.hd_exn group)
          (accept_loc_end @@ List.last_exn group)
      in

      List.iter transition_accept_groups ~f:(fun group ->
          match group with _ :: _ :: _ -> dup_accept_warning group | _ -> ());

      transition_accept_groups
    in

    let all_accept_groups =
      List.concat_map contr.ccomps ~f:check_transition_accepts
    in

    if List.for_all all_accept_groups ~f:List.is_empty && check_msg_amounts cmod
    then
      warn1
        (sprintf "No transition in contract %s contains an accept statement\n"
           (ACIdentifier.as_error_string contr.cname))
        warning_level_missing_accept
        (SR.get_loc (ACIdentifier.get_rep contr.cname))

  (* ************************************** *)
  (* ******** Interface to Accept ********* *)
  (* ************************************** *)

  let contr_sanity (cmod : cmodule) = check_accepts cmod
end
