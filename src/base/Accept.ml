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
   so again we only generate warnings not errors. *)

open Core_kernel
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
  (* TODO: Change this to CanonicalLiteral = Literals based on canonical names. *)
  module ACLiteral = FlattenedLiteral
  module ACType = ACLiteral.LType
  module ACIdentifier = ACType.TIdentifier
  module ACSyntax = ScillaSyntax (SR) (ER) (ACLiteral)
  open ACSyntax

  (* Warning level to use when contract has code paths with potentially
   * no accept statement. *)
  let warning_level_missing_accept = 1

  (* Warning level to use when contract has code paths with potentially
   * multiple accept statements. *)
  let warning_level_duplicate_accepts = 1

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

  let check_accepts (contr : contract) =
    let check_transition_accepts (transition : component) =
      let transition_accept_groups =
        List.map (find_accept_groups transition.comp_body) ~f:List.rev
      in

      let accept_loc_end (l : loc) = { l with cnum = l.cnum + 6 } in

      let dup_accept_warning (group : loc list) : unit =
        warn2
          ( sprintf
              "transition %s has a potential code path with duplicate accept \
               statements:\n"
              (ACIdentifier.as_error_string transition.comp_name)
          ^ String.concat ~sep:""
              (List.map group ~f:(fun loc ->
                   sprintf "  Accept at %s\n" (get_loc_str loc))) )
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

    if List.for_all all_accept_groups ~f:List.is_empty then
      warn0
        (sprintf "No transition in contract %s contains an accept statement\n"
           (ACIdentifier.as_error_string contr.cname))
        warning_level_missing_accept

  (* ************************************** *)
  (* ******** Interface to Accept ********* *)
  (* ************************************** *)

  let contr_sanity (cmod : cmodule) = check_accepts cmod.contr
end
