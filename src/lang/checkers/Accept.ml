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

open TypeUtil
open ErrorUtils
open Syntax

module ScillaAcceptChecker
         (SR : Rep)
         (ER : sig
            include Rep
            val [@warning "-32"] get_type : rep -> PlainTypes.t inferred_type
          end) = struct

  module EISyntax = ScillaSyntax (SR) (ER)

  open EISyntax

  (* Warning level to use when contract has code paths with potentially
   * no accept statement. *)
  let warning_level_missing_accept = 1

  (* Warning level to use when contract has code paths with potentially
   * multiple accept statements. *)
  let warning_level_duplicate_accepts = 1

  let find_accept_groups (stmts : stmt_annot list) : loc list list =
    let rec walk (seen : loc list list) (stmts : stmt_annot list) : loc list list =
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
      List.fold_left
        (fun (seen2 : loc list list) (stmt : stmt_annot) ->
          let loc = (stmt_loc stmt) in
          match fst stmt with
          | AcceptPayment ->
              (* Add this accept statement to the list of accepts
               * already seen on each code path reaching this point. *)
             List.map (fun accepts -> loc :: accepts) seen2
          | MatchStmt (_ident, branches) ->
             (* For each branch in the match statement we have a
                  new code path to "multiply" with the code paths
                  which already reached this point, so walk each
                  branch and build all the results into a new list.
              *)
             List.fold_left
               (fun seen3 (_pattern, branchstmts) ->
                 match walk seen2 branchstmts with
                 | [] -> seen3
                 | seen4 -> seen3 @ seen4)
               [] branches
          | _ -> seen2
        ) seen stmts
    in
    walk [[]] stmts

  let check_accepts (contr : contract) =
    let check_transition_accepts (transition : component) =
      let transition_accept_groups =
        List.map List.rev (find_accept_groups transition.comp_body)
      in

      let accept_loc_end (l : loc) =
        match l with
        | { fname=fname; lnum=lnum; cnum=cnum } ->
           { fname=fname; lnum=lnum; cnum= cnum + 6 }

      in

      let dup_accept_warning (group : loc list) : unit =
        (warn2
           (Core.sprintf
              "transition %s had a potential code path with duplicate accept statements:\n"
              (get_id transition.comp_name) ^
              String.concat ""
                (List.map
                   (fun loc -> Core.sprintf "  Accept at %s\n" (get_loc_str loc))
                   group))
           warning_level_duplicate_accepts
           (List.hd group)
           (accept_loc_end @@ BatList.last group))
      in

      List.iter
        (fun group ->
          match group with
          | _ :: _ :: _ -> dup_accept_warning group
          | _ -> ())
        transition_accept_groups;

      transition_accept_groups
    in

    let all_accept_groups =
      (List.fold_left
         (fun acc t -> acc @ check_transition_accepts t)
         [] contr.ccomps)
    in

    (match List.for_all BatList.is_empty all_accept_groups with
     | true ->
        (warn0
           (Core.sprintf "No transition in contract %s contains an accept statement\n"
              (get_id contr.cname))
           warning_level_missing_accept)
     | false -> ())

  (* ************************************** *)
  (* ******** Interface to Accept ********* *)
  (* ************************************** *)

  let contr_sanity (cmod : cmodule) = check_accepts cmod.contr

end
