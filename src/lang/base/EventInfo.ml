open ParserUtil
open TypeUtil
open Syntax

module SR = ParserRep
module ER = ParserRep
module STR = SR
module ETR = TypeChecker.ScillaTypechecker.ETR
module TypedSyntax = ScillaSyntax (STR) (ETR)
module TypeUtil = TypeUtilities (SR) (ER)

open TypedSyntax
open TypeUtil
open PrimTypes
open ContractUtil.MessagePayload
open MonadUtil
open Core.Result.Let_syntax

(* Given a contract, return a list of events it may create,
 * and the parameter types of the event. *)
let event_info (contr : contract) =

  (* Given a message and a current list of event info, extract
   * info from message and append to the list. *)
  let extract_from_message m acc =
    (* Check if this is for an event. *)
    (match (List.find_opt (fun (label, _) -> label = eventname_label) m) with
      | Some (_, epld) ->
        let emsg = "Error determining event name\n" in
        let%bind eventname = match epld with
          | MTag s -> pure s
          | MLit l -> (match l with | StringLit s -> pure s | _ -> fail emsg)
          | MVar _ -> fail emsg 
        in
        (* Get the type of the event parameters. *)
        let filtered_m = List.filter (fun (label, _) -> not (label = eventname_label)) m in
        let%bind m_types = mapM ~f:(fun (fname, pl) ->
          let%bind t = 
            (match pl with
            | MTag _ -> pure string_typ
            | MLit l -> literal_type l
            | MVar v -> 
              let t' = ETR.get_type (get_rep v) in
              pure t'.tp
            )in pure (fname, t)
          ) filtered_m in
        (* If we already have an entry for "eventname" in "acc", 
          * check that the type matches. Add entry otherwise. *)
        (match (List.find_opt (fun (n, _) -> n = eventname) acc) with
          | Some (_, tlist) -> (* verify types match *)
            let printer tplist =
              List.fold_left (fun acc (n, t) -> 
                  acc ^ (Printf.sprintf "(%s : %s); " n (pp_typ t))) "[" tplist
                  ^ "]" in 
            if m_types <> tlist then 
              fail @@ Printf.sprintf "Parameter mismatch for event %s. %s vs %s\n"
                eventname (printer tlist) (printer m_types)
            else
              pure @@ acc
          | None -> (* No entry. *)
            let entry = (eventname, m_types) in
              pure (entry :: acc)
        )
      | None -> (* Not for an event. *) pure acc
    ) in

  (* Loop through each transition *)
  foldM ~f:(fun acc trans ->
    (* Loop through each statement, looking for messages. *)
    let rec stmt_iter stmt_list acc = 
      match stmt_list with
      | (stmt, _)::stmt_list' -> 
        let%bind acc' =
        (match stmt with
         | MatchStmt (_, clauses) ->
            (* Recurse through all clauses. *)
            foldM ~f:(fun acc'' (_, stmt_list'') ->
              stmt_iter stmt_list'' acc''
            ) ~init:acc clauses
          (* Every message created gets bound to some variable. *)
         | Bind (_, (e, _)) ->
            (match e with
            | Message m -> extract_from_message m acc
            | _ -> (* Uninteresting expression. *) pure acc
            )
         | _ -> (* Uninteresting statement. *) pure acc
        ) in  stmt_iter stmt_list' acc'
      | [] -> pure acc
    in
      stmt_iter trans.tbody acc
  ) ~init:[] contr.ctrans

