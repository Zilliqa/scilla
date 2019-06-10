open TypeUtil
open Syntax
open ContractUtil.MessagePayload
open MonadUtil
open Core.Result.Let_syntax

module ScillaEventInfo
    (SR : Rep)
    (ER : sig
       include Rep
       val get_type : rep -> PlainTypes.t inferred_type
     end) = struct

  module SER = SR
  module EER = ER
  module EISyntax = ScillaSyntax (SR) (ER)
  module TU = TypeUtilities
  module SCU = ContractUtil.ScillaContractUtil (SR) (ER)

  open EISyntax
  open TU
  open SCU

  (* Given a contract, return a list of events it may create,
   * and the parameter types of the event. *)
  let event_info (cmod : cmodule) =

    (* Given a message and a current list of event info, extract
     * info from message and append to the list. *)
    let extract_from_message b m acc =
      let bloc = (ER.get_loc (get_rep b)) in
      (* Check if this is for an event. *)
      (match (List.find_opt (fun (label, _) -> label = eventname_label) m) with
       | Some (_, epld) ->
           let emsg = "Error determining event name\n" in
           let%bind eventname = match epld with
             | MLit l -> (match l with | StringLit s -> pure s | _ -> fail1 emsg bloc)
             (* Variables are not allowed for eventname_label to ensure that
              * all possible events can be determined statically. *)
             | MVar _ -> fail1 emsg bloc
           in
           (* Get the type of the event parameters. *)
           let filtered_m = List.filter (fun (label, _) -> not (label = eventname_label)) m in
           let%bind m_types = mapM ~f:(fun (fname, pl) ->
               let%bind t = 
                 (match pl with
                  | MLit l -> literal_type l
                  | MVar v -> 
                      let t' = ER.get_type (get_rep v) in
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
                  fail1 (Printf.sprintf "Parameter mismatch for event %s. %s vs %s\n"
                    eventname (printer tlist) (printer m_types)) bloc
                else
                  pure @@ acc
            | None -> (* No entry. *)
                let entry = (eventname, m_types) in
                pure (entry :: acc)
           )
       | None -> (* Not for an event. *) pure acc
      ) in

    
    fold_over_messages cmod ~init:[] ~f:extract_from_message

end
