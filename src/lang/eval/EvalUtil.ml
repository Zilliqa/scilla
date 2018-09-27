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

open Syntax
open Core
open MonadUtil
open EvalMonad
open EvalMonad.Let_syntax
open Stdint
open ContractUtil
open PrettyPrinters
open TypeUtil
open BuiltIns
open Gas

open ParserUtil
module SR = ParserRep
module ER = ParserRep
module EvalSyntax = ScillaSyntax (SR) (ER)
module EvalTypeUtilities = TypeUtilities (SR) (ER)
module EvalBuiltIns = ScillaBuiltIns (SR) (ER) 
module EvalGas = ScillaGas (SR) (ER)

open EvalSyntax
    
let rec subst_type_in_type tvar tp tm = match tm with
  | PrimType _ | Unit as p -> p
  (* Make sure the map's type is still primitive! *)
  | MapType (kt, vt) -> 
      let kts = subst_type_in_type tvar tp kt in
      let vts = subst_type_in_type tvar tp vt in
      MapType (kts, vts)
  | FunType (at, rt) -> 
      let ats = subst_type_in_type tvar tp at in
      let rts = subst_type_in_type tvar tp rt in
      FunType (ats, rts)
  | TypeVar n as tv ->
      if tvar = n then tp else tv
  | ADT (s, ts) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type tvar tp t) in
      ADT (s, ts')
  | PolyFun (arg, t) as pf -> 
      if tvar = arg then pf
      else PolyFun (arg, subst_type_in_type tvar tp t)


(* The same as above, but for a variable with locations *)
let subst_type_in_type' tv = subst_type_in_type (get_id tv)

let rec subst_type_in_literal tvar tp l = match l with
  | Map ((kt, vt), ls) -> 
      let kts = subst_type_in_type' tvar tp kt in
      let vts = subst_type_in_type' tvar tp vt in
      let ls' = List.map ls ~f:(fun (k, v) ->
        let k' = subst_type_in_literal tvar tp k in
        let v' = subst_type_in_literal tvar tp v in 
        (k', v')) in
      Map ((kts, vts), ls')
  | ADTValue (n, ts, ls) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type' tvar tp t) in
      let ls' = List.map ls ~f:(fun l -> subst_type_in_literal tvar tp l) in
      ADTValue (n, ts', ls')
  | _ -> l

(* Substitute type for a type variable *)
let rec subst_type_in_expr tvar tp (erep : expr_annot) =
  let (e, rep) = erep in
  match e with
  | Literal l -> (Literal (subst_type_in_literal tvar tp l), rep)
  | Var _ as v -> (v, rep)
  | Fun (f, t, body) ->
      let t_subst = subst_type_in_type' tvar tp t in 
      let body_subst = subst_type_in_expr tvar tp body in
      (Fun (f, t_subst, body_subst), rep)
  | TFun (tv, body) as tf ->
      if get_id tv = get_id tvar
      then (tf, rep)
      else 
        let body_subst = subst_type_in_expr tvar tp body in
        (TFun (tv, body_subst), rep)
  | Constr (n, ts, es) ->
      let ts' = List.map ts ~f:(fun t -> subst_type_in_type' tvar tp t) in
      (Constr (n, ts', es), rep)
  | App _ as app -> (app, rep)
  | Builtin _ as bi -> (bi, rep)
  | Let (i, tann, lhs, rhs) ->
      let tann' = Option.map tann ~f:(fun t -> subst_type_in_type' tvar tp t) in
      let lhs' = subst_type_in_expr tvar tp lhs in
      let rhs' = subst_type_in_expr tvar tp rhs in
      (Let (i, tann', lhs', rhs'), rep)
  | Message _ as m -> (m, rep)
  | MatchExpr (e, cs) ->
      let cs' = List.map cs ~f:(fun (p, b) -> (p, subst_type_in_expr tvar tp b)) in
      (MatchExpr(e, cs'), rep)
  | TApp (tf, tl) -> 
      let tl' = List.map tl ~f:(fun t -> subst_type_in_type' tvar tp t) in
      (TApp (tf, tl'), rep)
  | Fixpoint (f, t, body) ->
      let t' = subst_type_in_type' tvar tp t in
      let body' = subst_type_in_expr tvar tp body in
      (Fixpoint (f, t', body'), rep)

(* Return a builtin_op wrapped in EvalMonad *)
let builtin_executor i arg_tps arg_lits =
  let%bind (_, ret_typ, op) =
    fromR @@ EvalBuiltIns.BuiltInDictionary.find_builtin_op i arg_tps in
  let%bind cost = fromR @@ EvalGas.builtin_cost i arg_lits in
  let res () = op arg_lits ret_typ in
  checkwrap_opR res cost

(* Add a check that the just evaluated statement was in our gas limit. *)
let stmt_gas_wrap scon =
  let%bind cost = fromR @@ EvalGas.stmt_cost scon in
  let dummy () = pure () in (* the operation is already executed unfortunately *)
    checkwrap_op dummy cost "Ran out of gas evaluating statement"

(*****************************************************)
(* Update-only execution environment for expressions *)
(*****************************************************)
module Env = struct
  type ident = string

  (* Environment *)
  type t =
    (string * value) list
  and
  (* Fully reduced value *)
  value =
    | ValLit of literal
    | ValClosure of ER.rep Syntax.ident * typ * expr_annot * t
    | ValTypeClosure of ER.rep Syntax.ident * expr_annot * t                      
    | ValFix of ER.rep Syntax.ident * typ * expr_annot * t
  [@@deriving sexp]

  (* Pretty-printing *)
  let rec pp_value v = match v with
    | ValLit l ->  pp_literal l
    | ValFix _ -> "<fixpoint>"
    | ValTypeClosure _ -> "<type_closure>"
    | ValClosure _ -> "<closure>"
    (* | ValClosure (f, t, e, env) ->
         (pp_expr (Fun (f, t, e)))
          ^ ", " ^ (pp env) *)
  and pp ?f:(f = fun (_ : (string * value)) -> true) e =
    (* FIXME: Do not print folds *)
    let e_filtered = List.filter e ~f:f in
    let ps = List.map e_filtered
        ~f:(fun (k, v) -> " [" ^ k ^ " -> " ^ (pp_value v) ^ "]") in
    let cs = String.concat ~sep:",\n " ps in
    "{" ^ cs ^ " }"
  

  let empty = []

  let bind e k v =
    (k, v) :: List.filter ~f:(fun z -> fst z <> k) e

  let bind_all e kvs = 
    List.fold_left ~init:e ~f:(fun z (k, v) -> bind z k v) kvs
                                                                
  let lookup e k =
    let i = get_id k in
    match List.find ~f:(fun z -> fst z = i) e with 
    | Some x -> pure @@ snd x
    | None -> fail @@ sprintf
        "Identifier \"%s\" at %s is not bound in environment:\n"
        i (get_loc_str (get_rep k))
end


(**************************************************)
(*                 Blockchain State               *)
(**************************************************)
module BlockchainState = struct
  type t = (string * literal) list

  let lookup e k =
    match List.find ~f:(fun z -> fst z = k) e with 
    | Some x -> pure @@ snd x
    | None -> fail @@ sprintf
        "No value for key \"%s\" at in the blockchain state:\n%s"
        k (pp_literal_map e)  
end

(**************************************************)
(*          Runtime contract configuration        *)
(**************************************************)
module Configuration = struct

  (* Runtime contract configuration and operations with it *)
  type t = {
    (* Immutable variables *)
    env : Env.t;
    (* Contract fields *)
    fields : (string * literal) list;
    (* Contract balance *)
    balance : uint128;
    (* Was incoming money accepted? *)
    accepted : bool;
    (* Blockchain state *)
    blockchain_state : BlockchainState.t;
    (* Available incoming funds *)
    incoming_funds : uint128;
    (* Emitted messages *)
    emitted : literal list;
    (* Emitted events *)
    events : literal list
  }

  let pp conf =
    let pp_env = Env.pp conf.env in
    let pp_fields = pp_literal_map conf.fields in
    let pp_balance = Uint128.to_string conf.balance in
    let pp_accepted = Bool.to_string conf.accepted in
    let pp_bc_conf = pp_literal_map conf.blockchain_state in
    let pp_in_funds = Uint128.to_string conf.incoming_funds in
    let pp_emitted = pp_literal_list conf.emitted in
    let pp_events = pp_literal_list conf.events in
    sprintf "Confuration\nEnv =\n%s\nFields =\n%s\nBalance =%s\nAccepted=%s\n\\
    Blockchain conf =\n%s\nIncoming funds = %s\nEmitted Messages =\n%s\nEmitted events =\n%s\n"
      pp_env pp_fields pp_balance pp_accepted pp_bc_conf pp_in_funds pp_emitted pp_events

  (*  Manipulations with configuartion *)
  
  let store st k v =
    match v with 
    | Env.ValClosure _ | Env.ValFix _ | Env.ValTypeClosure _ ->
        fail @@ sprintf "Cannot store a closure below into a field %s:\n%s"
          k (Env.pp_value v)
    | Env.ValLit l ->
        (let s = st.fields in
         match List.find s ~f:(fun (z, _) -> z = k) with
         | Some (_, l') -> pure @@
             ({st with
              fields = (k, l) :: List.filter ~f:(fun z -> fst z <> k) s}
              , G_Store(l', l))
         | None -> fail @@ sprintf
               "No field \"%s\" in fields:\n%s" k (pp_literal_map s))

  let load st k =
    let i = get_id k in
    if i = balance_label
    then
      (* Balance is a special case *)
      let l = UintLit (Uint128L st.balance) in
      pure (l, G_Load(l))
    else
      (* Evenrything else is from fields *)
      let s = st.fields in
      match List.find ~f:(fun z -> fst z = i) s with 
      | Some x -> pure @@ (snd x, G_Load(snd x))
      | None -> fail @@ sprintf
            "No field \"%s\" in field map:\n%s" i (pp_literal_map s)

  let bind st k v =
    let e = st.env in
    {st with env = (k, v) :: List.filter ~f:(fun z -> fst z <> k) e}

  let lookup st k = Env.lookup st.env k

  let bc_lookup st k = BlockchainState.lookup st.blockchain_state k

  let accept_incoming st =
    let incoming' = st.incoming_funds in
    (* Although unsigned integer is used, and this check isn't
     * necessary, we have it just in case, some how a malformed
     * Uint128 literal manages to reach here.  *)
    if (Uint128.compare incoming' Uint128.zero) >= 0
    then
      let balance = Uint128.add st.balance incoming' in
      let accepted = true in
      let incoming_funds = Uint128.zero in
      pure @@ {st with balance; accepted; incoming_funds}
    else
      fail @@ sprintf "Incoming balance is negaitve (somehow):%s."
        (Uint128.to_string incoming')

  (* Check that message is well-formed before adding to the sending pool *)
  let rec validate_messages ls =
    (* TODO: implement more checks *)
    let validate_msg_payload pl =
      let has_tag = List.exists pl ~f:(fun (k, _) -> k = "tag") in      
      if has_tag then pure true
      else fail @@ sprintf "Message contents have no \"tag\" field:\n[%s]"
          (pp_literal_map pl)
    in
    match ls with
      | (Msg pl) :: tl ->
          let%bind _ = validate_msg_payload pl in
          validate_messages tl
      | [] -> pure true
      | m :: _ -> fail @@ sprintf "This is not a message:\n%s" (pp_literal m)

  (* Convert Scilla list to OCaml list *)
  let get_list_literal v =
      let rec convert_to_list = (function
        | ADTValue ("Nil", _, []) -> pure []
        | ADTValue ("Cons", _, [h; t]) ->
            let%bind rest = convert_to_list t in
            pure @@ h :: rest
        | l -> fail @@ sprintf "The literal is not a list:\n%s" (pp_literal l))
      in       
      match v with
      | (Env.ValFix _ | Env.ValClosure _ | Env.ValTypeClosure _ ) as v ->
          fail @@
          sprintf "Value should be a list of messages, but is a closure:\n%s"
            (Env.pp_value v)
      | Env.ValLit l -> convert_to_list l 

  let validate_outgoing_message m' =
    let open ContractUtil.MessagePayload in
    match m' with
    | Msg m ->
      (* All outgoing messages must have certain mandatory fields *)
      let tag_found = List.exists ~f:(fun (s, _) -> s = tag_label) m in
      let amount_found = List.exists ~f:(fun (s, _) -> s = amount_label) m in
      let recipient_found = List.exists ~f:(fun (s, _) -> s = recipient_label) m in
      let uniq_entries = List.for_all m
        ~f:(fun e -> (List.count m ~f:(fun e' -> fst e = fst e')) = 1) in
      if tag_found && amount_found && recipient_found && uniq_entries then pure m'
      else fail @@ sprintf 
        "Message %s is missing a mandatory field or has duplicate fields." (pp_literal (Msg m))
    | _ -> fail @@ sprintf "Literal %s is not a message, cannot be sent." (pp_literal m')

  let send_messages conf ms =
    let%bind ls' = get_list_literal ms in
    let%bind ls = mapM ~f:validate_outgoing_message ls' in
    let old_emitted = conf.emitted in
    let emitted = old_emitted @ ls in
    pure ({conf with emitted}, G_SendMsgs ls)

  let validate_event m' =
    let open ContractUtil.MessagePayload in
    match m' with
    | Msg m ->
      (* All events must have certain mandatory fields *)
      let eventname_found = List.exists ~f:(fun (s, _) -> s = eventname_label) m in
      let uniq_entries = List.for_all m
        ~f:(fun e -> (List.count m ~f:(fun e' -> fst e = fst e')) = 1) in
      if eventname_found && uniq_entries then pure m'
      else fail @@ sprintf 
        "Event %s is missing a mandatory field or has duplicate fields." (pp_literal (Msg m))
    | _ -> fail @@ sprintf "Literal %s is not a valid event argument." (pp_literal m')


  let create_event conf eparams_resolved =
    let%bind event = 
      match eparams_resolved with
      | Env.ValLit l -> 
        (match l with
        | Msg _ ->
          pure @@ l
        | _ -> fail @@ sprintf "Incorrect event parameter(s): %s\n" (pp_literal l))
      | (Env.ValFix _ | Env.ValClosure _ | Env.ValTypeClosure _ ) as v -> 
        fail @@ sprintf "Incorrect event parameters: %s\n" (Env.pp_value v)
    in
    let%bind event' = validate_event event in
    let old_events = conf.events in
    let events = event'::old_events in
    pure ({conf with events}, G_CreateEvnt event')
end

(*****************************************************)
(*         Contract state after initialization       *)
(*****************************************************)

module ContractState = struct

  type init_args = (string * literal) list 

  (* Runtime contract configuration and operations with it *)
  type t = {
    (* Immutable parameters *)
    env : Env.t;
    (* Contract fields *)
    fields : (string * literal) list;
    (* Contract balance *)
    balance : uint128;
  }

  (* Pretty-printing *)
  let pp cstate =
    let pp_params = Env.pp cstate.env in
    let pp_fields = pp_literal_map cstate.fields in
    let pp_balance = Uint128.to_string cstate.balance in
    sprintf "Contract State:\nImmutable parameters and libraries =\n%s\nMutable fields = \n%s\nBalance = %s\n"
      pp_params pp_fields pp_balance

end
