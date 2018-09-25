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
open Result.Let_syntax
open MonadUtil
open Syntax
open TypeUtil
open PrimTypes
open Schnorr
open PrettyPrinters

module ScillaGas
    (SR : Rep)
    (ER : Rep) = struct

  module GasTypeUtilities = TypeUtilities (SR) (ER)
  module GasSyntax = ScillaSyntax (SR) (ER)
  open GasTypeUtilities
  open GasSyntax

  (* The storage cost of a literal, based on it's size. *)
  let rec literal_cost lit =
    match lit with
      (* StringLits have fixed cost till a certain
         length and increased cost after that. *)
      | StringLit s ->
          let l = String.length s in
          pure @@ if l <= 20 then 20 else l
      | BNum _ -> pure @@ 32 (* 256 bits *)
      (* (bit-width, value) *)
      | IntLit x -> pure @@ (int_lit_width x) / 8
      | UintLit x -> pure @@ (uint_lit_width x) / 8
      (* (bit-width, value) *)
      | ByStrX (w, _) -> pure @@ w
      | ByStr s ->
          pure @@ (String.length s) - 2
      (* Message: an associative array *)    
      | Msg m ->
          foldM ~f:(fun acc (s, lit') ->
              let%bind cs = literal_cost (StringLit(s)) in
              let%bind clit' = literal_cost lit' in
              pure (acc + cs + clit')) ~init:0 m
      (* A dynamic map of literals *)    
      | Map (_, m) ->
          foldM ~f:(fun acc (lit1, lit2) ->
              let%bind clit1 = literal_cost lit1 in
              let%bind clit2 = literal_cost lit2 in
              pure (acc + clit1 + clit2)) ~init:0 m
      (* A constructor in HNF *)      
      | ADTValue (_, _, ll) ->
          foldM ~f:(fun acc lit' ->
              let%bind clit' = literal_cost lit' in
              pure (acc + clit')) ~init:0 ll

  let expr_static_cost erep =
    let (e, _) = erep in
    match e with
    | Literal _ | Var _ | Let _
    | Message _ | Fun _ | App _
    | Constr _ | TFun _ | TApp _ ->
        pure 1
    | MatchExpr (_, clauses) ->
        pure @@ List.length clauses
    | Fixpoint _ -> pure 1 (* more cost accounted during recursive evaluation. *)
    | Builtin _ -> pure 0 (* this is a dynamic cost. *)

  let stmt_cost scon = match scon with
    | G_Load l -> literal_cost l
    | G_Store (old_l, new_l) -> 
        let%bind old_cost =  literal_cost(old_l) in 
        let%bind new_cost = literal_cost(new_l) in
        let storage_cost = new_cost - old_cost in
        let op_cost = Int.max old_cost new_cost in
        pure @@ op_cost + storage_cost
    | G_Bind -> pure 1
    | G_MatchStmt num_clauses-> pure num_clauses
    | G_ReadFromBC -> pure 1
    | G_AcceptPayment -> pure 1
    | G_SendMsgs mlist -> foldM ~f:(fun acc m -> 
        let%bind c = literal_cost m in
        pure (acc + c)) ~init:0 mlist
    | G_CreateEvnt e -> literal_cost e

  (* A signature for functions that determine dynamic cost of built-in ops. *)
  (* op -> arguments -> base cost -> total cost *)
  type coster = string -> literal list -> int -> (int, string) result
  (* op, arg types, coster, base cost. *)
  type builtin_record = string * (typ list) * coster * int
  (* a static coster that only looks at base cost. *)
  let base_coster (_ : string) (_ : literal list) base = pure base

  let string_coster op args base =
    match op, args with
    | "eq", [StringLit s1; StringLit s2]
    | "concat", [StringLit s1; StringLit s2] ->
        pure @@ (String.length s1 + String.length s2) * base
    | "substr", [StringLit s; UintLit (Uint32L _); UintLit (Uint32L _)] ->
        pure @@ (String.length s) * base
    | _ -> fail @@ "Gas cost error for string built-in"

  let hash_coster op args base =
    let open BatOption in
    let%bind types = mapM args ~f:literal_type in
    match op, types, args with
    | "eq", [a1;a2], _
      when is_bystrx_type a1 && is_bystrx_type a2 &&
           get (bystrx_width a1) = get (bystrx_width a2)
      -> pure @@ get (bystrx_width a1) * base
    | "sha256hash", _, [a] ->
        pure @@ (String.length (pp_literal a) + 20) * base
    | "schnorr_gen_key_pair", _, _ -> pure 20 (* TODO *)
    | "schnorr_sign", _, [_;_;ByStr(s)]
    | "schnorr_verify", _, [_;ByStr(s);_] ->
        pure @@ (String.length s) * base
    | _ -> fail @@ "Gas cost error for hash built-in"

  let map_coster _ args base =
    match args with
    | Map (_, m)::_ ->
        (* TODO: Should these be linear? *)
        pure @@ (List.length m) * base
    | _ -> fail @@ "Gas cost error for map built-in"

  let to_nat_coster _ args base =
    match args with
    (* TODO: Is this good? *)
    | [UintLit (Uint32L i)] -> pure @@  (Stdint.Uint32.to_int i) * base
    | _ -> fail @@ "Gas cost error for to_nat built-in"

  let int_coster op args base =
    let base' = 
      match op with
      | "mul" -> base * 2
      | "div" | "rem" -> base * 4
      | _ -> base
    in
    let%bind w = match args with
      | [IntLit i] | [IntLit i; IntLit _] ->
        pure @@ int_lit_width i
      | [UintLit i] | [UintLit i; UintLit _] ->
        pure @@ uint_lit_width i
      | _ -> fail @@ "Gas cost error for integer built-in"
    in
      if w = 32 || w = 64 then pure base'
      else if w = 128 then pure (base' * 2)
      else if w = 256 then pure (base' * 4)
      else fail @@ "Gas cost error for integer built-in"

  let tvar s = TypeVar(s)

  (* built-in op costs are propotional to size of data they operate on. *)
  let builtin_records : builtin_record list = [
    (* Strings *)
    ("eq", [string_typ;string_typ], string_coster, 1);
    ("concat", [string_typ;string_typ], string_coster, 1);
    ("substr", [string_typ; tvar "'A"; tvar "'A"], string_coster, 1);

    (* Block numbers *)
    ("eq", [bnum_typ;bnum_typ], base_coster, 4);
    ("blt", [bnum_typ;bnum_typ], base_coster, 4);
    ("badd", [bnum_typ;tvar "'A"], base_coster, 4);

    (* Hashes *)
    ("eq", [tvar "'A"; tvar "'A"], hash_coster, 1);
    (* We currently only support `dist` for ByStr32. *)
    ("dist", [bystrx_typ hash_length; bystrx_typ hash_length], base_coster, 32);
    ("sha256hash", [tvar "'A"], hash_coster, 1);
    ("schnorr_gen_key_pair", [], hash_coster, 1);
    ("schnorr_sign", [bystrx_typ privkey_len; bystrx_typ pubkey_len; bystr_typ], hash_coster, 5);
    ("schnorr_verify", [bystrx_typ pubkey_len; bystr_typ; bystrx_typ signature_len], hash_coster, 5);

    (* Maps *)
    ("contains", [tvar "'A"; tvar "'A"], map_coster, 1);
    ("put", [tvar "'A"; tvar "'A"; tvar "'A"], map_coster, 1);
    ("get", [tvar "'A"; tvar "'A"], map_coster, 1);
    ("remove", [tvar "'A"; tvar "'A"], map_coster, 1);
    ("to_list", [tvar "'A"], map_coster, 1); 

    (* Integers *)
    ("eq", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("lt", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("add", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("sub", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("mul", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("div", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("rem", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("to_int32", [tvar "'A"], int_coster, 4);
    ("to_int64", [tvar "'A"], int_coster, 4);
    ("to_int128", [tvar "'A"], int_coster, 4);
    ("to_int256", [tvar "'A"], int_coster, 4);
    ("to_uint32", [tvar "'A"], int_coster, 4);
    ("to_uint64", [tvar "'A"], int_coster, 4);
    ("to_uint128", [tvar "'A"], int_coster, 4);
    ("to_uint256", [tvar "'A"], int_coster, 4);
    ("to_nat", [tvar "'A"], to_nat_coster, 1);
  ]

  let builtin_hashtbl =
    let open Caml in
    let ht : ((string, builtin_record list) Hashtbl.t) = Hashtbl.create 64 in
    List.iter (fun row ->
      let (opname, _, _, _) = row in
      match Hashtbl.find_opt ht opname with
      | Some p ->  Hashtbl.add ht opname (row::p)
      | None -> Hashtbl.add ht opname (row::[])
    ) builtin_records;
      ht

  let builtin_cost op_i arg_literals =
    let op = get_id op_i in
    let%bind arg_types = mapM arg_literals ~f:literal_type in
    let matcher (name, types, fcoster, base) = 
      (* The names and type list lengths must match and *)
      if name = op && List.length types = List.length arg_types
         && (List.for_all2_exn ~f:(fun t1 t2 ->
             (* the types should match *)
             type_equiv t1 t2 ||
             (* or the built-in record is generic *)
             (match t2 with | TypeVar _ -> true | _ -> false)) 
             arg_types types)
      then fcoster op arg_literals base (* this can fail too *)
      else fail @@ "Name or arity doesn't match"
    in
    let msg = sprintf "Unable to determine gas cost for \"%s %s\""
        op (pp_literal_list arg_literals) in
    let open Caml in
    let dict = match Hashtbl.find_opt builtin_hashtbl op with | Some rows -> rows | None -> [] in
    let %bind (_, cost) = tryM dict ~f:matcher ~msg:msg in
    pure cost

end
