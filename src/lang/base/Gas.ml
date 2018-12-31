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
open ErrorUtils
open Result.Let_syntax
open MonadUtil
open Syntax
open TypeUtil
open PrimTypes
open Schnorr
open PrettyPrinters

(* Arbitrarily picked, the largest prime less than 100. *)
let version_mismatch_penalty = 97

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
      | BNum _ -> pure @@ 64 (* Implemented using big-nums. *)
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
          Caml.Hashtbl.fold (fun lit1 lit2 acc' ->
            let%bind acc = acc' in
            let%bind clit1 = literal_cost lit1 in
            let%bind clit2 = literal_cost lit2 in
            pure (acc + clit1 + clit2)) m (pure 0)
      (* A constructor in HNF *)      
      | ADTValue (cn, _, ll) as als ->
        (* Make a special case for Lists, to avoid overflowing recursion. *)
        if cn = "Cons"
        then
          let rec walk elm acc_cost =
            match elm with
            | ADTValue ("Cons", _, [l;ll]) ->
              let%bind lcost = literal_cost l in
              walk ll (acc_cost + lcost)
            | ADTValue ("Nil", _, _) ->
              pure (acc_cost + 1)
            | _ -> fail0 "Malformed list while computing literal cost"
          in
          walk als 0
        else
          if List.is_empty ll then pure 1 else
          foldM ~f:(fun acc lit' ->
              let%bind clit' = literal_cost lit' in
              pure (acc + clit')) ~init:0 ll
      (* TODO: Check this *)
      | Clo _ -> pure @@ 0
      | TAbs _ -> pure @@ 0

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
    | G_MapUpdate (n, lopt)
    | G_MapGet (n, lopt) ->
      let%bind l_cost = 
        (* Deleting a key only has the cost of indexing 
           (to incentivice removal of data). *)
        (match lopt with | Some l -> (literal_cost l) | None -> pure 0) in
      pure @@ n + l_cost
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
  type coster = string -> literal list -> int -> (int, scilla_error list) result
  (* op, arg types, coster, base cost. *)
  type builtin_record = string * (typ list) * coster * int
  (* a static coster that only looks at base cost. *)
  let base_coster (_ : string) (_ : literal list) base = pure base

  let string_coster op args base =
    match op, args with
    | "eq", [StringLit s1; StringLit s2] ->
      pure @@ (Int.min (String.length s1) (String.length s2)) * base
    | "concat", [StringLit s1; StringLit s2] ->
        pure @@ (String.length s1 + String.length s2) * base
    | "substr", [StringLit s; UintLit (Uint32L i1); UintLit (Uint32L i2)] ->
        pure @@ (Int.min (String.length s)
                         (Stdint.Uint32.to_int i1+ Stdint.Uint32.to_int i2))
                 * base
    | "to_string", [l] ->
      let%bind c = literal_cost l in
      pure @@ (c * base)
    | _ -> fail0 @@ "Gas cost error for string built-in"

  let crypto_coster op args base =
    let open BatOption in
    let%bind types = mapM args ~f:literal_type in
    let div_ceil x y =
      if (x % y = 0) then x / y else (x / y) + 1
    in
    match op, types, args with
    | "eq", [a1;a2], _
      when is_bystrx_type a1 && is_bystrx_type a2 &&
           get (bystrx_width a1) = get (bystrx_width a2)
      -> pure @@ get (bystrx_width a1) * base
    | "sha256hash", _, [a] ->
        (* Block size of sha256hash is 512 *)
        pure @@ (div_ceil (String.length (pp_literal a)) 64) * 15 * base
    | "keccak256hash", _, [a] ->
        (* Block size of keccak256hash is 1088 *)
        pure @@ (div_ceil (String.length (pp_literal a)) 136) * 15 * base
    | "ripemd160hash", _, [a] ->
        (* Block size of ripemd160hash is 512 *)
        pure @@ (div_ceil (String.length (pp_literal a)) 64) * 10 * base
    | "ec_gen_key_pair", _, _ -> pure 20
    | "schnorr_sign", _, [_;_;ByStr(s)]
    | "ecdsa_sign", _, [_;ByStr(s)] ->
        let x = div_ceil ((String.length s) + 66) 64 in
        pure @@ (350 + (15 * x)) * base
    | "schnorr_verify", _, [_;ByStr(s);_]
    | "ecdsa_verify", _, [_;ByStr(s);_] ->
        let x = div_ceil ((String.length s) + 66) 64 in
        pure @@ (250 + (15 * x)) * base
    | "to_bystr", [a], _
      when is_bystrx_type a -> pure @@ get (bystrx_width a) * base
    | "concat", [a1;a2], _
      when is_bystrx_type a1 && is_bystrx_type a2 ->
      pure @@ (get(bystrx_width a1) + get(bystrx_width a2)) * base
    | _ -> fail0 @@ "Gas cost error for hash built-in"

  let map_coster op args base =
    match args with
    | Map (_, m) :: _ -> 
      (* get and contains do not make a copy of the Map, hence constant. *)
      (match op with
      | "get" | "contains" -> pure base 
      | _ -> pure (base + (base * Caml.Hashtbl.length m)))
    | _ -> fail0 @@ "Gas cost error for map built-in"

  let to_nat_coster _ args base =
    match args with
    (* TODO: Is this good? *)
    | [UintLit (Uint32L i)] -> pure @@  (Stdint.Uint32.to_int i) * base
    | _ -> fail0 @@ "Gas cost error for to_nat built-in"

  let int_conversion_coster w _ args base =
    match args with
    | [IntLit _] | [UintLit _] | [StringLit _] ->
      if w = 32 || w = 64 then pure base
      else if w = 128 then pure (base * 2)
      else if w = 256 then pure (base * 4)
      else fail0 @@ "Gas cost error for integer conversion"
    | _ -> fail0 @@ "Gas cost due to incorrect arguments for int conversion"

  let int_coster op args base =
    let%bind base' =
       match op with
        | "mul" | "div" | "rem" -> pure (base * 5)
        | "pow" ->
          (match args with 
          | [_; UintLit(Uint32L p)] -> pure (base * 5 * (Stdint.Uint32.to_int p))
          | _ -> fail0 @@ "Gas cost error for built-in pow")
        | _ -> pure base
    in
    let%bind w = match args with
      | [IntLit i; _] ->
        pure @@ int_lit_width i
      | [UintLit i; _] ->
        pure @@ uint_lit_width i
      | _ -> fail0 @@ "Gas cost error for integer built-in"
    in
      if w = 32 || w = 64 then pure base'
      else if w = 128 then pure (base' * 2)
      else if w = 256 then pure (base' * 4)
      else fail0 @@ "Gas cost error for integer built-in"

  let tvar s = TypeVar(s)

  (* built-in op costs are propotional to size of data they operate on. *)
  let builtin_records : builtin_record list = [
    (* Strings *)
    ("eq", [string_typ;string_typ], string_coster, 1);
    ("concat", [string_typ;string_typ], string_coster, 1);
    ("substr", [string_typ; tvar "'A"; tvar "'A"], string_coster, 1);
    ("to_string", [tvar "'A"], string_coster, 1);

    (* Block numbers *)
    ("eq", [bnum_typ;bnum_typ], base_coster, 32);
    ("blt", [bnum_typ;bnum_typ], base_coster, 32);
    ("badd", [bnum_typ;tvar "'A"], base_coster, 32);
    ("bsub", [bnum_typ;bnum_typ], base_coster, 32);

    (* Crypto *)
    ("eq", [tvar "'A"; tvar "'A"], crypto_coster, 1);
    (* We currently only support `dist` for ByStr32. *)
    ("dist", [bystrx_typ hash_length; bystrx_typ hash_length], base_coster, 32);
    ("to_bystr", [tvar "'A"], crypto_coster, 1);
    ("sha256hash", [tvar "'A"], crypto_coster, 1);
    ("keccak256hash", [tvar "'A"], crypto_coster, 1);
    ("ripemd160hash", [tvar "'A"], crypto_coster, 1);
    ("ec_gen_key_pair", [], crypto_coster, 1);
    ("schnorr_sign", [bystrx_typ privkey_len; bystrx_typ pubkey_len; bystr_typ], crypto_coster, 1);
    ("schnorr_verify", [bystrx_typ pubkey_len; bystr_typ; bystrx_typ signature_len], crypto_coster, 1);
    ("ecdsa_sign", [bystrx_typ Secp256k1Wrapper.privkey_len; bystr_typ], crypto_coster, 1);
    ("ecdsa_verify", [bystrx_typ Secp256k1Wrapper.pubkey_len; bystr_typ; bystrx_typ Secp256k1Wrapper.signature_len], crypto_coster, 1);
    ("concat", [tvar "'A"; tvar "'A"], crypto_coster, 1);

    (* Maps *)
    ("contains", [tvar "'A"; tvar "'A"], map_coster, 1);
    ("put", [tvar "'A"; tvar "'A"; tvar "'A"], map_coster, 1);
    ("get", [tvar "'A"; tvar "'A"], map_coster, 1);
    ("remove", [tvar "'A"; tvar "'A"], map_coster, 1);
    ("to_list", [tvar "'A"], map_coster, 1);
    ("size", [tvar "'A"], map_coster, 1); 

    (* Integers *)
    ("eq", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("lt", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("add", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("sub", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("mul", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("div", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("rem", [tvar "'A"; tvar "'A"], int_coster, 4);
    ("pow", [tvar "'A"; uint32_typ], int_coster, 4);
    ("to_int32", [tvar "'A"], int_conversion_coster 32, 4);
    ("to_int64", [tvar "'A"], int_conversion_coster 64, 4);
    ("to_int128", [tvar "'A"], int_conversion_coster 128, 4);
    ("to_int256", [tvar "'A"], int_conversion_coster 256, 4);
    ("to_uint32", [tvar "'A"], int_conversion_coster 32, 4);
    ("to_uint64", [tvar "'A"], int_conversion_coster 64, 4);
    ("to_uint128", [tvar "'A"], int_conversion_coster 128, 4);
    ("to_uint256", [tvar "'A"], int_conversion_coster 256, 4);
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
      else fail0 @@ "Name or arity doesn't match"
    in
    let msg = sprintf "Unable to determine gas cost for \"%s\"" op in
    let open Caml in
    let dict = match Hashtbl.find_opt builtin_hashtbl op with | Some rows -> rows | None -> [] in
    let %bind (_, cost) = tryM dict ~f:matcher ~msg:(fun () -> mk_error0 msg) in
    pure cost

end
