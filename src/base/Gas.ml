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
open ErrorUtils
open Result.Let_syntax
open MonadUtil
open Literal
open Syntax
open TypeUtil
open Scilla_crypto.Schnorr
open PrettyPrinters
open Datatypes.SnarkTypes

let scale_factor = Stdint.Uint64.of_int 8

(* Scale down the remaining gas to original metrics *)
let finalize_remaining_gas initial_gas_limit remaining_gas =
  let open Stdint in
  let remain = Uint64.div remaining_gas scale_factor in
  (* Ensure that at least one unit of gas is consumed. *)
  if Uint64.compare remain initial_gas_limit = 0 then
    Uint64.sub remain Uint64.one
  else remain

(* Arbitrarily picked, the largest prime less than 100. *)
let version_mismatch_penalty = 97

module ScillaGas (SR : Rep) (ER : Rep) = struct
  (* TODO: Change this to CanonicalLiteral = Literals based on canonical names. *)
  module GasLiteral = FlattenedLiteral
  module GasType = GasLiteral.LType
  module GasIdentifier = GasType.TIdentifier
  module GasSyntax = ScillaSyntax (SR) (ER) (GasLiteral)
  open TypeUtilities
  open GasType
  open GasLiteral
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
    | IntLit x -> pure @@ (int_lit_width x / 8)
    | UintLit x -> pure @@ (uint_lit_width x / 8)
    | ByStr bs -> pure @@ Bystr.width bs
    | ByStrX bs -> pure @@ Bystrx.width bs
    (* Message: an associative array *)
    | Msg m ->
        foldM
          ~f:(fun acc (s, lit') ->
            let%bind cs = literal_cost (StringLit s) in
            let%bind clit' = literal_cost lit' in
            pure (acc + cs + clit'))
          ~init:0 m
    (* A dynamic map of literals *)
    | Map (_, m) ->
        Caml.Hashtbl.fold
          (fun lit1 lit2 acc' ->
            let%bind acc = acc' in
            let%bind clit1 = literal_cost lit1 in
            let%bind clit2 = literal_cost lit2 in
            pure (acc + clit1 + clit2))
          m (pure 0)
    (* A constructor in HNF *)
    | ADTValue (cn, _, ll) as als ->
        (* Make a special case for Lists, to avoid overflowing recursion. *)
        if String.(cn = "Cons") then
          let rec walk elm acc_cost =
            match elm with
            | ADTValue ("Cons", _, [ l; ll ]) ->
                let%bind lcost = literal_cost l in
                walk ll (acc_cost + lcost)
            | ADTValue ("Nil", _, _) -> pure (acc_cost + 1)
            | _ -> fail0 "Malformed list while computing literal cost"
          in
          walk als 0
        else if List.is_empty ll then pure 1
        else
          foldM
            ~f:(fun acc lit' ->
              let%bind clit' = literal_cost lit' in
              pure (acc + clit'))
            ~init:0 ll
    (* Constant cost for forming a closure (similar to expr_static_cost below). *)
    | Clo _ -> pure @@ 1
    | TAbs _ -> pure @@ 1

  let expr_static_cost erep =
    let e, _ = erep in
    match e with
    | Literal _ | Var _ | Let _ | Message _ | Fun _ | App _ | Constr _ | TFun _
    | TApp _ ->
        pure 1
    | MatchExpr (_, clauses) -> pure @@ List.length clauses
    | Fixpoint _ ->
        pure 1 (* more cost accounted during recursive evaluation. *)
    | Builtin _ -> pure 0

  (* this is a dynamic cost. *)

  let stmt_cost scon =
    let rec map_sort_cost l =
      match l with
      | Map (_, kvlist) ->
          let sub_cost =
            Caml.Hashtbl.fold
              (fun _ vlit acc -> acc + map_sort_cost vlit)
              kvlist 0
          in
          let this_cost =
            let len = Caml.Hashtbl.length kvlist in
            if len > 0 then
              let log_len = Int.of_float (Float.log (Int.to_float len)) in
              len * log_len
            else 0
          in
          sub_cost + this_cost
      | _ -> 0
    in
    match scon with
    | G_Load l ->
        let%bind l_cost = literal_cost l in
        let sort_cost = map_sort_cost l in
        pure (l_cost + sort_cost)
    | G_Store l -> literal_cost l
    | G_MapUpdate (n, lopt) ->
        let%bind l_cost =
          match lopt with Some l -> literal_cost l | None -> pure 0
        in
        pure @@ (n + l_cost)
    | G_MapGet (n, lopt) ->
        let%bind l_cost, sort_cost =
          match lopt with
          | Some l ->
              let%bind l_cost = literal_cost l in
              pure (l_cost, map_sort_cost l)
          | None -> pure (0, 0)
        in
        pure (n + l_cost + sort_cost)
    | G_Bind -> pure 1
    | G_MatchStmt num_clauses -> pure num_clauses
    | G_ReadFromBC -> pure 1
    | G_AcceptPayment -> pure 1
    | G_SendMsgs mlist ->
        foldM
          ~f:(fun acc m ->
            let%bind c = literal_cost m in
            pure (acc + c))
          ~init:0 mlist
    | G_CreateEvnt e -> literal_cost e
    | G_CallProc -> pure 1

  (* A signature for functions that determine dynamic cost of built-in ops. *)
  (* op -> arguments -> base cost -> total cost *)
  type coster =
    builtin -> GasLiteral.t list -> int -> (int, scilla_error list) result

  (* op, arg types, coster, base cost. *)
  type builtin_record = builtin * GasType.t list * coster * int

  (* a static coster that only looks at base cost. *)
  let base_coster (_ : builtin) (_ : GasLiteral.t list) base = pure base

  let string_coster op args base =
    match (op, args) with
    | Builtin_eq, [ StringLit s1; StringLit s2 ] ->
        pure @@ (Int.min (String.length s1) (String.length s2) * base)
    | Builtin_concat, [ StringLit s1; StringLit s2 ] ->
        pure @@ ((String.length s1 + String.length s2) * base)
    | ( Builtin_substr,
        [ StringLit s; UintLit (Uint32L i1); UintLit (Uint32L i2) ] ) ->
        pure
        @@ Int.min (String.length s)
             (Stdint.Uint32.to_int i1 + Stdint.Uint32.to_int i2)
           * base
    | Builtin_strlen, [ StringLit s ] -> pure @@ (String.length s * base)
    | Builtin_to_string, [ l ] ->
        let%bind c = literal_cost l in
        pure @@ (c * base)
    | _ -> fail0 @@ "Gas cost error for string built-in"

  let crypto_coster op args base =
    let%bind types = mapM args ~f:literal_type in
    let div_ceil x y = if x % y = 0 then x / y else (x / y) + 1 in
    match (op, types, args) with
    | Builtin_eq, [ PrimType Bystr_typ; PrimType Bystr_typ ], [ ByStr a1; _ ] ->
        pure (Bystr.width a1 * base)
    | Builtin_eq, [ a1; a2 ], _
      when is_bystrx_type a1 && is_bystrx_type a2
           && Option.(value_exn (bystrx_width a1) = value_exn (bystrx_width a2))
      ->
        pure @@ (Option.value_exn (bystrx_width a1) * base)
    | Builtin_to_uint256, [ a ], _
      when is_bystrx_type a && Option.value_exn (bystrx_width a) <= 32 ->
        pure (32 * base)
    | Builtin_sha256hash, _, [ a ] | Builtin_schnorr_get_address, _, [ a ] ->
        (* Block size of sha256hash is 512 *)
        pure @@ (div_ceil (String.length (pp_literal a)) 64 * 15 * base)
    | Builtin_keccak256hash, _, [ a ] ->
        (* Block size of keccak256hash is 1088 *)
        pure @@ (div_ceil (String.length (pp_literal a)) 136 * 15 * base)
    | Builtin_ripemd160hash, _, [ a ] ->
        (* Block size of ripemd160hash is 512 *)
        pure @@ (div_ceil (String.length (pp_literal a)) 64 * 10 * base)
    | Builtin_schnorr_verify, _, [ _; ByStr s; _ ]
    | Builtin_ecdsa_verify, _, [ _; ByStr s; _ ] ->
        let x = div_ceil (Bystr.width s + 66) 64 in
        pure @@ ((250 + (15 * x)) * base)
    | Builtin_to_bystr, [ a ], _ when is_bystrx_type a ->
        pure @@ (Option.value_exn (bystrx_width a) * base)
    | Builtin_bech32_to_bystr20, _, [ prefix; addr ]
    | Builtin_bystr20_to_bech32, _, [ prefix; addr ] ->
        pure
        @@ (String.length (pp_literal prefix) + String.length (pp_literal addr))
           * base
    | Builtin_concat, [ a1; a2 ], _ when is_bystrx_type a1 && is_bystrx_type a2
      ->
        pure
        @@ Option.(
             (value_exn (bystrx_width a1) + value_exn (bystrx_width a2)) * base)
    | Builtin_alt_bn128_G1_add, _, _ -> pure @@ (20 * base)
    | Builtin_alt_bn128_G1_mul, _, [ _; s ] ->
        let%bind s' = scilla_scalar_to_ocaml s in
        let u = Integer256.Uint256.of_bytes_big_endian (Bytes.of_string s') 0 in
        let multiplier = Float.log (Integer256.Uint256.to_float u) in
        let multiplier_int = Float.to_int multiplier in
        pure @@ (20 * multiplier_int * base)
    | Builtin_alt_bn128_pairing_product, _, [ pairs ] ->
        let%bind opairs = scilla_g1g2pairlist_to_ocaml pairs in
        let list_len = List.length opairs in
        pure @@ (list_len * 40 * base)
    | _ -> fail0 @@ "Gas cost error for hash built-in"

  let map_coster op args base =
    match args with
    | Map (_, m) :: _ -> (
        (* size, get and contains do not make a copy of the Map, hence constant. *)
        match op with
        | Builtin_size | Builtin_get | Builtin_contains -> pure base
        | _ -> pure (base + (base * Caml.Hashtbl.length m)) )
    | _ -> fail0 @@ "Gas cost error for map built-in"

  let to_nat_coster _ args base =
    match args with
    | [ UintLit (Uint32L i) ] -> pure @@ (Stdint.Uint32.to_int i * base)
    | _ -> fail0 @@ "Gas cost error for to_nat built-in"

  let int_conversion_coster w _ args base =
    match args with
    | [ IntLit _ ] | [ UintLit _ ] | [ StringLit _ ] ->
        if w = 32 || w = 64 then pure base
        else if w = 128 then pure (base * 2)
        else if w = 256 then pure (base * 4)
        else fail0 @@ "Gas cost error for integer conversion"
    | _ -> fail0 @@ "Gas cost due to incorrect arguments for int conversion"

  let int_coster op args base =
    let%bind base' =
      match op with
      | Builtin_mul | Builtin_div | Builtin_rem -> pure (base * 5)
      | Builtin_pow -> (
          match args with
          | [ _; UintLit (Uint32L p) ] ->
              pure (base * 5 * Stdint.Uint32.to_int p)
          | _ -> fail0 @@ "Gas cost error for built-in pow" )
      | Builtin_isqrt ->
          let%bind ifloat =
            match args with
            | [ UintLit (Uint32L i) ] -> pure @@ Stdint.Uint32.to_float i
            | [ UintLit (Uint64L i) ] -> pure @@ Stdint.Uint64.to_float i
            | [ UintLit (Uint128L i) ] -> pure @@ Stdint.Uint128.to_float i
            | [ UintLit (Uint256L i) ] ->
                pure @@ Float.of_string @@ Integer256.Uint256.to_string i
            | _ -> fail0 "Invalid argument type to isqrt"
          in
          (* The +. 1.0 is just to ensure that we don't input 0 to Float.log. *)
          pure @@ (base * Int.of_float (Float.log (ifloat +. 1.0)))
      | _ -> pure base
    in
    let%bind w =
      match args with
      | IntLit i :: _ -> pure @@ int_lit_width i
      | UintLit i :: _ -> pure @@ uint_lit_width i
      | _ -> fail0 @@ "Gas cost error for integer built-in"
    in
    if w = 32 || w = 64 then pure base'
    else if w = 128 then pure (base' * 2)
    else if w = 256 then pure (base' * 4)
    else fail0 @@ "Gas cost error for integer built-in"

  let tvar s = TypeVar s

  [@@@ocamlformat "disable"]

  (* built-in op costs are propotional to size of data they operate on. *)
  let builtin_records : builtin_record list = [
    (* Strings *)
    (Builtin_eq, [string_typ;string_typ], string_coster, 1);
    (Builtin_concat, [string_typ;string_typ], string_coster, 1);
    (Builtin_substr, [string_typ; tvar "'A"; tvar "'A"], string_coster, 1);
    (Builtin_strlen, [string_typ], string_coster, 1);
    (Builtin_to_string, [tvar "'A"], string_coster, 1);
  
    (* Block numbers *)
    (Builtin_eq, [bnum_typ;bnum_typ], base_coster, 32);
    (Builtin_blt, [bnum_typ;bnum_typ], base_coster, 32);
    (Builtin_badd, [bnum_typ;tvar "'A"], base_coster, 32);
    (Builtin_bsub, [bnum_typ;bnum_typ], base_coster, 32);
  
    (* Crypto *)
    (Builtin_eq, [tvar "'A"; tvar "'A"], crypto_coster, 1);
    (Builtin_to_bystr, [tvar "'A"], crypto_coster, 1);
    (Builtin_bech32_to_bystr20, [string_typ;string_typ], crypto_coster, 4);
    (Builtin_bystr20_to_bech32, [string_typ;bystrx_typ address_length], crypto_coster, 4);
    (Builtin_to_uint256, [tvar "'A"], crypto_coster, 1);
    (Builtin_sha256hash, [tvar "'A"], crypto_coster, 1);
    (Builtin_keccak256hash, [tvar "'A"], crypto_coster, 1);
    (Builtin_ripemd160hash, [tvar "'A"], crypto_coster, 1);
    (Builtin_schnorr_verify, [bystrx_typ pubkey_len; bystr_typ; bystrx_typ signature_len], crypto_coster, 1);
    (Builtin_ecdsa_verify, [bystrx_typ Secp256k1Wrapper.pubkey_len; bystr_typ; bystrx_typ Secp256k1Wrapper.signature_len], crypto_coster, 1);
    (Builtin_concat, [tvar "'A"; tvar "'A"], crypto_coster, 1);
    (Builtin_schnorr_get_address, [bystrx_typ pubkey_len], crypto_coster, 1);
    (Builtin_alt_bn128_G1_add, [g1point_type; g1point_type], crypto_coster, 1);
    (Builtin_alt_bn128_G1_mul, [g1point_type; scalar_type], crypto_coster, 1);
    (Builtin_alt_bn128_pairing_product, [g1g2pair_list_type], crypto_coster, 1);
  
    (* Maps *)
    (Builtin_contains, [tvar "'A"; tvar "'A"], map_coster, 1);
    (Builtin_put, [tvar "'A"; tvar "'A"; tvar "'A"], map_coster, 1);
    (Builtin_get, [tvar "'A"; tvar "'A"], map_coster, 1);
    (Builtin_remove, [tvar "'A"; tvar "'A"], map_coster, 1);
    (Builtin_to_list, [tvar "'A"], map_coster, 1);
    (Builtin_size, [tvar "'A"], map_coster, 1);
  
    (* Integers *)
    (Builtin_eq, [tvar "'A"; tvar "'A"], int_coster, 4);
    (Builtin_lt, [tvar "'A"; tvar "'A"], int_coster, 4);
    (Builtin_add, [tvar "'A"; tvar "'A"], int_coster, 4);
    (Builtin_sub, [tvar "'A"; tvar "'A"], int_coster, 4);
    (Builtin_mul, [tvar "'A"; tvar "'A"], int_coster, 4);
    (Builtin_div, [tvar "'A"; tvar "'A"], int_coster, 4);
    (Builtin_rem, [tvar "'A"; tvar "'A"], int_coster, 4);
    (Builtin_pow, [tvar "'A"; uint32_typ], int_coster, 4);
    (Builtin_isqrt, [tvar "'A"], int_coster, 4);
    (Builtin_to_int32, [tvar "'A"], int_conversion_coster 32, 4);
    (Builtin_to_int64, [tvar "'A"], int_conversion_coster 64, 4);
    (Builtin_to_int128, [tvar "'A"], int_conversion_coster 128, 4);
    (Builtin_to_int256, [tvar "'A"], int_conversion_coster 256, 4);
    (Builtin_to_uint32, [tvar "'A"], int_conversion_coster 32, 4);
    (Builtin_to_uint64, [tvar "'A"], int_conversion_coster 64, 4);
    (Builtin_to_uint128, [tvar "'A"], int_conversion_coster 128, 4);
    (Builtin_to_uint256, [tvar "'A"], int_conversion_coster 256, 4);
    (Builtin_to_nat, [uint32_typ], to_nat_coster, 1);
  ]

  [@@@ocamlformat "enable"]

  let builtin_hashtbl =
    let open Caml in
    let ht : (builtin, builtin_record list) Hashtbl.t = Hashtbl.create 64 in
    List.iter
      (fun row ->
        let opname, _, _, _ = row in
        match Hashtbl.find_opt ht opname with
        | Some p -> Hashtbl.add ht opname (row :: p)
        | None -> Hashtbl.add ht opname [ row ])
      builtin_records;
    ht

  let builtin_cost (op, _) arg_literals =
    let%bind arg_types = mapM arg_literals ~f:literal_type in
    let matcher (name, types, fcoster, base) =
      (* The names and type list lengths must match and *)
      if
        [%equal: Syntax.builtin] name op
        && List.length types = List.length arg_types
        && List.for_all2_exn
             ~f:(fun t1 t2 ->
               (* the types should match *)
               [%equal: GasType.t] t1 t2
               ||
               (* or the built-in record is generic *)
               match t2 with TypeVar _ -> true | _ -> false)
             arg_types types
      then fcoster op arg_literals base (* this can fail too *)
      else fail0 @@ "Name or arity doesn't match"
    in
    let msg =
      sprintf "Unable to determine gas cost for \"%s\"" (pp_builtin op)
    in
    let dict =
      match Caml.Hashtbl.find_opt builtin_hashtbl op with
      | Some rows -> rows
      | None -> []
    in
    let%bind _, cost = tryM dict ~f:matcher ~msg:(fun () -> mk_error0 msg) in
    pure cost
end
