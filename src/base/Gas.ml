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
open Scilla_crypto.Schnorr
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
  module GasLiteral = GlobalLiteral
  module GasSyntax = ScillaSyntax (SR) (ER) (GasLiteral)
  module GasType = GasSyntax.SType
  module GI = GasSyntax.SIdentifier
  module GasGasCharge = GasSyntax.SGasCharge
  open GasType
  open GasLiteral
  open GasSyntax

  let address_typecheck_cost t =
    let size =
      match t with
      | Address (Some fts) ->
          (* look up _this_address and every listed field *)
          1 + IdLoc_Comp.Map.length fts
      | _ -> 0
    in
    let cost = 2 + size (* _balance and _nonce must also be looked up *) in
    GasGasCharge.StaticCost cost

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
          ~f:(fun acc (s, _t, lit') ->
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
        let open Datatypes in
        (* Make a special case for Lists, to avoid overflowing recursion. *)
        if is_cons_ctr_name cn then
          let rec walk elm acc_cost =
            match elm with
            | ADTValue (c, _, [ l; ll ]) when is_cons_ctr_name c ->
                let%bind lcost = literal_cost l in
                walk ll (acc_cost + lcost)
            | ADTValue (c, _, _) when is_nil_ctr_name c -> pure (acc_cost + 1)
            | _ ->
                fail0 ~kind:"Malformed list while computing literal cost"
                  ?inst:None
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
    | ADTValue (_, _, ls) ->
        List.fold ~init:0 ls ~f:(fun acc l -> acc + map_sort_cost l)
    | _ -> 0

  let rec expr_static_cost e =
    let ee, erep = e in
    let%bind e' =
      match ee with
      | Literal _ | Var _ | Message _ | App _ | Constr _ | TApp _ ->
          pure @@ GasExpr (GasGasCharge.StaticCost 1, e)
      | Fixpoint (f, t, e') ->
          let%bind e'' = expr_static_cost e' in
          pure
          @@ GasExpr (GasGasCharge.StaticCost 1, (Fixpoint (f, t, e''), erep))
      | Fun (f, t, e') ->
          let%bind e'' = expr_static_cost e' in
          pure @@ GasExpr (GasGasCharge.StaticCost 1, (Fun (f, t, e''), erep))
      | TFun (f, e') ->
          let%bind e'' = expr_static_cost e' in
          pure @@ GasExpr (GasGasCharge.StaticCost 1, (TFun (f, e''), erep))
      | Let (i, t, lhs, rhs) ->
          let%bind lhs' = expr_static_cost lhs in
          let%bind rhs' = expr_static_cost rhs in
          pure
          @@ GasExpr (GasGasCharge.StaticCost 1, (Let (i, t, lhs', rhs'), erep))
      | MatchExpr (o, clauses) ->
          let%bind clauses' =
            mapM clauses ~f:(fun (p, e') ->
                let%bind e'' = expr_static_cost e' in
                pure (p, e''))
          in
          pure
          @@ GasExpr
               ( GasGasCharge.StaticCost (List.length clauses),
                 (MatchExpr (o, clauses'), erep) )
      | Builtin _ ->
          (* We don't add costs for Builtin because we can't know it statically
           * without type info (Eval doesn't run the type checker). To know this
           * statically, call builtin_cost below, providing argument types. *)
          pure ee
      | GasExpr _ -> fail0 ~kind:"Unexpected gas charge" ?inst:None
    in
    pure (e', erep)

  (* this is a dynamic cost. *)
  let rec stmts_cost = function
    | [] -> pure []
    | (s, srep) :: rem_stmts ->
        let%bind s' =
          match s with
          | Load (x, _) | RemoteLoad (x, _, _) ->
              let g =
                GasStmt
                  (GasGasCharge.SumOf
                     ( GasGasCharge.SizeOf (GI.get_id x),
                       GasGasCharge.MapSortCost (GI.get_id x) ))
              in
              (* We charge *after* the load because we can't know the size before. *)
              pure @@ [ (s, srep); (g, srep) ]
          | Store (_, v) | SendMsgs v | CreateEvnt v ->
              let g = GasStmt (GasGasCharge.SizeOf (GI.get_id v)) in
              pure @@ [ (g, srep); (s, srep) ]
          | Bind (x, e) ->
              let g = GasStmt (GasGasCharge.StaticCost 1) in
              let%bind e' = expr_static_cost e in
              let s' = Bind (x, e') in
              pure @@ [ (g, srep); (s', srep) ]
          | ReadFromBC _ | CallProc _ ->
              let g = GasStmt (GasGasCharge.StaticCost 1) in
              pure @@ [ (g, srep); (s, srep) ]
          | TypeCast (_x, _r, t) ->
              let g = address_typecheck_cost t in
              pure @@ [ (GasStmt g, srep); (s, srep) ]
          | MapUpdate (_, klist, ropt) ->
              let n = GasGasCharge.StaticCost (List.length klist) in
              let g =
                match ropt with
                | Some r ->
                    GasGasCharge.SumOf (GasGasCharge.SizeOf (GI.get_id r), n)
                | None -> n
              in
              pure @@ [ (GasStmt g, srep); (s, srep) ]
          | MapGet (x, _, klist, _) | RemoteMapGet (x, _, _, klist, _) ->
              let n = GasGasCharge.StaticCost (List.length klist) in
              let g =
                GasGasCharge.SumOf
                  ( GasGasCharge.SumOf
                      ( GasGasCharge.SizeOf (GI.get_id x),
                        GasGasCharge.MapSortCost (GI.get_id x) ),
                    n )
              in
              pure @@ [ (s, srep); (GasStmt g, srep) ]
          | MatchStmt (x, clauses) ->
              let g = GasGasCharge.StaticCost (List.length clauses) in
              let%bind clauses' =
                mapM clauses ~f:(fun (p, stmts) ->
                    let%bind stmts' = stmts_cost stmts in
                    pure @@ (p, stmts'))
              in
              let s' = MatchStmt (x, clauses') in
              pure @@ [ (GasStmt g, srep); (s', srep) ]
          | AcceptPayment ->
              let g = GasStmt (GasGasCharge.StaticCost 1) in
              pure @@ [ (g, srep); (s, srep) ]
          | Iterate (l, _) ->
              let g = GasStmt (GasGasCharge.LengthOf (GI.get_id l)) in
              pure @@ [ (g, srep); (s, srep) ]
          | Throw eopt ->
              let g =
                match eopt with
                | Some e -> GasGasCharge.SizeOf (GI.get_id e)
                | None -> GasGasCharge.StaticCost 1
              in
              pure @@ [ (GasStmt g, srep); (s, srep) ]
          | GasStmt _ -> fail0 ~kind:"Unexpected gas charge" ?inst:None
        in

        let%bind rem_stmts' = stmts_cost rem_stmts in
        pure (s' @ rem_stmts')

  let lib_entry_cost = function
    | LibVar (v, topt, e) ->
        let%bind e' = expr_static_cost e in
        pure @@ LibVar (v, topt, e')
    | LibTyp _ as le -> pure le

  let lib_cost lib =
    let%bind lentries' = mapM lib.lentries ~f:lib_entry_cost in
    pure { lib with lentries = lentries' }

  let rec libtree_cost ltree =
    let%bind deps' = mapM ltree.deps ~f:libtree_cost in
    let%bind libn' = lib_cost ltree.libn in
    pure { libn = libn'; deps = deps' }

  let lmod_cost lmod =
    let%bind libs' = lib_cost lmod.libs in
    pure @@ { lmod with libs = libs' }

  let cmod_cost (cmod : cmodule) =
    let contr_cost contr =
      let comp_cost comp =
        let%bind body' = stmts_cost comp.comp_body in
        pure { comp with comp_body = body' }
      in
      let%bind constraint' = expr_static_cost contr.cconstraint in
      let%bind cfields' =
        mapM contr.cfields ~f:(fun (i, t, e) ->
            let%bind e' = expr_static_cost e in
            pure (i, t, e'))
      in
      let%bind ccomps' = mapM contr.ccomps ~f:comp_cost in
      pure
        {
          contr with
          cconstraint = constraint';
          cfields = cfields';
          ccomps = ccomps';
        }
    in
    let%bind libs' = option_mapM cmod.libs ~f:lib_cost in
    let%bind contr' = contr_cost cmod.contr in
    pure { cmod with libs = libs'; contr = contr' }

  (* A signature for functions that determine dynamic cost of built-in ops. *)
  (* op -> arguments -> base cost -> total cost *)
  type coster =
    builtin ->
    GasType.t list ->
    (* type arguments *)
    ER.rep GI.t list ->
    (* argument identifiers *)
    GasType.t list ->
    (* types of value arguments *)
    (GasGasCharge.gas_charge, scilla_error list) result

  (* op, arg types, coster, base cost. *)
  type builtin_record = builtin * GasType.t list * coster

  let string_coster op _targs args _arg_types =
    match (op, args) with
    | Builtin_eq, [ s1; s2 ] ->
        pure
        @@ GasGasCharge.MinOf
             ( GasGasCharge.SizeOf (GI.get_id s1),
               GasGasCharge.SizeOf (GI.get_id s2) )
    | Builtin_concat, [ s1; s2 ] ->
        pure
        @@ GasGasCharge.SumOf
             ( GasGasCharge.SizeOf (GI.get_id s1),
               GasGasCharge.SizeOf (GI.get_id s2) )
    | Builtin_substr, [ s; i1; i2 ] ->
        pure
        @@ GasGasCharge.MinOf
             ( GasGasCharge.SizeOf (GI.get_id s),
               GasGasCharge.SumOf
                 ( GasGasCharge.ValueOf (GI.get_id i1),
                   GasGasCharge.ValueOf (GI.get_id i2) ) )
    | Builtin_strlen, [ s ] -> pure @@ GasGasCharge.SizeOf (GI.get_id s)
    | Builtin_strrev, [ s ] -> pure @@ GasGasCharge.SizeOf (GI.get_id s)
    | Builtin_to_string, [ l ] | Builtin_to_ascii, [ l ] ->
        pure @@ GasGasCharge.SizeOf (GI.get_id l)
    | _ -> fail0 ~kind:"Gas cost error for string built-in" ?inst:None

  let crypto_coster op _targs args types =
    match (op, types, args) with
    | Builtin_eq, [ PrimType Bystr_typ; PrimType Bystr_typ ], [ a1; _ ] ->
        pure @@ GasGasCharge.SizeOf (GI.get_id a1)
    | ( Builtin_substr,
        [
          PrimType Bystr_typ;
          PrimType (Uint_typ Bits32);
          PrimType (Uint_typ Bits32);
        ],
        [ s; i1; i2 ] ) ->
        pure
        @@ GasGasCharge.MinOf
             ( GasGasCharge.SizeOf (GI.get_id s),
               GasGasCharge.SumOf
                 ( GasGasCharge.ValueOf (GI.get_id i1),
                   GasGasCharge.ValueOf (GI.get_id i2) ) )
    | Builtin_eq, [ a1; a2 ], _
      when is_bystrx_type a1 && is_bystrx_type a2
           && Option.(value_exn (bystrx_width a1) = value_exn (bystrx_width a2))
      ->
        let width = Option.value_exn (bystrx_width a1) in
        pure @@ GasGasCharge.StaticCost width
    | Builtin_to_uint32, [ a ], _
      when is_bystrx_type a && Option.value_exn (bystrx_width a) <= 4 ->
        pure @@ GasGasCharge.StaticCost 4
    | Builtin_to_uint64, [ a ], _
      when is_bystrx_type a && Option.value_exn (bystrx_width a) <= 8 ->
        pure @@ GasGasCharge.StaticCost 8
    | Builtin_to_uint128, [ a ], _
      when is_bystrx_type a && Option.value_exn (bystrx_width a) <= 16 ->
        pure @@ GasGasCharge.StaticCost 16
    | Builtin_to_uint256, [ a ], _
      when is_bystrx_type a && Option.value_exn (bystrx_width a) <= 32 ->
        pure @@ GasGasCharge.StaticCost 32
    | Builtin_sha256hash, _, [ a ]
    | Builtin_schnorr_get_address, _, [ a ]
    | Builtin_ecdsa_recover_pk, _, a :: _ ->
        (* Block size of sha256hash is 512 *)
        let s =
          GasGasCharge.SumOf
            ( GasGasCharge.SizeOf (GI.get_id a),
              GasGasCharge.MapSortCost (GI.get_id a) )
        in
        let%bind n = GasCharge.PositiveInt.create (64 * 15) in
        pure (GasGasCharge.DivCeil (s, n))
    | Builtin_keccak256hash, _, [ a ] ->
        (* Block size of keccak256hash is 1088 *)
        let s =
          GasGasCharge.SumOf
            ( GasGasCharge.SizeOf (GI.get_id a),
              GasGasCharge.MapSortCost (GI.get_id a) )
        in
        let%bind n = GasCharge.PositiveInt.create (136 * 15) in
        pure (GasGasCharge.DivCeil (s, n))
    | Builtin_ripemd160hash, _, [ a ] ->
        (* Block size of ripemd160hash is 512 *)
        let s =
          GasGasCharge.SumOf
            ( GasGasCharge.SizeOf (GI.get_id a),
              GasGasCharge.MapSortCost (GI.get_id a) )
        in
        let%bind n = GasCharge.PositiveInt.create (64 * 10) in
        pure (GasGasCharge.DivCeil (s, n))
    | Builtin_schnorr_verify, _, [ _; s; _ ]
    | Builtin_ecdsa_verify, _, [ _; s; _ ] ->
        (* x = div_ceil (Bystr.width s + 66) 64 *)
        let%bind divisor = GasCharge.PositiveInt.create 64 in
        let x =
          GasGasCharge.DivCeil
            ( GasGasCharge.SumOf
                (GasGasCharge.SizeOf (GI.get_id s), GasGasCharge.StaticCost 66),
              divisor )
        in
        (* (250 + (15 * x)) *)
        pure
          (GasGasCharge.SumOf
             ( GasGasCharge.StaticCost 250,
               GasGasCharge.ProdOf (GasGasCharge.StaticCost 15, x) ))
    | Builtin_to_bystr, [ a ], _ when is_bystrx_type a ->
        pure (GasGasCharge.StaticCost (Option.value_exn (bystrx_width a)))
    | Builtin_to_bystrx i, [ PrimType Bystr_typ ], _
    | Builtin_to_bystrx i, [ PrimType (Uint_typ _) ], _ ->
        pure (GasGasCharge.StaticCost i)
    | Builtin_bech32_to_bystr20, _, [ prefix; addr ]
    | Builtin_bystr20_to_bech32, _, [ prefix; addr ] ->
        let base = 4 in
        pure
          (GasGasCharge.ProdOf
             ( GasGasCharge.SumOf
                 ( GasGasCharge.SizeOf (GI.get_id prefix),
                   GasGasCharge.SizeOf (GI.get_id addr) ),
               GasGasCharge.StaticCost base ))
    | Builtin_concat, [ PrimType Bystr_typ; PrimType Bystr_typ ], [ s1; s2 ] ->
        pure
        @@ GasGasCharge.SumOf
             ( GasGasCharge.SizeOf (GI.get_id s1),
               GasGasCharge.SizeOf (GI.get_id s2) )
    | Builtin_concat, [ a1; a2 ], _ when is_bystrx_type a1 && is_bystrx_type a2
      ->
        pure
          (GasGasCharge.StaticCost
             Option.(value_exn (bystrx_width a1) + value_exn (bystrx_width a2)))
    | Builtin_alt_bn128_G1_add, _, _ -> pure (GasGasCharge.StaticCost 20)
    | Builtin_alt_bn128_G1_mul, _, [ _; s ] ->
        let multiplier =
          GasGasCharge.LogOf (GasGasCharge.ValueOf (GI.get_id s))
        in
        pure @@ GasGasCharge.ProdOf (GasGasCharge.StaticCost 20, multiplier)
    | Builtin_alt_bn128_G1_neg, _, _ -> pure (GasGasCharge.StaticCost 20)
    | Builtin_alt_bn128_pairing_product, _, [ pairs ] ->
        let list_len = GasGasCharge.LengthOf (GI.get_id pairs) in
        pure (GasGasCharge.ProdOf (GasGasCharge.StaticCost 40, list_len))
    | _ -> fail0 ~kind:"Gas cost error for hash built-in" ?inst:None

  let map_coster op _targs args _arg_types =
    match args with
    | m :: _ -> (
        (* size, get and contains do not make a copy of the Map, hence constant. *)
        match op with
        | Builtin_size | Builtin_get | Builtin_contains ->
            pure (GasGasCharge.StaticCost 1)
        | Builtin_to_list ->
            let len = GasGasCharge.LengthOf (GI.get_id m) in
            pure @@ GasGasCharge.ProdOf (len, GasGasCharge.LogOf len)
        | _ ->
            pure
              (GasGasCharge.SumOf
                 (GasGasCharge.StaticCost 1, GasGasCharge.LengthOf (GI.get_id m)))
        )
    | _ -> fail0 ~kind:"Gas cost error for map built-in" ?inst:None

  let to_nat_coster _ _targs args _arg_types =
    match args with
    | [ a ] -> pure (GasGasCharge.ValueOf (GI.get_id a))
    | _ -> fail0 ~kind:"Gas cost error for to_nat built-in" ?inst:None

  let int_conversion_coster w _ _targs _args arg_types =
    let base = 4 in
    match arg_types with
    | [ PrimType (Uint_typ _) ]
    | [ PrimType (Int_typ _) ]
    | [ PrimType String_typ ] ->
        if w = 32 || w = 64 then pure (GasGasCharge.StaticCost base)
        else if w = 128 then pure (GasGasCharge.StaticCost (base * 2))
        else if w = 256 then pure (GasGasCharge.StaticCost (base * 4))
        else fail0 ~kind:"Gas cost error for integer conversion" ?inst:None
    | _ ->
        fail0 ~kind:"Gas cost due to incorrect arguments for int conversion"
          ?inst:None

  let int_coster op _targs args arg_types =
    let base = 4 in
    let%bind base' =
      match op with
      | Builtin_mul | Builtin_div | Builtin_rem ->
          pure (GasGasCharge.StaticCost (5 * base))
      | Builtin_pow -> (
          match args with
          | [ _; p ] ->
              pure
                (GasGasCharge.ProdOf
                   ( GasGasCharge.StaticCost (base * 5),
                     GasGasCharge.ValueOf (GI.get_id p) ))
          | _ -> fail0 ~kind:"Gas cost error for built-in pow" ?inst:None)
      | Builtin_isqrt -> (
          match args with
          | [ a ] ->
              pure
                (GasGasCharge.ProdOf
                   ( GasGasCharge.StaticCost base,
                     GasGasCharge.LogOf (GasGasCharge.ValueOf (GI.get_id a)) ))
          | _ -> fail0 ~kind:"Invalid argument type to isqrt" ?inst:None)
      | _ -> pure (GasGasCharge.StaticCost base)
    in
    let%bind w =
      match arg_types with
      | a :: _ -> (
          match int_width a with
          | Some w -> pure w
          | None ->
              fail0 ~kind:"int_coster: cannot determine integer width"
                ?inst:None)
      | _ -> fail0 ~kind:"Gas cost error for integer built-in" ?inst:None
    in
    if w = 32 || w = 64 then pure base'
    else if w = 128 then
      pure (GasGasCharge.ProdOf (base', GasGasCharge.StaticCost 2))
    else if w = 256 then
      pure (GasGasCharge.ProdOf (base', GasGasCharge.StaticCost 4))
    else fail0 ~kind:"Gas cost error for integer built-in" ?inst:None

  let bnum_coster _op _targs _args _arg_types =
    pure (GasGasCharge.StaticCost 32)

  let tvar s = TypeVar s

  [@@@ocamlformat "disable"]

  (* built-in op costs are propotional to size of data they operate on. *)
  let builtin_records_find = function
    (* Polymorhpic *)
    | Builtin_eq -> [
      ([string_typ;string_typ], string_coster);
      ([bnum_typ;bnum_typ], bnum_coster);
      ([tvar "'A"; tvar "'A"], crypto_coster);
      ([tvar "'A"; tvar "'A"], int_coster)
    ];
    | Builtin_substr -> [
      ([string_typ; tvar "'A"; tvar "'A"], string_coster);
      ([bystr_typ; uint32_typ; uint32_typ], crypto_coster)
    ];
    | Builtin_concat -> [
      ([string_typ;string_typ], string_coster);
      ([tvar "'A"; tvar "'A"], crypto_coster)
    ];
    | Builtin_strrev -> [ ([tvar "'A"], string_coster) ]
  
    (* Strings *)
    | Builtin_strlen -> [([string_typ], string_coster); ([bystr_typ], string_coster);];
    | Builtin_to_string -> [([tvar "'A"], string_coster)];
    | Builtin_to_ascii -> [([tvar "'A"], string_coster)];
  
    (* Block numbers *)
    | Builtin_blt -> [([bnum_typ;bnum_typ], bnum_coster)];
    | Builtin_badd -> [([bnum_typ;tvar "'A"], bnum_coster)];
    | Builtin_bsub -> [([bnum_typ;bnum_typ], bnum_coster)];
  
    (* Crypto *)
    | Builtin_to_bystr -> [([tvar "'A"], crypto_coster)];
    | Builtin_to_bystrx _ -> [([tvar "'A"], crypto_coster)];
    | Builtin_bech32_to_bystr20 -> [([string_typ;string_typ], crypto_coster)];
    | Builtin_bystr20_to_bech32 -> [([string_typ;bystrx_typ Type.address_length], crypto_coster)];
    | Builtin_sha256hash -> [([tvar "'A"], crypto_coster)];
    | Builtin_keccak256hash -> [([tvar "'A"], crypto_coster)];
    | Builtin_ripemd160hash -> [([tvar "'A"], crypto_coster)];
    | Builtin_schnorr_verify -> [([bystrx_typ pubkey_len; bystr_typ; bystrx_typ signature_len], crypto_coster)];
    | Builtin_ecdsa_verify -> [([bystrx_typ Secp256k1Wrapper.pubkey_len; bystr_typ; bystrx_typ Secp256k1Wrapper.signature_len], crypto_coster)];
    | Builtin_ecdsa_recover_pk -> [([bystr_typ; bystrx_typ Secp256k1Wrapper.signature_len; uint32_typ], crypto_coster)];
    | Builtin_schnorr_get_address -> [([bystrx_typ pubkey_len], crypto_coster)];
    | Builtin_alt_bn128_G1_add -> [([g1point_type; g1point_type], crypto_coster)];
    | Builtin_alt_bn128_G1_mul -> [([g1point_type; scalar_type], crypto_coster)];
    | Builtin_alt_bn128_G1_neg -> [([g1point_type ], crypto_coster)];
    | Builtin_alt_bn128_pairing_product -> [([g1g2pair_list_type], crypto_coster)];
  
    (* Maps *)
    | Builtin_contains -> [([tvar "'A"; tvar "'A"], map_coster)];
    | Builtin_put -> [([tvar "'A"; tvar "'A"; tvar "'A"], map_coster)];
    | Builtin_get -> [([tvar "'A"; tvar "'A"], map_coster)];
    | Builtin_remove -> [([tvar "'A"; tvar "'A"], map_coster)];
    | Builtin_to_list -> [([tvar "'A"], map_coster)];
    | Builtin_size -> [([tvar "'A"], map_coster)];
  
    (* Integers *)
    | Builtin_lt -> [([tvar "'A"; tvar "'A"], int_coster)];
    | Builtin_add -> [([tvar "'A"; tvar "'A"], int_coster)];
    | Builtin_sub -> [([tvar "'A"; tvar "'A"], int_coster)];
    | Builtin_mul -> [([tvar "'A"; tvar "'A"], int_coster)];
    | Builtin_div -> [([tvar "'A"; tvar "'A"], int_coster)];
    | Builtin_rem -> [([tvar "'A"; tvar "'A"], int_coster)];
    | Builtin_pow -> [([tvar "'A"; uint32_typ], int_coster)];
    | Builtin_isqrt -> [([tvar "'A"], int_coster)];
  
    | Builtin_to_int32 -> [([tvar "'A"], int_conversion_coster 32)];
    | Builtin_to_int64 -> [([tvar "'A"], int_conversion_coster 64)];
    | Builtin_to_int128 -> [([tvar "'A"], int_conversion_coster 128)];
    | Builtin_to_int256 -> [([tvar "'A"], int_conversion_coster 256)];
    | Builtin_to_uint32 -> [
      ([tvar "'A"], crypto_coster); ([tvar "'A"], int_conversion_coster 32)
    ];
    | Builtin_to_uint64 -> [
      ([tvar "'A"], crypto_coster); ([tvar "'A"], int_conversion_coster 64)
    ];
    | Builtin_to_uint128 -> [
      ([tvar "'A"], crypto_coster); ([tvar "'A"], int_conversion_coster 128)
    ];
    | Builtin_to_uint256 -> [
      ([tvar "'A"], crypto_coster); ([tvar "'A"], int_conversion_coster 256)
    ];
  
    | Builtin_to_nat -> [([uint32_typ], to_nat_coster)];

  [@@@ocamlformat "enable"]

  let builtin_cost (op, _) ~targ_types ~arg_types arg_ids =
    let matcher (types, fcoster) =
      (* The names and type list lengths must match and *)
      if
        List.length types = List.length arg_types
        && List.for_all2_exn
             ~f:(fun expected actual ->
               (* the types should match *)
               type_assignable ~expected ~actual
               ||
               (* or the built-in record is generic *)
               match expected with TypeVar _ -> true | _ -> false)
             types arg_types
      then fcoster op targ_types arg_ids arg_types (* this can fail too *)
      else fail0 ~kind:"Name or arity doesn't match" ?inst:None
    in
    let%bind _, cost =
      tryM (builtin_records_find op) ~f:matcher ~msg:(fun () ->
          mk_error0 ~kind:"Unable to determine gas cost for builtin"
            ~inst:(pp_builtin op))
    in
    pure cost
end
