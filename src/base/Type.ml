(*
  This file is part of scilla.

  Copyright (c) 2020 - present Zilliqa Research Pvt. Ltd.
  
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
open! Int.Replace_polymorphic_compare
open Sexplib.Std
open ErrorUtils
open Identifier

(*******************************************************)
(*                         Types                       *)
(*******************************************************)

type int_bit_width = Bits32 | Bits64 | Bits128 | Bits256
[@@deriving sexp, equal]

type prim_typ =
  | Int_typ of int_bit_width
  | Uint_typ of int_bit_width
  | String_typ
  | Bnum_typ
  | Msg_typ
  | Event_typ
  | Exception_typ
  | Bystr_typ
  | Bystrx_typ of int
[@@deriving equal]

let sexp_of_prim_typ = function
  | Int_typ Bits32 -> Sexp.Atom "Int32"
  | Int_typ Bits64 -> Sexp.Atom "Int64"
  | Int_typ Bits128 -> Sexp.Atom "Int128"
  | Int_typ Bits256 -> Sexp.Atom "Int256"
  | Uint_typ Bits32 -> Sexp.Atom "Uint32"
  | Uint_typ Bits64 -> Sexp.Atom "Uint64"
  | Uint_typ Bits128 -> Sexp.Atom "Uint128"
  | Uint_typ Bits256 -> Sexp.Atom "Uint256"
  | String_typ -> Sexp.Atom "String"
  | Bnum_typ -> Sexp.Atom "BNum"
  | Msg_typ -> Sexp.Atom "Message"
  | Event_typ -> Sexp.Atom "Event"
  | Exception_typ -> Sexp.Atom "Exception"
  | Bystr_typ -> Sexp.Atom "ByStr"
  | Bystrx_typ b -> Sexp.Atom ("ByStr" ^ Int.to_string b)

let prim_typ_of_sexp _ = failwith "prim_typ_of_sexp is not implemented"

type t =
  | PrimType of prim_typ
  | MapType of t * t
  | FunType of t * t
  | ADT of loc Identifier.t * t list
  | TypeVar of string
  | PolyFun of string * t
  | Unit
[@@deriving sexp]

let int_bit_width_to_string = function
  | Bits32 -> "32"
  | Bits64 -> "64"
  | Bits128 -> "128"
  | Bits256 -> "256"

let pp_prim_typ = function
  | Int_typ bw -> "Int" ^ int_bit_width_to_string bw
  | Uint_typ bw -> "Uint" ^ int_bit_width_to_string bw
  | String_typ -> "String"
  | Bnum_typ -> "BNum"
  | Msg_typ -> "Message"
  | Event_typ -> "Event"
  | Exception_typ -> "Exception"
  | Bystr_typ -> "ByStr"
  | Bystrx_typ b -> "ByStr" ^ Int.to_string b

let rec pp_typ = function
  | PrimType t -> pp_prim_typ t
  | MapType (kt, vt) -> sprintf "Map (%s) (%s)" (pp_typ kt) (pp_typ vt)
  | ADT (name, targs) ->
      let elems =
        get_id name :: List.map targs ~f:(fun t -> sprintf "(%s)" (pp_typ t))
      in
      String.concat ~sep:" " elems
  | FunType (at, vt) -> sprintf "%s -> %s" (with_paren at) (pp_typ vt)
  | TypeVar tv -> tv
  | PolyFun (tv, bt) -> sprintf "forall %s. %s" tv (pp_typ bt)
  | Unit -> sprintf "()"

and with_paren t =
  match t with
  | FunType _ | PolyFun _ -> sprintf "(%s)" (pp_typ t)
  | _ -> pp_typ t

(****************************************************************)
(*                     Type substitutions                       *)
(****************************************************************)

(* Return free tvars in tp
    The return list doesn't contain duplicates *)
let free_tvars tp =
  let add vs tv = tv :: List.filter ~f:(String.( <> ) tv) vs in
  let rem vs tv = List.filter ~f:(String.( <> ) tv) vs in
  let rec go t acc =
    match t with
    | PrimType _ | Unit -> acc
    | MapType (kt, vt) -> go kt acc |> go vt
    | FunType (at, rt) -> go at acc |> go rt
    | TypeVar n -> add acc n
    | ADT (_, ts) -> List.fold_left ts ~init:acc ~f:(Fn.flip go)
    | PolyFun (arg, bt) ->
        let acc' = go bt acc in
        rem acc' arg
  in
  go tp []

let mk_fresh_var taken init =
  let tmp = ref init in
  let counter = ref 1 in
  while List.mem taken !tmp ~equal:String.( = ) do
    tmp := init ^ Int.to_string !counter;
    Int.incr counter
  done;
  !tmp

(* tm[tvar := tp] *)
let rec subst_type_in_type tvar tp tm =
  match tm with
  | PrimType _ | Unit -> tm
  (* Make sure the map's type is still primitive! *)
  | MapType (kt, vt) ->
      let kts = subst_type_in_type tvar tp kt in
      let vts = subst_type_in_type tvar tp vt in
      MapType (kts, vts)
  | FunType (at, rt) ->
      let ats = subst_type_in_type tvar tp at in
      let rts = subst_type_in_type tvar tp rt in
      FunType (ats, rts)
  | TypeVar n -> if String.(tvar = n) then tp else tm
  | ADT (s, ts) ->
      let ts' = List.map ts ~f:(subst_type_in_type tvar tp) in
      ADT (s, ts')
  | PolyFun (arg, t) ->
      if String.(tvar = arg) then tm
      else PolyFun (arg, subst_type_in_type tvar tp t)

(* note: this is sequential substitution of multiple variables,
          _not_ simultaneous substitution *)
let subst_types_in_type sbst tm =
  List.fold_left sbst ~init:tm ~f:(fun acc (tvar, tp) ->
      subst_type_in_type tvar tp acc)

let rename_bound_vars mk_new_name update_taken =
  let rec recursor t taken =
    match t with
    | MapType (kt, vt) -> MapType (kt, recursor vt taken)
    | FunType (at, rt) -> FunType (recursor at taken, recursor rt taken)
    | ADT (n, ts) ->
        let ts' = List.map ts ~f:(fun w -> recursor w taken) in
        ADT (n, ts')
    | PrimType _ | TypeVar _ | Unit -> t
    | PolyFun (arg, bt) ->
        let arg' = mk_new_name taken arg in
        let tv_new = TypeVar arg' in
        let bt1 = subst_type_in_type arg tv_new bt in
        let bt2 = recursor bt1 (update_taken arg' taken) in
        PolyFun (arg', bt2)
  in
  recursor

let refresh_tfun = rename_bound_vars mk_fresh_var List.cons

let canonicalize_tfun t =
  (* The parser doesn't allow type names to begin with '_'. *)
  let mk_new_name counter _ = "'_A" ^ Int.to_string counter in
  rename_bound_vars mk_new_name (const @@ Int.succ) t 1

(* Type equivalence *)
let equal t1 t2 =
  let t1' = canonicalize_tfun t1 in
  let t2' = canonicalize_tfun t2 in
  let rec equiv t1 t2 =
    match (t1, t2) with
    | PrimType p1, PrimType p2 -> [%equal: prim_typ] p1 p2
    | TypeVar v1, TypeVar v2 -> String.equal v1 v2
    | Unit, Unit -> true
    | ADT (tname1, tl1), ADT (tname2, tl2) ->
        equal_id tname1 tname2
        (* Cannot call type_equiv_list because we don't want to canonicalize_tfun again. *)
        && List.length tl1 = List.length tl2
        && List.for_all2_exn ~f:equiv tl1 tl2
    | MapType (t1_1, t1_2), MapType (t2_1, t2_2)
    | FunType (t1_1, t1_2), FunType (t2_1, t2_2) ->
        equiv t1_1 t2_1 && equiv t1_2 t2_2
    | PolyFun (v1, t1''), PolyFun (v2, t2'') ->
        String.equal v1 v2 && equiv t1'' t2''
    | _ -> false
  in
  equiv t1' t2'

(* The same as above, but for a variable with locations *)
let subst_type_in_type' tv = subst_type_in_type (get_id tv)

(****************************************************************)
(*                     PrimType utilities                       *)
(****************************************************************)

let int32_typ = PrimType (Int_typ Bits32)

let int64_typ = PrimType (Int_typ Bits64)

let int128_typ = PrimType (Int_typ Bits128)

let int256_typ = PrimType (Int_typ Bits256)

let uint32_typ = PrimType (Uint_typ Bits32)

let uint64_typ = PrimType (Uint_typ Bits64)

let uint128_typ = PrimType (Uint_typ Bits128)

let uint256_typ = PrimType (Uint_typ Bits256)

let string_typ = PrimType String_typ

let bnum_typ = PrimType Bnum_typ

let msg_typ = PrimType Msg_typ

let event_typ = PrimType Event_typ

let exception_typ = PrimType Exception_typ

let bystr_typ = PrimType Bystr_typ

let bystrx_typ b = PrimType (Bystrx_typ b)

let int_width = function
  | PrimType (Int_typ Bits32) | PrimType (Uint_typ Bits32) -> Some 32
  | PrimType (Int_typ Bits64) | PrimType (Uint_typ Bits64) -> Some 64
  | PrimType (Int_typ Bits128) | PrimType (Uint_typ Bits128) -> Some 128
  | PrimType (Int_typ Bits256) | PrimType (Uint_typ Bits256) -> Some 256
  | _ -> None

(* Given a ByStrX string, return integer X *)
let bystrx_width = function PrimType (Bystrx_typ w) -> Some w | _ -> None

let is_prim_type = function PrimType _ -> true | _ -> false

let is_int_type = function PrimType (Int_typ _) -> true | _ -> false

let is_uint_type = function PrimType (Uint_typ _) -> true | _ -> false

let is_bystrx_type = function PrimType (Bystrx_typ _) -> true | _ -> false
