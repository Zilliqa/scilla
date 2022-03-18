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
open Sexplib.Std
open! Int.Replace_polymorphic_compare
open ErrorUtils
open Identifier

let address_length = 20

module PrimType = struct
  type int_bit_width = Bits32 | Bits64 | Bits128 | Bits256
  [@@deriving sexp, equal, compare]

  type t =
    | Int_typ of int_bit_width
    | Uint_typ of int_bit_width
    | String_typ
    | Bnum_typ
    | Msg_typ
    | Event_typ
    | Exception_typ
    | ReplicateContr_typ
    | Bystr_typ
    | Bystrx_typ of int
  [@@deriving equal, sexp, compare]

  let sexp_of_t = function
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
    | ReplicateContr_typ -> Sexp.Atom "ReplicateContr"
    | Bystr_typ -> Sexp.Atom "ByStr"
    | Bystrx_typ b -> Sexp.Atom ("ByStr" ^ Int.to_string b)

  let t_of_sexp _ = failwith "prim_typ_of_sexp is not implemented"

  let int_bit_width_to_string = function
    | Bits32 -> "32"
    | Bits64 -> "64"
    | Bits128 -> "128"
    | Bits256 -> "256"

  let int_bit_width_to_int = function
    | Bits32 -> 32
    | Bits64 -> 64
    | Bits128 -> 128
    | Bits256 -> 256

  let pp_prim_typ = function
    | Int_typ bw -> "Int" ^ int_bit_width_to_string bw
    | Uint_typ bw -> "Uint" ^ int_bit_width_to_string bw
    | String_typ -> "String"
    | Bnum_typ -> "BNum"
    | Msg_typ -> "Message"
    | Event_typ -> "Event"
    | Exception_typ -> "Exception"
    | ReplicateContr_typ -> "ReplicateContr"
    | Bystr_typ -> "ByStr"
    | Bystrx_typ b -> "ByStr" ^ Int.to_string b
end

module TIdentifier_Loc (TIdentifier : ScillaIdentifier) = struct
  type t = loc TIdentifier.t

  let compare (a : t) (b : t) = TIdentifier.compare a b

  let equal (a : t) (b : t) = TIdentifier.equal a b

  let sexp_of_t = TIdentifier.sexp_of_t (fun l -> sexp_of_loc l)

  let t_of_sexp = TIdentifier.t_of_sexp (fun s -> loc_of_sexp s)
end

module IdLoc_Comp (TIdentifier : ScillaIdentifier) = struct
  module T = struct
    include TIdentifier_Loc (TIdentifier)
  end

  include T
  include Comparable.Make (T)
end

module type ScillaType = sig
  module TIdentifier : ScillaIdentifier

  module IdLoc_Comp : module type of IdLoc_Comp (TIdentifier)

  (* The types of addresses we care about.
   * Lattice:
        AnyAddr
           |
        CodeAddr
          / \
    LibAddr ContrAddr
   *)
  type 'a addr_kind =
    (* Any address in use. *)
    | AnyAddr
    (* Address containing a library or contract. *)
    | CodeAddr
    (* Address containing a library. *)
    | LibAddr
    (* Address containing a contract. *)
    | ContrAddr of 'a IdLoc_Comp.Map.t
  [@@deriving sexp]

  type t =
    | PrimType of PrimType.t
    | MapType of t * t
    | FunType of t * t
    | ADT of loc TIdentifier.t * t list
    | TypeVar of string
    | PolyFun of string * t
    | Unit
    | Address of t addr_kind
  [@@deriving sexp]

  val pp_typ : t -> string

  val pp_typ_error : t -> string

  (****************************************************************)
  (*                     Type substitutions                       *)
  (****************************************************************)

  val free_tvars : t -> string list

  val mk_fresh_var : string list -> string -> string

  val refresh_tfun : t -> string list -> t

  val canonicalize_tfun : t -> t

  val equal : t -> t -> bool

  val type_equivalent : t -> t -> bool

  val type_assignable : expected:t -> actual:t -> bool

  val subst_type_in_type : string -> t -> t -> t

  val subst_types_in_type : (string * t) list -> t -> t

  val subst_type_in_type' : 'a TIdentifier.t -> t -> t -> t

  (****************************************************************)
  (*                     PrimType utilities                       *)
  (****************************************************************)

  val is_prim_type : t -> bool

  val is_address_type : t -> bool

  val is_int_type : t -> bool

  val is_uint_type : t -> bool

  val is_bystrx_type : t -> bool

  val int_width : t -> int option

  val int32_typ : t

  val int64_typ : t

  val int128_typ : t

  val int256_typ : t

  val uint32_typ : t

  val uint64_typ : t

  val uint128_typ : t

  val uint256_typ : t

  val string_typ : t

  val bnum_typ : t

  val msg_typ : t

  val event_typ : t

  val exception_typ : t

  val replicate_contr_typ : t

  val bystr_typ : t

  val bystrx_typ : int -> t

  (* Given a ByStrX, return integer X *)
  val bystrx_width : t -> int option

  val address_typ : t addr_kind -> t
end

module MkType (I : ScillaIdentifier) = struct
  module TIdentifier = I

  (*******************************************************)
  (*                         Types                       *)
  (*******************************************************)
  module IdLoc_Comp = IdLoc_Comp (TIdentifier)

  (* The types of addresses we care about.
   * Lattice:
        AnyAddr
           |
        CodeAddr
          / \
    LibAddr ContrAddr
   *)
  type 'a addr_kind =
    (* Any address in use. *)
    | AnyAddr
    (* Address containing a library or contract. *)
    | CodeAddr
    (* Address containing a library. *)
    | LibAddr
    (* Address containing a contract. *)
    | ContrAddr of 'a IdLoc_Comp.Map.t
  [@@deriving sexp]

  type t =
    | PrimType of PrimType.t
    | MapType of t * t
    | FunType of t * t
    | ADT of loc TIdentifier.t * t list
    | TypeVar of string
    | PolyFun of string * t
    | Unit
    | Address of t addr_kind
  [@@deriving sexp]

  let pp_typ_helper is_error t =
    let rec recurser = function
      | PrimType t -> PrimType.pp_prim_typ t
      | MapType (kt, vt) -> sprintf "Map (%s) (%s)" (recurser kt) (recurser vt)
      | ADT (name, targs) ->
          let elems =
            (if is_error then TIdentifier.as_error_string name
            else TIdentifier.as_string name)
            :: List.map targs ~f:(fun t -> sprintf "(%s)" (recurser t))
          in
          String.concat ~sep:" " elems
      | FunType (at, vt) -> sprintf "%s -> %s" (with_paren at) (recurser vt)
      | TypeVar tv -> tv
      | PolyFun (tv, bt) -> sprintf "forall %s. %s" tv (recurser bt)
      | Unit -> sprintf "()"
      | Address AnyAddr -> "ByStr20 with end"
      | Address CodeAddr -> "ByStr20 with _codehash end"
      | Address LibAddr -> "ByStr20 with library end"
      | Address (ContrAddr fts) ->
          let elems =
            List.map (IdLoc_Comp.Map.to_alist fts) ~f:(fun (f, t) ->
                sprintf "field %s : %s"
                  (if is_error then TIdentifier.as_error_string f
                  else TIdentifier.as_string f)
                  (recurser t))
            |> String.concat ~sep:", "
          in
          sprintf "ByStr20 with contract %s%send" elems
            (if IdLoc_Comp.Map.is_empty fts then "" else " ")
    and with_paren t =
      match t with
      | FunType _ | PolyFun _ -> sprintf "(%s)" (recurser t)
      | _ -> recurser t
    in
    recurser t

  let pp_typ = pp_typ_helper false

  let pp_typ_error = pp_typ_helper true

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
      | Address AnyAddr | Address LibAddr | Address CodeAddr -> acc
      | Address (ContrAddr fts) ->
          IdLoc_Comp.Map.fold fts ~init:acc ~f:(fun ~key:_ ~data acc ->
              go data acc)
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
    | Address AnyAddr | Address LibAddr | Address CodeAddr -> tm
    | Address (ContrAddr fts) ->
        Address
          (ContrAddr (IdLoc_Comp.Map.map fts ~f:(subst_type_in_type tvar tp)))

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
      | Address AnyAddr | Address LibAddr | Address CodeAddr -> t
      | Address (ContrAddr fts) ->
          Address
            (ContrAddr (IdLoc_Comp.Map.map fts ~f:(fun t -> recursor t taken)))
    in
    recursor

  let refresh_tfun = rename_bound_vars mk_fresh_var List.cons

  let canonicalize_tfun t =
    (* The parser doesn't allow type names to begin with '_'. *)
    let mk_new_name counter _ = "'_A" ^ Int.to_string counter in
    rename_bound_vars mk_new_name (const @@ Int.succ) t 1

  (* Type equality - assumes that the types have been canonicalised first. *)
  let equal t1 t2 =
    let rec equiv t1 t2 =
      match (t1, t2) with
      | PrimType p1, PrimType p2 -> [%equal: PrimType.t] p1 p2
      | TypeVar v1, TypeVar v2 -> String.equal v1 v2
      | Unit, Unit -> true
      | ADT (tname1, tl1), ADT (tname2, tl2) ->
          TIdentifier.equal tname1 tname2
          (* Cannot call type_equiv_list because we don't want to canonicalize_tfun again. *)
          && List.length tl1 = List.length tl2
          && List.for_all2_exn ~f:equiv tl1 tl2
      | MapType (t1_1, t1_2), MapType (t2_1, t2_2)
      | FunType (t1_1, t1_2), FunType (t2_1, t2_2) ->
          equiv t1_1 t2_1 && equiv t1_2 t2_2
      | PolyFun (v1, t1''), PolyFun (v2, t2'') ->
          String.equal v1 v2 && equiv t1'' t2''
      | Address AnyAddr, Address AnyAddr
      | Address LibAddr, Address LibAddr
      | Address CodeAddr, Address CodeAddr ->
          true
      | Address (ContrAddr fts1), Address (ContrAddr fts2) ->
          IdLoc_Comp.Map.equal equiv fts1 fts2
      | _ -> false
    in
    equiv t1 t2

  (* Type equivalence *)
  let type_equivalent t1 t2 =
    let t1' = canonicalize_tfun t1 in
    let t2' = canonicalize_tfun t2 in
    equal t1' t2'

  let type_assignable ~expected ~actual =
    let to_typ' = canonicalize_tfun expected in
    let from_typ' = canonicalize_tfun actual in
    let rec assignable to_typ from_typ =
      match (to_typ, from_typ) with
      | Address AnyAddr, Address _ ->
          (* Any address is assignable to an address in use *)
          true
      | Address LibAddr, Address LibAddr -> true
      | Address CodeAddr, Address CodeAddr
      | Address CodeAddr, Address LibAddr
      | Address CodeAddr, Address (ContrAddr _) ->
          (* Any address containing code, library or contract is a code address. *)
          true
      | Address (ContrAddr tfts), Address (ContrAddr ffts) ->
          (* Check that tfts is a subset of ffts, and that types are assignable/equivalent. *)
          IdLoc_Comp.Map.for_alli tfts ~f:(fun ~key:tf ~data:tft ->
              match IdLoc_Comp.Map.find ffts tf with
              | None ->
                  (* to field does not appear in from type *)
                  false
              | Some fft ->
                  (* Matching field name. Types must be assignable. *)
                  assignable tft fft)
      | PrimType (Bystrx_typ len), Address _ when len = address_length ->
          (* Any address is assignable to ByStr20. *)
          true
      | MapType (kt1, vt1), MapType (kt2, vt2) ->
          assignable kt1 kt2 && assignable vt1 vt2
      | FunType (at1, vt1), FunType (at2, vt2) ->
          (* Contravariant in argument type! *)
          assignable at2 at1 && assignable vt1 vt2
      | ADT (n1, tlist1), ADT (n2, tlist2) -> (
          TIdentifier.equal n1 n2
          &&
          (* We can assume that type parameters only occur in covariant positions *)
          match List.for_all2 tlist1 tlist2 ~f:assignable with
          | Ok res -> res
          | Unequal_lengths -> false)
      | PolyFun (targ1, vt1), PolyFun (targ2, vt2) ->
          equal (TypeVar targ1) (TypeVar targ2) && assignable vt1 vt2
      | _, _ ->
          (* PrimType, Unit and TypeVar require equality up to canonicalisation. *)
          equal to_typ from_typ
    in
    assignable to_typ' from_typ'

  (* The same as above, but for a variable with locations *)
  let subst_type_in_type' tv = subst_type_in_type (TIdentifier.as_string tv)

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

  let replicate_contr_typ = PrimType ReplicateContr_typ

  let bystr_typ = PrimType Bystr_typ

  let bystrx_typ b = PrimType (Bystrx_typ b)

  let int_width = function
    | PrimType (Int_typ bits) | PrimType (Uint_typ bits) ->
        Some (PrimType.int_bit_width_to_int bits)
    | _ -> None

  (* Given a ByStrX string, return integer X *)
  let bystrx_width = function PrimType (Bystrx_typ w) -> Some w | _ -> None

  let address_typ fts = Address fts

  let is_prim_type = function PrimType _ -> true | _ -> false

  let is_address_type = function Address _ -> true | _ -> false

  let is_int_type = function PrimType (Int_typ _) -> true | _ -> false

  let is_uint_type = function PrimType (Uint_typ _) -> true | _ -> false

  let is_bystrx_type = function PrimType (Bystrx_typ _) -> true | _ -> false
end
