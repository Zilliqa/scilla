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
open Sexplib.Std
open MonadUtil
open ErrorUtils
open Stdint

exception SyntaxError of string * loc

(* Version of the interpreter (major, minor, patch) *)
let scilla_version = (0, 3, 0)

type 'rep ident =
  | Ident of string * 'rep
[@@deriving sexp]

let asId i = Ident (i, dummy_loc)
let asIdL i loc = Ident(i, loc)

let get_id i = match i with Ident (x, _) -> x
let get_rep i = match i with Ident (_, l) -> l

type bigint = Big_int.big_int

let mk_ident s = Ident (s, dummy_loc)

(* A few utilities on id. *)
let equal_id a b = get_id a = get_id b
let compare_id a b = compare (get_id a) (get_id b)
let dedup_id_list l = List.dedup_and_sort ~compare:compare_id l
let is_mem_id i l = List.exists l ~f:(equal_id i)

(*******************************************************)
(*                         Types                       *)
(*******************************************************)

type int_bit_width =
  | Bits32
  | Bits64
  | Bits128
  | Bits256
[@@deriving sexp]

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

type typ =
  | PrimType of prim_typ
  | MapType of typ * typ
  | FunType of typ * typ
  | ADT of string * typ list
  | TypeVar of string
  | PolyFun of string * typ
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
  | MapType (kt, vt) ->
      sprintf "Map (%s) (%s)" (pp_typ kt) (pp_typ vt )
  | ADT (name, targs) ->
      let elems = name :: (List.map targs
          ~f:(fun t -> sprintf "(%s)" (pp_typ t)))
      in
      String.concat ~sep:" " elems
  | FunType (at, vt) -> sprintf "%s -> %s" (with_paren at) (pp_typ vt)
  | TypeVar tv -> tv
  | PolyFun (tv, bt) -> sprintf "forall %s. %s" tv (pp_typ bt)
  | Unit -> sprintf "()"
and with_paren t = match t with
  | FunType _ | PolyFun _ -> sprintf "(%s)" (pp_typ t)
  | _ -> pp_typ t

(*******************************************************)
(*                      Literals                       *)
(*******************************************************)

(* The first component is a primitive type *)
type mtype = typ * typ
[@@deriving sexp]
let pp_mtype (kt, vt) = pp_typ (MapType(kt, vt))

let address_length = 20
let hash_length = 32

type int_lit =
  | Int32L of int32 | Int64L of int64 | Int128L of int128 | Int256L of Integer256.int256

let sexp_of_int_lit = function
  | Int32L i' -> Sexp.Atom ("Int32 " ^ Int32.to_string i')
  | Int64L i' -> Sexp.Atom ("Int64 " ^ Int64.to_string i')
  | Int128L i' -> Sexp.Atom ("Int128 " ^ Int128.to_string i')
  | Int256L i' -> Sexp.Atom ("Int256 " ^ Integer256.Int256.to_string i')

let int_lit_of_sexp _ = failwith "int_lit_of_sexp is not implemented"

type uint_lit =
  | Uint32L of uint32 | Uint64L of uint64 | Uint128L of uint128 | Uint256L of Integer256.uint256

let sexp_of_uint_lit = function
  | Uint32L i' -> Sexp.Atom ("Uint32 " ^ Uint32.to_string i')
  | Uint64L i' -> Sexp.Atom ("Uint64 " ^ Uint64.to_string i')
  | Uint128L i' -> Sexp.Atom ("Uint128 " ^ Uint128.to_string i')
  | Uint256L i' -> Sexp.Atom ("Uint256 " ^ Integer256.Uint256.to_string i')

let uint_lit_of_sexp _ = failwith "uint_lit_of_sexp is not implemented"

module type BYSTR = sig
  type t [@@deriving sexp]
  val width : t -> int
  val parse_hex : string -> t
  val hex_encoding : t -> string
  val to_raw_bytes : t -> string
  val of_raw_bytes : int -> string -> t option
  val equal : t -> t -> bool
  val concat : t -> t -> t
end

module Bystr : BYSTR = struct
  type t = string [@@deriving sexp]

  let width = String.length

  let parse_hex s =
    if not (String.equal (String.prefix s 2) "0x") then
      raise @@ Invalid_argument "hex conversion: 0x prefix is missing"
    else
      let s_nopref = String.drop_prefix s 2 in
      if String.length s_nopref = 0 then
        raise @@ Invalid_argument "hex conversion: empty byte sequence"
      else
        Hex.to_string (`Hex s_nopref)

  let hex_encoding bs = "0x" ^ Hex.show @@ Hex.of_string bs

  let to_raw_bytes = Fn.id

  let of_raw_bytes expected_width raw =
    Option.some_if (String.length raw = expected_width) raw

  let equal = String.equal

  let concat = (^)
end

module type BYSTRX = sig
  type t [@@deriving sexp]
  val width : t -> int
  val parse_hex : string -> t
  val hex_encoding : t -> string
  val to_raw_bytes : t -> string
  val of_raw_bytes : int -> string -> t option
  val equal : t -> t -> bool
  val concat : t -> t -> t
  val to_bystr : t -> Bystr.t
end

module Bystrx : BYSTRX = struct
  include Bystr
  let to_bystr = Fn.id
end

(* [Specialising the Return Type of Closures]

   The syntax for literals implements a _shallow embedding_ of
   closures and type abstractions (cf. constructors `Clo` and `TAbs`).
   Since our computations are all in CPS (cf. [Evaluation in CPS]), so
   should be the computations, encapsulated by those two forms.
   However, for the time being, we want to keep the type `literal`
   non-parametric. This is at odds with the priniciple of keeping
   computations in CPS parametric in their result type.

   Therefore, for now we have a compromise of fixing the result of
   evaluating expressions to be as below. In order to restore the
   genericity enabled by CPS, we provide an "impedance matcher",
   described in [Continuation for Expression Evaluation]. 

*)
type literal =
  | StringLit of string
  (* Cannot have different integer literals here directly as Stdint does not derive sexp. *)
  | IntLit of int_lit
  | UintLit of uint_lit
  | BNum of string
  (* Byte string with a statically known length. *)
  | ByStrX of Bystrx.t
  (* Byte string without a statically known length. *)
  | ByStr of Bystr.t
  (* Message: an associative array *)
  | Msg of (string * literal) list
  (* A dynamic map of literals *)
  | Map of mtype * (literal, literal) Hashtbl.t
  (* A constructor in HNF *)
  | ADTValue of string * typ list * literal list
  (* An embedded closure *)
  | Clo of (literal -> 
            (literal, scilla_error list, 
             uint64 -> ((literal * (string * literal) list) * uint64, scilla_error list * uint64) result) 
              CPSMonad.t)
  (* A type abstraction *)
  | TAbs of (typ -> 
             (literal, scilla_error list, 
              uint64 -> ((literal * (string * literal) list) * uint64, scilla_error list * uint64) result) 
               CPSMonad.t)
[@@deriving sexp]


(* Builtins *)
type builtin =
  | Builtin_eq
  | Builtin_concat
  | Builtin_substr
  | Builtin_strlen
  | Builtin_to_string
  | Builtin_blt
  | Builtin_badd
  | Builtin_bsub
  | Builtin_to_uint256
  | Builtin_sha256hash
  | Builtin_keccak256hash
  | Builtin_ripemd160hash
  | Builtin_to_bystr
  | Builtin_bech32_to_bystr20
  | Builtin_bystr20_to_bech32
  | Builtin_schnorr_verify
  | Builtin_ecdsa_verify
  (* https://github.com/Zilliqa/scilla/pull/486#discussion_r266069221 *)
  (*
  | Builtin_ec_gen_key_pair (* in gas coster only *)
  | Builtin_schnorr_gen_key_pair (* in cashflow checker only *)
  | Builtin_schnorr_sign (* in cashflow checker only *)
  | Builtin_ecdsa_sign (* in gas coster only *)
  *)
  | Builtin_contains
  | Builtin_put
  | Builtin_get
  | Builtin_remove
  | Builtin_to_list
  | Builtin_size
  | Builtin_lt
  | Builtin_add
  | Builtin_sub
  | Builtin_mul
  | Builtin_div
  | Builtin_rem
  | Builtin_pow
  | Builtin_to_int32
  | Builtin_to_int64
  | Builtin_to_int128
  | Builtin_to_int256
  | Builtin_to_uint32
  | Builtin_to_uint64
  | Builtin_to_uint128
  | Builtin_to_nat
  | Builtin_schnorr_get_address
[@@deriving sexp]

type 'rep builtin_annot = builtin * 'rep
[@@deriving sexp]

let pp_builtin b = match b with
  | Builtin_eq -> "eq"
  | Builtin_concat -> "concat"
  | Builtin_substr -> "substr"
  | Builtin_strlen -> "strlen"
  | Builtin_to_string -> "to_string"
  | Builtin_blt -> "blt"
  | Builtin_badd -> "badd"
  | Builtin_bsub -> "bsub"
  | Builtin_to_uint256 -> "to_uint256"
  | Builtin_sha256hash -> "sha256hash"
  | Builtin_keccak256hash -> "keccak256hash"
  | Builtin_ripemd160hash -> "ripemd160hash"
  | Builtin_to_bystr -> "to_bystr"
  | Builtin_bech32_to_bystr20 -> "bech32_to_bystr20"
  | Builtin_bystr20_to_bech32 -> "bystr20_to_bech32"
  | Builtin_schnorr_verify -> "schnorr_verify"
  | Builtin_ecdsa_verify -> "ecdsa_verify"
  | Builtin_schnorr_get_address -> "schnorr_get_address"
  | Builtin_contains -> "contains"
  | Builtin_put -> "put"
  | Builtin_get -> "get"
  | Builtin_remove -> "remove"
  | Builtin_to_list -> "to_list"
  | Builtin_size -> "size"
  | Builtin_lt -> "lt"
  | Builtin_add -> "add"
  | Builtin_sub -> "sub"
  | Builtin_mul -> "mul"
  | Builtin_div -> "div"
  | Builtin_rem -> "rem"
  | Builtin_pow -> "pow"
  | Builtin_to_int32 -> "to_int32"
  | Builtin_to_int64 -> "to_int64"
  | Builtin_to_int128 -> "to_int128"
  | Builtin_to_int256 -> "to_int256"
  | Builtin_to_uint32 -> "to_uint32"
  | Builtin_to_uint64 -> "to_uint64"
  | Builtin_to_uint128 -> "to_uint128"
  | Builtin_to_nat -> "to_nat"

let parse_builtin s loc = match s with
  | "eq" -> Builtin_eq
  | "concat" -> Builtin_concat
  | "substr" -> Builtin_substr
  | "strlen" -> Builtin_strlen
  | "to_string" -> Builtin_to_string
  | "blt" -> Builtin_blt
  | "badd" -> Builtin_badd
  | "bsub" -> Builtin_bsub
  | "to_uint256" -> Builtin_to_uint256
  | "sha256hash" -> Builtin_sha256hash
  | "keccak256hash" -> Builtin_keccak256hash
  | "ripemd160hash" -> Builtin_ripemd160hash
  | "to_bystr" -> Builtin_to_bystr
  | "bech32_to_bystr20" -> Builtin_bech32_to_bystr20
  | "bystr20_to_bech32" -> Builtin_bystr20_to_bech32
  | "schnorr_verify" -> Builtin_schnorr_verify
  | "ecdsa_verify" -> Builtin_ecdsa_verify
  | "schnorr_get_address" -> Builtin_schnorr_get_address
  | "contains" -> Builtin_contains
  | "put" -> Builtin_put
  | "get" -> Builtin_get
  | "remove" -> Builtin_remove
  | "to_list" -> Builtin_to_list
  | "size" -> Builtin_size
  | "lt" -> Builtin_lt
  | "add" -> Builtin_add
  | "sub" -> Builtin_sub
  | "mul" -> Builtin_mul
  | "div" -> Builtin_div
  | "rem" -> Builtin_rem
  | "pow" -> Builtin_pow
  | "to_int32" -> Builtin_to_int32
  | "to_int64" -> Builtin_to_int64
  | "to_int128" -> Builtin_to_int128
  | "to_int256" -> Builtin_to_int256
  | "to_uint32" -> Builtin_to_uint32
  | "to_uint64" -> Builtin_to_uint64
  | "to_uint128" -> Builtin_to_uint128
  | "to_nat" -> Builtin_to_nat
  | _ -> raise (SyntaxError ((sprintf "\"%s\" is not a builtin" s), loc))

(****************************************************************)
(*         Type substitutions on unannotated syntax             *)
(****************************************************************)

(* Return free tvars in tp
    The return list doesn't contain duplicates *)
let free_tvars tp =
  let add vs tv = tv :: List.filter ~f:((<>) tv) vs in
  let rem vs tv = List.filter ~f:((<>) tv) vs in
  let rec go t acc = (match t with
      | PrimType _ | Unit -> acc
      | MapType (kt, vt) -> go kt acc |> go vt
      | FunType (at, rt) -> go at acc |> go rt
      | TypeVar n -> add acc n
      | ADT (_, ts) ->
          List.fold_left ts ~init:acc ~f:(Fn.flip go)
      | PolyFun (arg, bt) ->
          let acc' = go bt acc in
          rem acc' arg) in
  go tp []

let mk_fresh_var taken init =
  let tmp = ref init in
  let counter = ref 1 in
  while List.mem taken !tmp ~equal:(=) do
    tmp := init ^ (Int.to_string !counter);
    Int.incr counter
  done;
  !tmp


(* tm[tvar := tp] *)
let rec subst_type_in_type tvar tp tm = match tm with
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
  | TypeVar n ->
      if tvar = n then tp else tm
  | ADT (s, ts) ->
      let ts' = List.map ts ~f:(subst_type_in_type tvar tp) in
      ADT (s, ts')
  | PolyFun (arg, t) ->
      if tvar = arg then tm
      else PolyFun (arg, subst_type_in_type tvar tp t)

(* note: this is sequential substitution of multiple variables,
          _not_ simultaneous substitution *)
let subst_types_in_type sbst tm =
  List.fold_left sbst ~init:tm
    ~f:(fun acc (tvar, tp) -> subst_type_in_type tvar tp acc)

let rename_bound_vars mk_new_name update_taken =
  let rec recursor t taken = match t with
    | MapType (kt, vt) -> MapType (kt, recursor vt taken)
    | FunType (at, rt) ->
        FunType (recursor at taken, recursor rt taken)
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
  in recursor

let refresh_tfun = rename_bound_vars mk_fresh_var List.cons

let canonicalize_tfun t =
  (* The parser doesn't allow type names to begin with '_'. *)
  let mk_new_name counter _ = "'_A" ^ Int.to_string counter in
  rename_bound_vars mk_new_name (const @@ Int.succ) t 1

(* The same as above, but for a variable with locations *)
let subst_type_in_type' tv = subst_type_in_type (get_id tv)

let rec subst_type_in_literal tvar tp l = match l with
  | Map ((kt, vt), ls) ->
      let kts = subst_type_in_type' tvar tp kt in
      let vts = subst_type_in_type' tvar tp vt in
      let ls' = Hashtbl.create (Hashtbl.length ls) in
      let _ = Hashtbl.iter (fun k v  ->
          let k' = subst_type_in_literal tvar tp k in
          let v' = subst_type_in_literal tvar tp v in
          Hashtbl.add ls' k' v') ls in
      Map ((kts, vts), ls')
  | ADTValue (n, ts, ls) ->
      let ts' = List.map ts ~f:(subst_type_in_type' tvar tp) in
      let ls' = List.map ls ~f:(subst_type_in_literal tvar tp) in
      ADTValue (n, ts', ls')
  | _ -> l


(*******************************************************)
(*               Types of components                   *)
(*******************************************************)

type component_type =
  | CompTrans
  | CompProc

let component_type_to_string ctp =
  match ctp with
  | CompTrans -> "transition"
  | CompProc -> "procedure"
  
(*******************************************************)
(*                   Annotations                       *)
(*******************************************************)

module type Rep = sig
  type rep
  val get_loc : rep -> loc

  val mk_id_address : string -> rep ident
  val mk_id_uint128 : string -> rep ident
  val mk_id_uint32  : string -> rep ident
  val mk_id_bnum    : string -> rep ident
  val mk_id_string  : string -> rep ident

  val rep_of_sexp : Sexp.t -> rep
  val sexp_of_rep : rep -> Sexp.t
  
  (* TODO, Issue #179: These functions are only used in TypeCache.ml. 
     See if they can be eliminated somehow *)
  val parse_rep : string -> rep
  val get_rep_str: rep -> string
end

(*******************************************************)
(*          Annotated scilla syntax                    *)
(*******************************************************)

module ScillaSyntax (SR : Rep) (ER : Rep) = struct

(*******************************************************)
(*                   Expressions                       *)
(*******************************************************)

  type payload =
    | MLit of literal
    | MVar of ER.rep ident
  [@@deriving sexp]

  type pattern =
    | Wildcard
    | Binder of ER.rep ident
    | Constructor of string * (pattern list)
  [@@deriving sexp]

  type expr_annot = expr * ER.rep
  and expr =
    | Literal of literal
    | Var of ER.rep ident
    | Let of ER.rep ident * typ option * expr_annot * expr_annot
    | Message of (string * payload) list
    | Fun of ER.rep ident * typ * expr_annot
    | App of ER.rep ident * ER.rep ident list
    | Constr of string * typ list * ER.rep ident list
    | MatchExpr of ER.rep ident * (pattern * expr_annot) list
    | Builtin of ER.rep builtin_annot * ER.rep ident list
    (* Advanced features: to be added in Scilla 0.2 *)                 
    | TFun of ER.rep ident * expr_annot
    | TApp of ER.rep ident * typ list
    (* Fixpoint combinator: used to implement recursion principles *)                 
    | Fixpoint of ER.rep ident * typ * expr_annot
  [@@deriving sexp]

  let expr_rep erep = snd erep
      
  let expr_loc erep =
    let l = ER.get_loc (expr_rep erep) in
    Option.some_if (l.cnum <> -1) l

  (* SExp printing for Expr for structural printing. *)
  let spp_expr e =
    sexp_of_expr e |> Sexplib.Sexp.to_string

  let pp_expr e = spp_expr e

  (*******************************************************)
  (*                   Statements                        *)
  (*******************************************************)

  type stmt_annot = stmt * SR.rep
  and stmt =
    | Load of ER.rep ident * ER.rep ident
    | Store of ER.rep ident * ER.rep ident
    | Bind of ER.rep ident * expr_annot
    (* m[k1][k2][..] := v OR delete m[k1][k2][...] *)
    | MapUpdate of ER.rep ident * (ER.rep ident list) * ER.rep ident option
    (* v <- m[k1][k2][...] OR b <- exists m[k1][k2][...] *)
    (* If the bool is set, then we interpret this as value retrieve, 
       otherwise as an "exists" query. *)
    | MapGet of ER.rep ident * ER.rep ident * (ER.rep ident list) * bool
    | MatchStmt of ER.rep ident * (pattern * stmt_annot list) list
    | ReadFromBC of ER.rep ident * string
    | AcceptPayment
    | SendMsgs of ER.rep ident
    | CreateEvnt of ER.rep ident
    | CallProc of SR.rep ident * ER.rep ident list
    | Throw of (ER.rep ident) option
  [@@deriving sexp]
  
  let stmt_rep srep = snd srep
  
  let stmt_loc s = SR.get_loc (stmt_rep s)

  let spp_stmt s =
    sexp_of_stmt s |> Sexplib.Sexp.to_string

  let pp_stmt s =
    spp_stmt s

  (**************************************************)
  (*          Statement evaluation info             *)
  (**************************************************)
  type stmt_eval_context =
    (* literal being loaded *)
    | G_Load of literal
    (* literal being stored *)
    | G_Store of literal
    (* none *)
    | G_Bind
    (* nesting depth, new value *)
    | G_MapUpdate of int * literal option
    (* nesting depth, literal retrieved *)
    | G_MapGet of int * literal option
    (* number of clauses *)
    | G_MatchStmt of int
    | G_ReadFromBC
    | G_AcceptPayment
    | G_SendMsgs of literal list
    | G_CreateEvnt of literal
    | G_CallProc

  (*******************************************************)
  (*                    Contracts                        *)
  (*******************************************************)

  type component =
    { comp_type   : component_type;
      comp_name   : SR.rep ident;
      comp_params : (ER.rep ident * typ) list;
      comp_body   : stmt_annot list }
  
  type ctr_def =
    { cname : ER.rep ident; c_arg_types : typ list }
  
  type lib_entry =
    | LibVar of ER.rep ident * typ option * expr_annot
    | LibTyp of ER.rep ident * ctr_def list

  type library =
    { lname : SR.rep ident;
      lentries : lib_entry list }
  
  type contract =
    { cname   : SR.rep ident;
      cparams : (ER.rep ident  * typ) list;
      cfields : (ER.rep ident * typ * expr_annot) list;
      ccomps  : component list; }

  (* Contract module: libary + contract definiton *)
  type cmodule =
    { smver : int;                (* Scilla major version of the contract. *)
      cname : SR.rep ident;
      libs  : library option;     (* lib functions defined in the module *)
      (* List of imports / external libs with an optional namespace. *)
      elibs : (SR.rep ident * SR.rep ident option) list;
      contr : contract }

  (* Library module *)
  type lmodule =
    { 
      (* List of imports / external libs with an optional namespace. *)
      elibs : (SR.rep ident * SR.rep ident option) list;
      libs : library; (* lib functions defined in the module *)
    }

  (* A tree of libraries linked to their dependents *)
  type libtree =
    { 
      libn : library;      (* The library this node represents *)
      deps : libtree list  (* List of dependent libraries *)
    }

  let pp_cparams ps =
    let cs = List.map ps ~f:(fun (i, t) ->
        get_id i ^ " : " ^
        (sexp_of_typ t |> Sexplib.Sexp.to_string)) in
    "[" ^ (String.concat ~sep:", " cs) ^ "]"

  (* Substitute type for a type variable *)
  let rec subst_type_in_expr tvar tp erep =
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

  (* get variables that get bound in pattern. *)
  let get_pattern_bounds p =
    let rec accfunc p acc =
      match p with
      | Wildcard -> acc
      | Binder i -> i::acc
      | Constructor (_, plist) ->
          List.fold plist ~init:acc ~f:(fun acc p' -> accfunc p' acc)
    in accfunc p []

  (* Returns a list of free variables in expr. *)
  let free_vars_in_expr erep =
    (* get elements in "l" that are not in bound_vars. *)
    let get_free l bound_vars =
      List.filter l ~f:(fun i -> not (is_mem_id i bound_vars)) in
  
    (* The main function that does the job. *)
    let rec recurser erep bound_vars acc =
      let (e, _) = erep in
      match e with
      | Literal _ -> acc
      | Var v | TApp (v, _) -> if is_mem_id v bound_vars then acc else v :: acc
      | Fun (f, _, body) | Fixpoint (f, _, body) -> recurser body (f :: bound_vars) acc
      | TFun (_, body) -> recurser body bound_vars acc
      | Constr (_, _, es) -> (get_free es bound_vars) @ acc
      | App (f, args) -> (get_free (f :: args) bound_vars) @ acc
      | Builtin (_f, args) -> (get_free args bound_vars) @ acc
      | Let (i, _, lhs, rhs) ->
        let acc_lhs = recurser lhs bound_vars acc in
        recurser rhs (i::bound_vars) acc_lhs
      | Message margs ->
        List.fold margs ~init:acc ~f:(fun acc (_, x) ->
          (match x with
           | MLit _ -> acc
           | MVar v ->  if is_mem_id v bound_vars then acc else v :: acc)
        )
      | MatchExpr (v, cs) ->
        let fv = if is_mem_id v bound_vars then acc else v::acc in
        List.fold cs ~init:fv ~f: (fun acc (p, e) ->
          (* bind variables in pattern and recurse for expression. *)
          let bound_vars' = (get_pattern_bounds p) @ bound_vars in
          recurser e bound_vars' acc
        )
    in
    let fvs = recurser erep [] [] in
    dedup_id_list fvs

(* Is expr dependent on any ident in blist.
 * This is the same as checking if a free var
 * in expr is present in blist. *)
  let free_vars_dep_check erep blist =
    (* Utility: is any m in ml, in l. *)
    let any_is_mem ml l =
      List.exists ml ~f:(fun i -> is_mem_id i l) in
    (* Get list of free variables in expression *)
    let fvs = free_vars_in_expr erep in
    (* and check if any of them are in blist. *)
    any_is_mem fvs blist

  (****************************************************************)
  (*                  Better error reporting                      *)
  (****************************************************************)
  let get_failure_msg erep phase opt =
    let (e, rep) = erep in
    let sloc = ER.get_loc rep in
    (match e with
    | Literal _ ->
        sprintf "Type error in literal. %s\n"
           phase
    | Var i ->
        sprintf "Type error in variable `%s`:\n"
           (get_id i)
    | Let (i, _, _, _) ->
        sprintf "Type error in the initialiser of `%s`:\n"
           (get_id i)
    | Message _ ->
        sprintf "Type error in message.\n"
          
    | Fun _ ->
        sprintf "Type error in function:\n"
          
    | App (f, _) ->
        sprintf "Type error in application of `%s`:\n"
           (get_id f)
    | Constr (s, _, _) ->
        sprintf "Type error in constructor `%s`:\n"
           s
    | MatchExpr (x, _) ->
        sprintf
          "Type error in pattern matching on `%s`%s (or one of its branches):\n"
           (get_id x) opt 
    | Builtin ((i, _), _) ->
        sprintf "Type error in built-in application of `%s`:\n"
           (pp_builtin i)
    | TApp (tf, _) ->
        sprintf "Type error in type application of `%s`:\n"
           (get_id tf)
    | TFun (tf, _) ->
        sprintf "Type error in type function `%s`:\n"
           (get_id tf)
    | Fixpoint (f, _, _) ->
        sprintf "Type error in fixpoint application with an argument `%s`:\n"
          (get_id f)
    ), sloc

  let get_failure_msg_stmt srep phase opt =
    let (s, rep) = srep in
    let sloc = SR.get_loc rep in
    (match s with
    | Load (x, f) ->
        sprintf "Type error in reading value of `%s` into `%s`:\n %s"
           (get_id f) (get_id x) phase
    | Store (f, r) ->
        sprintf "Type error in storing value of `%s` into the field `%s`:\n"
           (get_id r) (get_id f)
    | Bind (x, _) ->
        sprintf "Type error in the binding to into `%s`:\n"
           (get_id x)
    | MapGet (_, m, keys, _) ->
        (sprintf "Type error in getting map value %s" (get_id m)) ^
        (List.fold keys ~init:"" ~f:(fun acc k -> acc ^ "[" ^ (get_id k) ^ "]")) ^ "\n"
    | MapUpdate (m, keys, _) ->
        (sprintf "Type error in updating map %s" (get_id m)) ^
        (List.fold keys ~init:"" ~f:(fun acc k -> acc ^ "[" ^ (get_id k) ^ "]")) ^ "\n"
    | MatchStmt (x, _) ->
        sprintf
          "Type error in pattern matching on `%s`%s (or one of its branches):\n"
           (get_id x) opt 
    | ReadFromBC (x, _) ->
        sprintf "Error in reading from blockchain state into `%s`:\n"
           (get_id x)
    | AcceptPayment ->
        sprintf "Error in accepting payment\n"
    | SendMsgs i ->
        sprintf "Error in sending messages `%s`:\n"
           (get_id i)
    | CreateEvnt i ->
        sprintf "Error in create event `%s`:\n"
           (get_id i)
    | CallProc (p, _) ->
        sprintf "Error in call of procedure '%s':\n"
           (get_id p)
    | Throw i ->
        let is = match i with | Some id -> "of '" ^ (get_id id) ^ "'" | None -> "" in
        sprintf "Error in throw %s:\n" is
    ), sloc

  let wrap_with_info (msg, sloc) res = match res with
    | Ok _ -> res
    | Error e -> Error ({emsg = msg; startl = sloc; endl = dummy_loc}::e)

  let wrap_err e phase ?opt:(opt = "") res =
    match res with
    | Ok _ -> res
    (* Handle a special case where we're dealing with the most precise error. *)
    | Error (e' :: []) ->
      let m, l = get_failure_msg e phase opt in
      if e'.startl = dummy_loc then
        Error (mk_error1 (m ^ e'.emsg) l)
      else
        Error (mk_error2 (m ^ e'.emsg) e'.startl e'.endl)
    | _ -> wrap_with_info (get_failure_msg e phase opt) res

  let wrap_serr s phase ?opt:(opt = "") res =
    match res with
    | Ok _ -> res
      (* Handle a special case where we're dealing with the most precise error. *)
    | Error (e' :: []) ->
      let m, l = get_failure_msg_stmt s phase opt in
      if e'.startl = dummy_loc then
        Error (mk_error1 (m ^ e'.emsg) l)
      else
        Error (mk_error2 (m ^ e'.emsg) e'.startl e'.endl)
    | _ ->
      wrap_with_info (get_failure_msg_stmt s phase opt) res
  
end

