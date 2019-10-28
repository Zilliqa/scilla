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
open ErrorUtils
open PrettyPrinters
open ParserUtil
open DebugMessage
open RunnerUtil
open PatternChecker
open PrimTypes
open SanityChecker
open GasUseAnalysis
open EventInfo
open Cashflow
open Accept

module ParsedSyntax = ParserUtil.ParsedSyntax
module PSRep = ParserRep
module PERep = ParserRep

module Rec = Recursion.ScillaRecursion (PSRep) (PERep)
module RecSRep = Rec.OutputSRep
module RecERep = Rec.OutputERep

module TC = TypeChecker.ScillaTypechecker (RecSRep) (RecERep)
module TCSRep = TC.OutputSRep
module TCERep = TC.OutputERep

module PMC = ScillaPatternchecker (TCSRep) (TCERep)
module PMCSRep = PMC.SPR
module PMCERep = PMC.EPR

module SC = ScillaSanityChecker (TCSRep) (TCERep)
module EI = ScillaEventInfo (PMCSRep) (PMCERep)

module GUA = ScillaGUA (TCSRep) (TCERep)
module CF = ScillaCashflowChecker (TCSRep) (TCERep)
module AC = ScillaAcceptChecker (TCSRep) (TCERep)

(* Check that the module parses *)
let check_parsing ctr syn = 
  let cmod = FrontEndParser.parse_file syn ctr in
  if Result.is_ok cmod then
    plog @@ sprintf "\n[Parsing]:\n module [%s] is successfully parsed.\n" ctr;
  cmod

let loc_to_json (l : loc) =
  `Assoc [
    ("source_file", `String l.fname);
    ("line", `Int l.lnum);
    ("column", `Int l.cnum);
    ("node_type", `String "Location")
  ]

let ident_to_json (i: loc ident) =
    `Assoc [ 
        ("loc", (loc_to_json (get_rep i))) ;
        ("identifier", `String (get_id i));
        ("node_type", `String "Identifier")
    ] 

let rec type_to_json = function
    | PrimType t ->
        `Assoc [("node_type", `String "PrimType"); ("name", `String (pp_prim_typ t))]
    | MapType (kt, vt) ->
        `Assoc [("node_type", `String "MapType"); ("key_type", type_to_json kt); ("val_type", type_to_json vt)]
    | ADT (name, targs) ->
      let targs_json = List.map targs ~f:type_to_json in
      `Assoc [("node_type", `String "ADT"); ("name", `String name); ("type_args",  `List targs_json)]
    | FunType (at, vt) ->
        `Assoc [("node_type", `String "FunType"); ("arg_type", type_to_json at); ("val_type", type_to_json vt)]
    | TypeVar tv ->
        `Assoc [("node_type", `String "TypeVar"); ("name", `String tv)]
    | PolyFun (tv, bt) ->
        `Assoc [("node_type", `String "PolyFun"); ("type_val", `String tv); ("body_type", type_to_json bt)]
    | Unit ->
        `Assoc [("node_type", `String "UnitType");]

let rec pattern_to_json (p: ParsedSyntax.pattern) =
    match p with
    | Wildcard ->
        `Assoc [("node_type", `String "WildcardPattern");]
    | Binder (i) ->
        `Assoc [("node_type", `String "BinderPattern"); ("variable", (ident_to_json i));]
    (*of string * (pattern list)*)
    | Constructor (s, p)  ->
        let patterns_json = `List (List.map p ~f:pattern_to_json) in
        `Assoc [("node_type", `String "ConstructorPattern"); ("constructor_name", `String s); ("patterns", patterns_json);]

let builtin_annot_to_json (b, l) = 
    let node_type_json = ("node_type", `String "Builtin") in
    let l_json = ("loc", loc_to_json l) in
    let b_json = ("builtin_operator", `String (pp_builtin b)) in
    `Assoc [l_json; b_json; node_type_json]

let literal_to_json lit =
  match lit with
  | StringLit v->
    let node_type_json = ("node_type", `String "StringLiteral") in
    let vj = ("value", `String v) in
    `Assoc [node_type_json; vj]
  | BNum v ->
    let node_type_json = ("node_type", `String "BNumLiteral") in
    let vj = ("value", `String v) in
    `Assoc [node_type_json; vj]
  | ByStr bs ->
    let node_type_json = ("node_type", `String "ByStrLiteral") in
    let vj = ("value",  `String (Bystr.hex_encoding bs)) in
    `Assoc [node_type_json; vj]
  | ByStrX bs ->
    let node_type_json = ("node_type", `String "ByStrXLiteral") in
    let vj = ("value", `String (Bystrx.hex_encoding bs)) in
    `Assoc [node_type_json; vj]
  | IntLit x  ->
    let node_type_json = ("node_type", `String "IntLiteral") in
    let vj = ("value", `String (string_of_int_lit x)) in
    let wj = ("width", `Int (int_lit_width x) ) in
    `Assoc [node_type_json; vj; wj]
  | UintLit x ->
    let node_type_json = ("node_type", `String "UintLiteral") in
    let vj = ("value", `String (string_of_uint_lit x) ) in
    let wj = ("width", `Int (uint_lit_width x) ) in
    `Assoc [node_type_json; vj; wj]
  (*| Map of mtype * (literal, literal) Hashtbl.t*)
  | Map ((kt, vt), kvs) ->
    let ktj = ("key_type", type_to_json kt) in
    let vtj = ("val_type", type_to_json vt) in
    let node_type_json = ("node_type", `String "MapLiteral") in
    let vj = ("mvalues", `List (mapvalues_to_json kvs)) in
    `Assoc [ktj; vtj; node_type_json; vj]
  | ADTValue (n, t, v) as ls ->
    let node_type_json = ("node_type", `String "ADTValueLiteral") in
    let open Datatypes in
    let (a, _) = lookup_constructor_exn n in
     if a.tname = "List"
    then
      (* We make an exception for Lists and print them as a JSON array. *)
      (match Datatypes.scilla_list_to_ocaml_rev ls with
      | Ok ls' -> 
        let ls'' = List.rev_map ls' ~f:(fun a -> literal_to_json a) in
        `Assoc [node_type_json; ( "value", `List ls'')]
      | Error emsg -> raise (Utils.InternalError emsg))
    else
      let argtl = adttyps_to_json t in
      let argl = adtargs_to_json v in
        `Assoc [node_type_json; ( "value",
        `Assoc [
          ("constructor", `String n);
          ("argtypes", `List argtl);
          ("arguments", `List argl)
        ])]
  | _ -> `Null


let payload_to_json p =
    match p with
    | ParsedSyntax.MLit l ->
        `Assoc [("node_type", `String "PayloadLiteral"); ("literal", literal_to_json l)]
    | ParsedSyntax.MVar i ->
        `Assoc [("node_type", `String "PayloadVariable"); ("value", ident_to_json i)]


let rec expr_to_json (e : ParsedSyntax.expr) = 
    match e with
    | Literal l ->
            [("node_type", `String "LiteralExpression"); ("value", literal_to_json l)]
    | Var n ->
        let nj = ("variable", ident_to_json n) in
        [("node_type", `String "VarExpression"); nj]
    | Let (v, t, e, b) ->
        let vj = ("variable", ident_to_json v) in
        let tj =
            match t with
                | Some t' -> ("variable_type", type_to_json t')
                | None -> ("variable_type", `Null) in
        let ej = ("expression", expr_annot_to_json e) in
        let bj = ("body", expr_annot_to_json b) in
        [("node_type", `String "LetExpression"); vj; tj; ej; bj;]
    | Message margs ->
        let message_arg_to_json (s, p) = 
            let p_json = payload_to_json p in 
            `Assoc [("node_type", `String "MessageArgument"); ("variable", `String  s); ("payload", p_json)] in
        let msg_args_json = ("margs", `List ( List.map ~f:(message_arg_to_json) margs)) in
        (*let msg_args_json = JSON.Message.message_to_json margs in *)
        [("node_type", `String "MessageExpression"); msg_args_json;]
    | Fun (lhs, t, rhs) ->
        let lhsj = ("lhs", ident_to_json lhs) in
        let lhstj = ("lhs_type", type_to_json t) in
        let rhsj = ("rhs_expr", expr_annot_to_json rhs) in
        [("node_type", `String "FunExpression"); lhsj; rhsj; lhstj;]
    | App (v, e) ->
        let lhsj = ("lhs", ident_to_json v) in
        let rhsj = ("rhs_list", `List (List.map ~f:ident_to_json e) ) in
        [("node_type", `String "AppExpression"); lhsj; rhsj]
    | Constr (n, ts, args) ->
        let nj = ("constructor_name", `String n) in
        let tsj = ("types", `List ( List.map ~f:(fun x -> type_to_json x) ts)) in
        let argsj = ("args", `List (List.map ~f:ident_to_json args) ) in
        [("node_type", `String "ConstrExpression"); nj; tsj; argsj]
    | MatchExpr (lhs, rhs) ->
        let lhsj = ("lhs", ident_to_json lhs) in
        let mexpr_case_to_json =
            fun (p, e) ->
                let pj = pattern_to_json p in 
                let ej = expr_annot_to_json e in 
                `Assoc [("node_type", `String "MatchExpressionCase"); ("pattern", pj); ("expression", ej)] in
        let rhsj  = ("cases", `List (List.map rhs ~f:mexpr_case_to_json)) in
        [("node_type", `String "MatchExpression"); lhsj; rhsj]
    | Builtin (b, vs) ->
        let b_json = ("builtin_function", builtin_annot_to_json b) in
        let vs_json = ("arguments", `List (List.map ~f:ident_to_json vs)) in
        [("node_type", `String "BuiltinExpression"); vs_json; b_json]
    (* Advanced features: to be added in Scilla 0.2 *)
    | TFun (_,_) -> 
        [("node_type", `String "TFunExpression")]
    | TApp (_,_) -> 
        [("node_type", `String "TAppExpression")]
    (* Fixpoint combinator: used to implement recursion principles *)
    | Fixpoint (_, _, _) ->
        [("node_type", `String "FixpointExpression")]


and expr_annot_to_json ((e : ParsedSyntax.expr), (l :loc)) = 
    let lj = ("loc", loc_to_json l) in 
    let expr_json = expr_to_json e in
    `Assoc (lj:: expr_json)

(*| MatchStmt of ER.rep ident * (pattern * stmt_annot list) list*)
let rec mstmt_case_to_json ((p: ParsedSyntax.pattern), (sa: ParsedSyntax.stmt_annot list)) =
    let sa_json = ("pattern_body", `List (List.map sa ~f:stmt_annot_to_json)) in
    let p_json = pattern_to_json p in
    `Assoc [ 
        sa_json;
        ("pattern", p_json);
        ("node_type", `String "MatchStatementCase")
    ] 

and stmt_to_json (s : ParsedSyntax.stmt) =
    match s with
    | Load (a, b)->
            let a_json = ("lhs", ident_to_json a) in
            let b_json = ("rhs", ident_to_json b) in
            [a_json; b_json; ("node_type", `String "LoadStatement")]
    | Store (a, b)->
            let a_json = ("lhs", ident_to_json a) in
            let b_json = ("rhs", ident_to_json b) in
            [a_json; b_json; ("node_type", `String "StoreStatement")]
    | Bind (v, e)->
            let v_json = ("lhs", ident_to_json v) in
            let e_json = ("rhs_expr", expr_annot_to_json e) in
            [("node_type", `String "BindStatement"); v_json; e_json]
    (* m[k1][k2][..] := v OR delete m[k1][k2][...] *)
    | MapUpdate (m, keys, v)->
            let map_json = ("map_name", ident_to_json m) in
            let v_json =
                match v with
                | Some v' -> ("rhs", ident_to_json v')
                | None -> ("rhs", `Null) in
            let keys_json = ("keys", `List (List.map keys ~f:ident_to_json)) in
            [("node_type", `String "MapUpdateStatement"); map_json; v_json; keys_json]
    (* v <- m[k1][k2][...] OR b <- exists m[k1][k2][...] *)
    (* If the bool is set, then we interpret this as value retrieve, 
       otherwise as an "exists" query. *)
    | MapGet (v, m, keys, b)->
            let map_json = ("map_name", ident_to_json m) in
            let v_json = ("lhs", ident_to_json v) in
            let keys_json = ("keys", `List (List.map keys ~f:ident_to_json)) in
            let b_json = ("is_value_retrieve", `Bool b) in
            [("node_type", `String "MapGetStatement"); map_json; v_json; keys_json; b_json]
    | MatchStmt (v, c)->
            let v_json = ("arg", ident_to_json v) in
            let c_json  = ("cases", `List (List.map c ~f:mstmt_case_to_json)) in
            [v_json; c_json; ("node_type", `String "MatchStatement")]
    | ReadFromBC (v, s)->
            let v_json = ("lhs", ident_to_json v) in
            let s_json  = ("rhs_str", `String s) in
            [v_json; s_json; ("node_type", `String "ReadFromBCStatement")]
    | AcceptPayment ->
            [("node_type", `String "AcceptPaymentStatement")]
    | SendMsgs v ->
            let v_json = ("arg", ident_to_json v) in
            [v_json; ("node_type", `String "SendMsgsStatement")]
    | CreateEvnt v ->
            let v_json = ("arg", ident_to_json v) in
            [v_json; ("node_type", `String "CreateEvntStatement")]
    | CallProc (v, msgs)->
            let v_json = ("arg", ident_to_json v) in
            let msgs_json = ("messages", `List (List.map msgs ~f:ident_to_json)) in
            [v_json; msgs_json; ("node_type", `String "CallProcStatement")]
    | Throw v ->
            let v_json =
                match v with
                | Some v' -> ("arg", ident_to_json v')
                | None -> ("arg", `Null) in
            [("node_type", `String "ThrowStatement"); v_json]


and stmt_annot_to_json ((s : ParsedSyntax.stmt), (l :loc)) = 
    let locj = ("loc", loc_to_json l) in
    let sj = stmt_to_json s in
    `Assoc (locj :: sj)

let field_to_json (i, t, e) = 
    `Assoc [ 
        ("field_name", ident_to_json i) ;
        ("field_type", type_to_json t) ;
        ("expression", expr_annot_to_json e);
        ("node_type", `String "Field")
    ]

let param_to_json (i, t) = 
    `Assoc [ 
        ("parameter_name", ident_to_json i);
        ("parameter_type", type_to_json t);
        ("node_type", `String "Parameter")
    ]

let component_to_json (c : ParsedSyntax.component) =
    let node_type_json = ("node_type", `String "Component") in
    let comp_name_json = ("name", ident_to_json c.comp_name ) in
    let comp_params_json = ("params", `List (List.map c.comp_params ~f:param_to_json)) in
    let comp_body = ("body", `List (List.map c.comp_body ~f:stmt_annot_to_json)) in
    `Assoc [ comp_params_json ; comp_name_json; comp_body; node_type_json]

let contract_to_json (c : ParsedSyntax.contract) =
    let node_type_json = ("node_type", `String "Contract") in
    let cname_json = ("name", ident_to_json c.cname) in
    let cparams_json = ("params", `List (List.map c.cparams ~f:param_to_json)) in
    let cfields_json = ("fields", `List (List.map c.cfields ~f:field_to_json)) in
    let components_json = ("components", `List (List.map c.ccomps ~f:component_to_json)) in
    `Assoc [cname_json; cparams_json; cfields_json; components_json; node_type_json]

let ctr_def_to_json (c : ParsedSyntax.ctr_def) =
    let node_type_json = ("node_type", `String "CtrDef") in
    let cname_json = ("ctr_def_name", ident_to_json c.cname) in
    let c_arg_types_json = ("c_arg_types", `List ( List.map ~f:(fun x -> type_to_json x) c.c_arg_types)) in
    `Assoc [cname_json; c_arg_types_json; node_type_json;]

let lib_entry_to_json le =
    match le with
    | ParsedSyntax.LibVar (i, t, e) ->
        let t_json = match t with
            | Some t' -> type_to_json t'
            | None -> `Null in
        `Assoc [
            ("node_type", `String "LibraryVariable");
            ("name", ident_to_json i) ;
            ("variable_type", t_json);
            ("expression", expr_annot_to_json e);
        ]
    | ParsedSyntax.LibTyp (i, ctr_defs) ->
        `Assoc [
            ("name", ident_to_json i) ;
            ("node_type", `String "LibraryType");
            ("ctr_defs", `List (List.map ~f:ctr_def_to_json ctr_defs));
        ]

let library_to_json (l : ParsedSyntax.library) =
    let node_type_json = ("node_type", `String "Library") in
    let lname_json = ("library_name", ident_to_json l.lname) in
    let lentries_json = ("library_entries", `List(List.map ~f:lib_entry_to_json l.lentries)) in
    `Assoc [lname_json; lentries_json; node_type_json]

let elibrary_to_json (name, alias) =
    let node_type_json = ("node_type", `String "ExternalLibrary") in
    let name_json = ("name", ident_to_json name) in
    let a_json = match alias with
        |  Some a' -> ("alias", ident_to_json a')
        |  None -> ("alias", `Null) in
    `Assoc [name_json; a_json; node_type_json]

let cmodule_to_json (cmod : ParsedSyntax.cmodule) =
    let smver_json = ("scilla_major_version", `Int cmod.smver) in
    let cname_json = ("name", ident_to_json cmod.cname) in
    let libs_json = match cmod.libs with
        | Some l -> ("library", library_to_json l)
        | None  -> ("library", `Null) in
    let elibs_json = ("external_libraries", `List (List.map ~f:elibrary_to_json cmod.elibs)) in
    let contr_json = ("contract", contract_to_json cmod.contr) in
    let node_type_json = ("node_type", `String "ContractModule") in
    `Assoc [smver_json; cname_json; libs_json; elibs_json; contr_json; node_type_json]

let lmodule_to_json (lmod : ParsedSyntax.lmodule) =
    let libs_json = ("library", library_to_json lmod.libs) in
    let elibs_json = ("external_libraries", `List (List.map ~f:elibrary_to_json lmod.elibs)) in
    let node_type_json = ("node_type", `String "LibraryModule") in
    `Assoc [libs_json; elibs_json; node_type_json]

let scillalib_to_json (file) =
    let r = check_parsing file ScillaParser.Incremental.lmodule in
    match r with
        | Error e -> fatal_error_gas e (Stdint.Uint64.of_int 0)
        | Ok lmod ->
            let mod_json =  lmodule_to_json lmod in
            pout (sprintf "%s\n" (Yojson.Basic.pretty_to_string mod_json))


let scilla_to_json (file) =
    let r = check_parsing file ScillaParser.Incremental.cmodule in
    match r with
        | Error e -> fatal_error_gas e (Stdint.Uint64.of_int 0)
        | Ok cmod -> 
            let mod_json =  cmodule_to_json cmod in
            pout (sprintf "%s\n" (Yojson.Basic.pretty_to_string mod_json))

let () =
    let cli = parse_cli () in
    let f_ext = FilePath.get_extension cli.input_file in
    match f_ext with
        | "scillib" -> scillalib_to_json cli.input_file
        | "scilla" -> scilla_to_json cli.input_file
        | _ -> pout(sprintf "Unhandled type %s\n" f_ext)
