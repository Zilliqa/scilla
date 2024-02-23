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

(** Scilla code formatter *)

open Scilla_base
open Base
open PPrint

[@@@ocamlformat "disable"]

module Format (SR : Syntax.Rep) (ER : Syntax.Rep) (Lit : Literal.ScillaLiteral) =
struct

  (* instantiated syntax extended with comments *)
  module Ast = ExtendedSyntax.ExtendedScillaSyntax (SR) (ER) (Lit)

  module type DOC = sig
    val of_type : Ast.SType.t -> PPrint.document
    val of_expr : Ast.expr_annot -> PPrint.document
    val of_literal : Lit.t -> PPrint.document
    val of_contract_module : Ast.cmodule -> PPrint.document
  end

  module Doc : DOC = struct

    (* -- helpers -- *)

    (* number of spaces to indent *)
    let indentation = 2

    let forall_kwd = !^"forall"
    let empy_map_kwd = !^"Emp"
    let fun_kwd = !^"fun"
    let tfun_kwd = !^"tfun"
    let in_kwd = !^"in"
    let let_kwd = !^"let"
    let map_kwd = !^"Map"
    let builtin_kwd = !^"builtin"
    let match_kwd = !^"match"
    let with_kwd = !^"with"
    let end_kwd = !^"end"
    let delete_kwd = !^"delete"
    let exists_kwd = !^"exists"
    let accept_kwd = !^"accept"
    let as_kwd = !^"as"
    let send_kwd = !^"send"
    let event_kwd = !^"event"
    let throw_kwd = !^"throw"
    let contract_kwd = !^"contract"
    let field_kwd = !^"field"
    let of_kwd = !^"of"
    let type_kwd = !^"type"
    let import_kwd = !^"import"
    let library_kwd = !^"library"
    let scilla_version_kwd = !^"scilla_version"
    let blocknumber_kwd = !^"BLOCKNUMBER"
    let chainid_kwd = !^"CHAINID"
    let timestamp_kwd = !^"TIMESTAMP"
    let replicate_contract_kwd = !^"REPLICATE_CONTRACT"
    let arrow = !^"->"
    let darrow = !^"=>"
    let at = !^"@"
    let pipe = !^"|"
    let rev_arrow = !^"<-"
    let assign = !^":="
    let blockchain_arrow = !^"<-&"

    (** concat two documents with an unbreakable space *)
    let ( ^^^ ) left right = left ^^ space ^^ right

    let indent doc = nest indentation doc

    (* redefine PPrint's shorthand to allow changing indentation *)
    let ( ^//^ ) left right = prefix indentation 1 left right

    (* Add parentheses only if the condition if true *)
    let parens_if cond doc = if cond then parens doc else doc

    (* Wrap document in comment symbols *)
    let comment = enclose !^"(*" !^"*)"

    (* Concatenate multiple comments to a single comment. *)
    let concat_comments ?(sep=hardline) cs =
      List.fold_left cs ~init:[] ~f:(fun acc c -> acc @ [comment !^c])
      |> concat_map (fun c -> c ^^^ sep)

    (** Add formatted [comments] around [doc]. *)
    let wrap_comments comments doc =
      let spaced s =
        let has_prefix prefix = String.is_prefix s ~prefix in
        let has_suffix suffix = String.is_suffix s ~suffix in
        let s = if has_prefix " " || has_prefix "*" then s else " " ^ s in
        let s = if has_suffix " " || has_suffix "*" then s else s ^ " " in
        s
      in
      let left, above, right =
        List.fold_left comments
          ~init:([],[],[])
          ~f:(fun (acc_l, acc_a, acc_r) -> function
            | (_, s, Ast.ComLeft) ->
                acc_l @ [comment !^(spaced s); space], acc_a, acc_r
            | (_, s, Ast.ComAbove) ->
                acc_l, (comment !^(spaced s))::acc_a, acc_r
            | (_, s, Ast.ComRight) ->
                acc_l, acc_a, [space; comment !^(spaced s)] @ acc_r)
        |> fun (l, a, r) ->
            let a' = if List.is_empty a then empty
                    else (concat_map (fun c -> c ^^^ hardline) a)
            in
            let l' = concat l in
            let r' = concat r in
            l', a', r'
      in
      concat [above; left; doc; right]

    let of_builtin b = !^(Syntax.pp_builtin b)

    let of_id id = !^(Ast.SIdentifier.as_error_string id)

    let of_ann_id (id, comments) = of_id id |> wrap_comments comments

    let of_ann_ids ids =
      separate_map space of_ann_id ids

    let rec of_type_with_prec p typ =
      let open Ast.SType in
      match typ with
      | PrimType t -> !^(Type.PrimType.pp_prim_typ t) (* TODO: temporary solution *)
      | Unit -> !^"()" (* This cannot happen in source code *)
      | TypeVar tv -> !^tv
      | FunType (at, vt) ->
          parens_if (p > 0) @@ group ((of_type_with_prec 1 at) ^^^ arrow) ^/^ (of_type_with_prec 0 vt)
        (* TODO: test MapType and PolyFun pretty-printing *)
      | MapType (kt, vt) ->
          parens_if (p > 0) @@ map_kwd ^//^ (of_type_with_prec 1 kt) ^/^ (of_type_with_prec 1 vt)
      | ProcType _ ->
          failwith "ProcType annotation cannot appear in source code"
      | PolyFun (tv, bt) ->
          parens_if (p > 0) @@ forall_kwd ^^^ !^tv ^^ dot ^//^ (of_type_with_prec 0 bt)
      | ADT (tid, tys) ->
          let ty_cons = of_id tid in
          let ty_args = separate_map space (of_type_with_prec 1) tys in
          if List.is_empty tys then ty_cons
          else parens_if (p > 0) @@ ty_cons ^//^ ty_args
      | Address addr_kind -> of_addr_kind p addr_kind

    and of_type typ = of_type_with_prec 0 typ

    and of_addr_kind p kind =
      parens_if (p > 0) @@
      match kind with
      (* Any address in use *)
      | AnyAddr -> !^"ByStr20 with end"
      (* Address containing a library *)
      | LibAddr -> !^"ByStr20 with library end"
      (* Address containing a library or contract *)
      | CodeAddr -> !^"ByStr20 with _codehash end"
      (* Address containing a contract *)
      | ContrAddr fields_map ->
        let alist = Ast.SType.IdLoc_Comp.Map.to_alist fields_map in
        let contract_fields =
          separate_map
            (comma ^^ break 1)
            (fun (f, ty) -> group (field_kwd ^/^ of_id f ^/^ colon ^/^ of_type ty))
            alist
        in
        if List.is_empty alist then !^"ByStr20 with contract end"
        else
          surround indentation 1
            !^"ByStr20 with contract"
            contract_fields
            end_kwd

    (* whitespace-separated non-primitive types need to be parenthesized *)
    let of_types typs ~sep =
      group @@ separate_map sep (fun ty -> of_type_with_prec 1 ty) typs

    let of_typed_ann_id id typ = of_ann_id id ^^^ colon ^//^ group (of_type typ)

    let rec of_literal lit =
      let rec walk p = function
        | Lit.StringLit s ->
            !^[%string "\"$(String.escaped s)\""]
        | Lit.IntLit i ->
            let bit_width = Int.to_string @@ Lit.int_lit_width i in
            let value = Lit.string_of_int_lit i in
            parens_if (p > 0) @@ !^[%string "Int$(bit_width) $value"]
        | Lit.UintLit i ->
            let bit_width = Int.to_string @@ Lit.uint_lit_width i in
            let value = Lit.string_of_uint_lit i in
            parens_if (p > 0) @@ !^[%string "Uint$(bit_width) $value"]
        | Lit.BNum b ->
            let value = Literal.BNumLit.get b in
            parens_if (p > 0) @@ !^[%string "BNum $value"]
        | Lit.ByStr _bs ->
          failwith "Pretty-printing of ByStr literals in expressions cannot happen"
          (* AFAIR, there is no way in Scilla to have ByStr literals in expressions.
            We might need to print the result of evaluation but there is a pretty printer for values *)
        | Lit.ByStrX bsx ->
            !^[%string "$(Lit.Bystrx.hex_encoding bsx)"]
        | Lit.Clo _ -> !^"<closure>"
        | Lit.TAbs _ -> !^"<type_closure>"
        | Lit.ADTValue (name, _tys, lits) ->
            let constructor = !^(Lit.LType.TIdentifier.Name.as_error_string name) in
            if List.is_empty lits then
              constructor
            else
              let cons_params = separate_map space (walk 1) lits in
              parens_if (p > 0) @@ constructor ^//^ cons_params
        | Lit.Map ((key_type, value_type), table) ->
            if Caml.Hashtbl.length table = 0 then
              let kt = of_type key_type
              and vt = of_type value_type in
              (* TODO: remove unneeded parens around types *)
              parens_if (p > 0) @@ empy_map_kwd ^//^ (parens kt) ^/^ (parens vt)
            else
              failwith "Pretty-printing of non-empty map literals cannot happen while printing expressions"
        | Lit.Msg typed_assocs ->
          (* we ignore type information here *)
          braces (
            separate_map
              (semi ^^ break 1)
              (fun (field, _typ, value) -> !^field ^^^ colon ^^^ of_literal value)
              typed_assocs
          )
      in walk 0 lit

    let of_payload = function
      | Ast.MLit lit -> of_literal lit
      | Ast.MVar id -> of_ann_id id

    let of_pattern pat =
      let rec of_pattern_aux ~top_parens = function
      | Ast.Wildcard -> !^"_"
      | Ast.Binder id -> of_ann_id id
      | Ast.Constructor (constr_id, pats) ->
          let constr_id = of_ann_id constr_id in
          if List.is_empty pats then
            constr_id
          else
            let constr_args = separate_map (break 1) (of_pattern_aux ~top_parens:true) pats in
            parens_if top_parens (constr_id ^//^ constr_args)
      in
      of_pattern_aux ~top_parens:false pat

    let rec of_expr (expr, _ann, comments) =
      (match expr with
      | Ast.Literal lit -> of_literal lit
      | Ast.Var id -> of_ann_id id
      | Ast.Fun (id, typ, body) ->
        (* TODO: nested functions should not be indented:
           fun (a : String) =>
             fun (s : Uint32) => ...
           vs
           fun (a : String) =>
           fun (s : Uint32) => ...
        *)
        let body = of_expr body in
        (* fun ($id : $typ) =>
             $body *)
        fun_kwd ^^^ parens (of_typed_ann_id id typ) ^^^ darrow ^^ indent (hardline ^^ body)
      | Ast.App (fid, args) ->
        let fid = of_ann_id fid
        and args = of_ann_ids args in
        fid ^//^ args
      | Ast.Builtin ((builtin, _ann), _types, typed_ids) ->
        let builtin = of_builtin builtin
        and args = of_ann_ids typed_ids in
        builtin_kwd ^^^ builtin ^//^ args
      | Ast.Let (id, otyp, lhs, body) ->
        let id =
          match otyp with
          | None -> of_ann_id id
          | Some typ -> of_typed_ann_id id typ
        and lhs = of_expr lhs
        and body = of_expr body in
        (*
          TODO:
          Need special case for expressions like this one:
            let succ =
              let b1 = andb sum_test mul_test in
              andb paired_test b1
            in
            ...
          Now these are displayed as follows:
            let succ = let b1 = andb sum_test mul_test in andb paired_test b1 in ...

          For instance, forcing a hard newline for LHS should work here:
            let foo =
              $lhs
            in
            $body
          But, in general, things like
            let x = Uint 42 in ...
          would look weird.
        *)
        (group (group (let_kwd ^^^ id ^^^ equals ^//^ lhs) ^/^ in_kwd)) ^/^ body
      | Ast.TFun (ty_var, body) ->
        let ty_var = of_ann_id ty_var
        and body = of_expr body in
        (* tfun $ty_var => $body *)
        (* (^/^) -- means concat with _breakable_ space *)
        tfun_kwd ^^^ ty_var ^^^ darrow ^//^ body
      | Ast.TApp (id, typs) ->
        let tfid = of_ann_id id
        (* TODO: remove unnecessary parens around primitive types:
           e.g. "Nat" does not need parens but "forall 'X. 'X" needs them in type applications *)
        and typs = separate_map space (fun typ -> parens @@ of_type typ) typs in
        at ^^ tfid ^//^ typs
      | Ast.MatchExpr (ident, branches) ->
        match_kwd ^^^ of_ann_id ident ^^^ with_kwd ^/^
        separate_map hardline
          (fun (pat, e, cs) ->
            let doc =
              pipe ^^^ of_pattern pat ^^^ darrow
            in
            let doc = if not @@ List.is_empty cs then
                      doc ^^^ concat_comments ~sep:space cs
                      else doc
            in
            let doc = doc ^//^ group (of_expr e) in
            group (doc))
          branches
        ^^ hardline ^^ end_kwd
      | Ast.Constr (id, typs, args) ->
        let id = of_ann_id id
        (* TODO: remove unnecessary parens around primitive types *)
        and args_doc = of_ann_ids args in
        if Base.List.is_empty typs then
          if Base.List.is_empty args then id else id ^//^ args_doc
        else
          let typs = separate_map space (fun typ -> parens @@ of_type typ) typs in
          if Base.List.is_empty args then
            id ^//^ braces typs
          else
            id ^//^ braces typs ^//^ args_doc
      | Ast.Message assocs ->
          surround indentation 1
            lbrace
            (separate_map
              (semi ^^ break 1)
              (fun (field, value) -> !^field ^^^ colon ^^^ of_payload value)
              assocs)
            rbrace
      | Fixpoint _ -> failwith "Fixpoints cannot appear in user contracts"
      | GasExpr _ -> failwith "Gas annotations cannot appear in user contracts's expressions"
      ) |> wrap_comments comments

      let of_map_access map keys =
        let map = of_ann_id map
        and keys = concat_map (fun k -> brackets @@ of_ann_id k) keys in
        map ^^ keys

      let rec of_stmt (stmt, _ann, comments) =
        (match stmt with
        | Ast.Load (id, field) ->
          of_ann_id id ^^^ rev_arrow ^//^ of_ann_id field
        | Ast.RemoteLoad (id, addr, field) ->
          of_ann_id id ^^^ blockchain_arrow ^//^ of_ann_id addr ^^ dot ^^ of_ann_id field
        | Ast.Store (field, id) ->
          of_ann_id field ^^^ assign ^//^ of_ann_id id
        | Ast.Bind (id, expr) ->
          of_ann_id id ^^^ equals ^//^ of_expr expr
        | Ast.MapUpdate (map, keys, mode) ->
          (* m[k1][k2][..] := v OR delete m[k1][k2][...] *)
          (match mode with
           | Some value -> of_map_access map keys ^^^ assign ^//^ of_ann_id value
           | None -> delete_kwd ^^^ of_map_access map keys)
        | Ast.MapGet (id, map, keys, mode) ->
          (* v <- m[k1][k2][...] OR b <- exists m[k1][k2][...] *)
          (* If the bool is set, then we interpret this as value retrieve,
            otherwise as an "exists" query. *)
           if mode then
            of_ann_id id ^^^ rev_arrow ^//^ of_map_access map keys
           else
            of_ann_id id ^^^ rev_arrow ^//^ exists_kwd ^^^ of_map_access map keys
        | Ast.RemoteMapGet (id, addr, map, keys, mode) ->
          (* v <-& adr.m[k1][k2][...] OR b <-& exists adr.m[k1][k2][...] *)
          (* If the bool is set, then we interpret this as value retrieve,
            otherwise as an "exists" query. *)
           if mode then
            of_ann_id id ^^^ blockchain_arrow ^//^ of_ann_id addr ^^ dot ^^ of_map_access map keys
           else
            of_ann_id id ^^^ blockchain_arrow ^//^ exists_kwd ^^^ of_ann_id addr ^^ dot ^^ of_map_access map keys
        | Ast.MatchStmt (id, branches) ->
          match_kwd ^^^ of_ann_id id ^^^ with_kwd ^/^
          separate_map hardline
            (fun (pat, stmts, cs) ->
              let doc = pipe ^^^ of_pattern pat ^^^ darrow in
              let doc = if not @@ List.is_empty cs then
                        doc ^^^ concat_comments ~sep:space cs
                        else doc
              in
              let doc = doc ^//^ group (of_stmts stmts) in
              group (doc))
            branches
          ^^ hardline ^^ end_kwd
        | Ast.ReadFromBC (id, query) ->
          let query =
            match query with
            | CurBlockNum -> blocknumber_kwd
            | ChainID -> chainid_kwd
            | Timestamp ts -> timestamp_kwd ^^ parens (of_ann_id ts)
            | ReplicateContr (addr, init_params) ->
              replicate_contract_kwd ^^ parens (of_ann_id addr ^^ comma ^^^ of_ann_id init_params)
          in
          of_ann_id id ^^^ blockchain_arrow ^//^ query
        | Ast.TypeCast (id, addr, typ) ->
          of_ann_id id ^^^ blockchain_arrow ^//^ of_ann_id addr ^^^ as_kwd ^^^ of_type typ
        | Ast.AcceptPayment ->
          accept_kwd
        | Ast.Return id ->
          !^"_return" ^//^ assign ^//^ of_ann_id id
        | Ast.Iterate (arg_list, proc) ->
          (* forall l p *)
          forall_kwd ^//^ of_ann_id arg_list ^//^ of_ann_id proc
        | Ast.SendMsgs msgs ->
          send_kwd ^//^ of_ann_id msgs
        | Ast.CreateEvnt events ->
          event_kwd ^//^ of_ann_id events
        | Ast.CallProc (id_opt, proc, args) ->
          let call =
            if List.is_empty args then of_ann_id proc
            else of_ann_id proc ^//^ of_ann_ids args
          in
          (match id_opt with
           | None -> call
           | Some id -> of_ann_id id ^^^ equals ^//^ call)
        | Ast.Throw oexc ->
          (match oexc with
           | None -> throw_kwd
           | Some exc -> throw_kwd ^//^ of_ann_id exc)
        | Ast.GasStmt _ -> failwith "Gas annotations cannot appear in user contracts's statements"
        ) |> wrap_comments comments

      and of_stmts stmts =
        separate_map (semi ^^ hardline) (fun s -> of_stmt s) stmts

      (* contract, transition, procedure parameters *)
      let of_parameters typed_params ~sep =
        surround indentation 0
          lparen
          (separate_map
            (comma ^^ sep)
            (fun (p, typ) -> of_typed_ann_id p typ)
            typed_params)
          rparen

      let of_component Ast.{comp_comments; comp_type; comp_name; comp_params; comp_body; comp_return} =
        let comp_type = !^(Syntax.component_type_to_string comp_type)
        and comp_name = of_ann_id comp_name
        and comp_params = of_parameters comp_params ~sep:(break 1)
        and comp_body = of_stmts comp_body in
        let signature = match comp_return with
        | None ->
          (group (comp_type ^^^ comp_name ^//^ comp_params))
        | Some ty ->
          (group (comp_type ^^^ comp_name ^//^ comp_params ^//^ colon ^//^ of_type ty))
        in
        concat_comments comp_comments ^^ signature ^^
          indent (hardline ^^ comp_body) ^^ hardline ^^
        end_kwd

      let of_ctr_def Ast.{cname; c_comments; c_arg_types} =
        let constructor_name = of_ann_id cname
        and constructor_args_types =
          (* TODO: break sequences of long types (e.g. ByStr20 with contract ................... end Uint256 is unreadable) *)
          of_types ~sep:(break 1) c_arg_types
        in
        if List.is_empty c_arg_types then
          constructor_name
          ^//^ concat_comments ~sep:space c_comments
        else
          align (group (constructor_name ^//^ of_kwd) ^//^
                 constructor_args_types ^//^
                 concat_comments ~sep:space c_comments)

      let of_lib_entry = function
        | Ast.LibVar (comments, definition, otyp, expr) ->
          let definition =
            match otyp with
            | None -> of_ann_id definition
            | Some typ -> of_typed_ann_id definition typ
          and expr = of_expr expr in
          concat_comments comments ^^
          let_kwd ^^^ definition ^^^ equals ^//^ expr
        | Ast.LibTyp (comments, typ_name, constr_defs) ->
          let typ_name = of_ann_id typ_name
          and constr_defs =
            separate_map hardline (fun cd -> pipe ^^^ of_ctr_def cd) constr_defs
          in
          concat_comments comments ^^
          type_kwd ^^^ typ_name ^^^ equals ^^ hardline ^^
          constr_defs

      let of_library Ast.{lname; lentries} =
        library_kwd ^^^ of_ann_id lname ^^
        if List.is_empty lentries then hardline
        else
          let lentries =
            separate_map
              (twice hardline)
              (fun lentry -> of_lib_entry lentry)
              lentries
          in
          twice hardline ^^ lentries ^^ hardline

      let of_contract Ast.{cname; cparams; cconstraint; cfields; ccomps} =
        let cname = of_ann_id cname
        and cparams = of_parameters cparams ~sep:hardline
        and cconstraint =
          let true_ctr = Lit.LType.TIdentifier.Name.parse_simple_name "True" in
          match cconstraint with
          | (Ast.Literal (Lit.ADTValue (c, [], [])), _annot, _comment) when [%equal: _] c true_ctr ->
            (* trivially True contract constraint does not get rendered *)
            empty
          | _ ->
            with_kwd ^^
            indent (hardline ^^ of_expr cconstraint) ^^ hardline ^^
            darrow ^^ hardline
        and cfields =
          if List.is_empty cfields then empty
          else
            separate_map
              (twice hardline)
                (fun (comments, field, typ, init) ->
                  concat_comments comments ^^
                  field_kwd ^^^ of_typed_ann_id field typ ^^^ equals ^//^ of_expr init)
              cfields
            ^^ twice hardline
        and ccomps =
          separate_map (twice hardline) (fun c -> of_component c) ccomps
        in
        contract_kwd ^^^ (cname ^//^ cparams) ^^ hardline ^^
        cconstraint ^^ twice hardline ^^
        cfields ^^
        ccomps

      let of_contract_module Ast.{smver; file_comments; lib_comments; libs; elibs; contr_comments; contr} =
        let imports =
          let import_lib (lib, onamespace) =
            match onamespace with
            | None -> of_ann_id lib
            | Some namespace -> of_ann_id lib ^^^ as_kwd ^^^ of_ann_id namespace
          in
          let imported_libs =
            separate_map (hardline) (fun imp -> import_lib imp) elibs
          in
          if List.is_empty elibs then empty
          else group (import_kwd ^//^ imported_libs) ^^ twice hardline
        and contract_library =
          match libs with
          | Some lib -> of_library lib ^^ twice hardline
          | None -> empty
        in
        scilla_version_kwd ^^^ !^(Int.to_string smver) ^^ hardline ^^
        concat_comments file_comments ^^
        hardline ^^
        imports ^^
        concat_comments lib_comments ^^
        contract_library ^^
        concat_comments contr_comments ^^
        of_contract contr ^^ hardline
  end

  (* format width *)
  let width = 80

  let doc2str : PPrint.document -> string =
    fun doc ->
      let buffer = Buffer.create 100 in
      PPrint.ToBuffer.pretty 1.0 width buffer doc;
      Buffer.contents buffer

  let type_to_string t = doc2str @@ Doc.of_type t
  let expr_to_string e = doc2str @@ Doc.of_expr e
  let contract_to_string c = doc2str @@ Doc.of_contract_module c
end

[@@@ocamlformat "enable"]

module LocalLiteralSyntax =
  Format (ParserUtil.ParserRep) (ParserUtil.ParserRep) (Literal.LocalLiteral)
