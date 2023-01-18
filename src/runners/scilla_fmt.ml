(*
  This file is part of scilla.

  Copyright (c) 2022 - present Zilliqa Research Pvt. Ltd.

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
open Scilla_base
open Scilla_format
open Literal
open GlobalConfig
open ErrorUtils
open PrettyPrinters
module FEParser = FrontEndParser.ScillaFrontEndParser (LocalLiteral)

type file_name = string
type output_format = Source_code | Sexpression | Json

let unpack_ast_exn = function Ok res -> res | Error e -> fatal_error e

(* Deannotate AST to dump S-expressions without location information *)
module DeannotRep = struct
  type rep = unit [@@deriving sexp, to_yojson]

  let dummy_rep = ()
  let get_loc _ = dummy_loc
  let equal_rep _ _ = true
  let address_rep = ()
  let uint128_rep = ()
  let uint32_rep = ()
  let bnum_rep = ()
  let string_rep = ()
  let parse_rep _ = ()
  let get_rep_str _ = "()"
end

module Deannot =
  SyntaxAnnotMapper.MapSyntax (ParserUtil.ParserRep) (ParserUtil.ParserRep)
    (LocalLiteral)
    (DeannotRep)
    (DeannotRep)

let dummy_loc _loc = { fname = ""; lnum = 0; cnum = 0 }

let deannotate_expression e =
  Deannot.expr_annot e ~fe:(fun _ -> ()) ~fl:dummy_loc ~fs:(fun _ -> ())

let deannotate_contract_module cmod =
  Deannot.cmodule cmod ~fe:(fun _ -> ()) ~fl:dummy_loc ~fs:(fun _ -> ())

let scilla_json_fmt deannotated human_readable file =
  let open FilePath in
  let open StdlibTracker in
  let yojson_to_string json =
    if human_readable then Yojson.Safe.pretty_to_string json
    else Yojson.Safe.to_string json
  in
  if check_extension file file_extn_library then
    (* library modules *)
    failwith "Formatting of Scilla library modules is not implemented yet"
  else if check_extension file file_extn_contract then
    (* contract modules *)
    let ast = file |> FEParser.parse_cmodule |> unpack_ast_exn in
    if deannotated then
      ast |> deannotate_contract_module |> Deannot.cmodule_to_yojson
      |> yojson_to_string
    else ast |> FEParser.FESyntax.cmodule_to_yojson |> yojson_to_string
  else if check_extension file file_extn_expression then
    (* expressions *)
    let ast = file |> FEParser.parse_expr_from_file |> unpack_ast_exn in
    if deannotated then
      ast |> deannotate_expression |> Deannot.expr_annot_to_yojson
      |> yojson_to_string
    else ast |> FEParser.FESyntax.expr_annot_to_yojson |> yojson_to_string
  else fatal_error (mk_error0 ~kind:"Unknown file extension" ?inst:None)

let scilla_sexp_fmt deannotated human_readable file =
  let open FilePath in
  let open StdlibTracker in
  let sexp_to_string sexp =
    if human_readable then Sexplib.Sexp.to_string_hum sexp
    else Sexplib.Sexp.to_string sexp
  in
  sexp_to_string
    (if check_extension file file_extn_library then
     (* library modules *)
     (* file
        |> FEParser.parse_lmodule
        |> unpack_ast_exn
        |> *)
     failwith "Formatting of Scilla library modules is not implemented yet"
    else if check_extension file file_extn_contract then
      (* contract modules *)
      let ast = file |> FEParser.parse_cmodule |> unpack_ast_exn in
      if deannotated then
        ast |> deannotate_contract_module |> Deannot.sexp_of_cmodule
      else ast |> FEParser.FESyntax.sexp_of_cmodule
    else if check_extension file file_extn_expression then
      (* expressions *)
      let ast = file |> FEParser.parse_expr_from_file |> unpack_ast_exn in
      if deannotated then
        ast |> deannotate_expression |> Deannot.sexp_of_expr_annot
      else ast |> FEParser.FESyntax.sexp_of_expr_annot
    else fatal_error (mk_error0 ~kind:"Unknown file extension" ?inst:None))

let scilla_source_code_fmt file =
  let open FilePath in
  let open StdlibTracker in
  let tr () =
    ExtendedSyntax.LocalLiteralTransformer.mk (FEParser.get_comments ())
  in
  if check_extension file file_extn_library then
    (* library modules *)
    (* file
       |> FEParser.parse_lmodule
       |> unpack_ast_exn
       |> *)
    failwith "Formatting of Scilla library modules is not implemented yet"
  else if check_extension file file_extn_contract then
    (* contract modules *)
    file |> FEParser.parse_cmodule |> unpack_ast_exn
    |> ExtendedSyntax.LocalLiteralTransformer.extend_cmodule (tr ())
    |> Formatter.LocalLiteralSyntax.contract_to_string
  else if check_extension file file_extn_expression then
    (* expressions *)
    file |> FEParser.parse_expr_from_file |> unpack_ast_exn
    |> ExtendedSyntax.LocalLiteralTransformer.extend_expr (tr ())
    |> Formatter.LocalLiteralSyntax.expr_to_string
  else fatal_error (mk_error0 ~kind:"Unknown file extension" ?inst:None)

let scilla_fmt output_format deannotated human_readable files =
  match files with
  | [] -> `Error (true, "File is not specified")
  | _ :: _ :: _ -> `Error (true, "Cannot process more than one file")
  | [ file ] -> (
      try
        let result_string =
          match output_format with
          | Source_code -> scilla_source_code_fmt file
          | Sexpression -> scilla_sexp_fmt deannotated human_readable file
          | Json -> scilla_json_fmt deannotated human_readable file
        in
        print_endline result_string;
        `Ok ()
      with FatalError msg -> `Error (false, msg))

open Cmdliner

let output_format =
  Arg.(
    value
    & vflag Source_code
        [
          ( Sexpression,
            info [ "sexp"; "s" ]
              ~doc:"Dump Scilla source code as an S-expression." );
          (Json, info [ "json"; "j" ] ~doc:"Dump Scilla source code as JSON.");
        ])

let deannotated =
  let doc =
    "Remove location information and other annotations from the output \
     S-expression."
  in
  Arg.(value & flag & info [ "deannot"; "d" ] ~doc)

let human_readable =
  let doc = "Make the output S-expression human readable." in
  Arg.(value & flag & info [ "human-readable"; "h" ] ~doc)

let files = Arg.(value & pos_all file [] & info [] ~docv:"FILE")

let cmd =
  let doc = "Format Scilla source files" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) formats the specified $(i,FILE), which can be a Scilla \
         contract (.scilla),a Scilla expression (.scilexp), or a Scilla \
         library (.scillib).";
      `P "By default it formats source code to the 80 character width.";
      `P "The formatter can also dump Scilla files as S-expressions.";
    ]
  in
  let info =
    Cmd.info "scilla-fmt" ~version:PrettyPrinters.scilla_version_string ~doc
      ~man
  in
  Cmd.v info
    Term.(
      ret
        (const scilla_fmt $ output_format $ deannotated $ human_readable $ files))

let main () = exit (Cmd.eval cmd)
let () = main ()
