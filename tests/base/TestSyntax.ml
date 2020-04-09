open Core_kernel
open! Int.Replace_polymorphic_compare
open Stdint
open OUnit2
open Identifier
open Type
open Literal
open Syntax
open ParsedSyntax
open PrimTypes
open PrettyPrinters

let parse_expr_wrapper exprstr =
  match FrontEndParser.parse_expr exprstr with
  | Ok expr -> expr
  | Error _ ->
      assert_failure ("Error parsing test expression:\n" ^ exprstr ^ "\n")

let ident_list_printer ls = String.concat ~sep:";" (List.map ~f:get_id ls)

let unannotated_syntax_tests =
  "test suite for unannotated syntax"
  >::: List.map
         ~f:(fun (name, assertion) -> name >:: const assertion)
         [
           ( "free_tvars",
             assert_equal
               ~printer:(fun vs -> [%sexp_of: string list] vs |> Sexp.to_string)
               [ "'Y"; "'X" ]
               (free_tvars (FunType (TypeVar "'X", TypeVar "'Y"))) );
           ( "subst_types_in_type",
             assert_equal ~printer:pp_typ (TypeVar "'X")
               (subst_types_in_type
                  [ ("'X", TypeVar "'Y"); ("'Y", TypeVar "'X") ]
                  (TypeVar "'X")) );
           ( "mk_fresh_var-if-not-taken",
             assert_equal ~printer:Fn.id "'X" (mk_fresh_var [ "'Y" ] "'X") );
           ( "mk_fresh_var-if-taken",
             assert_equal ~printer:Fn.id "'X1" (mk_fresh_var [ "'X" ] "'X") );
           ( "refresh_tfun-1",
             assert_equal ~printer:pp_typ
               (PolyFun ("'X", TypeVar "'X"))
               (refresh_tfun (PolyFun ("'X", TypeVar "'X")) []) );
           ( "refresh_tfun-2",
             assert_equal ~printer:pp_typ
               (PolyFun ("'X", PolyFun ("'X1", TypeVar "'X1")))
               (refresh_tfun (PolyFun ("'X", PolyFun ("'X", TypeVar "'X"))) [])
           );
           ( "refresh_tfun-3",
             assert_equal ~printer:pp_typ
               (FunType
                  (PolyFun ("'X", TypeVar "'X"), PolyFun ("'X", TypeVar "'X")))
               (refresh_tfun
                  (FunType
                     (PolyFun ("'X", TypeVar "'X"), PolyFun ("'X", TypeVar "'X")))
                  []) );
           ( "refresh_tfun-4",
             assert_equal ~printer:pp_typ
               (PolyFun ("'X1", TypeVar "'X1"))
               (refresh_tfun (PolyFun ("'X", TypeVar "'X")) [ "'X" ]) );
           ( "canonicalize_tfun-1",
             assert_equal ~printer:pp_typ
               (PolyFun ("'_A1", PolyFun ("'_A2", TypeVar "'_A2")))
               (canonicalize_tfun
                  (PolyFun ("'X", PolyFun ("'X", TypeVar "'X")))) );
           ( "canonicalize_tfun-2",
             assert_equal ~printer:pp_typ
               (FunType
                  ( PolyFun ("'_A1", TypeVar "'_A1"),
                    PolyFun ("'_A1", TypeVar "'_A1") ))
               (canonicalize_tfun
                  (FunType
                     (PolyFun ("'X", TypeVar "'X"), PolyFun ("'X", TypeVar "'X"))))
           );
           ( "subst_type_in_literal-1",
             assert_equal ~printer:pp_literal
               (IntLit (Int32L (Int32.of_int 42)))
               (subst_type_in_literal (asId "'X")
                  (FunType (int32_typ, int32_typ))
                  (IntLit (Int32L (Int32.of_int 42)))) );
           ( "subst_type_in_literal-2",
             assert_equal ~printer:pp_literal
               (Map ((int32_typ, int32_typ), Caml.Hashtbl.create 4))
               (subst_type_in_literal (asId "'X") int32_typ
                  (Map ((TypeVar "'X", TypeVar "'X"), Caml.Hashtbl.create 4)))
           );
           ( "free_tvars-1",
             assert_equal ~printer:ident_list_printer ~cmp:(List.equal equal_id)
               [ asId "a" ]
               (free_vars_in_expr (parse_expr_wrapper "a")) );
           ( "free_tvars-2",
             assert_equal ~printer:ident_list_printer ~cmp:(List.equal equal_id)
               [ asId "a" ]
               (let expr = "fun (b : Uint32) => a" in
                free_vars_in_expr (parse_expr_wrapper expr)) );
           ( "free_tvars-3",
             assert_equal ~printer:ident_list_printer ~cmp:(List.equal equal_id)
               [ asId "b" ]
               (let expr = "fun (a : Uint32) => b a" in
                free_vars_in_expr (parse_expr_wrapper expr)) );
           ( "free_tvars-4",
             assert_equal ~printer:ident_list_printer ~cmp:(List.equal equal_id)
               [ asId "a" ]
               (let expr =
                  "match a with \n\
                  \                   | Pair a b => a \n\
                  \                   end"
                in
                free_vars_in_expr (parse_expr_wrapper expr)) );
           ( "free_tvars-5",
             assert_equal ~printer:ident_list_printer ~cmp:(List.equal equal_id)
               [ asId "a" ]
               (let expr =
                  "match a with \n\
                  \                   | Pair a b => \n\
                  \                    let c = builtin add a b in\n\
                  \                    c\n\
                  \                   end"
                in
                free_vars_in_expr (parse_expr_wrapper expr)) );
           ( "free_tvars-6",
             assert_equal ~printer:ident_list_printer ~cmp:(List.equal equal_id)
               [ asId "a"; asId "d" ]
               (let expr =
                  "match a with \n\
                  \                   | Pair a b => \n\
                  \                    let c = builtin add a b in\n\
                  \                    d\n\
                  \                   end"
                in
                free_vars_in_expr (parse_expr_wrapper expr)) );
           ( "free_tvars-7",
             assert_equal ~printer:ident_list_printer ~cmp:(List.equal equal_id)
               [ asId "a" ]
               (let expr = "let b = a in\n                   Int32 0" in
                free_vars_in_expr (parse_expr_wrapper expr)) );
         ]

let all_tests _ = "syntax_tests" >::: [ unannotated_syntax_tests ]
