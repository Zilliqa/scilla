open Core
open Stdint
open OUnit2
open Syntax
open PrimTypes
open PrettyPrinters

let unannotated_syntax_tests =
  "test suite for unannotated syntax" >:::
  List.map ~f:(fun (name, assertion) -> name >:: const assertion) [
    "free_tvars",
    assert_equal ~printer:(fun vs -> [%sexp_of: string list] vs |> Sexp.to_string)
      ["'Y"; "'X"]
      (free_tvars (FunType ((TypeVar "'X"), (TypeVar "'Y"))));

    "subst_types_in_type",
    assert_equal ~printer:pp_typ
      (TypeVar "'X")
      (subst_types_in_type ["'X", TypeVar "'Y"; "'Y", TypeVar "'X"] (TypeVar "'X"));

    "mk_fresh_var-if-not-taken",
    assert_equal ~printer:Fn.id
      "'X"
      (mk_fresh_var ["'Y"] "'X");
    "mk_fresh_var-if-taken",
    assert_equal ~printer:Fn.id
      "'X1"
      (mk_fresh_var ["'X"] "'X");

    "refresh_tfun-1",
    assert_equal ~printer:pp_typ
      (PolyFun ("'X", TypeVar "'X"))
      (refresh_tfun (PolyFun ("'X", TypeVar "'X")) []);
    "refresh_tfun-2",
    assert_equal ~printer:pp_typ
      (PolyFun ("'X", PolyFun ("'X1", TypeVar "'X1")))
      (refresh_tfun (PolyFun ("'X", PolyFun ("'X", TypeVar "'X"))) []);
    "refresh_tfun-3",
    assert_equal ~printer:pp_typ
      (FunType (PolyFun ("'X", TypeVar "'X"), (PolyFun ("'X", TypeVar "'X"))))
      (refresh_tfun (FunType (PolyFun ("'X", TypeVar "'X"), (PolyFun ("'X", TypeVar "'X")))) []);

    "canonicalize_tfun-1",
    assert_equal ~printer:pp_typ
      (PolyFun ("'_A1", PolyFun ("'_A2", TypeVar "'_A2")))
      (canonicalize_tfun (PolyFun ("'X", PolyFun ("'X", TypeVar "'X"))));
    "canonicalize_tfun-2",
    assert_equal ~printer:pp_typ
      (FunType (PolyFun ("'_A1", TypeVar "'_A1"), (PolyFun ("'_A1", TypeVar "'_A1"))))
      (canonicalize_tfun (FunType (PolyFun ("'X", TypeVar "'X"), (PolyFun ("'X", TypeVar "'X")))));

    "subst_type_in_literal-1",
    assert_equal ~printer:pp_literal
      (IntLit (Int32L (Int32.of_int 42)))
      (subst_type_in_literal (asId "'X") (FunType (int32_typ, int32_typ)) (IntLit (Int32L (Int32.of_int 42))));
    "subst_type_in_literal-2",
    assert_equal ~printer:pp_literal
      (Map ((int32_typ, int32_typ), Caml.Hashtbl.create 4))
      (subst_type_in_literal (asId "'X") int32_typ (Map ((TypeVar "'X", TypeVar "'X"), Caml.Hashtbl.create 4)))
  ]

let syntax_tests = "syntax_tests" >::: [unannotated_syntax_tests]
