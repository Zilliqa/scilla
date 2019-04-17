open Core
open Stdint
open OUnit2
open SafeArith

(* We test by comparing the result of the safe arithmetic operations
 * on small integer types with the result of non-wrapping (precise)
 * int operations.
 * An implicit assumption here is that native int type can fit
 * any result of SmallInt operations, including multiplication.
 *)

module TestArith (SmallInt : Int) = struct

  type arith_error =
    | Overflow
    | Underflow
    | Div_by_zero
  [@@deriving sexp]

  let min_small_int = SmallInt.(to_int min_int)
  let max_small_int = SmallInt.(to_int max_int)

  let test binop big_binop a_int b_int =
    let open SmallInt in
    let actual =
      let a = of_int a_int in
      let b = of_int b_int in
      try
        Ok (binop a b)
      with
        | IntOverflow -> Error Overflow
        | IntUnderflow -> Error Underflow
        | Division_by_zero -> Error Div_by_zero
    in
    let expected =
      try
        let exp = big_binop a_int b_int in
        if exp < min_small_int then Error Underflow
        else if exp > max_small_int then Error Overflow
        else Ok (of_int exp)
      with Division_by_zero -> Error Div_by_zero
    in
    let pp_diff fmt _ =
      Format.pp_print_string fmt (sprintf "a = %d" a_int);
      Format.pp_force_newline fmt ();
      Format.pp_print_string fmt (sprintf "b = %d" b_int)
    in
    assert_equal expected actual ~pp_diff
      ~printer:(fun v ->
         v |>
         Result.sexp_of_t
           (Fn.compose Int.sexp_of_t SmallInt.to_int)
           sexp_of_arith_error
         |> Sexp.to_string)

  let test_all binop big_binop =
    for a_int = min_small_int to max_small_int do
      for b_int = min_small_int to max_small_int do
        test binop big_binop a_int b_int
      done
    done
end

module I8_safe = SafeInt(Int8)
module TestSigned = TestArith(Int8)

module U8_safe = SafeUint(Uint8)
module TestUnsigned = TestArith(Uint8)

let builtin_arith_8bit_tests =
  let open Base in
  "test suite for 8-bit builtin arithmetic binary operations" >:::
  List.map ~f:(fun (name, assertion) -> name >:: Fn.const assertion) [
    "signed 8-bit: safe add",
    TestSigned.test_all I8_safe.add Int.( + );
    "signed 8-bit: safe sub",
    TestSigned.test_all I8_safe.sub Int.( - );
    "signed 8-bit: safe mul",
    TestSigned.test_all I8_safe.mul Int.( * );
    "signed 8-bit: safe div",
    TestSigned.test_all I8_safe.div Int.( / );
    "signed 8-bit: safe rem",
    TestSigned.test_all I8_safe.rem Int.rem;

    "unsigned 8-bit: safe add",
    TestUnsigned.test_all U8_safe.add Int.( + );
    "unsigned 8-bit: safe sub",
    TestUnsigned.test_all U8_safe.sub Int.( - );
    "unsigned 8-bit: safe mul",
    TestUnsigned.test_all U8_safe.mul Int.( * );
    "unsigned 8-bit: safe div",
    TestUnsigned.test_all U8_safe.div Int.( / );
    "unsigned 8-bit: safe rem",
    TestUnsigned.test_all U8_safe.rem Int.rem;
  ]

let arith_builtin_tests = "arith_builtin_tests" >::: [builtin_arith_8bit_tests]
