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
open Stdint
open OUnit2
open Scilla_base
open SafeArith

(* We test by comparing the result of the safe arithmetic operations
 * on small integer types with the result of non-wrapping (precise)
 * int operations.
 * An implicit assumption here is that native int type can fit
 * any result of SmallInt operations, including multiplication.
 *)

module TestArith (SmallInt : Int) = struct
  type arith_error = Overflow | Underflow | Div_by_zero [@@deriving sexp]

  let min_small_int = SmallInt.(to_int min_int)

  let max_small_int = SmallInt.(to_int max_int)

  let test binop big_binop a_int b_int =
    let open SmallInt in
    let actual =
      let a = of_int a_int in
      let b = of_int b_int in
      try Ok (binop a b) with
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
    assert_equal expected actual ~pp_diff ~printer:(fun v ->
        v
        |> Result.sexp_of_t
             (Fn.compose Int.sexp_of_t SmallInt.to_int)
             sexp_of_arith_error
        |> Sexp.to_string)

  let test_all binop big_binop =
    for a_int = min_small_int to max_small_int do
      for b_int = min_small_int to max_small_int do
        test binop big_binop a_int b_int
      done
    done

  let test_all_pow pow_op =
    let open Base in
    (* for integer `**` power function *)
    for a_int = 0 to 2 do
      for b_int = 0 to 6 do
        let b32 = Uint32.of_int b_int in
        let actual = SmallInt.to_int (pow_op (SmallInt.of_int a_int) b32) in
        let expected = a_int ** b_int in
        assert_bool
          (sprintf "Got pow %d %d = %d, but expected %d" a_int b_int actual
             expected)
          (expected = actual)
      done
    done

  let test_all_isqrt isqrt_op =
    for a = min_small_int to max_small_int do
      let asqrt = SmallInt.to_int @@ isqrt_op (SmallInt.of_int a) in
      (* asqrt * asqrt <= a && asqrt+1 * asqrt+1 > a *)
      assert_bool
        (sprintf "Got isqrt %d = %d" a asqrt)
        (asqrt * asqrt <= a && (asqrt + 1) * (asqrt + 1) > a)
    done
end

module I8_safe = SafeInt (Int8)
module TestSigned = TestArith (Int8)
module U8_safe = SafeUint (Uint8)
module TestUnsigned = TestArith (Uint8)

let builtin_arith_8bit_tests =
  let open Base in
  "test suite for 8-bit builtin arithmetic binary operations"
  >::: List.map
         ~f:(fun (name, assertion) -> name >:: Fn.const assertion)
         [
           ("signed 8-bit: safe add", TestSigned.test_all I8_safe.add Int.( + ));
           ("signed 8-bit: safe sub", TestSigned.test_all I8_safe.sub Int.( - ));
           ("signed 8-bit: safe mul", TestSigned.test_all I8_safe.mul Int.( * ));
           ("signed 8-bit: safe div", TestSigned.test_all I8_safe.div Int.( / ));
           ("signed 8-bit: safe rem", TestSigned.test_all I8_safe.rem Int.rem);
           ("signed 8-bit: safe pow", TestSigned.test_all_pow I8_safe.pow);
           ( "unsigned 8-bit: safe add",
             TestUnsigned.test_all U8_safe.add Int.( + ) );
           ( "unsigned 8-bit: safe sub",
             TestUnsigned.test_all U8_safe.sub Int.( - ) );
           ( "unsigned 8-bit: safe mul",
             TestUnsigned.test_all U8_safe.mul Int.( * ) );
           ( "unsigned 8-bit: safe div",
             TestUnsigned.test_all U8_safe.div Int.( / ) );
           ( "unsigned 8-bit: safe rem",
             TestUnsigned.test_all U8_safe.rem Int.rem );
           ("unsigned 8-bit: isqrt", TestUnsigned.test_all_isqrt U8_safe.isqrt);
           ("unsigned 8-bit: safe pow", TestUnsigned.test_all_pow U8_safe.pow);
         ]

module All = struct
  let tests _ = "arith_builtin_tests" >::: [ builtin_arith_8bit_tests ]
end
