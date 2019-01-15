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

open Stdint

type uint256 = 
  { high : uint128;
    low  : uint128 }

(* Get the lower 64b of a 128b value. *)
let getlow (x : uint128) : uint128 =
  Uint128.shift_right (Uint128.shift_left x 64) 64
 
(* Get the higher 64b of a 128b value. *)
let gethigh (x : uint128) : uint128 =
  Uint128.shift_right x 64

(* join 0A and 0B together into AB *)
let highlow (a : uint128) (b : uint128) : uint128 =
  Uint128.add (Uint128.shift_left a 64) (getlow b)

(* https://stackoverflow.com/questions/1218149/arbitrary-precision-arithmetic-explanation *)
(* https://github.com/calccrypto/uint256_t/blob/master/uint256_t.cpp *)
module Uint256 = struct
  type t = uint256
  let zero = { high = Uint128.zero; low = Uint128.zero }
  let one = { high = Uint128.zero; low = Uint128.one }
  let max_int = { high = Uint128.max_int; low = Uint128.max_int }
  let min_int = zero

  let add a b =
    let low = Uint128.add a.low b.low in
    let carry = 
      if (Uint128.compare low a.low) < 0 || (Uint128.compare low b.low) < 0
        then Uint128.one else Uint128.zero in
    let high = Uint128.add (Uint128.add a.high b.high) carry in
      { high = high; low = low }

  let sub a b =
    let low = Uint128.sub a.low b.low in
    let borrow =
      if (Uint128.compare a.low b.low) < 0
        then Uint128.one else Uint128.zero in
    let high = Uint128.sub (Uint128.sub a.high b.high) borrow in
      { high = high; low = low }

  let shift_left a shift =
    if shift >= 256 then zero else
    if shift = 128 then { high = a.low; low = Uint128.zero } else
    if shift = 0 then a else
    if shift < 128 then 
      let high = Uint128.add 
        (Uint128.shift_left a.high shift)
        (Uint128.shift_right a.low (128 - shift)) in
      let low = Uint128.shift_left a.low shift in
      { high = high; low = low }
    else
      (* shift is greater than 128 *)
      { high = Uint128.shift_left a.low (shift - 128); low = Uint128.zero }

  let shift_right a shift =
    if shift >= 256 then zero else
    if shift = 128 then { high = Uint128.zero; low = a.high } else
    if shift = 0 then a else
    if shift < 128 then
      let high = Uint128.shift_right a.high shift in
      let low = Uint128.add
        (Uint128.shift_left a.high (128-shift))
        (Uint128.shift_right a.low shift) in
      { high = high; low = low }
    else
      (* shift is greater than 128 *)
      { high = Uint128.zero; low = Uint128.shift_right a.high (shift - 128) }

  (* For unsigned, logical and arithmetic right shifts are same. *)
  let shift_right_logical = shift_right

  let logand a b =
    { high = Uint128.logand a.high b.high; low = Uint128.logand a.low b.low }

  let logor a b =
    { high = Uint128.logor a.high b.high; low = Uint128.logor a.low b.low }

  let logxor a b =
    { high = Uint128.logxor a.high b.high; low = Uint128.logxor a.low b.low }

  let lognot a =
    { high = Uint128.lognot a.high; low = Uint128.lognot a.low }

  (* 
   * (a : Uint256) * (b : Uint256) =
   *
   *                      a3   a2   a1   a0   *   b3   b2   b1   b0 
   *                      -----------------------------------------
   *                 c04  t03  t02  t01  t00  ;
   *            c14  t13  t12  t11  t10   X   ;
   *       c24  t23  t22  t21  t20   X    X   ;
   *  c34  t33  t32  t31  t30   X    X    X   ;
   *  --------------------------------------  ;
   *                       r3   r2   r1   r0
  *) 
  let mul a b =
    let a0 = getlow a.low in let a1 = gethigh a.low in
    let a2 = getlow a.high in let a3 = gethigh a.high in
    let b0 = getlow b.low in let b1 = gethigh b.low in
    let b2 = getlow b.high in let b3 = gethigh b.high in
    (* first row *)
    let b0a0 = Uint128.mul b0 a0 in
    let t00 = getlow b0a0 in
    let b0a1 = Uint128.mul b0 a1 in
    let s_b0a1 = Uint128.add (gethigh b0a0) b0a1 in
    let t01 = getlow s_b0a1 in
    let b0a2 = Uint128.mul b0 a2 in
    let s_b0a2 = Uint128.add (gethigh s_b0a1) b0a2 in
    let t02 = getlow s_b0a2 in
    let b0a3 = Uint128.mul b0 a3 in
    let s_b0a3 = Uint128.add (gethigh s_b0a2) b0a3 in
    let t03 = getlow s_b0a3 in
    (* second row *)
    let b1a0 = Uint128.mul b1 a0 in
    let t10 = getlow b1a0 in
    let b1a1 = Uint128.mul b1 a1 in
    let s_b1a1 = Uint128.add (gethigh b1a0) b1a1 in
    let t11 = getlow s_b1a1 in
    let b1a2 = Uint128.mul b1 a2 in
    let s_b1a2 = Uint128.add (gethigh s_b1a1) b1a2 in
    let t12 = getlow s_b1a2 in
    (* third row *)
    let b2a0 = Uint128.mul b2 a0 in
    let t20 = getlow b2a0 in
    let b2a1 = Uint128.mul b2 a1 in
    let s_b2a1 = Uint128.add (gethigh b2a0) b2a1 in
    let t21 = getlow s_b2a1 in
    (* fourth row *)
    let b3a0 = Uint128.mul b3 a0 in
    let t30 = getlow b3a0 in
    (* Now add the rows up *)
    let r0 = t00 in
    let s_r1 = Uint128.add t01 t10 in
    let r1 = getlow s_r1 in
    let s_r2 = Uint128.add (Uint128.add (Uint128.add t02 t11) t20) (gethigh s_r1) in
    let r2 = getlow s_r2 in
    let s_r3 = Uint128.add (Uint128.add (Uint128.add (Uint128.add t03 t12) t21) t30) (gethigh s_r2) in
    let r3 = getlow s_r3 in
      { high = highlow r3 r2; low = highlow r1 r0 }

  (* Set bit i of a. i=0 will set the least significant bit. *)
  let setbit a i =
    logor (shift_left one i) a

  (* Clear bit i of a. i=0 will clear the least significant bit. *)
  let clearbit a i =
    logand (lognot (setbit zero i)) a

  (* Is the bit at position i set?, where i=0 is the least significant bit. *)
  let isset a i =
    compare (setbit a i) a = 0

  (* 
   * https://en.wikipedia.org/wiki/Division_algorithm#Integer_division_(unsigned)_with_remainder
   *
   *  if D = 0 then error(DivisionByZeroException) end
   *  Q := 0                  -- Initialize quotient and remainder to zero
   *  R := 0                     
   *  for i := n − 1 .. 0 do  -- Where n is number of bits in N
   *    R := R << 1           -- Left-shift R by 1 bit
   *    R(0) := N(i)          -- Set the least-significant bit of R equal to bit i of the numerator
   *    if R ≥ D then
   *      R := R − D
   *      Q(i) := 1
   *    end
   *  end
  *)

  let divrem a b =
    if (compare b zero) = 0 then raise Division_by_zero else
    (* If we can do 128b arithmetic, do it, hoping that it may be faster. *)
    if (compare a.high Uint128.zero) = 0 && (compare b.high Uint128.zero) = 0
    then ({ high = Uint128.zero; low = Uint128.div a.low b.low },
          { high = Uint128.zero; low = Uint128.rem a.low b.low })
    else
    (* for i := n − 1 .. 0 *)
    let rec divloop q r i =
      (* loop terminate condition *)
      if i < 0 then (q, r) else
      (* R := R << 1 *)
      let r' = shift_left r 1 in
      (* R(0) := N(i) *)
      let r'' = if isset a i then setbit r' 0 else clearbit r' 0 in
      let (q', r''') = 
        (* if R ≥ D then *)
        if (compare r'' b) >= 0
        (* R := R − D; Q(i) := 1 *)
        then (setbit q i, sub r'' b)
        else (q, r'') in
      (* next iteration *)
      divloop q' r''' (i-1)
    in
      divloop zero zero 255

  let div a b =
    let (q, _) = divrem a b in
      q

  let rem a b =
    let (_, r) = divrem a b in
      r

  let abs a = a

  let neg _ = raise (Failure ("Cannot negate Uint256"))

  let compare a b =
    if Uint128.compare a.high b.high < 0 then -1 else
    if Uint128.compare a.high b.high > 0 then 1 else
    (* compare lower halfs *)
      Uint128.compare a.low b.low

  let of_string s =
    let cl = Extlib.ExtString.String.to_list s in
    List.fold_left (fun i c ->
      let ten = { high = Uint128.zero; low = Uint128.of_string "10" } in
      let m = mul i ten in
      match c with
      | '0' -> m
      | '1' -> add m { high = Uint128.zero; low = Uint128.of_string "1" }
      | '2' -> add m { high = Uint128.zero; low = Uint128.of_string "2" }
      | '3' -> add m { high = Uint128.zero; low = Uint128.of_string "3" }
      | '4' -> add m { high = Uint128.zero; low = Uint128.of_string "4" }
      | '5' -> add m { high = Uint128.zero; low = Uint128.of_string "5" }
      | '6' -> add m { high = Uint128.zero; low = Uint128.of_string "6" }
      | '7' -> add m { high = Uint128.zero; low = Uint128.of_string "7" }
      | '8' -> add m { high = Uint128.zero; low = Uint128.of_string "8" }
      | '9' -> add m { high = Uint128.zero; low = Uint128.of_string "9" }
      | _ -> raise (Failure ("Invalid Uint256 string: " ^ s))
    ) zero cl

  let to_string ui =
    if (compare ui zero) = 0 then "0" else
    let ten = { high = Uint128.zero; low = Uint128.of_string "10" } in
    let c = ["0";"1";"2";"3";"4";"5";"6";"7";"8";"9"] in
    let rec app i s =
      if (compare i zero) = 0 then s else
      let (q,r) = divrem i ten in
      let s' = app q s in
      let d = Uint128.to_int r.low in
        s' ^ (List.nth c d)
    in
      app ui ""

  let to_bytes_big_endian ui buf off =
    let _ = Uint128.to_bytes_big_endian ui.high buf off in
    let _ = Uint128.to_bytes_big_endian ui.low buf (off+16) in
    ()

  let to_bytes_little_endian ui buf off =
    let _ = Uint128.to_bytes_little_endian ui.low buf off in
    let _ = Uint128.to_bytes_little_endian ui.high buf (off+16) in
    ()

  let of_bytes_big_endian buff off =
    {
      high = Uint128.of_bytes_big_endian buff off;
      low = Uint128.of_bytes_big_endian buff (off+16)
    }

  let of_bytes_little_endian buff off =
    {
      low = Uint128.of_bytes_little_endian buff off;
      high = Uint128.of_bytes_little_endian buff (off+16)
    }

end

(*  https://github.com/andrenth/ocaml-stdint/blob/master/lib/int128_stubs.c *)
module Int256 = struct
  type t = uint256

  let zero = { high = Uint128.zero; low = Uint128.zero }
  let one = { high = Uint128.zero; low = Uint128.one }
  let max_int = Uint256.clearbit Uint256.max_int 255
  let min_int = Uint256.setbit zero 255

  let add a b =
    (* 2s complement will take care ! *)
    Uint256.add a b

  let sub a b =
    (* 2s complement will take care ! *)
    Uint256.sub a b

  let shift_left a shift =
    Uint256.shift_left a shift

  (* For signed, logical and arithmetic right shifts are different. *)
  let shift_right _ _ =
    raise (Failure "Int256: shift_right not implemented")

  let shift_right_logical a shift =
    Uint256.shift_right a shift

  let logand a b =
    Uint256.logand a b

  let logor a b =
    Uint256.logor a b

  let logxor a b =
    Uint256.logxor a b

  let lognot a =
    Uint256.lognot a

  let mul a b =
    (* 2s complement will take care ! *)
    Uint256.mul a b

  (* Set bit i of a. i=0 will set the least significant bit. *)
  let setbit a i =
    Uint256.setbit a i

  (* Clear bit i of a. i=0 will clear the least significant bit. *)
  let clearbit a i =
    Uint256.clearbit a i

  (* Is the bit at position i set?, where i=0 is the least significant bit. *)
  let isset a i =
    Uint256.isset a i
  
  let isneg a =
    isset a 255

  let neg a =
    (* take 2s complement *)
    add (lognot a) one

  let divrem x y =
    if isneg x then
      let x' = neg x in
      if isneg y then
        let y' = neg y in
        let (q, r) = Uint256.divrem x' y' in
        (q, neg r)
      else
        let (q, r) = Uint256.divrem x' y in
        (neg q, neg r)
    else
      if isneg y then
        let y' = neg y in
        let (q, r) = Uint256.divrem x y' in
        (neg q, r)
      else
        Uint256.divrem x y

  let div a b =
    let (q, _) = divrem a b in
      q

  let rem a b =
    let (_, r) = divrem a b in
      r

  let abs a =
    if isneg a then neg a else a

  let compare a b =
    if isneg a && not (isneg b) then -1
    else if not (isneg a) && isneg b then 1
    else Uint256.compare a b

  let of_string s =
    let (s', n) = 
      if String.get s 0 = '-'
           (* s' = s without the leading '-' *)
      then (String.sub s 1 ((String.length s) - 1), true)
      else (s, false)
    in
    let i = 
      try
        Uint256.of_string s'
      with
      | _ -> raise (Failure ("Invalid Int256 string: " ^ s))
      in
      if isneg i && i <> min_int then
        (* if i is negative, then the number is too big.
         * It can't be represented in 255 bits.
         * Unless it's min_int. 2s complement has one extra 
         * negative number representable, than number of positives. *)
        raise (Failure ("Invalid Int256 string: " ^ s))
      else
        if n then neg i else i

  let to_string i =
    if isneg i then "-" ^ Uint256.to_string (neg i)
    else Uint256.to_string i

  let to_bytes_big_endian i buf off =
    Uint256.to_bytes_big_endian i buf off

  let to_bytes_little_endian i buf off =
    Uint256.to_bytes_little_endian i buf off

  let of_bytes_big_endian buf off =
    Uint256.of_bytes_big_endian buf off

  let of_bytes_little_endian buf off =
    Uint256.of_bytes_little_endian buf off

end

type int256 = Int256.t
