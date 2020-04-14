open Base

(* No need to open! Int.Replace_polymorphic_compare
   because open Base already disables polymorphic comparisons except for ints *)
open Stdint

exception IntOverflow

exception IntUnderflow

(*******************************************************)
(* Manipulating with arbitrary integer representations *)
(*******************************************************)

module type IntRep = sig
  type t

  val compare : t -> t -> int

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val rem : t -> t -> t

  val shift_right : t -> int -> t

  val zero : t

  val one : t

  val min_int : t

  val max_int : t
end

module SafeInt (Unsafe : IntRep) = struct
  let add a b =
    let open Unsafe in
    let r = add a b in
    (* if a > 0 && b > 0 && r < 0 then we have an overflow*)
    if compare a zero > 0 && compare b zero > 0 && compare r zero < 0 then
      raise IntOverflow
      (* if a < 0 && b < 0 && r >= 0 then we have an underflow *)
      (* r = 0 is possible in the case of a = b = min_int *)
    else if compare a zero < 0 && compare b zero < 0 && compare r zero >= 0 then
      raise IntUnderflow
    else r

  let sub a b =
    let open Unsafe in
    let r = sub a b in
    (* if a >= 0 && b < 0 && r < 0 then we have an overflow *)
    (* the corner case here is a = 0, b = min_int *)
    if compare a zero >= 0 && compare b zero < 0 && compare r zero < 0 then
      raise IntOverflow
      (* if a < 0 && b > 0 && r > 0 then we have an underflow *)
    else if compare a zero < 0 && compare b zero > 0 && compare r zero > 0 then
      raise IntUnderflow
    else r

  let mul a b =
    let open Unsafe in
    let r = mul a b in
    (* http://www.informit.com/articles/article.aspx?p=1959565&seqNum=13 *)
    (* if b < 0 && a = int_min OR if b != 0 && r / b != a *)
    if compare b zero < 0 && compare a min_int = 0 then raise IntOverflow
    else if compare b zero <> 0 && compare (div r b) a <> 0 then
      (* sign a = sign b ? *)
      if compare a zero = compare b zero then raise IntOverflow
      else raise IntUnderflow
    else r

  let div a b =
    let open Unsafe in
    (* Integer overflow during division occurs in a very specific case. *)
    (* https://stackoverflow.com/a/30400252/2128804 *)
    if compare a min_int = 0 && compare b (sub zero one) = 0 then
      raise IntOverflow
    else
      (* Division_by_zero is taken care of by underlying implementation. *)
      div a b

  (* Division_by_zero is taken care of by underlying implementation. *)
  let rem = Unsafe.rem

  let pow a b =
    let rec pow_aux acc b' =
      if Uint32.compare b' Uint32.zero = 0 then acc
      else pow_aux (mul a acc) (Uint32.pred b')
    in
    pow_aux Unsafe.one b

  let lt a b = Unsafe.compare a b < 0
end

module SafeUint (Unsafe : IntRep) = struct
  let add a b =
    let open Unsafe in
    let r = add a b in
    (* if r < a || r < b then we have an overflow *)
    if compare r a < 0 || compare r b < 0 then raise IntOverflow else r

  let sub a b =
    let open Unsafe in
    let r = sub a b in
    (* if a < b then we have an underflow *)
    if compare a b < 0 then raise IntUnderflow else r

  let mul a b =
    let open Unsafe in
    let r = mul a b in
    (* if b != 0 && r / b != a *)
    if compare b zero <> 0 && compare (div r b) a <> 0 then raise IntOverflow
    else r

  (* Division_by_zero is taken care of by underlying implementation. *)
  let div = Unsafe.div

  (* Division_by_zero is taken care of by underlying implementation. *)
  let rem = Unsafe.rem

  let pow a b =
    let rec pow_aux acc b' =
      if Uint32.compare b' Uint32.zero = 0 then acc
      else pow_aux (mul a acc) (Uint32.pred b')
    in
    pow_aux Unsafe.one b

  let lt a b = Unsafe.compare a b < 0

  let isqrt n =
    let div2 m = Unsafe.shift_right m 1 in
    (* https://math.stackexchange.com/a/2469503/167002 *)
    let rec recurser x y =
      (* if x <= y then x *)
      if Unsafe.compare x y <= 0 then x
      else
        let x' =
          (* Checks against overflow *)
          let saturating_add a b =
            let open Unsafe in
            let r = add a b in
            (* if r < a || r < b then we have an overflow *)
            if compare r a < 0 || compare r b < 0 then max_int else r
          in
          (* (x + y) / 2 *)
          div2 (saturating_add x y)
        in
        let y' = div n x' in
        recurser x' y'
    in
    recurser n Unsafe.one
end
