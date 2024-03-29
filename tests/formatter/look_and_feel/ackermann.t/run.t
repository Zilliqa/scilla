  $ scilla-fmt ackermann.scilexp
  (* some helper functions one would hope to find in stdlib *)
  let nat_succ : Nat -> Nat =
    fun (n : Nat) =>
      Succ n
  in
  (* [nat_iter 'A f n] = f^n -- functional power operator *)
  let nat_iter : forall 'A. ('A -> 'A) -> Nat -> 'A -> 'A =
    tfun 'A =>
      fun (f : 'A -> 'A) =>
        fun (n : Nat) =>
          fun (x0 : 'A) =>
            let folder = @nat_fold ('A) in
            let step =
              fun (x : 'A) =>
                fun (unused : Nat) =>
                  f x
            in
            folder step x0 n
  in
  let ackermann : Nat -> Nat -> Nat =
    let iter_nat = @nat_iter (Nat) in
    let iter_nat_nat = @nat_iter (Nat -> Nat) in
    let zero = Zero in
    let one = Succ zero in
    let f =
      fun (ack : Nat -> Nat) =>
        let x0 = ack one in
        fun (n : Nat) =>
          iter_nat ack n x0
    in
    fun (n : Nat) =>
      iter_nat_nat f n nat_succ
  in
  (* tests *)
  let uint0 = Uint32 0 in
  let uint1 = Uint32 1 in
  let uint2 = Uint32 2 in
  let uint3 = Uint32 3 in
  let uint4 = Uint32 4 in
  let uint5 = Uint32 5 in
  let uint6 = Uint32 6 in
  let uint7 = Uint32 7 in
  let uint9 = Uint32 9 in
  let uint11 = Uint32 11 in
  let uint13 = Uint32 13 in
  let uint29 = Uint32 29 in
  let uint61 = Uint32 61 in
  let uint125 = Uint32 125 in
  let n0 = builtin to_nat uint0 in
  let n1 = builtin to_nat uint1 in
  let n2 = builtin to_nat uint2 in
  let n3 = builtin to_nat uint3 in
  let n4 = builtin to_nat uint4 in
  let n5 = builtin to_nat uint5 in
  let n6 = builtin to_nat uint6 in
  let n7 = builtin to_nat uint7 in
  let n9 = builtin to_nat uint9 in
  let n11 = builtin to_nat uint11 in
  let n13 = builtin to_nat uint13 in
  let n29 = builtin to_nat uint29 in
  let n61 = builtin to_nat uint61 in
  let n125 = builtin to_nat uint125 in
  let ack00 = ackermann n0 n0 in
  let ack01 = ackermann n0 n1 in
  let ack02 = ackermann n0 n2 in
  let ack03 = ackermann n0 n3 in
  let ack04 = ackermann n0 n4 in
  let ack10 = ackermann n1 n0 in
  let ack11 = ackermann n1 n1 in
  let ack12 = ackermann n1 n2 in
  let ack13 = ackermann n1 n3 in
  let ack14 = ackermann n1 n4 in
  let ack20 = ackermann n2 n0 in
  let ack21 = ackermann n2 n1 in
  let ack22 = ackermann n2 n2 in
  let ack23 = ackermann n2 n3 in
  let ack24 = ackermann n2 n4 in
  let ack30 = ackermann n3 n0 in
  let ack31 = ackermann n3 n1 in
  let ack32 = ackermann n3 n2 in
  let ack33 = ackermann n3 n3 in
  let ack34 = ackermann n3 n4 in
  let ack40 = ackermann n4 n0 in
  True
