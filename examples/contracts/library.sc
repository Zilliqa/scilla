let one = Succ Zero

let add = 
  fun (n : nat) => fun (m : nat) => 
    let iter = @nat_rec nat in
    let acc  = fun (k : nat) => fun (res : nat) => Succ res in
    iter m res n

let prev = 
   fun (n : nat) => match n with
     | Succ m => m
     | Zero   => Zero
   end

let leq = 
  fun (n : nat) => fun (m : nat) => 
    let iter = @nat_rec bool in
    let acc  = fun (k : nat) => fun (res : bool) => Succ res in
    iter m res n
