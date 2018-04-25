(***************************************************)
(*               Associated library                *)
(***************************************************)
library Crowdfunding

let andb = 
  fun (b : Bool) => fun (c : Bool) =>
    match b with 
    | False => False
    | True  => match c with 
               | False => False
               | True  => True
               end
    end

let orb = 
  fun (b : Bool) => fun (c : Bool) =>
    match b with 
    | True  => True
    | False => match c with 
               | False => False
               | True  => True
               end
    end

let negb = fun (b : Bool) => 
  match b with
  | True => False
  | False => True
  end

let blk_leq =
  fun (blk1 : bnum) => (blk2 : bnum) =>
  let bc1 = builtin blt blk1 blk2_block in 
  let bc2 = builtin eq blk1 blk2 in 
  orb bc1 bc2

let one_msg = 
  fun (msg : message) => 
   let nil_msg = @Nil {message} in
   Cons {message} msg nil_msg

let check_update = 
  fun (bs : map address nat) =>
    fun (sender : address) =>
      fun (amount : nat) =>
  let c = builtin contains bs sender in
  match c with 
  | False => 
    let bs1 = builtin put bs sender amount in
    Some bs1 
  | True  => None 

let nat_leq = 
  fun (n : nat) => fun (m : nat) => 
    let i1 = builtin toInt n in
    let i2 = builtin toInt m in 
    let b1 = builtin lt i1 i2 in
    let b2 = builtin eq i1 i2 in
    orb b1 b2 

let accepted_code = 1
let missed_deadline_code = 2
let already_backed_code  = 3
let not_owner_code  = 4
let too_early_code  = 5
let get_funds_code  = 6
  
(***************************************************)
(*             The contract definition             *)
(***************************************************)
contract Crowdfunding

(*  Parameters *)
 (owner     : address,
  max_block : bnum,
  goal      : int)

(* Mutable fields *)
field backers : map address nat = Emp;
field funded : bool = False;

transition Donate (sender: address, amount: nat)
  blk <- & Blk_number;
  in_time = blk_leq blk max_block;
  match in_time with 
  | True  => 
    bs  <- backers;
    res = check_update bs sender amount;
    match res with
    | None => 
      msg  = {tag : Main; to : sender; amount : 0; 
              code : missed_deadline_code};
      msgs = one_msg msg;
      send msgs
    | Some bs1 =>
      backers := bs1; 
      accept amount; 
      msg  = {tag : Main; to : sender; amount : 0; 
              code : accepted_code};
      msgs = one_msg msg;
      send msgs     
     end  
  | False => 
    msg  = {tag : Main; to : sender; amount : 0; 
            code : already_backed_code};
    msgs = one_msg msg;
    send msgs
  end 
end

transition GetFunds (sender: address)
  is_owner = builtin eq is_owner sender;
  match is_owner with
  | False => 
    msg  = {tag : Main; to : sender; amount : 0; 
            code : not_owner_code};
    msgs = one_msg msg;
    send msgs
  | True => 
    blk <- & Blk_number;
    in_time = blk_leq blk max_block;
    after_deadline = negb in_time;
    match after_deadline with 
    | False =>  
      msg  = {tag : Main; to : sender; amount : 0; 
              code : too_early_code};
      msgs = one_msg msg;
      send msgs
    | True => 
      (* Allow to withdraw independently of a goal *)
      bal <- balance;
      funded := True;
      msg  = {tag : Main; to : owner; amount : bal; 
              code : get_funds_code};
      msgs = one_msg msg;
      send msgs
    end
  end   
end

(* transition ClaimBack *)
