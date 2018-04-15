(*
ZilGame contract implements the following game:

1. Two players, A and B submit hashes of some integer values without
   revealing the pre-images.

2. After the first player has submitted, the timer starts for the second
   one, so the submission can be only made within 5 (or so) blocks
   after the first one, to guarantee some fairness. The second
   submitted solution is rejected is he missed the deadline.

3. Each of the players can then try to claim their reward via the
   ClaimReward transition. The winner is determined based on how close
   the solution is to the puzzle (wee need distance on hashes).
   The players must submit their pre-images to claim the reward, and
   it must match. If the player who submitted second failed to submit,
   the winner is the first one automatically.
 
Question: what primitive should we have in the language to check the
current block number? Or should it be the previous block number? What
is reasonable here?

*)


(***************************************************)
(*               Associated library                *)
(***************************************************)
library ZilGame

(*  Here come definitions of auxiliary functions  *)

let orb = 
  fun (b : Bool) => fun (c : Bool) =>
    match b with 
    | True  => True
    | False => match c with 
               | False => False
               | True  => True
               end
    end

let one_msg = 
  fun (msg : message) => 
   let nil_msg = Nil {message} in
   Cons {message} msg nil_msg

let no_msg = Nil {message}

let check_sender 
  fun (s : address) =>
  fun (a : address) =>
  fun (b : address) =>
  let b1 = builtin eq s a in
  let b2 = builtin eq s b in
  orb b1 b2

(***************************************************)
(*             The contract definition             *)
(***************************************************)
contract ZilGame 
  (player_a : address,
   player_b : address,
   puzzle   : hash)

(* Initial balance is not stated explicitly:   *)
(* it's initialized when creating the contract *)

field player_a_hash : option hash = None
field player_b_hash : option hash = None
field timer         : option (pair int bnum) = None
field game_on     : bool = False

transition Play
  (sender: address, guess: hash)
  let is_player = check_sender in
  match is_player with
  | False => send no_msg
  | True  => 
    (* Implement updating routine and determine the player *)
    (* Determine whether can submit *)
  end


(* Players can submit their guesses *)

end

(* 
Each player can try to reclaim their reward. This transition will
check eligibility, solution quality, and the hash pre-image submitted
(an integer value) and then will send the reward, ending the game
*)
transition ClaimReward
  (sender: address, solution: int)
  pa <- player_a_hash;
  pb <- player_b_hash;
  (* TODO: implement me *)
  winner = determine_winner puzzle solution pa pb; 
  bal <- balance;
  (* TODO: implement me *)
  msgs_done = form_msg winner bal game_on sender;
  match msgs_done with
  | And False msgs =>
    game_on := False;
    send msgs
  | And True _ => send no_msg
  end     
end


