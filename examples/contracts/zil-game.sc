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

The language should provide access to the following blockchain parameters
NUMBER      Get the last mined block's number
BLOCKHASH   Get the hash of the last block 
TIMESTAMP   Get the last block's timestamp
*)


(***************************************************)
(*               Associated library                *)
(***************************************************)
library ZilGame

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

let one_msg = 
  fun (msg : message) => 
   let nil_msg = Nil {message} in
   Cons {message} msg nil_msg

let no_msg = Nil {message}

let update_hash = 
  fun (oh : option hash) =>
  fun (h : hash) =>
  match oh with
  | Some x => Some x
  | None   => Some h
  end

let update_timer = 
  fun (tm : option bnum) =>
  fun (b : bnum) =>
  match tm with
  | Some x => Some x
  | None   => 
    let b1 = builtin badd b 11 in
    Some b1
  end

(* b is within the time window *)
let can_play = 
  fun (tm : option bnum) =>
  fun (b : bnum) =>
  match tm with
  | None => True
  | Some b1 =>
    builtin blt b b1
  end     

let time_to_claim = 
  fun (tm : option bnum) =>
  fun (b : bnum) =>
  match tm with
  | None => False
  | Some b1 =>
    let c1 = builtin blt b b1 in
    negb c1
  end     

let check_validity = 
  fun (a        : address) =>
  fun (solution : int) =>
  fun (pa       : address) =>
  fun (pb       : address) =>
  fun (guess_a  : option hash) =>
  fun (guess_b  : option hash) =>
  let ca = builtin eq pa a in
  let cb = builtin eq pb a in
  let xa = And ca guess_a in 
  let xb = And cb guess_b in 
  match xa with
  | And True (Some g) =>
    let h = builtin hash solution in
    builtin eq h g 
  | _ =>
    match xb with
    | And True (Some g) =>
      let h = builtin sha256_hash solution in
      builtin eq h g 
  | False => False

(* In the case of equali results, or no results *)
(* the prise goes to the owner *)
let determine_winner = 
  fun (puzzle   : hash) =>
  fun (guess_a  : option hash) =>
  fun (guess_b  : option hash) =>
  fun (pa       : address) =>
  fun (pb       : address) =>
  fun (oa       : address) =>
  let gab = And guess_a guess_b in
  match gab with
  | And (Some ga) (Some gb) =>
    let d1 = builtin dist puzzle ga in
    let d2 = builtin dist puzzle gb in
    let c1 = builtin lt d1 d2 in
    match c1 with 
    | True => pa
    | False => 
      let c2 = builtin eq d1 d2 in
      match c2 with 
      | False => pb
      | True  => oa
      end
    end
  | And (Some _) None => pa
  | And None (Some _) => pb
  | And None None     => oa
  end

let solution_submitted = 1
let time_window_missed = 2
let not_a_player = 3  
let too_early_to_claim = 4
let wrong_sender_or_solution = 5
let here_is_the_reward = 6

(***************************************************)
(*             The contract definition             *)
(***************************************************)
contract ZilGame 
  (owner    : address,
   player_a : address,
   player_b : address,
   puzzle   : hash)

(* Initial balance is not stated explicitly:   *)
(* it's initialized when creating the contract *)

field player_a_hash : option hash = None
field player_b_hash : option hash = None
field timer         : option bum  = None
field game_on       : bool = False

transition Play
  (sender: address, guess: hash)
  tm_opt <- timer;
  b <- & NUMBER;
  (* Check the timer *)
  match can_play tm_opt b with
  | False => 
      msg  = {tag : Main; to : sender; amount : 0; 
              code : time_window_missed};
      msgs = one_msg msg;
      send msgs        
  | True  => 
    isa = builtin eq sender player_a;
    isb = builtin eq sender player_b;
    match isa with
    | True => 
      game_on := True;
      ah <- player_a_hash;
      hopt = update_hash ah guess;
      player_a_hash := hopt;
      tm1 = update_timer tm_opt b;
      timer := tm1;
      msg  = {tag : Main; to : sender; amount : 0; 
              code : solution_submitted};
      msgs = one_msg msg;
      send msgs        
    | False =>
      match isb with 
      | True =>
        game_on := True;
        bh <- player_b_hash;
        hopt = update_hash bh guess;
        player_b_hash := hopt
        tm1 = update_timer tm_opt b;
        timer := tm1;
        msg  = {tag : Main; to : sender; amount : 0; 
                code : solution_submitted};
        msgs = one_msg msg;
        send msgs        
      | False => 
        msg  = {tag : Main; to : sender; amount : 0; 
                code : not_a_player};
        msgs = one_msg msg;
        send msgs        
    end
  end
end

(* 
Each player can try to reclaim their reward. This transition will
check eligibility, solution quality, and the hash pre-image submitted
(an integer value) and then will send the reward, ending the game
*)
transition ClaimReward
  (sender: address, solution: int)
  tm_opt <- timer;
  b <- & NUMBER;
  (* Check the timer *)
  match time_to_claim tm_opt b with
  | False => 
      msg  = {tag : Main; to : sender; amount : 0; 
              code : too_early_to_claim};
      msgs = one_msg msg;
      send msgs        
  | True  => 
    pa <- player_a_hash;
    pb <- player_b_hash;
    is_valid = check_validity sender solution player_a player_b pa pb;
    match is_valid with
    | False =>
      msg  = {tag : Main; to : sender; amount : 0; 
              code : wrong_sender_or_solution};
      msgs = one_msg msg;
      send msgs        
    | True  =>
      winner = determine_winner puzzle pa pb player_a player_b owner; 
      bal <- & BALANCE;
      msg  = {tag : Main; to : winner; amount : bal; 
              code : here_is_the_reward};
      game_on := False;
      msgs = one_msg msg;
      send msgs
      end     
    end
  end   
end


