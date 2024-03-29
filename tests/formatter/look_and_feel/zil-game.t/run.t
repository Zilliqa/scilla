  $ scilla-fmt zil-game.scilla
  scilla_version 0
  (***************************************************)
  (*               Associated library                *)
  (***************************************************)
  
  import BoolUtils
  
  library ZilGame
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  let no_msg = Nil {(Message)}
  
  let hash_dist =
    fun (h0 : ByStr32) =>
      fun (h1 : ByStr32) =>
        let h00 = builtin to_uint256 h0 in
        let h11 = builtin to_uint256 h1 in
        let lt = builtin lt h00 h11 in
        match lt with
        | False => builtin sub h00 h11
        | True => builtin sub h11 h00
        end
  
  let update_hash =
    fun (oh : Option ByStr32) =>
      fun (h : ByStr32) =>
        match oh with
        | Some x => Some {(ByStr32)} x
        | None => Some {(ByStr32)} h
        end
  
  let update_timer =
    fun (tm : Option BNum) =>
      fun (b : BNum) =>
        match tm with
        | Some x => Some {(BNum)} x
        | None =>
          let window = Uint32 11 in
          let b1 = builtin badd b window in
          Some {(BNum)} b1
        end
  
  (* b is within the time window *)
  let can_play =
    fun (tm : Option BNum) =>
      fun (b : BNum) =>
        match tm with
        | None => True
        | Some b1 => builtin blt b b1
        end
  
  let time_to_claim =
    fun (tm : Option BNum) =>
      fun (b : BNum) =>
        match tm with
        | None => False
        | Some b1 => let c1 = builtin blt b b1 in negb c1
        end
  
  let check_validity =
    fun (a : ByStr20) =>
      fun (solution : Int128) =>
        fun (pa : ByStr20) =>
          fun (pb : ByStr20) =>
            fun (guess_a : Option ByStr32) =>
              fun (guess_b : Option ByStr32) =>
                let ca = builtin eq pa a in
                let cb = builtin eq pb a in
                let xa = Pair {(Bool) (Option ByStr32)} ca guess_a in
                let xb = Pair {(Bool) (Option ByStr32)} cb guess_b in
                match xa with
                | Pair True (Some g) =>
                  let h = builtin sha256hash solution in builtin eq h g
                | _ =>
                  match xb with
                  | Pair True (Some g) =>
                    let h = builtin sha256hash solution in builtin eq h g
                  | _ => False
                  end
                end
  
  (* Owner can withdraw balance if deadline has passed *)
  let can_withdraw =
    fun (tm : BNum) =>
      fun (b : BNum) =>
        let window = Uint32 30 in
        let deadline = builtin badd tm window in
        builtin blt deadline b
  
  (* In the case of equal results, or no results the prise goes to the owner *)
  let determine_winner =
    fun (puzzle : ByStr32) =>
      fun (guess_a : Option ByStr32) =>
        fun (guess_b : Option ByStr32) =>
          fun (pa : ByStr20) =>
            fun (pb : ByStr20) =>
              fun (oa : ByStr20) =>
                let gab = Pair {(Option ByStr32) (Option ByStr32)} guess_a guess_b
                in
                match gab with
                | Pair (Some ga) (Some gb) =>
                  let d1 = hash_dist puzzle ga in
                  let d2 = hash_dist puzzle gb in
                  let c1 = builtin lt d1 d2 in
                  match c1 with
                  | True => pa
                  | False =>
                    let c2 = builtin eq d1 d2 in
                    match c2 with
                    | False => pb
                    | True => oa
                    end
                  end
                | Pair (Some _) None => pa
                | Pair None (Some _) => pb
                | Pair None None => oa
                end
  
  let solution_submitted = Int32 1
  
  let time_window_missed = Int32 2
  
  let not_a_player = Int32 3
  
  let too_early_to_claim = Int32 4
  
  let wrong__sender_or_solution = Int32 5
  
  let here_is_the_reward = Int32 6
  
  let cannot_withdraw = Int32 7
  
  
  (***************************************************)
  (*             The contract definition             *)
  (***************************************************)
  contract ZilGame
    (
      owner : ByStr20,
      player_a : ByStr20,
      player_b : ByStr20,
      puzzle : ByStr32
    )
  
  
  (* Initial balance is not stated explicitly: it's initialized when creating the contract. *)
  field player_a_hash : Option ByStr32 = None {(ByStr32)}
  
  field player_b_hash : Option ByStr32 = None {(ByStr32)}
  
  field timer : Option BNum = None {(BNum)}
  
  transition Play (guess : ByStr32)
    tm_opt <- timer;
    b <-& BLOCKNUMBER;
    (* Check the timer *)
    c = can_play tm_opt b;
    match c with
    | False =>
      msg =
        {
          _tag : "";
          _recipient : _sender;
          _amount : Uint128 0;
          code : time_window_missed
        };
      msgs = one_msg msg;
      send msgs
    | True =>
      isa = builtin eq _sender player_a;
      isb = builtin eq _sender player_b;
      tt = True;
      match isa with
      | True =>
        ah <- player_a_hash;
        hopt = update_hash ah guess;
        player_a_hash := hopt;
        tm1 = update_timer tm_opt b;
        timer := tm1;
        msg =
          {
            _tag : "";
            _recipient : _sender;
            _amount : Uint128 0;
            code : solution_submitted
          };
        msgs = one_msg msg;
        send msgs
      | False =>
        match isb with
        | True =>
          bh <- player_b_hash;
          hopt = update_hash bh guess;
          player_b_hash := hopt;
          tm1 = update_timer tm_opt b;
          timer := tm1;
          msg =
            {
              _tag : "";
              _recipient : _sender;
              _amount : Uint128 0;
              code : solution_submitted
            };
          msgs = one_msg msg;
          send msgs
        | False =>
          msg =
            {
              _tag : "";
              _recipient : _sender;
              _amount : Uint128 0;
              code : not_a_player
            };
          msgs = one_msg msg;
          send msgs
        end
      end
    end
  end
  
  transition ClaimReward (solution : Int128)
    tm_opt <- timer;
    b <-& BLOCKNUMBER;
    (* Check the timer *)
    ttc = time_to_claim tm_opt b;
    match ttc with
    | False =>
      msg =
        {
          _tag : "";
          _recipient : _sender;
          _amount : Uint128 0;
          code : too_early_to_claim
        };
      msgs = one_msg msg;
      send msgs
    | True =>
      pa <- player_a_hash;
      pb <- player_b_hash;
      is_valid = check_validity _sender solution player_a player_b pa pb;
      match is_valid with
      | False =>
        msg =
          {
            _tag : "";
            _recipient : _sender;
            _amount : Uint128 0;
            code : wrong__sender_or_solution
          };
        msgs = one_msg msg;
        send msgs
      | True =>
        winner = determine_winner puzzle pa pb player_a player_b owner;
        bal <- _balance;
        msg =
          {
            _tag : "";
            _recipient : winner;
            _amount : bal;
            code : here_is_the_reward
          };
        ff = False;
        msgs = one_msg msg;
        send msgs
      end
    end
  end
  
  transition Withdraw ()
    tm = _creation_block;
    b <-& BLOCKNUMBER;
    cw = can_withdraw tm b;
    is_owner = builtin eq owner _sender;
    bal <- _balance;
    good_to_go = andb cw is_owner;
    match good_to_go with
    | True =>
      msg =
        {
          _tag : "";
          _recipient : owner;
          _amount : bal;
          code : here_is_the_reward
        };
      msgs = one_msg msg;
      e = { _eventname : "GameOver" };
      event e;
      send msgs
    | False =>
      msg =
        {
          _tag : "";
          _recipient : _sender;
          _amount : Uint128 0;
          code : cannot_withdraw
        };
      msgs = one_msg msg;
      send msgs
    end
  end
  
