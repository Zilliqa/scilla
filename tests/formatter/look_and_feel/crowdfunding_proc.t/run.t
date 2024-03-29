  $ scilla-fmt crowdfunding_proc.scilla
  scilla_version 0
  (***************************************************)
  (*                 Scilla version                  *)
  (***************************************************)
  (***************************************************)
  (*               Associated library                *)
  (***************************************************)
  
  import BoolUtils
  
  library Crowdfunding
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  let blk_leq =
    fun (blk1 : BNum) =>
      fun (blk2 : BNum) =>
        let bc1 = builtin blt blk1 blk2 in
        let bc2 = builtin eq blk1 blk2 in
        orb bc1 bc2
  
  let get_funds_allowed =
    fun (cur_block : BNum) =>
      fun (max_block : BNum) =>
        fun (balance : Uint128) =>
          fun (goal : Uint128) =>
            let in_time = blk_leq cur_block max_block in
            let deadline_passed = negb in_time in
            let target_not_reached = builtin lt balance goal in
            let target_reached = negb target_not_reached in
            andb deadline_passed target_reached
  
  let claimback_allowed =
    fun (balance : Uint128) =>
      fun (goal : Uint128) =>
        fun (already_funded : Bool) =>
          let target_not_reached = builtin lt balance goal in
          let not_already_funded = negb already_funded in
          andb target_not_reached not_already_funded
  
  let accepted_code = Int32 1
  
  let missed_deadline_code = Int32 2
  
  let already_backed_code = Int32 3
  
  let not_owner_code = Int32 4
  
  let too_early_code = Int32 5
  
  let got_funds_code = Int32 6
  
  let cannot_get_funds = Int32 7
  
  let cannot_reclaim_code = Int32 8
  
  let reclaimed_code = Int32 9
  
  
  (***************************************************)
  (*             The contract definition             *)
  (***************************************************)
  contract Crowdfunding
    (
      (*  Parameters *)
      owner : ByStr20,
      max_block : BNum,
      goal : Uint128
    )
  with
    (* Contract constraint *)
    let zero = Uint128 0 in
    builtin lt zero goal
  =>
  
  
  (* Mutable fields *)
  field backers : Map ByStr20 Uint128 = Emp (ByStr20) (Uint128)
  
  field funded : Bool = False
  
  procedure DonationEvent (failure : Bool, error_code : Int32)
    match failure with
    | False =>
      e =
        {
          _eventname : "DonationSuccess";
          donor : _sender;
          amount : _amount;
          code : accepted_code
        };
      event e
    | True =>
      e =
        {
          _eventname : "DonationFailure";
          donor : _sender;
          amount : _amount;
          code : error_code
        };
      event e
    end
  end
  
  procedure PerformDonate ()
    c <- exists backers[_sender];
    match c with
    | False =>
      accept;
      backers[_sender] := _amount;
      DonationEvent c accepted_code
    | True => DonationEvent c already_backed_code
    end
  end
  
  transition Donate ()
    blk <-& BLOCKNUMBER;
    in_time = blk_leq blk max_block;
    match in_time with
    | True => PerformDonate
    | False =>
      t = True;
      DonationEvent t missed_deadline_code
    end
  end
  
  procedure GetFundsFailure (error_code : Int32)
    e =
      {
        _eventname : "GetFundsFailure";
        caller : _sender;
        amount : _amount;
        code : error_code
      };
    event e
  end
  
  procedure PerformGetFunds ()
    bal <- _balance;
    tt = True;
    funded := tt;
    msg = { _tag : ""; _recipient : owner; _amount : bal; code : got_funds_code };
    msgs = one_msg msg;
    send msgs
  end
  
  transition GetFunds ()
    is_owner = builtin eq owner _sender;
    match is_owner with
    | False => GetFundsFailure not_owner_code
    | True =>
      blk <-& BLOCKNUMBER;
      bal <- _balance;
      allowed = get_funds_allowed blk max_block bal goal;
      match allowed with
      | False => GetFundsFailure cannot_get_funds
      | True => PerformGetFunds
      end
    end
  end
  
  procedure ClaimBackFailure (error_code : Int32)
    e =
      {
        _eventname : "ClaimBackFailure";
        caller : _sender;
        amount : _amount;
        code : error_code
      };
    event e
  end
  
  procedure PerformClaimBack (amount : Uint128)
    delete backers[_sender];
    msg =
      { _tag : ""; _recipient : _sender; _amount : amount; code : reclaimed_code };
    msgs = one_msg msg;
    e =
      {
        _eventname : "ClaimBackSuccess";
        caller : _sender;
        amount : amount;
        code : reclaimed_code
      };
    event e;
    send msgs
  end
  
  (* transition ClaimBack *)
  transition ClaimBack ()
    blk <-& BLOCKNUMBER;
    after_deadline = builtin blt max_block blk;
    match after_deadline with
    | False => ClaimBackFailure too_early_code
    | True =>
      bal <- _balance;
      f <- funded;
      allowed = claimback_allowed bal goal f;
      match allowed with
      | False => ClaimBackFailure cannot_reclaim_code
      | True =>
        res <- backers[_sender];
        match res with
        | None =>
          (* Sender has not donated *)
          ClaimBackFailure cannot_reclaim_code
        | Some v => PerformClaimBack v
        end
      end
    end
  end
  
