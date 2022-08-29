  $ scilla-fmt crowdfunding_governor.scilla
  scilla_version 0
  
  library CrowdfundingGovernor
  
  let zero = Uint128 0
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  let deadline_passed =
    fun (cur_block : BNum) =>
      fun (max_block : BNum) =>
        builtin blt max_block cur_block
  
  let target_not_reached =
    fun (balance : Uint128) =>
      fun (goal : Uint128) =>
        builtin lt balance goal
  
  type Error =
  | SenderIsNotContractOwner
  | SenderAlreadyDonated
  | SenderHasNotDonated
  | DeadlineHasNotPassedYet
  | DeadlineHasPassed
  | TargetIsReached
  | TargetIsNotReached
  
  let make_error =
    fun (result : Error) =>
      let result_code =
        match result with
        | SenderIsNotContractOwner => Int32 -1
        | SenderAlreadyDonated => Int32 -2
        | SenderHasNotDonated => Int32 -3
        | DeadlineHasNotPassedYet => Int32 -4
        | DeadlineHasPassed => Int32 -5
        | TargetIsReached => Int32 -6
        | TargetIsNotReached => Int32 -7
        end
      in
      { _exception : "Error"; code : result_code }
  
  
  contract CrowdfundingGovernor
    (
      contract_owner : ByStr20,
      max_block : BNum,
      goal : Uint128,
      storage : ByStr20 with contract field backers : Map ByStr20 Uint128 end
    )
  with
    let zero = Uint128 0 in
    builtin lt zero goal
  =>
  
  
  procedure Throw (error : Error)
    e = make_error error;
    throw e
  end
  
  procedure RequireContractOwner ()
    is_contract_owner = builtin eq _sender contract_owner;
    match is_contract_owner with
    | False =>
      e = SenderIsNotContractOwner;
      Throw e
    | True =>
    end
  end
  
  procedure SetBackersKey (key : ByStr20, val : Uint128)
    msg =
      {
        _tag : "SetBackersKey";
        _recipient : storage;
        _amount : zero;
        key : key;
        val : val
      };
    msgs = one_msg msg;
    send msgs
  end
  
  procedure DeleteBackersKey (key : ByStr20)
    msg =
      {
        _tag : "DeleteBackersKey";
        _recipient : storage;
        _amount : zero;
        key : key
      };
    msgs = one_msg msg;
    send msgs
  end
  
  procedure RequireBeforeDeadline ()
    blk <-& BLOCKNUMBER;
    after_deadline = deadline_passed blk max_block;
    match after_deadline with
    | True =>
      e = DeadlineHasPassed;
      Throw e
    | False =>
    end
  end
  
  procedure RequireAfterDeadline ()
    blk <-& BLOCKNUMBER;
    after_deadline = deadline_passed blk max_block;
    match after_deadline with
    | False =>
      e = DeadlineHasNotPassedYet;
      Throw e
    | True =>
    end
  end
  
  procedure RequireTargetNotReached ()
    bal <- _balance;
    is_target_not_reached = target_not_reached bal goal;
    match is_target_not_reached with
    | False =>
      e = TargetIsReached;
      Throw e
    | True =>
    end
  end
  
  procedure RequireTargetReached ()
    bal <- _balance;
    is_target_not_reached = target_not_reached bal goal;
    match is_target_not_reached with
    | True =>
      e = TargetIsNotReached;
      Throw e
    | False =>
    end
  end
  
  procedure RequireSenderHasNotDonated ()
    already_donated <-& exists storage.backers[_sender];
    match already_donated with
    | True =>
      e = SenderAlreadyDonated;
      Throw e
    | False =>
    end
  end
  
  procedure SendFunds (recipient : ByStr20, amount : Uint128)
    msg = { _tag : "AddFunds"; _recipient : recipient; _amount : amount };
    msgs = one_msg msg;
    send msgs
  end
  
  transition Donate ()
    RequireBeforeDeadline;
    RequireSenderHasNotDonated;
    accept;
    SetBackersKey _sender _amount
  end
  
  transition GetFunds ()
    RequireContractOwner;
    RequireAfterDeadline;
    RequireTargetReached;
    amount <- _balance;
    SendFunds contract_owner amount
  end
  
  transition ClaimBack ()
    RequireAfterDeadline;
    RequireTargetNotReached;
    oamount <-& storage.backers[_sender];
    match oamount with
    | None =>
      e = SenderHasNotDonated;
      Throw e
    | Some amount =>
      SendFunds _sender amount;
      DeleteBackersKey _sender
    end
  end
  
