  $ scilla-fmt earmarked-coin.scilla
  scilla_version 0
  
  import BoolUtils
  
  library EarmarkedCoin
  
  type EarmarkedCoin =
  | EarmarkedCoin of Uint128 ByStr20
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  let success_code = Int32 1
  
  let already_earmarked_code = Int32 2
  
  let not_authorized_code = Int32 3
  
  let did_not_earmark_code = Int32 4
  
  
  contract EarmarkedCoin ()
  
  
  field earmarked_coins : Map ByStr20 EarmarkedCoin =
    Emp (ByStr20) (EarmarkedCoin)
  
  procedure TransferFunds (amount : Uint128, recipient : ByStr20)
    msg = { _tag : ""; _recipient : recipient; _amount : amount };
    msgs = one_msg msg;
    send msgs
  end
  
  procedure SuccessfulTransferOfFunds (from : ByStr20, to : ByStr20)
    e =
      {
        _eventname : "SuccessfulTransferOfFunds";
        from : from;
        to : to;
        code : success_code
      };
    event e
  end
  
  procedure FailedToTransferFunds
    (from : ByStr20, to : ByStr20, error_code : Int32)
    e =
      {
        _eventname : "FailedToTransferFunds";
        from : from;
        to : to;
        code : error_code
      };
    event e
  end
  
  transition Earmark (recip : ByStr20)
    c <- exists earmarked_coins[_sender];
    match c with
    | False =>
      accept;
      e_coin = EarmarkedCoin _amount recip;
      earmarked_coins[_sender] := e_coin;
      SuccessfulTransferOfFunds _sender _this_address
    | True => FailedToTransferFunds _sender _this_address already_earmarked_code
    end
  end
  
  transition ClaimForRecipient (earmarked_coin_address : ByStr20)
    e_coin_opt <- earmarked_coins[earmarked_coin_address];
    match e_coin_opt with
    | Some (EarmarkedCoin amount recipient) =>
      authorized_to_claim = builtin eq recipient _sender;
      match authorized_to_claim with
      | True =>
        TransferFunds amount recipient;
        delete earmarked_coins[earmarked_coin_address];
        SuccessfulTransferOfFunds _this_address _sender
      | False => FailedToTransferFunds _this_address _sender not_authorized_code
      end
    | None => FailedToTransferFunds _this_address _sender did_not_earmark_code
    end
  end
  
  transition ClaimForCreator ()
    e_coin_opt <- earmarked_coins[_sender];
    match e_coin_opt with
    | Some (EarmarkedCoin amount _) =>
      TransferFunds amount _sender;
      delete earmarked_coins[_sender];
      SuccessfulTransferOfFunds _this_address _sender
    | None => FailedToTransferFunds _this_address _sender did_not_earmark_code
    end
  end
  
