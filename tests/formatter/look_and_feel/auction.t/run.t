  $ scilla-fmt auction.scilla
  scilla_version 0
  (***************************************************)
  (*               Associated library                *)
  (***************************************************)
  
  import BoolUtils
  
  library OpenAuction
  
  let blk_leq =
    fun (blk1 : BNum) =>
      fun (blk2 : BNum) =>
        let bc1 = builtin blt blk1 blk2 in
        let bc2 = builtin eq blk1 blk2 in
        orb bc1 bc2
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  let late_to_bid_code = Int32 1
  
  let too_early_to_bid_code = Int32 2
  
  let bid_too_low_code = Int32 3
  
  let first_bid_accepted_code = Int32 4
  
  let bid_accepted_code = Int32 5
  
  let money_sent_code = Int32 6
  
  let nothing_to_withdraw_code = Int32 7
  
  let auction_is_still_on_code = Int32 8
  
  let auction_end_code = Int32 9
  
  
  (***************************************************)
  (*             The contract definition             *)
  (***************************************************)
  contract OpenAuction
    (
      (*  Parameters *)
      auctionStart : BNum,
      biddingTime : Uint128,
      beneficiary : ByStr20
    )
  
  
  (* Mutable fields *)
  field ended : Bool = False
  
  field highestBidder : Option ByStr20 = None {(ByStr20)}
  
  field highestBid : Uint128 = Uint128 0
  
  field pendingReturns : Map ByStr20 Uint128 = Emp (ByStr20) (Uint128)
  
  (* Transition 1: bidding *)
  transition Bid ()
    blk <-& BLOCKNUMBER;
    endtime = builtin badd auctionStart biddingTime;
    after_end = let one = Uint128 1 in builtin badd endtime one;
    e <- ended;
    in_time = blk_leq after_end blk;
    flag1 = orb in_time e;
    early = blk_leq blk auctionStart;
    match early with
    | True =>
      msg =
        {
          _tag : "";
          _recipient : _sender;
          _amount : Uint128 0;
          code : too_early_to_bid_code
        };
      msgs = one_msg msg;
      send msgs
    | False =>
      match flag1 with
      | True =>
        msg =
          {
            _tag : "";
            _recipient : _sender;
            _amount : Uint128 0;
            code : late_to_bid_code
          };
        msgs = one_msg msg;
        send msgs
      | False =>
        hb <- highestBid;
        (* Checks if the bid is too low *)
        sufficientBid = builtin lt hb _amount;
        match sufficientBid with
        | False =>
          msg =
            {
              _tag : "";
              _recipient : _sender;
              _amount : Uint128 0;
              code : bid_too_low_code
            };
          msgs = one_msg msg;
          send msgs
        | True =>
          accept;
          hbPrev <- highestBidder;
          match hbPrev with
          | Some prevHighestBidder =>
            (* There is already a highest bidder *)
            option_pendingReturnsForPrevHB <- pendingReturns[prevHighestBidder];
            getPRForPrevHighestBidder =
              match option_pendingReturnsForPrevHB with
              | Some pendingReturnsForPrevHB =>
                (* User already has some balance in the pending returns that is not claimed *)
                builtin add hb pendingReturnsForPrevHB
              | None => hb
              end;
            (* Prev highest bidder has no pending returns. *)
            pendingReturns[prevHighestBidder] := getPRForPrevHighestBidder;
            (* Update the highest bidder *)
            bidder = Some {(ByStr20)} _sender;
            highestBidder := bidder;
            highestBid := _amount;
            ev =
              {
                _eventname : "Bid";
                code : bid_accepted_code;
                addr : _sender;
                amount : _amount
              };
            event ev
          | None =>
            (* Process first bid *)
            first_bidder = Some {(ByStr20)} _sender;
            highestBidder := first_bidder;
            highestBid := _amount;
            ev1 =
              {
                _eventname : "Bid";
                code : first_bid_accepted_code;
                addr : _sender;
                amount : _amount
              };
            event ev1
          end
        end
      end
    end
  end
  
  (* Transition 2: claiming money back *)
  transition Withdraw ()
    prs <- pendingReturns;
    pr = builtin get prs _sender;
    match pr with
    | None =>
      msg =
        {
          _tag : "";
          _recipient : _sender;
          _amount : Uint128 0;
          code : nothing_to_withdraw_code
        };
      msgs = one_msg msg;
      send msgs
    | Some v =>
      delete pendingReturns[_sender];
      e = { _eventname : "Withdraw Successful"; addr : _sender };
      event e;
      msg =
        { _tag : ""; _recipient : _sender; _amount : v; code : money_sent_code };
      msgs = one_msg msg;
      send msgs
    end
  end
  
  (* Transition 3: auction ends *)
  transition AuctionEnd ()
    blk <-& BLOCKNUMBER;
    e <- ended;
    t1 = builtin badd auctionStart biddingTime;
    t2 = blk_leq t1 blk;
    t3 = negb e;
    t4 = andb t2 t3;
    match t4 with
    | False =>
      msg =
        {
          _tag : "";
          _recipient : _sender;
          _amount : Uint128 0;
          code : auction_is_still_on_code
        };
      msgs = one_msg msg;
      send msgs
    | True =>
      val = True;
      ended := val;
      hb <- highestBid;
      ev = { _eventname : "Auction ended"; highest_bid : hb };
      event ev;
      msg =
        {
          _tag : "";
          _recipient : beneficiary;
          _amount : hb;
          code : auction_end_code;
          highest_bid : hb
        };
      msgs = one_msg msg;
      send msgs
    end
  end
  
