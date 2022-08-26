  $ scilla-fmt simple-dex-remote-reads.scilla
  scilla_version 0
  
  import IntUtils
  
  library SimpleExchangeLib
  
  type Order =
  | Order of
      ByStr20
      ByStr20 with contract
        field allowances : Map ByStr20 (Map ByStr20 Uint128)
      end
      Uint128
      ByStr20 with contract
        field allowances : Map ByStr20 (Map ByStr20 Uint128)
      end
      Uint128
  
  let true = True
  
  let false = False
  
  let zero = Uint128 0
  
  let one = Uint128 1
  
  let one_msg : Message -> List Message =
    fun (msg : Message) =>
      let mty = Nil {(Message)} in
      Cons {(Message)} msg mty
  
  let two_msgs : Message -> Message -> List Message =
    fun (msg1 : Message) =>
      fun (msg2 : Message) =>
        let first = one_msg msg1 in
        Cons {(Message)} msg2 first
  
  let mk_transfer_msg :
    Bool -> ByStr20 -> ByStr20 -> ByStr20 -> Uint128 -> Message =
    fun (transfer_from : Bool) =>
      fun (token_address : ByStr20) =>
        fun (from : ByStr20) =>
          fun (to : ByStr20) =>
            fun (amount : Uint128) =>
              let tag =
                match transfer_from with
                | True => "TransferFrom"
                | False => "Transfer"
                end
              in
              {
                _recipient : token_address;
                _tag : tag;
                _amount : Uint128 0;
                from : from;
                to : to;
                amount : amount
              }
  
  let mk_place_order_msg :
    ByStr20 -> ByStr20 -> ByStr20 -> Uint128 -> List Message =
    fun (token_address : ByStr20) =>
      fun (from : ByStr20) =>
        fun (to : ByStr20) =>
          fun (amount : Uint128) =>
            let msg = mk_transfer_msg true token_address from to amount in
            one_msg msg
  
  let mk_make_order_msgs :
    ByStr20 ->
    Uint128 ->
    ByStr20 ->
    Uint128 ->
    ByStr20 ->
    ByStr20 ->
    ByStr20 ->
    List Message =
    fun (token_sell_address : ByStr20) =>
      fun (sell_amount : Uint128) =>
        fun (token_buy_address : ByStr20) =>
          fun (buy_amount : Uint128) =>
            fun (this_address : ByStr20) =>
              fun (order_placer : ByStr20) =>
                fun (order_maker : ByStr20) =>
                  let sell_msg =
                    mk_transfer_msg
                      false token_sell_address this_address order_maker sell_amount
                  in
                  let buy_msg =
                    mk_transfer_msg
                      true token_buy_address order_maker order_placer buy_amount
                  in
                  two_msgs sell_msg buy_msg
  
  
  contract SimpleExchange (initial_admin : ByStr20 with end)
  
  
  field admin : ByStr20 with end = initial_admin
  
  field listed_tokens :
    Map
      String
      (ByStr20 with contract
        field allowances : Map ByStr20 (Map ByStr20 Uint128)
      end) =
    Emp
      (String)
      (ByStr20 with contract
        field allowances : Map ByStr20 (Map ByStr20 Uint128)
      end)
  
  field active_orders : Map Uint128 Order = Emp (Uint128) (Order)
  
  field next_order_no : Uint128 = zero
  
  procedure ThrowListingStatusException
    (token_code : String, expected_status : Bool, actual_status : Bool)
    e =
      {
        _exception : "UnexpectedListingStatus";
        token_code : token_code;
        expected : expected_status;
        actual : actual_status
      };
    throw e
  end
  
  procedure ThrowInsufficientAllowanceException
    (token : ByStr20, expected : Uint128, actual : Uint128)
    e =
      {
        _exception : "InsufficientAllowance";
        token : token;
        expected : expected;
        actual : actual
      };
    throw e
  end
  
  procedure CheckSenderIsAdmin ()
    current_admin <- admin;
    is_admin = builtin eq _sender current_admin;
    match is_admin with
    | True =>
    | False =>
      e = { _exception : "SenderIsNotAdmin" };
      throw e
    end
  end
  
  transition SetAdmin (new_admin : ByStr20 with end)
    CheckSenderIsAdmin;
    admin := new_admin
  end
  
  procedure CheckIsTokenUnlisted (token_code : String)
    token_code_is_listed <- exists listed_tokens[token_code];
    match token_code_is_listed with
    | True => ThrowListingStatusException token_code false token_code_is_listed
    | False =>
    end
  end
  
  transition ListToken
    (
      token_code : String,
      new_token :
        ByStr20 with contract
          field allowances : Map ByStr20 (Map ByStr20 Uint128)
        end
    )
    CheckSenderIsAdmin;
    CheckIsTokenUnlisted token_code;
    listed_tokens[token_code] := new_token
  end
  
  procedure CheckAllowance
    (
      token :
        ByStr20 with contract
          field allowances : Map ByStr20 (Map ByStr20 Uint128)
        end,
      expected : Uint128
    )
    actual_opt <-& token.allowances[_sender][_this_address];
    actual =
      match actual_opt with
      | Some x => x
      | None => zero
      end;
    is_sufficient = uint128_le expected actual;
    match is_sufficient with
    | True =>
    | False => ThrowInsufficientAllowanceException token expected actual
    end
  end
  
  procedure AddOrder (order : Order)
    order_no <- next_order_no;
    active_orders[order_no] := order;
    new_order_no = builtin add order_no one;
    next_order_no := new_order_no
  end
  
  transition PlaceOrder
    (
      token_code_sell : String,
      sell_amount : Uint128,
      token_code_buy : String,
      buy_amount : Uint128
    )
    token_sell_opt <- listed_tokens[token_code_sell];
    token_buy_opt <- listed_tokens[token_code_buy];
    match token_sell_opt with
    | Some token_sell =>
      match token_buy_opt with
      | Some token_buy =>
        CheckAllowance token_sell sell_amount;
        msg = mk_place_order_msg token_sell _sender _this_address sell_amount;
        send msg;
        order = Order _sender token_sell sell_amount token_buy buy_amount;
        AddOrder order
      | None => ThrowListingStatusException token_code_buy true false
      end
    | None => ThrowListingStatusException token_code_sell true false
    end
  end
  
  transition MatchOrder (order_id : Uint128)
    order <- active_orders[order_id];
    match order with
    | Some (Order order_placer sell_token sell_amount buy_token buy_amount) =>
      CheckAllowance buy_token buy_amount;
      msgs =
        mk_make_order_msgs
          sell_token sell_amount buy_token buy_amount _this_address order_placer _sender;
      send msgs;
      delete active_orders[order_id]
    | None =>
      e = { _exception : "UnknownOrder"; order_id : order_id };
      throw e
    end
  end
  
  procedure CheckInitiator (initiator : ByStr20)
    initiator_is_this = builtin eq initiator _this_address;
    match initiator_is_this with
    | True =>
    | False =>
      e =
        {
          _exception : "UnexpecedTransfer";
          token_address : _sender;
          initiator : initiator
        };
      throw e
    end
  end
  
  transition RecipientAcceptTransferFrom
    (initiator : ByStr20, sender : ByStr20, recipient : ByStr20, amount : Uint128)
    CheckInitiator initiator
  end
  
  transition TransferFromSuccessCallBack
    (initiator : ByStr20, sender : ByStr20, recipient : ByStr20, amount : Uint128)
    CheckInitiator initiator
  end
  
  transition TransferSuccessCallBack
    (initiator : ByStr20, sender : ByStr20, recipient : ByStr20, amount : Uint128)
    CheckInitiator initiator
  end
  
