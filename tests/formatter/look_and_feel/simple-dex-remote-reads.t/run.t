  $ scilla-fmt simple-dex-remote-reads.scilla
  scilla_version 0
  (* This contract is only used in the documentation at scilla.readthedocs.io.
     No tests are run as part of our test suite *)
  
  import IntUtils
  
  library SimpleExchangeLib
  
  (* Order placer, sell token, sell amount, buy token, buy amount *)
  type Order =
  | Order of
      ByStr20
      (ByStr20 with contract
        field allowances : Map ByStr20 (Map ByStr20 Uint128)
      end)
      Uint128
      (ByStr20 with contract
        field allowances : Map ByStr20 (Map ByStr20 Uint128)
      end)
      Uint128
  
  
  (* Helper values and functions *)
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
                from : (* No Zil are transferred, only custom tokens *)
                from;
                to : to;
                amount : amount
              }
  
  let mk_place_order_msg :
    ByStr20 -> ByStr20 -> ByStr20 -> Uint128 -> List Message =
    fun (token_address : ByStr20) =>
      fun (from : ByStr20) =>
        fun (to : ByStr20) =>
          fun (amount : Uint128) =>
            (* Construct a TransferFrom messsage to transfer from seller's allowance to exhange *)
            let msg = mk_transfer_msg true token_address from to amount in
            (* Create a singleton list *)
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
                  (* Construct a Transfer messsage to transfer from exchange to maker *)
                  let sell_msg =
                    mk_transfer_msg
                      false token_sell_address this_address order_maker sell_amount
                  in
                  (* Construct a TransferFrom messsage to transfer from maker to placer *)
                  let buy_msg =
                    mk_transfer_msg
                      true token_buy_address order_maker order_placer buy_amount
                  in
                  (* Create a singleton list *)
                  two_msgs sell_msg buy_msg
  
  
  contract SimpleExchange
    (
      (* Ensure that the initial admin is an address that is in use *)
      initial_admin : ByStr20 with end
    )
  
  
  (* Active admin. *)
  (* NOTE: We don't need to read any fields from the admin                   *)
  (* address, so we could have chosen ByStr20 as the field type. This could  *)
  (* have been done without changing the type of initial_admin or the        *)
  (* new_admin transition parameter, because ByStr20 with end is a subtype   *)
  (* of ByStr20, which means that a value of type ByStr20 with end can be    *)
  (* used when a ByStr20 is expected. initial_admin is checked to be in use  *)
  (* when the contract is deployed, and new_admin transition parameter is    *)
  (* checked to be in use when the SetAdmin transition is invoked, so we     *)
  (* can still be sure that we don't accidentally set an admin field to an   *)
  (* address that is not in use.                                             *)
  (*                                                                         *)
  (* The advantage of choosing ByStr20 with end as the field type is that    *)
  (* we don't accidentally set the admin address to an unchecked ByStr20 in  *)
  (* some other part of the code by mistake.                                 *)
  field admin : ByStr20 with end = initial_admin
  
  (* Tokens listed on the exchange. *)
  (* We identify the token by its exchange code, and map it to the address     *)
  (* of the contract implementing the token. The contract at that address must *)
  (* contain an allowances field that we can remote read.                      *)
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
  
  (* Active orders, identified by the order number *)
  field active_orders : Map Uint128 Order = Emp (Uint128) (Order)
  
  (* The order number to use when the next order is placed *)
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
  
  (* Check that _sender is the active admin.          *)
  (* If not, throw an error and abort the transaction *)
  procedure CheckSenderIsAdmin ()
    current_admin <- admin;
    is_admin = builtin eq _sender current_admin;
    match is_admin with
    | True => (* Nothing to do *)
    | False =>
      (* Construct an exception object and throw it *)
      e = { _exception : "SenderIsNotAdmin" };
      throw e
    end
  end
  
  (* Change the active admin *)
  (* NOTE: Only the former admin is allowed to appoint a new admin *)
  transition SetAdmin (new_admin : ByStr20 with end)
    (* Only the former admin may appoint a new admin *)
    CheckSenderIsAdmin;
    admin := new_admin
  end
  
  (* Check that a given token code is not already listed. If it is, throw an error. *)
  procedure CheckIsTokenUnlisted (token_code : String)
    (* Is the token code listed? *)
    token_code_is_listed <- exists listed_tokens[token_code];
    match token_code_is_listed with
    | True =>
      (* Incorrect listing status *)
      ThrowListingStatusException token_code false token_code_is_listed
    | False => (* Nothing to do *)
    end
  end
  
  (* List a new token on the exchange. Only the admin may list new tokens.  *)
  (* If a token code is already in use, raise an error                      *)
  (* NOTE: The token address must have an allowances field. If the supplied *)
  (* address does not contain such a field, the transition fails, and the   *)
  (* transaction aborts.                                                    *)
  transition ListToken
    (
      token_code : String,
      new_token :
        ByStr20 with contract
          field allowances : Map ByStr20 (Map ByStr20 Uint128)
        end
    )
    (* Only the admin may list new tokens.  *)
    CheckSenderIsAdmin;
    (* Only new token codes are allowed.  *)
    CheckIsTokenUnlisted token_code;
    (* Everything is ok. The token can be listed *)
    listed_tokens[token_code] := new_token
  end
  
  (* Check that the sender has allowed access to sufficient funds *)
  procedure CheckAllowance
    (
      token :
        ByStr20 with contract
          field allowances : Map ByStr20 (Map ByStr20 Uint128)
        end,
      expected : Uint128
    )
    actual_opt <-& token.allowances[_sender][_this_address];
    (* Find actual allowance. Use 0 if None is given *)
    actual =
      match actual_opt with
      | Some x => x
      | None => zero
      end;
    is_sufficient = uint128_le expected actual;
    match is_sufficient with
    | True => (* Nothing to do *)
    | False => ThrowInsufficientAllowanceException token expected actual
    end
  end
  
  procedure AddOrder (order : Order)
    (* Get the next order number *)
    order_no <- next_order_no;
    (* Add the order *)
    active_orders[order_no] := order;
    (* Update the next_order_no field *)
    new_order_no = builtin add order_no one;
    next_order_no := new_order_no
  end
  
  (* Place an order on the exchange *)
  transition PlaceOrder
    (
      token_code_sell : String,
      sell_amount : Uint128,
      token_code_buy : String,
      buy_amount : Uint128
    )
    (* Check that the tokens are listed *)
    token_sell_opt <- listed_tokens[token_code_sell];
    token_buy_opt <- listed_tokens[token_code_buy];
    match token_sell_opt with
    | Some token_sell =>
      match token_buy_opt with
      | Some token_buy =>
        (* Check that the placer has allowed sufficient funds to be accessed *)
        CheckAllowance token_sell sell_amount;
        (* Transfer the sell tokens to the exchange for holding. Construct a TransferFrom message to the token contract. *)
        msg = mk_place_order_msg token_sell _sender _this_address sell_amount;
        (* Send message when the transition completes. *)
        send msg;
        (* Create order and add to list of active orders  *)
        order = Order _sender token_sell sell_amount token_buy buy_amount;
        AddOrder order
      | None =>
        (* Unlisted token *)
        ThrowListingStatusException token_code_buy true false
      end
    | None =>
      (* Unlisted token *)
      ThrowListingStatusException token_code_sell true false
    end
  end
  
  transition MatchOrder (order_id : Uint128)
    order <- active_orders[order_id];
    match order with
    | Some (Order order_placer sell_token sell_amount buy_token buy_amount) =>
      (* Check that the placer has allowed sufficient funds to be accessed *)
      CheckAllowance buy_token buy_amount;
      (* Create the two transfer messages and send them *)
      msgs =
        mk_make_order_msgs
          sell_token sell_amount buy_token buy_amount _this_address order_placer _sender;
      send msgs;
      (* Order has now been matched, so remove it *)
      delete active_orders[order_id]
    | None =>
      e = { _exception : "UnknownOrder"; order_id : order_id };
      throw e
    end
  end
  
  procedure CheckInitiator (initiator : ByStr20)
    initiator_is_this = builtin eq initiator _this_address;
    match initiator_is_this with
    | True => (* Do nothing *)
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
    (* The exchange only accepts transfers that it itself has initiated.  *)
    CheckInitiator initiator
  end
  
  transition TransferFromSuccessCallBack
    (initiator : ByStr20, sender : ByStr20, recipient : ByStr20, amount : Uint128)
    (* The exchange only accepts transfers that it itself has initiated.  *)
    CheckInitiator initiator
  end
  
  transition TransferSuccessCallBack
    (initiator : ByStr20, sender : ByStr20, recipient : ByStr20, amount : Uint128)
    (* The exchange only accepts transfers that it itself has initiated.  *)
    CheckInitiator initiator
  end
  
