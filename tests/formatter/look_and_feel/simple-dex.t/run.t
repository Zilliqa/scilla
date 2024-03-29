  $ scilla-fmt simple-dex.scilla
  scilla_version 0
  
  import PairUtils
  
  (* Simple DEX : P2P Token Trades    *)
  (* Disclaimer: This contract is experimental and meant for testing purposes only *)
  (* DO NOT USE THIS CONTRACT IN PRODUCTION *)
  library SimpleDex
  
  (* Pair helpers *)
  let getAddressFromPair = @fst (ByStr20) (Uint128)
  
  let getValueFromPair = @snd (ByStr20) (Uint128)
  
  (* Event for errors *)
  let make_error_event =
    fun (location : String) =>
      fun (msg : String) =>
        { _eventname : "Error"; raisedAt : location; message : msg }
  
  (* Order = { tokenA, valueA, tokenB, valueB } *)
  type Order =
  | Order of ByStr20 Uint128 ByStr20 Uint128
  
  (* Create an orderID based on the hash of the parameters *)
  let createOrderId =
    fun (order : Order) =>
      builtin sha256hash order
  
  (* Create one transaction message *)
  let transaction_msg =
    fun (recipient : ByStr20) =>
      fun (tag : String) =>
        fun (transferFromAddr : ByStr20) =>
          fun (transferToAddr : ByStr20) =>
            fun (transferAmt : Uint128) =>
              {
                _tag : tag;
                _recipient : recipient;
                _amount : Uint128 0;
                from : transferFromAddr;
                to : transferToAddr;
                tokens : transferAmt
              }
  
  (* Wrap one transaction message as singleton list *)
  let transaction_msg_as_list =
    fun (recipient : ByStr20) =>
      fun (tag : String) =>
        fun (transferFromAddr : ByStr20) =>
          fun (transferToAddr : ByStr20) =>
            fun (transferAmt : Uint128) =>
              let one_msg =
                fun (msg : Message) =>
                  let nil_msg = Nil {(Message)} in
                  Cons {(Message)} msg nil_msg
              in
              let msg =
                transaction_msg
                  recipient tag transferFromAddr transferToAddr transferAmt
              in
              one_msg msg
  
  (* Compute the new pending return val *)
  (* If no existing records are found, return `incomingTokensAmt` *)
  (* else, return `incomingTokenAmt` + existing value *)
  let computePendingReturnsVal =
    fun (prevVal : Option Uint128) =>
      fun (incomingTokensAmt : Uint128) =>
        match prevVal with
        | Some v => builtin add v incomingTokensAmt
        | None => incomingTokensAmt
        end
  
  
  (***************************************************)
  (*             The contract definition             *)
  (***************************************************)
  contract SimpleDex (contractOwner : ByStr20)
  
  
  (* Orderbook: mapping (orderIds => ( (tokenA, valueA) (tokenB, valueB) )) *)
  (* @param: tokenA: Contract address of token A *)
  (* @param: valueA: total units of token A offered by maker *)
  (* @param: tokenB: Contract address of token B *)
  (* @param: valueB: total units of token B requsted by maker *)
  field orderbook : Map ByStr32 Order = Emp (ByStr32) (Order)
  
  (* Order info stores the mapping ( orderId => (tokenOwnerAddress, expirationBlock)) *)
  field orderInfo : Map ByStr32 (Pair ByStr20 BNum) =
    Emp (ByStr32) (Pair ByStr20 BNum)
  
  (* Ledger of how much the _sender can claim from the contract *)
  (* mapping ( walletAddress => mapping (tokenContracts => amount) ) *)
  field pendingReturns : Map ByStr20 (Map ByStr20 Uint128) =
    Emp (ByStr20) (Map ByStr20 Uint128)
  
  (* Maker creates an order to exchange valueA of tokenA for valueB of tokenB *)
  transition makeOrder
    (
      tokenA : ByStr20,
      valueA : Uint128,
      tokenB : ByStr20,
      valueB : Uint128,
      expirationBlock : BNum
    )
    currentBlock <-& BLOCKNUMBER;
    validExpirationBlock =
      let minBlocksFromCreation = Uint128 50 in
      let minExpiration = builtin badd currentBlock minBlocksFromCreation in
      builtin blt minExpiration expirationBlock;
    match validExpirationBlock with
    | True =>
      (* Creates a new order *)
      newOrder = Order tokenA valueA tokenB valueB;
      orderId = createOrderId newOrder;
      orderbook[orderId] := newOrder;
      (* Updates orderInfo with maker's address and expiration blocknumber *)
      p = Pair {(ByStr20) (BNum)} _sender expirationBlock;
      orderInfo[orderId] := p;
      e = { _eventname : "Order Created"; hash : orderId };
      event e;
      (* Transfer tokens from _sender to the contract address  *)
      msgs =
        let tag = "TransferFrom" in
        let zero = Uint128 0 in
        transaction_msg_as_list tokenA tag _sender _this_address valueA;
      send msgs
    | False =>
      e =
        let func = "makeOrder" in
        let error_msg =
          "Expiration block must be at least 50 blocks more than current block"
        in
        make_error_event func error_msg;
      event e
    end
  end
  
  (* Taker fills an order *)
  transition fillOrder (orderId : ByStr32)
    getOrder <- orderbook[orderId];
    match getOrder with
    | Some (Order tokenA valueA tokenB valueB) =>
      (* Check the expiration block *)
      optionOrderInfo <- orderInfo[orderId];
      match optionOrderInfo with
      | Some info =>
        currentBlock <-& BLOCKNUMBER;
        blockBeforeExpiration =
          let getBNum = @snd (ByStr20) (BNum) in
          let expirationBlock = getBNum info in
          builtin blt currentBlock expirationBlock;
        match blockBeforeExpiration with
        | True =>
          makerAddr =
            let getMakerAddr = @fst (ByStr20) (BNum) in
            getMakerAddr info;
          (* Updates taker with the tokens that he is entitled to claim *)
          prevVal <- pendingReturns[_sender][tokenA];
          takerAmt = computePendingReturnsVal prevVal valueA;
          pendingReturns[_sender][tokenA] := takerAmt;
          prevVal_1 <- pendingReturns[makerAddr][tokenB];
          makerAmt = computePendingReturnsVal prevVal_1 valueB;
          pendingReturns[makerAddr][tokenB] := makerAmt;
          (* Delete orders from the orderbook and orderinfo *)
          delete orderInfo[orderId];
          delete orderbook[orderId];
          e = { _eventname : "Order Filled"; hash : orderId };
          event e;
          (* Transfer tokens from _sender to the contract address  *)
          msgs =
            let tag = "TransferFrom" in
            transaction_msg_as_list tokenB tag _sender _this_address valueB;
          send msgs
        | False =>
          e =
            let func = "fillOrder" in
            let error_msg =
              "Current block number exceeds the expiration block set"
            in
            make_error_event func error_msg;
          event e
        end
      | None =>
        e =
          let func = "fillOrder" in
          let error_msg = "OrderId not found" in
          make_error_event func error_msg;
        event e
      end
    | None =>
      e =
        let func = "fillOrder" in
        let error_msg = "OrderId not found" in
        make_error_event func error_msg;
      event e
    end
  end
  
  (* Allows users to claim back their tokens from the smart contract *)
  transition ClaimBack (token : ByStr20)
    getAmtOutstanding <- pendingReturns[_sender][token];
    match getAmtOutstanding with
    | Some amtOutstanding =>
      delete pendingReturns[_sender][token];
      e =
        {
          _eventname : "Claimback Successful";
          caller : _sender;
          tokenAddr : token;
          amt : amtOutstanding
        };
      event e;
      (* Transfer tokens from _sender to the contract address  *)
      msgs =
        let tag = "TransferFrom" in
        transaction_msg_as_list token tag _this_address _sender amtOutstanding;
      send msgs
    | None =>
      e =
        let func = "claimBack" in
        let error_msg = "No Pending Returns for Sender and Contract Address found"
        in
        make_error_event func error_msg;
      event e
    end
  end
  
  (* Maker can cancel his order *)
  transition cancelOrder (orderId : ByStr32)
    getOrderInfo <- orderInfo[orderId];
    match getOrderInfo with
    | Some orderInfoForId =>
      makerAddr =
        let getMakerAddr = @fst (ByStr20) (BNum) in
        getMakerAddr orderInfoForId;
      checkSender = builtin eq makerAddr _sender;
      match checkSender with
      | True =>
        (* Sender is the maker, proceed with cancellation *)
        fetchOrder <- orderbook[orderId];
        match fetchOrder with
        | Some (Order tokenA valueA _ _) =>
          (* Updates taker with the tokens that he is entitled to claim *)
          prevVal <- pendingReturns[_sender][tokenA];
          takerAmt = computePendingReturnsVal prevVal valueA;
          pendingReturns[_sender][tokenA] := takerAmt;
          (* Delete orders from the orderbook and orderinfo *)
          delete orderInfo[orderId];
          delete orderbook[orderId];
          e = { _eventname : "Cancel order successful"; hash : orderId };
          event e
        | (* @note: For consistency, we use claimback instead of sending the tokens  *)
        (* back to the maker *)
        None =>
          e =
            let func = "cancelOrder" in
            let error_msg = "OrderID not found" in
            make_error_event func error_msg;
          event e
        end
      | False =>
        (* Unauthorized transaction *)
        e =
          let func = "cancelOrder" in
          let error_msg = "Sender is not maker of the order" in
          make_error_event func error_msg;
        event e
      end
    | None =>
      (* Order ID not found *)
      e =
        let func = "cancelOrder" in
        let error_msg = "OrderID not found" in
        make_error_event func error_msg;
      event e
    end
  end
  
