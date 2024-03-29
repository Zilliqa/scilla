  $ scilla-fmt fungible-token.scilla
  scilla_version 0
  (* This contract implements a fungible token interface a la ERC20.*)
  
  (***************************************************)
  (*               Associated library                *)
  (***************************************************)
  library FungibleToken
  
  let one = Uint128 1
  
  let zero = Uint128 0
  
  let min_int =
    fun (a : Uint128) =>
      fun (b : Uint128) =>
        let alt = builtin lt a b in
        match alt with
        | True => a
        | False => b
        end
  
  let le_int =
    fun (a : Uint128) =>
      fun (b : Uint128) =>
        let x = builtin lt a b in
        match x with
        | True => True
        | False =>
          let y = builtin eq a b in
          match y with
          | True => True
          | False => False
          end
        end
  
  (* returns singleton List Message *)
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  
  (***************************************************)
  (*             The contract definition             *)
  (***************************************************)
  contract FungibleToken
    (
      owner : ByStr20,
      total_tokens : Uint128,
      decimals : Uint32,
      name : String,
      symbol : String
    )
  
  
  (* Initial balance is not stated explicitly: it's initialized when creating the contract. *)
  field balances : Map ByStr20 Uint128 =
    let m = Emp (ByStr20) (Uint128) in
    builtin put m owner total_tokens
  
  field allowed : Map ByStr20 (Map ByStr20 Uint128) =
    Emp (ByStr20) (Map ByStr20 Uint128)
  
  transition BalanceOf (tokenOwner : ByStr20)
    bal <- balances[tokenOwner];
    match bal with
    | Some v =>
      msg =
        {
          _tag : "BalanceOfResponse";
          _recipient : _sender;
          _amount : zero;
          address : tokenOwner;
          balance : v
        };
      msgs = one_msg msg;
      send msgs
    | None =>
      msg =
        {
          _tag : "BalanceOfResponse";
          _recipient : _sender;
          _amount : zero;
          address : tokenOwner;
          balance : zero
        };
      msgs = one_msg msg;
      send msgs
    end
  end
  
  transition TotalSupply ()
    msg =
      {
        _tag : "TotalSupplyResponse";
        _recipient : _sender;
        _amount : zero;
        caller : _sender;
        totalSupply : total_tokens
      };
    msgs = one_msg msg;
    send msgs
  end
  
  transition Transfer (to : ByStr20, tokens : Uint128)
    bal <- balances[_sender];
    match bal with
    | Some b =>
      can_do = le_int tokens b;
      match can_do with
      | True =>
        (* subtract tokens from _sender and add it to "to" *)
        new_sender_bal = builtin sub b tokens;
        balances[_sender] := new_sender_bal;
        (* Adds tokens to "to" address *)
        to_bal <- balances[to];
        new_to_bal =
          match to_bal with
          | Some x => builtin add x tokens
          | None => tokens
          end;
        balances[to] := new_to_bal;
        msg =
          {
            _tag : "TransferSuccess";
            _recipient : _sender;
            _amount : zero;
            sender : _sender;
            recipient : to;
            amount : tokens
          };
        msgs = one_msg msg;
        send msgs
      | False =>
        (* balance not sufficient. *)
        msg =
          {
            _tag : "TransferFailure";
            _recipient : _sender;
            _amount : zero;
            sender : _sender;
            recipient : to;
            amount : zero
          };
        msgs = one_msg msg;
        send msgs
      end
    | None =>
      (* no balance record, can't transfer *)
      msg =
        {
          _tag : "TransferFailure";
          _recipient : _sender;
          _amount : zero;
          sender : _sender;
          recipient : to;
          amount : zero
        };
      msgs = one_msg msg;
      send msgs
    end
  end
  
  transition TransferFrom (from : ByStr20, to : ByStr20, tokens : Uint128)
    bal <- balances[from];
    (* Check if _sender has been authorized by "from" *)
    sender_allowed_from <- allowed[from][_sender];
    match bal with
    | Some a =>
      match sender_allowed_from with
      | Some b =>
        (* We can only transfer the minimum of available or authorized tokens *)
        t = min_int a b;
        can_do = le_int tokens t;
        match can_do with
        | True =>
          (* tokens is what we should subtract from "from" and add to "to" *)
          new_from_bal = builtin sub a tokens;
          balances[from] := new_from_bal;
          to_bal <- balances[to];
          match to_bal with
          | Some tb =>
            new_to_bal = builtin add tb tokens;
            balances[to] := new_to_bal
          | None =>
            (* "to" has no balance. So just set it to tokens *)
            balances[to] := tokens
          end;
          (* reduce "allowed" by "tokens" *)
          new_allowed = builtin sub b tokens;
          allowed[from][_sender] := new_allowed;
          msg =
            {
              _tag : "TransferFromSuccess";
              _recipient : _sender;
              _amount : zero;
              sender : from;
              recipient : to;
              amount : tokens
            };
          msgs = one_msg msg;
          send msgs
        | False =>
          msg =
            {
              _tag : "TransferFromFailure";
              _recipient : _sender;
              _amount : zero;
              sender : from;
              recipient : to;
              amount : zero
            };
          msgs = one_msg msg;
          send msgs
        end
      | None =>
        msg =
          {
            _tag : "TransferFromFailure";
            _recipient : _sender;
            _amount : zero;
            sender : from;
            recipient : to;
            amount : zero
          };
        msgs = one_msg msg;
        send msgs
      end
    | None =>
      msg =
        {
          _tag : "TransferFromFailure";
          _recipient : _sender;
          _amount : zero;
          sender : from;
          recipient : to;
          amount : zero
        };
      msgs = one_msg msg;
      send msgs
    end
  end
  
  transition Approve (spender : ByStr20, tokens : Uint128)
    allowed[_sender][spender] := tokens;
    msg =
      {
        _tag : "ApproveSuccess";
        _recipient : _sender;
        _amount : zero;
        approver : _sender;
        spender : spender;
        amount : tokens
      };
    msgs = one_msg msg;
    send msgs
  end
  
  transition Allowance (tokenOwner : ByStr20, spender : ByStr20)
    spender_allowance <- allowed[tokenOwner][spender];
    match spender_allowance with
    | Some n =>
      msg =
        {
          _tag : "AllowanceResponse";
          _recipient : _sender;
          _amount : zero;
          owner : tokenOwner;
          spender : spender;
          amount : n
        };
      msgs = one_msg msg;
      send msgs
    | None =>
      msg =
        {
          _tag : "AllowanceResponse";
          _recipient : _sender;
          _amount : zero;
          owner : tokenOwner;
          spender : spender;
          amount : zero
        };
      msgs = one_msg msg;
      send msgs
    end
  end
  
