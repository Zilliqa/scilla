  $ scilla-fmt wallet_2.scilla
  scilla_version 0
  
  import
    ListUtils
    IntUtils
    BoolUtils
  
  library WalletLib
  
  let mk_contract_initialized_event = { _eventname : "Contract initialized" }
  
  let mk_transaction_added_event =
    fun (tc : Uint32) =>
      fun (recipient : ByStr20) =>
        fun (amount : Uint128) =>
          fun (tag : String) =>
            {
              _eventname : "Transaction created";
              transactionId : tc;
              recipient : recipient;
              amount : amount;
              tag : tag
            }
  
  let mk_transaction_executed_event =
    fun (tc : Uint32) =>
      fun (recipient : ByStr20) =>
        fun (amount : Uint128) =>
          fun (tag : String) =>
            {
              _eventname : "Transaction executed";
              transactionId : tc;
              recipient : recipient;
              amount : amount;
              tag : tag
            }
  
  let mk_signed_transaction_event =
    fun (tc : Uint32) =>
      { _eventname : "Transaction signed"; transactionId : tc }
  
  let mk_signature_revoked_event =
    fun (tc : Uint32) =>
      { _eventname : "Signature revoked"; transactionId : tc }
  
  type Error =
  | NonOwnerCannotSign
  | UnknownTransactionId
  | InsufficientFunds
  | NoSignatureListFound
  | AlreadySigned
  | NotAlreadySigned
  | InvalidContract
  | InvalidAmount
  | NotEnoughSignatures
  | SenderMayNotExecute
  | NonOwnerCannotSubmit
  | IncorrectSignatureCount
  
  let mk_error_event =
    fun (err : Error) =>
      let err_code =
        match err with
        | NonOwnerCannotSign => Int32 -1
        | UnknownTransactionId => Int32 -2
        | InsufficientFunds => Int32 -3
        | NoSignatureListFound => Int32 -4
        | AlreadySigned => Int32 -5
        | NotAlreadySigned => Int32 -6
        | InvalidContract => Int32 -7
        | InvalidAmount => Int32 -8
        | NotEnoughSignatures => Int32 -9
        | SenderMayNotExecute => Int32 -10
        | NonOwnerCannotSubmit => Int32 -11
        | IncorrectSignatureCount => Int32 -12
        end
      in
      { _eventname : "WalletError"; err_code : err_code }
  
  let t = True
  
  let f = False
  
  let zero = Uint32 0
  
  let one = Uint32 1
  
  let transaction_inc = one
  
  type Transaction =
  | Trans of ByStr20 Uint128 String
  
  let mk_owners_map =
    fun (owners : List ByStr20) =>
      let init = Emp (ByStr20) (Bool) in
      let iter =
        fun (acc : Map ByStr20 Bool) =>
          fun (cur_owner : ByStr20) =>
            builtin put acc cur_owner t
      in
      let folder = @list_foldl (ByStr20) (Map ByStr20 Bool) in
      folder iter init owners
  
  let transaction_msg =
    fun (recipient : ByStr20) =>
      fun (amount : Uint128) =>
        fun (tag : String) =>
          { _tag : tag; _recipient : recipient; _amount : amount }
  
  let transaction_msg_as_list =
    fun (recipient : ByStr20) =>
      fun (amount : Uint128) =>
        fun (tag : String) =>
          let one_msg =
            fun (msg : Message) =>
              let nil_msg = Nil {(Message)} in
              Cons {(Message)} msg nil_msg
          in
          let msg = transaction_msg recipient amount tag in
          one_msg msg
  
  
  contract Wallet
    (
      owners_list : List ByStr20,
      required_signatures : Uint32
    )
  with
    let len = @list_length (ByStr20) in
    let no_of_owners = len owners_list in
    let owners_ok = builtin lt zero no_of_owners in
    let required_sigs_not_too_low = builtin lt zero required_signatures in
    let required_sigs_too_high = builtin lt no_of_owners required_signatures in
    let required_sigs_not_too_high = negb required_sigs_too_high in
    let required_sigs_ok =
      andb required_sigs_not_too_high required_sigs_not_too_low
    in
    let all_ok = andb required_sigs_ok owners_ok in
    match all_ok with
    | True =>
      let owners_map = mk_owners_map owners_list in
      let size_of_owners_map = builtin size owners_map in
      builtin eq size_of_owners_map no_of_owners
    | False => False
    end
  =>
  
  
  field owners : Map ByStr20 Bool = mk_owners_map owners_list
  
  field transactionCount : Uint32 = Uint32 0
  
  field signatures : Map Uint32 (Map ByStr20 Bool) =
    Emp (Uint32) (Map ByStr20 Bool)
  
  field signature_counts : Map Uint32 Uint32 = Emp (Uint32) (Uint32)
  
  field transactions : Map Uint32 Transaction = Emp (Uint32) (Transaction)
  
  procedure MakeError (err : Error)
    e = mk_error_event err;
    event e
  end
  
  procedure AddSignature (transactionId : Uint32, signee : ByStr20)
    sig <- exists signatures[transactionId][signee];
    match sig with
    | False =>
      count <- signature_counts[transactionId];
      match count with
      | None => signature_counts[transactionId] := one
      | Some c =>
        new_c = builtin add c one;
        signature_counts[transactionId] := new_c
      end;
      signatures[transactionId][signee] := t;
      e = mk_signed_transaction_event transactionId;
      event e
    | True =>
      err = AlreadySigned;
      MakeError err
    end
  end
  
  transition SubmitTransaction
    (recipient : ByStr20, amount : Uint128, tag : String)
    sender_is_owner <- exists owners[_sender];
    match sender_is_owner with
    | False =>
      err = NonOwnerCannotSubmit;
      MakeError err
    | True =>
      tc <- transactionCount;
      zero = Uint128 0;
      amount_is_zero = builtin eq amount zero;
      match amount_is_zero with
      | True =>
        err = InvalidAmount;
        MakeError err
      | False =>
        transaction = Trans recipient amount tag;
        transactions[tc] := transaction;
        AddSignature tc _sender;
        tc_new = builtin add tc transaction_inc;
        transactionCount := tc_new;
        e = mk_transaction_added_event tc recipient amount tag;
        event e
      end
    end
  end
  
  transition SignTransaction (transactionId : Uint32)
    sender_is_owner <- exists owners[_sender];
    match sender_is_owner with
    | False =>
      err = NonOwnerCannotSign;
      MakeError err
    | True =>
      transaction <- transactions[transactionId];
      match transaction with
      | None =>
        err = UnknownTransactionId;
        MakeError err
      | Some _ => AddSignature transactionId _sender
      end
    end
  end
  
  procedure DeleteTransaction (transactionId : Uint32)
    delete transactions[transactionId];
    delete signatures[transactionId];
    delete signature_counts[transactionId]
  end
  
  transition ExecuteTransaction (transactionId : Uint32)
    transaction_opt <- transactions[transactionId];
    match transaction_opt with
    | None =>
      err = UnknownTransactionId;
      MakeError err
    | Some (Trans recipient amount tag) =>
      recipient_is_sender = builtin eq recipient _sender;
      sender_is_owner <- exists owners[_sender];
      sender_may_execute = orb recipient_is_sender sender_is_owner;
      match sender_may_execute with
      | False =>
        err = SenderMayNotExecute;
        MakeError err
      | True =>
        bal <- _balance;
        not_enough_money = builtin lt bal amount;
        match not_enough_money with
        | True =>
          err = InsufficientFunds;
          MakeError err
        | False =>
          sig_count_opt <- signature_counts[transactionId];
          match sig_count_opt with
          | None =>
            err = NoSignatureListFound;
            MakeError err
          | Some sig_count =>
            not_enough_signatures = builtin lt sig_count required_signatures;
            match not_enough_signatures with
            | True =>
              err = NotEnoughSignatures;
              MakeError err
            | False =>
              DeleteTransaction transactionId;
              msgs = transaction_msg_as_list recipient amount tag;
              send msgs;
              e = mk_transaction_executed_event transactionId recipient amount tag;
              event e
            end
          end
        end
      end
    end
  end
  
  transition RevokeSignature (transactionId : Uint32)
    sig <- exists signatures[transactionId][_sender];
    match sig with
    | False =>
      err = NotAlreadySigned;
      MakeError err
    | True =>
      count <- signature_counts[transactionId];
      match count with
      | None =>
        err = IncorrectSignatureCount;
        MakeError err
      | Some c =>
        c_is_zero = builtin eq c zero;
        match c_is_zero with
        | True =>
          err = IncorrectSignatureCount;
          MakeError err
        | False =>
          new_c = builtin sub c one;
          signature_counts[transactionId] := new_c;
          delete signatures[transactionId][_sender];
          e = mk_signature_revoked_event transactionId;
          event e
        end
      end
    end
  end
  
  transition AddFunds ()
    accept
  end
  
