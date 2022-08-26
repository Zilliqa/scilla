  $ scilla-fmt wallet.scilla
  scilla_version 0
  
  import
    ListUtils
    IntUtils
  
  library WalletLib
  
  let mk_transaction_added_event =
    fun (tc : Uint32) =>
      { _eventname : "Transaction created"; transactionId : tc }
  
  let mk_transaction_signed_event =
    fun (no_of_sigs : Uint32) =>
      { _eventname : "Transaction signed"; signature_count : no_of_sigs }
  
  let mk_candidate_owner_added_event = { _eventname : "Candiate owner added" }
  
  let mk_owner_signed_event =
    fun (no_of_sigs : Uint32) =>
      { _eventname : "Owner signed"; signature_count : no_of_sigs }
  
  let mk_new_owner_approved_event = { _eventname : "New owner approved" }
  
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
  | SenderIsNotRecipient
  | CandidateAlreadyAdded
  | UnknownCandidate
  | CandidateAlreadyOwner
  
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
        | SenderIsNotRecipient => Int32 -10
        | CandidateAlreadyAdded => Int32 -11
        | UnknownCandidate => Int32 -12
        | CandidateAlreadyOwner => Int32 -13
        end
      in
      { _eventname : "WalletError"; err_code : err_code }
  
  let transaction_inc = Uint32 1
  
  let empty_sigs = Emp (ByStr20) (Bool)
  
  type Transaction =
  | Trans of ByStr20 Uint128
  
  let mk_owners_map =
    fun (initial_owners : List ByStr20) =>
      let init = Emp (ByStr20) (Bool) in
      let iter =
        fun (acc : Map ByStr20 Bool) =>
          fun (cur_owner : ByStr20) =>
            let mem = builtin get acc cur_owner in
            match mem with
            | Some True => acc
            | _ => let t = True in builtin put acc cur_owner t
            end
      in
      let folder = @list_foldl (ByStr20) (Map ByStr20 Bool) in
      folder iter init initial_owners
  
  let check_contract_validity =
    fun (owners : Map ByStr20 Bool) =>
      let no_of_owners = builtin size owners in
      let zero = Uint32 0 in
      builtin lt zero no_of_owners
  
  let transaction_executed = Int32 2
  
  let transaction_msg =
    fun (recipient : ByStr20) =>
      fun (amount : Uint128) =>
        fun (tag : String) =>
          {
            _tag : tag;
            _recipient : recipient;
            _amount : amount;
            code : transaction_executed
          }
  
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
  
  let address_mem =
    fun (sender : ByStr20) =>
      fun (mem_map : Map ByStr20 Bool) =>
        let mem = builtin get mem_map sender in
        match mem with
        | None => False
        | Some False => False
        | Some True => True
        end
  
  
  contract Wallet
    (
      initial_owners : List ByStr20,
      required_signatures : Uint32
    )
  
  
  field validity_checked : Bool = False
  
  field contract_valid : Bool = False
  
  field owners : Map ByStr20 Bool = Emp (ByStr20) (Bool)
  
  field transactionCount : Uint32 = Uint32 0
  
  field signatures : Map Uint32 (Map ByStr20 Bool) =
    Emp (Uint32) (Map ByStr20 Bool)
  
  field transactions : Map Uint32 Transaction = Emp (Uint32) (Transaction)
  
  field owner_signatures : Map ByStr20 (Map ByStr20 Bool) =
    Emp (ByStr20) (Map ByStr20 Bool)
  
  transition SubmitTransaction (recipient : ByStr20, amount : Uint128)
    tc <- transactionCount;
    zero = Uint128 0;
    amount_is_zero = builtin eq amount zero;
    match amount_is_zero with
    | True =>
      err = InvalidAmount;
      e = mk_error_event err;
      event e
    | False =>
      transaction = Trans recipient amount;
      ts_tmp <- transactions;
      ts_new = builtin put ts_tmp tc transaction;
      sigs_tmp <- signatures;
      sigs_new = builtin put sigs_tmp tc empty_sigs;
      tc_new = builtin add tc transaction_inc;
      transactionCount := tc_new;
      transactions := ts_new;
      signatures := sigs_new;
      e = mk_transaction_added_event tc;
      event e
    end
  end
  
  transition SignTransaction (transactionId : Uint32)
    sender_mem = address_mem _sender;
    owners_tmp <- owners;
    sender_is_owner = sender_mem owners_tmp;
    match sender_is_owner with
    | False =>
      err = NonOwnerCannotSign;
      e = mk_error_event err;
      event e
    | True =>
      ts_tmp <- transactions;
      transaction = builtin get ts_tmp transactionId;
      match transaction with
      | None =>
        err = UnknownTransactionId;
        e = mk_error_event err;
        event e
      | Some (Trans recipient amount) =>
        sigs_tmp <- signatures;
        sigs_opt = builtin get sigs_tmp transactionId;
        match sigs_opt with
        | None =>
          err = NoSignatureListFound;
          e = mk_error_event err;
          event e
        | Some sigs =>
          sender_has_signed = sender_mem sigs;
          match sender_has_signed with
          | True =>
            err = AlreadySigned;
            e = mk_error_event err;
            event e
          | False =>
            t = True;
            new_sigs = builtin put sigs _sender t;
            new_signatures = builtin put sigs_tmp transactionId new_sigs;
            signatures := new_signatures
          end
        end
      end
    end
  end
  
  transition ExecuteTransaction (transactionId : Uint32, tag : String)
    transactions_tmp <- transactions;
    transaction_opt = builtin get transactions_tmp transactionId;
    match transaction_opt with
    | None =>
      err = UnknownTransactionId;
      e = mk_error_event err;
      event e
    | Some (Trans recipient amount) =>
      recipient_is_sender = builtin eq recipient _sender;
      match recipient_is_sender with
      | False =>
        err = SenderIsNotRecipient;
        e = mk_error_event err;
        event e
      | True =>
        bal <- _balance;
        not_enough_money = builtin lt bal amount;
        match not_enough_money with
        | True =>
          err = InsufficientFunds;
          e = mk_error_event err;
          event e
        | False =>
          signatures_tmp <- signatures;
          sigs_opt = builtin get signatures_tmp transactionId;
          match sigs_opt with
          | None =>
            err = UnknownTransactionId;
            e = mk_error_event err;
            event e
          | Some sigs =>
            no_of_sigs = builtin size sigs;
            not_enough_signatures = builtin lt no_of_sigs required_signatures;
            match not_enough_signatures with
            | True =>
              err = NotEnoughSignatures;
              e = mk_error_event err;
              event e
            | False =>
              new_transactions = builtin remove transactions_tmp transactionId;
              transactions := new_transactions;
              new_signatures = builtin remove signatures_tmp transactionId;
              signatures := new_signatures;
              msgs = transaction_msg_as_list recipient amount tag;
              send msgs
            end
          end
        end
      end
    end
  end
  
  transition RevokeSignature (transactionId : Uint32)
    sigs_tmp <- signatures;
    sigs_opt = builtin get sigs_tmp transactionId;
    match sigs_opt with
    | None =>
      err = NoSignatureListFound;
      e = mk_error_event err;
      event e
    | Some sigs =>
      sender_has_signed = address_mem _sender sigs;
      match sender_has_signed with
      | False =>
        err = NotAlreadySigned;
        e = mk_error_event err;
        event e
      | True =>
        new_sigs = builtin remove sigs _sender;
        new_signatures = builtin put sigs_tmp transactionId new_sigs;
        signatures := new_signatures
      end
    end
  end
  
  transition RevokeOwnerSignature (new_owner : ByStr20)
    sigs_tmp <- owner_signatures;
    sigs_opt = builtin get sigs_tmp new_owner;
    match sigs_opt with
    | None =>
      err = NoSignatureListFound;
      e = mk_error_event err;
      event e
    | Some sigs =>
      sender_has_signed = address_mem _sender sigs;
      match sender_has_signed with
      | False =>
        err = NotAlreadySigned;
        e = mk_error_event err;
        event e
      | True =>
        new_sigs = builtin remove sigs _sender;
        new_signatures = builtin put sigs_tmp new_owner new_sigs;
        owner_signatures := new_signatures
      end
    end
  end
  
  transition AddCandidateOwner (candidate : ByStr20)
    checked <- validity_checked;
    match checked with
    | False =>
      owners_map = mk_owners_map initial_owners;
      owners := owners_map;
      valid_contract = check_contract_validity owners_map;
      contract_valid := valid_contract;
      checked_now = True;
      validity_checked := checked_now
    | True =>
    end;
    valid <- contract_valid;
    match valid with
    | False =>
      err = InvalidContract;
      e = mk_error_event err;
      event e
    | True =>
      owner_signatures_tmp <- owner_signatures;
      sigs_option = builtin get owner_signatures_tmp candidate;
      match sigs_option with
      | Some _ =>
        err = CandidateAlreadyAdded;
        e = mk_error_event err;
        event e
      | None =>
        owners_tmp <- owners;
        owner_option = builtin get owners_tmp candidate;
        match owner_option with
        | Some _ =>
          err = CandidateAlreadyOwner;
          e = mk_error_event err;
          event e
        | None =>
          empty_sigs = Emp (ByStr20) (Bool);
          new_owner_signatures =
            builtin put owner_signatures_tmp candidate empty_sigs;
          owner_signatures := new_owner_signatures
        end
      end
    end
  end
  
  transition SignOffNewOwner (candidate : ByStr20)
    sender_mem = address_mem _sender;
    t = True;
    owners_tmp <- owners;
    sender_is_owner = sender_mem owners_tmp;
    match sender_is_owner with
    | False =>
      err = NonOwnerCannotSign;
      e = mk_error_event err;
      event e
    | True =>
      owner_signatures_tmp <- owner_signatures;
      sigs_option = builtin get owner_signatures_tmp candidate;
      match sigs_option with
      | None =>
        err = UnknownCandidate;
        e = mk_error_event err;
        event e
      | Some sigs =>
        sender_has_signed = sender_mem sigs;
        match sender_has_signed with
        | True =>
          err = AlreadySigned;
          e = mk_error_event err;
          event e
        | False =>
          new_sigs = builtin put sigs _sender t;
          new_owner_signatures =
            builtin put owner_signatures_tmp candidate new_sigs;
          owner_signatures := new_owner_signatures;
          no_of_sigs = builtin size new_sigs;
          e = mk_owner_signed_event no_of_sigs;
          event e
        end
      end
    end
  end
  
  transition ClaimOwnership ()
    owner_signatures_tmp <- owner_signatures;
    sigs_option = builtin get owner_signatures_tmp _sender;
    match sigs_option with
    | None =>
      err = UnknownCandidate;
      e = mk_error_event err;
      event e
    | Some sigs =>
      current_owners <- owners;
      no_of_owners = builtin size current_owners;
      no_of_sigs = builtin size sigs;
      all_have_signed = uint32_eq no_of_sigs no_of_owners;
      match all_have_signed with
      | False =>
        err = NotEnoughSignatures;
        e = mk_error_event err;
        event e
      | True =>
        new_owner_signatures = builtin remove owner_signatures_tmp _sender;
        owner_signatures := new_owner_signatures;
        t = True;
        new_owners = builtin put current_owners _sender t;
        owners := new_owners;
        e = mk_new_owner_approved_event;
        event e
      end
    end
  end
  
  transition AddFunds ()
    checked <- validity_checked;
    match checked with
    | False =>
      owners_map = mk_owners_map initial_owners;
      owners := owners_map;
      valid_contract = check_contract_validity owners_map;
      contract_valid := valid_contract;
      checked_now = True;
      validity_checked := checked_now
    | True =>
    end;
    valid <- contract_valid;
    match valid with
    | False =>
      err = InvalidContract;
      e = mk_error_event err;
      event e
    | True => accept
    end
  end
  
