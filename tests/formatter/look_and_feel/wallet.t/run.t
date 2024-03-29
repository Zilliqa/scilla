  $ scilla-fmt wallet.scilla
  scilla_version 0
  
  import
    ListUtils
    IntUtils
  
  (***************************************************)
  (*               Associated library                *)
  (***************************************************)
  library WalletLib
  
  (* Event for communicating a new transaction id *)
  let mk_transaction_added_event =
    fun (tc : Uint32) =>
      { _eventname : "Transaction created"; transactionId : tc }
  
  (* Event for communicating the signing of a transaction *)
  let mk_transaction_signed_event =
    fun (no_of_sigs : Uint32) =>
      { _eventname : "Transaction signed"; signature_count : no_of_sigs }
  
  (* Event for communicating the addition of a new owner *)
  let mk_candidate_owner_added_event = { _eventname : "Candiate owner added" }
  
  (* Event for communicating the signing off of a new owner *)
  let mk_owner_signed_event =
    fun (no_of_sigs : Uint32) =>
      { _eventname : "Owner signed"; signature_count : no_of_sigs }
  
  (* Event for communicating the addition of a new owner *)
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
  
  (* Error events *)
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
  
  (* One (potential) transaction, consisting of a recipient address and an amount *)
  type Transaction =
  | Trans of ByStr20 Uint128
  
  (* Make map of owners *)
  let mk_owners_map =
    fun (initial_owners : List ByStr20) =>
      let init = Emp (ByStr20) (Bool) in
      let iter =
        fun (acc : Map ByStr20 Bool) =>
          fun (cur_owner : ByStr20) =>
            let mem = builtin get acc cur_owner in
            match mem with
            | Some True =>
              (* owner already added *)
              acc
            | _ =>
              (* owner not yet added, or removed *)
              let t = True in
              builtin put acc cur_owner t
            end
      in
      let folder = @list_foldl (ByStr20) (Map ByStr20 Bool) in
      folder iter init initial_owners
  
  (* Check that the number of distinct owners is greater than 0 *)
  let check_contract_validity =
    fun (owners : Map ByStr20 Bool) =>
      let no_of_owners = builtin size owners in
      let zero = Uint32 0 in
      builtin lt zero no_of_owners
  
  let transaction_executed = Int32 2
  
  (* Create one transaction message *)
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
  
  (* Wrap one transaction message as singleton list *)
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
  
  (* list_mem for owners *)
  let address_mem =
    fun (sender : ByStr20) =>
      fun (mem_map : Map ByStr20 Bool) =>
        let mem = builtin get mem_map sender in
        match mem with
        | None => False
        | Some False => False
        | Some True => True
        end
  
  
  (***************************************************)
  (*             The contract definition             *)
  (*                                                 *)
  (* This contract holds funds that can be paid out  *)
  (* to arbitrary users, provided that enough people *)
  (* the collection of owners sign off on the payout *)
  (*                                                 *)
  (* The transaction must be added to the contract   *)
  (* before signatures can be collected. Once enough *)
  (* signatures are collected, the recipient can ask *)
  (* for the transaction to be executed and the      *)
  (* money paid out.                                 *)
  (*                                                 *)
  (* A new owner can be added, provided that every   *)
  (* existing user signs off on the new owner. Once  *)
  (* all existing owners have signed for the new     *)
  (* owner, the new owner can claim ownership and be *)
  (* added to the owners map.                        *)
  (*                                                 *)
  (* If an owner changes his mind about a            *)
  (* transaction or an owner he has already signed   *)
  (* for, the signature can be revoked until the     *)
  (* transaction is executed or the owner has been   *)
  (* approved.                                       *)
  (***************************************************)
  contract Wallet
    (
      initial_owners : List ByStr20,
      required_signatures : Uint32
    )
  
  
  (* Funds are not allowed to be added if the contract is not valid *)
  field validity_checked : Bool = False
  
  field contract_valid : Bool = False
  
  (* adr -> True indicates that an owner *)
  (* adr not in map indicates non-owner *)
  (* adr -> False is not used *)
  (* The initial owners will be added as owners when funds are *)
  (* initially added to the contract. *)
  field owners : Map ByStr20 Bool = Emp (ByStr20) (Bool)
  
  field transactionCount : Uint32 = Uint32 0
  
  (* Collected signatures for transactions *)
  field signatures : Map Uint32 (Map ByStr20 Bool) =
    Emp (Uint32) (Map ByStr20 Bool)
  
  (* Transactions *)
  field transactions : Map Uint32 Transaction = Emp (Uint32) (Transaction)
  
  (* Collected signatures for new owners *)
  field owner_signatures : Map ByStr20 (Map ByStr20 Bool) =
    Emp (ByStr20) (Map ByStr20 Bool)
  
  (* Submit a transaction for future signoff *)
  transition SubmitTransaction (recipient : ByStr20, amount : Uint128)
    tc <- transactionCount;
    zero = Uint128 0;
    amount_is_zero = builtin eq amount zero;
    match amount_is_zero with
    | True =>
      (* Illegal transaction *)
      err = InvalidAmount;
      e = mk_error_event err;
      event e
    | False =>
      (* Create new transaction *)
      transaction = Trans recipient amount;
      (* Add transaction to outstanding list of transactions *)
      ts_tmp <- transactions;
      ts_new = builtin put ts_tmp tc transaction;
      (* Add empty list of signatures *)
      sigs_tmp <- signatures;
      sigs_new = builtin put sigs_tmp tc empty_sigs;
      (* Increment transaction counter *)
      tc_new = builtin add tc transaction_inc;
      (* Update fields *)
      transactionCount := tc_new;
      transactions := ts_new;
      signatures := sigs_new;
      (* Create event with transaction Id *)
      e = mk_transaction_added_event tc;
      event e
    end
  end
  
  (* Sign off on an existing transaction *)
  transition SignTransaction (transactionId : Uint32)
    (* Helper function *)
    sender_mem = address_mem _sender;
    (* Only the owner is allowed to sign off transactions *)
    owners_tmp <- owners;
    sender_is_owner = sender_mem owners_tmp;
    match sender_is_owner with
    | False =>
      err = NonOwnerCannotSign;
      e = mk_error_event err;
      event e
    | True =>
      (* Transaction must have been submitted *)
      ts_tmp <- transactions;
      transaction = builtin get ts_tmp transactionId;
      match transaction with
      | None =>
        err = UnknownTransactionId;
        e = mk_error_event err;
        event e
      | Some (Trans recipient amount) =>
        (* Transaction must occur in signatures map *)
        sigs_tmp <- signatures;
        sigs_opt = builtin get sigs_tmp transactionId;
        match sigs_opt with
        | None =>
          err = NoSignatureListFound;
          e = mk_error_event err;
          event e
        | Some sigs =>
          (* Sender must not have signed already *)
          sender_has_signed = sender_mem sigs;
          match sender_has_signed with
          | True =>
            err = AlreadySigned;
            e = mk_error_event err;
            event e
          | False =>
            (* Signature is valid. Add to collected signatures *)
            t = True;
            new_sigs = builtin put sigs _sender t;
            new_signatures = builtin put sigs_tmp transactionId new_sigs;
            signatures := new_signatures
          end
        end
      end
    end
  end
  
  (* Execute signed-off transaction *)
  transition ExecuteTransaction (transactionId : Uint32, tag : String)
    transactions_tmp <- transactions;
    transaction_opt = builtin get transactions_tmp transactionId;
    match transaction_opt with
    | None =>
      (* Transaction was not found. *)
      err = UnknownTransactionId;
      e = mk_error_event err;
      event e
    | Some (Trans recipient amount) =>
      (* Only the recipient can initiate the transaction *)
      recipient_is_sender = builtin eq recipient _sender;
      match recipient_is_sender with
      | False =>
        err = SenderIsNotRecipient;
        e = mk_error_event err;
        event e
      | True =>
        (* Check for sufficient funds  *)
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
            (* Signatures not found, even though the transaction exists. *)
            err = UnknownTransactionId;
            e = mk_error_event err;
            event e
          | Some sigs =>
            (* Check for sufficient number of signatures *)
            no_of_sigs = builtin size sigs;
            not_enough_signatures = builtin lt no_of_sigs required_signatures;
            match not_enough_signatures with
            | True =>
              err = NotEnoughSignatures;
              e = mk_error_event err;
              event e
            | False =>
              (* Transaction approved, and enough money available. *)
              (* Remove transaction and signatures, and execute. *)
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
  
  (* Revoke signature of existing transaction, if it has not yet been executed. *)
  transition RevokeSignature (transactionId : Uint32)
    (* Transaction must occur in signatures map *)
    sigs_tmp <- signatures;
    sigs_opt = builtin get sigs_tmp transactionId;
    match sigs_opt with
    | None =>
      err = NoSignatureListFound;
      e = mk_error_event err;
      event e
    | Some sigs =>
      (* Sender must have signed already *)
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
  
  (* Revoke signature for new owner *)
  transition RevokeOwnerSignature (new_owner : ByStr20)
    (* new owner must occur in signatures map *)
    sigs_tmp <- owner_signatures;
    sigs_opt = builtin get sigs_tmp new_owner;
    match sigs_opt with
    | None =>
      err = NoSignatureListFound;
      e = mk_error_event err;
      event e
    | Some sigs =>
      (* Sender must have signed already *)
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
  
  (* Add candidate owner *)
  transition AddCandidateOwner (candidate : ByStr20)
    (* Check validity of contract. *)
    (* Owners map must be initialized for new owners to be added. *)
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
    (* Only accept funds if the contract is valid. *)
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
        (* Candidate already added *)
        err = CandidateAlreadyAdded;
        e = mk_error_event err;
        event e
      | None =>
        owners_tmp <- owners;
        owner_option = builtin get owners_tmp candidate;
        match owner_option with
        | Some _ =>
          (* Candidate is already an owner *)
          err = CandidateAlreadyOwner;
          e = mk_error_event err;
          event e
        | None =>
          (* New candidate *)
          empty_sigs = Emp (ByStr20) (Bool);
          new_owner_signatures =
            builtin put owner_signatures_tmp candidate empty_sigs;
          owner_signatures := new_owner_signatures
        end
      end
    end
  end
  
  (* Sign off on new owner. *)
  transition SignOffNewOwner (candidate : ByStr20)
    (* Helpers *)
    sender_mem = address_mem _sender;
    t = True;
    (* Only owners are allowed to sign off new owners *)
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
        (* Unknown candidate *)
        err = UnknownCandidate;
        e = mk_error_event err;
        event e
      | Some sigs =>
        (* Sender must not have signed already *)
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
          (* Create event with owner id *)
          no_of_sigs = builtin size new_sigs;
          e = mk_owner_signed_event no_of_sigs;
          event e
        end
      end
    end
  end
  
  (* Promote _sender to owner, if all existing owners have signed *)
  transition ClaimOwnership ()
    owner_signatures_tmp <- owner_signatures;
    sigs_option = builtin get owner_signatures_tmp _sender;
    (* Check if all owners have signed *)
    match sigs_option with
    | None =>
      (* Unknown candidate *)
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
        (* Not enough signatures *)
        err = NotEnoughSignatures;
        e = mk_error_event err;
        event e
      | True =>
        (* Enough signatures collected. *)
        (* Remove signatures, and add sender to owner collection *)
        new_owner_signatures = builtin remove owner_signatures_tmp _sender;
        owner_signatures := new_owner_signatures;
        t = True;
        new_owners = builtin put current_owners _sender t;
        owners := new_owners;
        (* Create event with owner id *)
        e = mk_new_owner_approved_event;
        event e
      end
    end
  end
  
  (* Add funds to wallet *)
  transition AddFunds ()
    (* Check validity of contract. If the contract is invalid, funds may become locked *)
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
    (* Only accept funds if the contract is valid. *)
    valid <- contract_valid;
    match valid with
    | False =>
      err = InvalidContract;
      e = mk_error_event err;
      event e
    | True => accept
    end
  end
  
