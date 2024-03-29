  $ scilla-fmt Polynetwork.scilla
  scilla_version 0
  
  import
    Polynetwork
    BoolUtils
  
  library Polynetwork_local
  
  let nullAddress = 0x0000000000000000000000000000000000000000
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  
  contract Polynetwork (thisChainID : Uint64)
  
  
  (*
   * Scilla cross chain tx hash indexed by the automatically increased index.
   * This map exists for the reason that Poly chain can verify the existence
   * of cross chain request tx coming from Scilla
   *)
  field f_zilToPolyTxHashMap : Map Uint256 ByStr32 = Emp (Uint256) (ByStr32)
  
  field f_zilToPolyTxHashIndex : Uint256 = Uint256 0
  
  field f_curKeepers : List ByStr20 = Nil {(ByStr20)}
  
  field f_curStartHeight : Uint32 = Uint32 0
  
  field f_fromChainTxExist : Map Uint64 (Map ByStr32 Unit) =
    Emp (Uint64) (Map ByStr32 Unit)
  
  procedure verifyPubkeysAndUpdate
    (pubkeys : List Pubkey, h_nextBookkeeper : ByStr20, h_height : Uint32)
    nextbookkeeper_keepers = verifyPubkey pubkeys;
    match nextbookkeeper_keepers with
    | Pair nextBookKeeper keepers =>
      (* Ensure that Header's nextBookKeeper is same as the one from verifyPubkey *)
      nbk_eq = builtin eq nextBookKeeper h_nextBookkeeper;
      match nbk_eq with
      | True =>
        f_curStartHeight := h_height;
        f_curKeepers := keepers
      | False =>
        e = { _exception : "NextBookers Illegal" };
        throw e
      end
    end
  end
  
  transition initGenesisBlock (rawHeader : ByStr, pubkeys : List Pubkey)
    curKeepers <- f_curKeepers;
    match curKeepers with
    | Nil =>
      header_o = deserialize_Header rawHeader zero_uint32;
      match header_o with
      | Some
        (Pair
          (Header
            h_version
            h_chainid
            h_prevBlockHash
            h_txnroot
            h_crossStatesRoot
            h_blockRoot
            h_timestamp
            h_height
            h_consensusData
            h_consensusPayload
            h_nextBookkeeper)
          nextpos) =>
        verifyPubkeysAndUpdate pubkeys h_nextBookkeeper h_height;
        e = { _eventname : "GenesisBlock"; height : h_height; header : rawHeader };
        event e
      | None =>
        e = { _exception : "Error deserializing header" };
        throw e
      end
    | Cons _ _ =>
      e = { _exception : "Already Initialized" };
      throw e
    end
  end
  
  procedure checkAndMarkFromChainTxn (chainID : Uint64, txHash : ByStr)
    txHash32_o = builtin to_bystr32 txHash;
    match txHash32_o with
    | Some txHash32 =>
      already_exists <- exists f_fromChainTxExist[chainID][txHash32];
      match already_exists with
      | True =>
        e = { _exception : "Txn already executed" };
        throw e
      | False => f_fromChainTxExist[chainID][txHash32] := unit
      end
    | None =>
      e = { _exception : "Txn hash invalid" };
      throw e
    end
  end
  
  procedure executeCrossChainTxn
    (txparam : TxParam, fromChainId : Uint64, crossChainTxHash : ByStr)
    match txparam with
    | TxParam txHash crossChainID fromContract toChainID toContract method args =>
      toChainIDOk = builtin eq toChainID thisChainID;
      match toChainIDOk with
      | True =>
        toContractAddr_o = builtin to_bystr20 toContract;
        match toContractAddr_o with
        | Some toContractAddr =>
          method_name = builtin to_ascii method;
          m =
            {
              _tag : method_name;
              args : args;
              fromContractAddr : fromContract;
              fromChainId : fromChainId;
              _amount : Uint128 0;
              _recipient : toContractAddr
            };
          mo = one_msg m;
          send mo;
          e =
            {
              _eventname : "VerifyHeaderAndExecuteTxEvent";
              fromChainId : fromChainId;
              toContractAddr : toContractAddr;
              crossChainTxHash : crossChainTxHash;
              fromChainTxHash : txHash
            };
          event e
        | None =>
          e = { _exception : "Address format mismatch" };
          throw e
        end
      | False =>
        e = { _exception : "This txn is not for Zilliqa network" };
        throw e
      end
    end
  end
  
  (*  @notice              Verify Poly chain header and proof, execute the cross chain tx from Poly chain to Zilliqa
   *  @param proof         Poly chain tx merkle proof
   *  @param rawHeader     The header containing crossStateRoot to verify the above tx merkle proof
   *  @param headerProof   The header merkle proof used to verify rawHeader
   *  @param curRawHeader  Any header in current epoch consensus of Poly chain
   *  @param headerSig     The coverted signature veriable for solidity derived from Poly chain consensus nodes' signature
   *                       used to verify the validity of curRawHeader
   *  @return              true or false
   *)
  transition verifyHeaderAndExecuteTx
    (
      proof : Proof,
      rawHeader : ByStr,
      headerProof : Proof,
      curRawHeader : ByStr,
      headerSig : List Signature
    )
    header_o = deserialize_Header rawHeader zero_uint32;
    curKeepers <- f_curKeepers;
    curStartHeight <- f_curStartHeight;
    n = lengther_address curKeepers;
    m = compute_m n;
    match header_o with
    | Some
      (Pair
        (Header
          h_version
          h_chainid
          h_prevBlockHash
          h_txnroot
          h_crossStatesRoot
          h_blockRoot
          h_timestamp
          h_height
          h_consensusData
          h_consensusPayload
          h_nextBookkeeper)
        nextpos) =>
      is_lt = builtin lt h_height curStartHeight;
      match is_lt with
      | True =>
        signed = verifySig curRawHeader headerSig curKeepers m;
        match signed with
        | True =>
          curHeader_o = deserialize_Header curRawHeader zero_uint32;
          match curHeader_o with
          | Some
            (Pair
              (Header
                h_version
                h_chainid
                h_prevBlockHash
                h_txnroot
                h_crossStatesRoot
                h_blockRoot
                h_timestamp
                h_height
                h_consensusData
                h_consensusPayload
                h_nextBookkeeper)
              nextpos) =>
            proof_o = merkle_prove headerProof h_blockRoot;
            match proof_o with
            | Some proveValue =>
              proveValue32_o = builtin to_bystr32 proveValue;
              match proveValue32_o with
              | Some proveValue32 =>
                headerHash = get_header_hash rawHeader;
                proof_ok = builtin eq headerHash proveValue32;
                match proof_ok with
                | True => (* Do nothing *)
                | False =>
                  e = { _exception : "Merkle proof invalid" };
                  throw e
                end
              | None =>
                e = { _exception : "merkle_prove result incorrect" };
                throw e
              end
            | None =>
            end
          | None =>
            e = { _exception : "Error deserializing header" };
            throw e
          end
        | False =>
          e = { _exception : "Signature verification failed" };
          throw e
        end
      | False =>
        signed = verifySig rawHeader headerSig curKeepers m;
        match signed with
        | True => (* Do nothing *)
        | False =>
          e = { _exception : "Signature verification failed" };
          throw e
        end
      end;
      toMerkleValueBs_o = merkle_prove proof h_crossStatesRoot;
      match toMerkleValueBs_o with
      | Some toMerkleValueBs =>
        toMerkleValue_o = deserialize_ToMerkleValue toMerkleValueBs zero_uint32;
        match toMerkleValue_o with
        | Some (Pair (ToMerkleValue txhash fromChainId txparam) _) =>
          checkAndMarkFromChainTxn fromChainId txhash;
          executeCrossChainTxn txparam fromChainId txhash
        | None =>
          e = { _exception : "Merkle value deserialization failed" };
          throw e
        end
      | None =>
        e = { _exception : "Merkle proof invalid" };
        throw e
      end
    | None =>
      e = { _exception : "Error deserializing header" };
      throw e
    end
  end
  
  (*  @notice              change Poly chain consensus book keeper
   *  @param rawHeader     Poly chain change book keeper block raw header
   *  @param pubKeyList    Poly chain consensus nodes public key list
   *  @param sigList       Poly chain consensus nodes signature list
   *  @return              true or false
   *)
  transition changeBookKeeper
    (rawHeader : ByStr, pubkeys : List Pubkey, sigList : List Signature)
    header_o = deserialize_Header rawHeader zero_uint32;
    curStartHeight <- f_curStartHeight;
    match header_o with
    | Some
      (Pair
        (Header
          h_version
          h_chainid
          h_prevBlockHash
          h_txnroot
          h_crossStatesRoot
          h_blockRoot
          h_timestamp
          h_height
          h_consensusData
          h_consensusPayload
          h_nextBookkeeper)
        nextpos) =>
      heightOk = builtin lt curStartHeight h_height;
      nextBookKeeperOk = let b = builtin eq h_nextBookkeeper nullAddress in negb b;
      both_ok = andb heightOk nextBookKeeperOk;
      match both_ok with
      | True =>
        curKeepers <- f_curKeepers;
        n = lengther_address curKeepers;
        m = compute_m n;
        signed = verifySig rawHeader sigList curKeepers m;
        match signed with
        | True =>
          verifyPubkeysAndUpdate pubkeys h_nextBookkeeper h_height;
          e =
            {
              _eventname : "ChangeBookKeeper";
              height : h_height;
              header : rawHeader
            };
          event e
        | False =>
          e = { _exception : "Signature verification failed" };
          throw e
        end
      | False =>
        e =
          {
            _exception : "Header height lower than cur epoch heigh / Next bookkeeper empty"
          };
        throw e
      end
    | None =>
      e = { _exception : "Error deserializing header" };
      throw e
    end
  end
  
  procedure updateZilTxHash (txHashIndex : Uint256, rawParamHash : ByStr32)
    f_zilToPolyTxHashMap[txHashIndex] := rawParamHash;
    one_uint256 = Uint256 1;
    newTxHashIndex = builtin add txHashIndex one_uint256;
    f_zilToPolyTxHashIndex := newTxHashIndex
  end
  
  (*  @notice              ERC20 token cross chain to other blockchain.
   *                       this function push tx event to blockchain
   *  @param toChainId     Target chain id
   *  @param toContract    Target smart contract address in target block chain
   *  @param txData        Transaction data for target chain, include to_address, amount
   *  @return              true or false
   *)
  transition crossChain
    (toChainId : Uint64, toContract : ByStr, method : ByStr, txData : ByStr)
    txHashIndex <- f_zilToPolyTxHashIndex;
    paramTxHash = let b = builtin to_bystr32 txHashIndex in builtin to_bystr b;
    crossChainId =
      let this_bs = builtin to_bystr _this_address in
      let s = builtin concat this_bs paramTxHash in
      let h = builtin sha256hash s in
      builtin to_bystr h;
    fromContract = builtin to_bystr _sender;
    txp =
      TxParam
        paramTxHash crossChainId fromContract toChainId toContract method txData;
    (* Serialize the TxParam object *)
    empty_bystr = let b = 0x in builtin to_bystr b;
    rawParam = append_TxParam empty_bystr txp;
    rawParamHash = builtin keccak256hash rawParam;
    updateZilTxHash txHashIndex rawParamHash;
    e =
      {
        _eventname : "CrossChainEvent";
        origin : _origin;
        paramTxHash : paramTxHash;
        sender : _sender;
        toChainId : toChainId;
        toContract : toContract;
        rawParam : rawParam
      };
    event e
  end
  
