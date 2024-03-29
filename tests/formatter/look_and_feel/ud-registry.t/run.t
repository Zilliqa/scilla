  $ scilla-fmt ud-registry.scilla
  scilla_version 0
  (* Give some love to the UD Dev Team Bogdan, Ryan, Don and Ali. *)
  
  import
    BoolUtils
    ListUtils
  
  library RegistryLib
  
  let zeroUint64 = Uint64 0
  
  let zeroByStr20 = 0x0000000000000000000000000000000000000000
  
  let zeroByStr32 =
    0x0000000000000000000000000000000000000000000000000000000000000000
  
  let nilByStr20 = Nil {(ByStr20)}
  
  let nilMessage = Nil {(Message)}
  
  let oneMsg =
    fun (msg : Message) =>
      Cons {(Message)} msg nilMessage
  
  let eqByStr20 =
    fun (bs1 : ByStr20) =>
      fun (bs2 : ByStr20) =>
        builtin eq bs1 bs2
  
  let listByStr20Contains =
    fun (list : List ByStr20) =>
      fun (bs : ByStr20) =>
        let listMemByStr20 = @list_mem (ByStr20) in
        listMemByStr20 eqByStr20 bs list
  
  let listByStr20Excludes =
    fun (list : List ByStr20) =>
      fun (bs : ByStr20) =>
        let b = listByStr20Contains list bs in
        negb b
  
  let listByStr20FilterOut =
    fun (list : List ByStr20) =>
      fun (bs : ByStr20) =>
        let listByStr20Filter = @list_filter (ByStr20) in
        let fn =
          fun (v : ByStr20) =>
            let b = builtin eq v bs in
            negb b
        in
        listByStr20Filter fn list
  
  let xandb =
    fun (b1 : Bool) =>
      fun (b2 : Bool) =>
        match b1 with
        | True =>
          match b2 with
          | True => True
          | False => False
          end
        | False =>
          match b2 with
          | True => False
          | False => True
          end
        end
  
  let eAdminSet =
    fun (address : ByStr20) =>
      fun (isApproved : Bool) =>
        { _eventname : "AdminSet"; address : address; isApproved : isApproved }
  
  let eApprovedFor =
    fun (user : ByStr20) =>
      fun (operator : ByStr20) =>
        fun (isApproved : Bool) =>
          {
            _eventname : "ApprovedFor";
            user : user;
            operator : operator;
            isApproved : isApproved
          }
  
  let eApproved =
    fun (address : ByStr20) =>
      { _eventname : "Approved"; address : address }
  
  let eNewRegistrar =
    fun (address : ByStr20) =>
      { _eventname : "NewRegistrar"; address : address }
  
  let eNewDomain =
    fun (parent : ByStr32) =>
      fun (label : String) =>
        { _eventname : "NewDomain"; parent : parent; label : label }
  
  let eConfigured =
    fun (node : ByStr32) =>
      fun (owner : ByStr20) =>
        fun (resolver : ByStr20) =>
          {
            _eventname : "Configured";
            node : node;
            owner : owner;
            resolver : resolver
          }
  
  let eError =
    fun (msg : String) =>
      { _eventname : "Error"; msg : msg }
  
  type Record =
  | Record of ByStr20 ByStr20
  
  let recordMemberOwner =
    fun (maybeRecord : Option Record) =>
      match maybeRecord with
      | None => zeroByStr20
      | Some record =>
        match record with
        | Record owner resolver => owner
        end
      end
  
  let parentLabelToNode =
    fun (parent : ByStr32) =>
      fun (label : String) =>
        let labelHash = builtin sha256hash label in
        let nodeInput = builtin concat parent labelHash in
        builtin sha256hash nodeInput
  
  let getIsOAO =
    fun (sender : ByStr20) =>
      fun (recordOwner : ByStr20) =>
        fun (maybeApproved : Option ByStr20) =>
          fun (maybeOperators : Option (List ByStr20)) =>
            let isOwner = builtin eq sender recordOwner in
            let isApproved =
              match maybeApproved with
              | None => False
              | Some approved => builtin eq sender approved
              end
            in
            let isOperator =
              match maybeOperators with
              | None => False
              | Some operators => listByStr20Contains operators sender
              end
            in
            let b1 = orb isOwner isApproved in
            orb b1 isOperator
  
  
  contract Registry
    (
      initialOwner : ByStr20,
      rootNode : ByStr32
    )
  
  
  field records : Map ByStr32 Record =
    let empty = Emp (ByStr32) (Record) in
    let rootRecord = Record initialOwner zeroByStr20 in
    builtin put empty rootNode rootRecord
  
  field registrar : ByStr20 = zeroByStr20
  
  field approvals : Map ByStr32 ByStr20 = Emp (ByStr32) (ByStr20)
  
  field operators : Map ByStr20 (List ByStr20) = Emp (ByStr20) (List ByStr20)
  
  field admins : List ByStr20 = Cons {(ByStr20)} initialOwner nilByStr20
  
  transition setAdmin (address : ByStr20, isApproved : Bool)
    currentAdmins <- admins;
    isSenderAdmin = listByStr20Contains currentAdmins _sender;
    match isSenderAdmin with
    | True =>
      needsToChange =
        let b = listByStr20Excludes currentAdmins address in
        xandb b isApproved;
      match needsToChange with
      | True =>
        newAdmins =
          match isApproved with
          | True => Cons {(ByStr20)} address currentAdmins
          | False => listByStr20FilterOut currentAdmins address
          end;
        admins := newAdmins;
        e = eAdminSet address isApproved;
        event e
      | _ =>
      end
    | False =>
      e = let m = "Sender not root node owner" in eError m;
      event e
    end
  end
  
  transition approve (node : ByStr32, address : ByStr20)
    maybeRecord <- records[node];
    recordOwner = recordMemberOwner maybeRecord;
    isSenderNodeOwner = builtin eq _sender recordOwner;
    match isSenderNodeOwner with
    | True =>
      maybeApproved <- approvals[node];
      currentlyApproved =
        match maybeApproved with
        | None => zeroByStr20
        | Some approved => approved
        end;
      needsToChange = let b = builtin eq currentlyApproved address in negb b;
      match needsToChange with
      | True =>
        approvals[node] := address;
        e = eApproved address;
        event e
      | _ =>
      end
    | False =>
      e = let m = "Sender not node owner" in eError m;
      event e
    end
  end
  
  transition approveFor (address : ByStr20, isApproved : Bool)
    maybeOperators <- operators[_sender];
    currentOperators =
      match maybeOperators with
      | None => nilByStr20
      | Some ops => ops
      end;
    needsToChange =
      let b = listByStr20Excludes currentOperators address in
      xandb b isApproved;
    match needsToChange with
    | True =>
      newOperators =
        match isApproved with
        | True => Cons {(ByStr20)} address currentOperators
        | False => listByStr20FilterOut currentOperators address
        end;
      operators[_sender] := newOperators;
      e = eApprovedFor _sender address isApproved;
      event e
    | _ =>
    end
  end
  
  transition configureNode (node : ByStr32, owner : ByStr20, resolver : ByStr20)
    maybeRecord <- records[node];
    maybeApproved <- approvals[node];
    recordOwner = recordMemberOwner maybeRecord;
    maybeOperators <- operators[recordOwner];
    isSenderOAO = getIsOAO _sender recordOwner maybeApproved maybeOperators;
    match isSenderOAO with
    | True =>
      newRecord = Record owner resolver;
      records[node] := newRecord;
      e = eConfigured node owner resolver;
      event e;
      msgs =
        let m =
          {
            _tag : "onConfigureSuccess";
            node : node;
            owner : owner;
            _amount : Uint128 0;
            _recipient : _sender
          }
        in
        oneMsg m;
      send msgs
    | False =>
      e = let m = "Sender not node owner, approved or operator" in eError m;
      event e;
      msgs =
        let m =
          {
            _tag : "onConfigureFailure";
            node : node;
            owner : recordOwner;
            _amount : Uint128 0;
            _recipient : _sender
          }
        in
        oneMsg m;
      send msgs
    end
  end
  
  transition configureResolver (node : ByStr32, resolver : ByStr20)
    maybeRecord <- records[node];
    maybeApproved <- approvals[node];
    recordOwner = recordMemberOwner maybeRecord;
    maybeOperators <- operators[recordOwner];
    isSenderOAO = getIsOAO _sender recordOwner maybeApproved maybeOperators;
    match isSenderOAO with
    | True =>
      newRecord = Record recordOwner resolver;
      records[node] := newRecord;
      e = eConfigured node recordOwner resolver;
      event e
    | False =>
      e = let m = "Sender not node owner, approved or operator" in eError m;
      event e
    end
  end
  
  transition transfer (node : ByStr32, owner : ByStr20)
    maybeRecord <- records[node];
    maybeApproved <- approvals[node];
    recordOwner = recordMemberOwner maybeRecord;
    maybeOperators <- operators[recordOwner];
    isSenderOAO = getIsOAO _sender recordOwner maybeApproved maybeOperators;
    match isSenderOAO with
    | True =>
      delete approvals[node];
      newRecord = Record owner zeroByStr20;
      records[node] := newRecord;
      e = eConfigured node owner zeroByStr20;
      event e;
      msgs =
        let m =
          {
            _tag : "onTransferSuccess";
            node : node;
            owner : owner;
            _amount : Uint128 0;
            _recipient : _sender
          }
        in
        oneMsg m;
      send msgs
    | False =>
      e = let m = "Sender not node owner, approved or operator" in eError m;
      event e;
      msgs =
        let m =
          {
            _tag : "onTransferFailure";
            node : node;
            owner : owner;
            _amount : Uint128 0;
            _recipient : _sender
          }
        in
        oneMsg m;
      send msgs
    end
  end
  
  transition assign (parent : ByStr32, label : String, owner : ByStr20)
    maybeRecord <- records[parent];
    maybeApproved <- approvals[parent];
    recordOwner = recordMemberOwner maybeRecord;
    maybeOperators <- operators[recordOwner];
    isSenderOAO = getIsOAO _sender recordOwner maybeApproved maybeOperators;
    match isSenderOAO with
    | True =>
      node = parentLabelToNode parent label;
      recordExists <- exists records[node];
      match recordExists with
      | False =>
        e = eNewDomain parent label;
        event e
      | _ =>
      end;
      delete approvals[node];
      newRecord = Record owner zeroByStr20;
      records[node] := newRecord;
      e = eConfigured node owner zeroByStr20;
      event e;
      msgs =
        let m =
          {
            _tag : "onAssignSuccess";
            parent : parent;
            label : label;
            owner : owner;
            _amount : Uint128 0;
            _recipient : _sender
          }
        in
        oneMsg m;
      send msgs
    | False =>
      e = let m = "Sender not parent owner, approved or operator" in eError m;
      event e;
      msgs =
        let m =
          {
            _tag : "onAssignFailure";
            parent : parent;
            label : label;
            owner : recordOwner;
            _amount : Uint128 0;
            _recipient : _sender
          }
        in
        oneMsg m;
      send msgs
    end
  end
  
  transition bestow (label : String, owner : ByStr20, resolver : ByStr20)
    currentAdmins <- admins;
    node = parentLabelToNode rootNode label;
    recordExists <- exists records[node];
    maybeRecord <- records[node];
    currentRegistrar <- registrar;
    isOk =
      let isSenderAdmin = listByStr20Contains currentAdmins _sender in
      let isSenderRegistrar = builtin eq currentRegistrar _sender in
      let isOkSender = orb isSenderRegistrar isSenderAdmin in
      let recordOwner = recordMemberOwner maybeRecord in
      let recordIsUnowned = builtin eq recordOwner zeroByStr20 in
      let recordIsOwnedByRegistrar = builtin eq recordOwner currentRegistrar in
      let isRegistrarSenderAndOwned =
        andb recordIsOwnedByRegistrar isSenderRegistrar
      in
      let isOkRecordOwner = orb recordIsUnowned isRegistrarSenderAndOwned in
      andb isOkSender isOkRecordOwner;
    match isOk with
    | True =>
      match recordExists with
      | False =>
        e = eNewDomain rootNode label;
        event e
      | _ =>
      end;
      newRecord = Record owner resolver;
      records[node] := newRecord;
      e = eConfigured node owner resolver;
      event e
    | False =>
      e = let m = "Sender admin" in eError m;
      event e
    end
  end
  
  transition setRegistrar (address : ByStr20)
    currentAdmins <- admins;
    isOk = listByStr20Contains currentAdmins _sender;
    match isOk with
    | True =>
      e = eNewRegistrar address;
      event e;
      registrar := address
    | _ =>
    end
  end
  
  transition register (parent : ByStr32, label : String)
    node = parentLabelToNode parent label;
    maybeRecord <- records[node];
    maybeApproved <- approvals[node];
    recordOwner = recordMemberOwner maybeRecord;
    approved =
      match maybeApproved with
      | None => zeroByStr20
      | Some approved => approved
      end;
    currentRegistrar <- registrar;
    isOk =
      let isRecordUnowned = builtin eq recordOwner zeroByStr20 in
      let isUnapproved = builtin eq approved zeroByStr20 in
      andb isRecordUnowned isUnapproved;
    match isOk with
    | True =>
      accept;
      msgs =
        let m =
          {
            _tag : "register";
            _amount : _amount;
            _recipient : currentRegistrar;
            origin : _sender;
            node : node;
            parent : parent;
            label : label
          }
        in
        oneMsg m;
      send msgs
    | False =>
    end
  end
  
  transition onResolverConfigured (node : ByStr32)
    maybeRecord <- records[node];
    match maybeRecord with
    | None =>
    | Some record =>
      match record with
      | Record owner resolver =>
        isOk = builtin eq resolver _sender;
        match isOk with
        | True =>
          e = eConfigured node owner resolver;
          event e
        | False =>
        end
      end
    end
  end
  
