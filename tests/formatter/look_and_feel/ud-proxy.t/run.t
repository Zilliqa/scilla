  $ scilla-fmt ud-proxy.scilla
  scilla_version 0
  
  import
    BoolUtils
    ListUtils
  
  library HoldingLib
  
  let true = True
  
  let nilMessage = Nil {(Message)}
  
  let oneMsg =
    fun (msg : Message) =>
      Cons {(Message)} msg nilMessage
  
  let eAdminSet =
    fun (address : ByStr20) =>
      fun (isApproved : Bool) =>
        { _eventname : "AdminSet"; address : address; isApproved : isApproved }
  
  let eError = { _eventname : "Error" }
  
  
  contract Admin
    (
      initialAdmin : ByStr20,
      registry : ByStr20
    )
  
  
  field admins : Map ByStr20 Bool =
    let empty = Emp (ByStr20) (Bool) in
    builtin put empty initialAdmin true
  
  transition setAdmin (address : ByStr20, isApproved : Bool)
    maybeAdmin <- admins[_sender];
    isSenderAdmin =
      match maybeAdmin with
      | Some approval => approval
      | None => False
      end;
    match isSenderAdmin with
    | True =>
      admins[address] := isApproved;
      e = eAdminSet address isApproved;
      event e
    | False => event eError
    end
  end
  
  transition bestow (label : String, owner : ByStr20, resolver : ByStr20)
    maybeAdmin <- admins[_sender];
    isSenderAdmin =
      match maybeAdmin with
      | Some isAdmin => isAdmin
      | None => False
      end;
    match isSenderAdmin with
    | True =>
      msgs =
        let m =
          {
            _tag : "bestow";
            _recipient : registry;
            _amount : Uint128 0;
            label : label;
            owner : owner;
            resolver : resolver
          }
        in
        oneMsg m;
      send msgs
    | False => event eError
    end
  end
  
