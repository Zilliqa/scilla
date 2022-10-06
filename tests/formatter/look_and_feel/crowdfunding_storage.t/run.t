  $ scilla-fmt crowdfunding_storage.scilla
  scilla_version 0
  
  library CrowdfundingStorage
  
  (* Error exceptions *)
  type Error =
  | GovernorIsNotSetYet
  | SenderIsNotGovernor
  | SenderIsNotContractOwner
  
  let make_error =
    fun (result : Error) =>
      let result_code =
        match result with
        | GovernorIsNotSetYet => Int32 -1
        | SenderIsNotGovernor => Int32 -2
        | SenderIsNotContractOwner => Int32 -3
        end
      in
      { _exception : "Error"; code : result_code }
  
  
  (* This contract does not support transferring contract ownership
     for the sake of simplicity of exposition *)
  contract CrowdfundingStorage (contract_owner : ByStr20)
  
  
  (* fields of the original contract *)
  field backers : Map ByStr20 Uint128 = Emp (ByStr20) (Uint128)
  
  (* special fields to support upgradeability *)
  field governor : Option ByStr20 = None {(ByStr20)}
  
  procedure Throw (error : Error)
    e = make_error error;
    throw e
  end
  
  procedure RequireGovernor ()
    optional_governor <- governor;
    match optional_governor with
    | None =>
      e = GovernorIsNotSetYet;
      Throw e
    | Some gov =>
      is_governor = builtin eq _sender gov;
      match is_governor with
      | False =>
        e = SenderIsNotGovernor;
        Throw e
      | True => (* do nothing *)
      end
    end
  end
  
  procedure RequireContractOwner ()
    is_contract_owner = builtin eq _sender contract_owner;
    match is_contract_owner with
    | False =>
      e = SenderIsNotContractOwner;
      Throw e
    | True => (* do nothing *)
    end
  end
  
  (* transition to modify the storage fields and field maps *)
  transition SetBackersKey (key : ByStr20, val : Uint128)
    RequireGovernor;
    backers[key] := val
  end
  
  transition DeleteBackersKey (key : ByStr20)
    RequireGovernor;
    delete backers[key]
  end
  
  (* transitions to support upgradeability *)
  transition SetStorageGovernor (new_governor : ByStr20)
    RequireContractOwner;
    gov = Some {(ByStr20)} new_governor;
    governor := gov
  end
  
