  $ scilla-fmt chainid.scilla
  scilla_version 0
  
  (***************************************************)
  (*             The contract definition             *)
  (***************************************************)
  contract HelloWorld ()
  
  
  transition EventChainID ()
    cid <-& CHAINID;
    e = { _eventname : "ChainID"; chain_id : cid };
    event e
  end
  
