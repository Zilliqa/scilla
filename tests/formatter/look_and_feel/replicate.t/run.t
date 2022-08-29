  $ scilla-fmt replicate.scilla
  scilla_version 0
  
  contract Foo ()
  
  
  transition rep (bar : ByStr20 with contract end)
    foo = { _replicate_contract : "" };
    foo_addr <-& REPLICATE_CONTRACT(bar, foo);
    e = { _eventname : "Replicated"; new_addr : foo_addr };
    event e
  end
  
  transition cfdeploy (cfaddr : ByStr20 with contract end)
    owner = _sender;
    max_block = BNum 100;
    goal = Uint128 1000;
    m =
      {
        _replicate_contract : "";
        owner : owner;
        max_block : max_block;
        goal : goal
      };
    newcf_addr <-& REPLICATE_CONTRACT(cfaddr, m);
    e = { _eventname : "Replicated"; new_addr : newcf_addr };
    event e
  end
  
  transition cfdeploy_incorrect (cfaddr : ByStr20 with contract end)
    owner = _sender;
    max_block = Uint32 100;
    goal = Uint128 1000;
    m =
      {
        _replicate_contract : "";
        owner : owner;
        max_block : max_block;
        goal : goal
      };
    newcf_addr <-& REPLICATE_CONTRACT(cfaddr, m);
    e = { _eventname : "Replicated"; new_addr : newcf_addr };
    event e
  end
  
