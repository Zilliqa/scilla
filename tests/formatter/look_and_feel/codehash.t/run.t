  $ scilla-fmt codehash.scilla
  scilla_version 0
  
  contract Codehash ()
  
  
  transition foo (addr : ByStr20 with _codehash end)
    bar <-& addr._codehash;
    e = { _eventname : "Success"; bar : bar };
    event e
  end
  
  transition foo2 (addr : ByStr20)
    addr_ <-& addr as ByStr20 with _codehash end;
    match addr_ with
    | Some addr__ =>
      bar <-& addr__._codehash;
      e = { _eventname : "Success"; bar : bar };
      event e
    | None =>
      e = { _eventname : "Failure" };
      event e
    end
  end
  
  transition foo3 (addr : ByStr20 with library end)
    bar <-& addr._codehash;
    e = { _eventname : "Success"; bar : bar };
    event e
  end
  
  transition foo4 (addr : ByStr20 with contract end)
    bar <-& addr._codehash;
    e = { _eventname : "Success"; bar : bar };
    event e
  end
  
