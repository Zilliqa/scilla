  $ scilla-fmt timestamp.scilla
  scilla_version 0
  
  contract HelloWorld ()
  
  
  transition EventTimestamp ()
    bnum = BNum 100;
    ts <-& TIMESTAMP(bnum);
    e = { _eventname : "TS"; timestamp : ts };
    event e
  end
  
