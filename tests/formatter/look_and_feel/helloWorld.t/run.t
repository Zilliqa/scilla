  $ scilla-fmt helloWorld.scilla
  scilla_version 0
  (* HelloWorld contract *)
  
  import ListUtils
  
  (***************************************************)
  (*               Associated library                *)
  (***************************************************)
  library HelloWorld
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  let not_owner_code = Int32 1
  
  let set_hello_code = Int32 2
  
  
  (***************************************************)
  (*             The contract definition             *)
  (***************************************************)
  contract HelloWorld (owner : ByStr20)
  
  
  field welcome_msg : String = ""
  
  transition setHello (msg : String)
    is_owner = builtin eq owner _sender;
    match is_owner with
    | False =>
      e = { _eventname : "setHello()"; code : not_owner_code };
      event e
    | True =>
      welcome_msg := msg;
      e = { _eventname : "setHello()"; code : set_hello_code };
      event e
    end
  end
  
  transition getHello ()
    r <- welcome_msg;
    e = { _eventname : "getHello()"; msg : r };
    event e
  end
  
  transition multipleMsgs ()
    msg1 = { _tag : ""; _recipient : _sender; _amount : Uint128 0 };
    msg2 = { _tag : ""; _recipient : _sender; _amount : Uint128 0 };
    msgs1 = one_msg msg1;
    msgs2 = Cons {(Message)} msg2 msgs1;
    send msgs2
  end
  
  transition contrAddr ()
    msg1 = { _eventname : "ContractAddress"; addr : _this_address };
    event msg1
  end
  
