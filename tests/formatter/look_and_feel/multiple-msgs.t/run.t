  $ scilla-fmt multiple-msgs.scilla
  scilla_version 0
  
  import ListUtils
  
  library HelloWorld
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  
  contract HelloWorld ()
  
  
  transition multipleMsgs ()
    msg1 = { _tag : ""; _recipient : _sender; _amount : Uint128 0 };
    msg2 = { _tag : ""; _recipient : _sender; _amount : Uint128 0 };
    msgs1 = one_msg msg1;
    msgs2 = Cons {(Message)} msg2 msgs1;
    send msgs2
  end
  
