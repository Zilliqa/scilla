  $ scilla-fmt polymorphic_address.scilla
  scilla_version 0
  
  import ListUtils
  
  library AddressListTraversalLib
  
  let f :
    forall 'A.
      (ByStr20 with contract field f : 'A, field x : Uint128 end) ->
      ByStr20 with contract field f : 'A, field x : Uint128 end =
    tfun 'A =>
      fun (x : ByStr20 with contract field f : 'A, field x : Uint128 end) =>
        x
  
  
  contract AddressListTraversal ()
  
  
  field res_1 : Uint32 = Uint32 0
  
  field res_2 : Uint128 = Uint128 0
  
  transition Test1
    (param1 : ByStr20 with contract field f : Uint32, field x : Uint128 end)
    f_spec = @f (Uint32);
    p1 = f_spec param1;
    res <-& p1.f;
    res_1 := res
  end
  
  transition Test2
    (param1 : ByStr20 with contract field f : Uint128, field x : Uint128 end)
    f_spec = @f (Uint128);
    p1 = f_spec param1;
    res <-& p1.f;
    res_2 := res
  end
  
