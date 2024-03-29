  $ scilla-fmt address_eq_test.scilla
  scilla_version 0
  
  contract AddressEqTest ()
  
  
  field eq_test_res : Bool = False
  
  field to_bystr_res : ByStr = let x = 0x12 in builtin to_bystr x
  
  field to_uint_res : Uint256 = Uint256 0
  
  field concat_res : ByStr40 =
    0xabfeccdc9012345678901234567890f777564322abfeccdc9012345678901234567890f777564322
  
  field test_map :
    Map (ByStr20 with contract field f : Uint32 end) (ByStr20 with contract end) =
    Emp (ByStr20 with contract field f : Uint32 end) (ByStr20 with contract end)
  
  field map_res_1 : Bool = True
  
  field map_res_2 :
    Map (ByStr20 with contract field f : Uint32 end) (ByStr20 with contract end) =
    Emp (ByStr20 with contract field f : Uint32 end) (ByStr20 with contract end)
  
  field map_res_3 : Option (ByStr20 with contract end) =
    None {(ByStr20 with contract end)}
  
  field map_res_4 :
    Map (ByStr20 with contract field f : Uint32 end) (ByStr20 with contract end) =
    Emp (ByStr20 with contract field f : Uint32 end) (ByStr20 with contract end)
  
  field map_res_5 :
    List
      (Pair
        (ByStr20 with contract field f : Uint32 end) (ByStr20 with contract end)) =
    Nil
      {(Pair
        (ByStr20 with contract field f : Uint32 end) (ByStr20 with contract end))}
  
  field map_res_6 : Uint32 = Uint32 0
  
  field test_map2 : Map (ByStr20 with contract field f : Uint32 end) Uint128 =
    Emp (ByStr20 with contract field f : Uint32 end) (Uint128)
  
  field test_map3 : Map Uint128 (ByStr20 with contract end) =
    Emp (Uint128) (ByStr20 with contract end)
  
  field test_to_string : String = ""
  
  field test_to_ascii : String = ""
  
  field test_strrev : String = ""
  
  transition Test1
    (
      param1 : ByStr20 with contract field f : Uint128, field g : Int32 end,
      param2 : ByStr20 with contract field f : Uint128, field h : Bool end
    )
    x = builtin eq param1 param2;
    eq_test_res := x
  end
  
  transition Test2
    (
      param1 : ByStr20 with contract field f : Uint32, field g : Int32 end,
      param2 : ByStr20 with contract field f : Uint128, field h : Bool end
    )
    x = builtin eq param1 param2;
    eq_test_res := x
  end
  
  transition Test3
    (
      param1 : ByStr20 with contract field f : Uint32, field g : Int32 end,
      param2 : ByStr20
    )
    x = builtin eq param1 param2;
    eq_test_res := x
  end
  
  transition Test4
    (
      param1 : ByStr20 with contract field f : Uint32, field g : Int32 end,
      param2 : ByStr20 with end
    )
    x = builtin eq param1 param2;
    eq_test_res := x
  end
  
  transition Test5
    (
      param1 : ByStr20 with contract field f : Uint32, field g : Int32 end,
      param2 : ByStr20 with contract end
    )
    x = builtin eq param1 param2;
    eq_test_res := x
  end
  
  transition Test6
    (param1 : ByStr20 with contract field f : Uint32, field g : Int32 end)
    x = builtin to_bystr param1;
    to_bystr_res := x
  end
  
  transition Test7
    (param1 : ByStr20 with contract field f : Uint32, field g : Int32 end)
    x = builtin to_uint256 param1;
    to_uint_res := x
  end
  
  transition Test8
    (
      param1 : ByStr20 with contract field f : Uint32, field g : Int32 end,
      param2 : ByStr20 with contract end
    )
    x = builtin concat param1 param2;
    concat_res := x
  end
  
  transition Test9
    (
      param1 : ByStr20 with contract field f : Uint32, field g : Int32 end,
      param2 : ByStr20 with contract end
    )
    mp <- test_map;
    mp1 = builtin contains mp param1;
    map_res_1 := mp1;
    mp2 = builtin put mp param1 param2;
    map_res_2 := mp2;
    mp3 = builtin get mp2 param1;
    map_res_3 := mp3;
    mp4 = builtin remove mp2 param1;
    map_res_4 := mp4;
    mp5 = builtin to_list mp2;
    map_res_5 := mp5;
    mp6 = builtin size mp2;
    map_res_6 := mp6
  end
  
  transition Test10
    (param1 : ByStr20 with contract field f : Uint32, field g : Int32 end)
    mp <- test_map2;
    zero = Uint128 0;
    mp2 = builtin put mp param1 zero;
    test_map2 := mp2;
    mp3 <- test_map3;
    mp4 = builtin put mp3 zero param1;
    test_map3 := mp4
  end
  
  transition Test11
    (param1 : ByStr20 with contract field f : Uint32, field g : Int32 end)
    x = builtin to_string param1;
    test_to_string := x;
    y = builtin to_ascii param1;
    test_to_ascii := y;
    z = builtin strrev param1;
    test_strrev := z
  end
  
