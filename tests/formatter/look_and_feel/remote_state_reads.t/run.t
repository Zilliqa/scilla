  $ scilla-fmt remote_state_reads.scilla
  scilla_version 0
  
  library RRLib
  
  (* Tests various aspects of address types and remote state reads *)
  type AddressADT =
  | Address1 of (ByStr20 with end)
  | Address2 of (ByStr20 with contract field admin : ByStr20 with end end)
  
  
  contract RRContract
    (
      (* Any address in use *)
      cparam1 : ByStr20 with end,
      (* Any contract address *)
      cparam2 : ByStr20 with contract end,
      (* Address with various fields *)
      cparam3 :
        ByStr20 with contract
          field admin : ByStr20 with end,
          field other_map : Map Uint128 (ByStr20 with end),
          field owners : Map (ByStr20 with end) Bool,
          field signatures : Map Uint32 (Map (ByStr20 with end) Bool),
          field transactionCount : Uint32
        end
    )
  
  
  field assign_test_1 : ByStr20 with end = cparam3
  
  field assign_test_2 : ByStr20 with contract field transactionCount : Uint32 end =
    cparam3
  
  field assign_test_3 : ByStr20 with contract field admin : ByStr20 with end end =
    cparam3
  
  field assign_test_4 : ByStr20 with contract field admin : ByStr20 end = cparam3
  
  field assign_test_5 : ByStr20 with contract field owners : Map ByStr20 Bool end =
    cparam3
  
  field assign_test_6 :
    ByStr20 with contract field signatures : Map Uint32 (Map ByStr20 Bool) end =
    cparam3
  
  field assign_test_7 :
    ByStr20 with contract field other_map : Map Uint128 ByStr20 end = cparam3
  
  field assign_test_8 : AddressADT = Address1 cparam1
  
  field assign_test_9 : List AddressADT = Nil {(AddressADT)}
  
  field assign_test_10 : Map Uint128 (Map Uint128 AddressADT) =
    Emp (Uint128) (Map Uint128 AddressADT)
  
  field remote_reads_test_res_1_1 : Uint128 = Uint128 0
  
  (* _balance of remote1 *)
  field remote_reads_test_res_2_1 : Uint128 = Uint128 0
  
  (* _balance of remote2 *)
  field remote_reads_test_res_3_1 : Uint128 = Uint128 0
  
  (* _balance of remote3 *)
  field remote_reads_test_res_3_3 : Uint32 = Uint32 0
  
  (* transactionCount of remote3 *)
  field remote_reads_test_res_3_4 : ByStr20 with end = cparam3
  
  (* admin of remote3 *)
  field remote_reads_test_res_3_5 : Uint128 = Uint128 0
  
  (* _balance of admin of remote3 *)
  field remote_reads_test_res_3_6 : Map (ByStr20 with end) Bool =
    Emp (ByStr20 with end) (Bool)
  
  (* owners of remote3 *)
  field remote_reads_test_res_3_7 : Bool = True
  
  (* exists of owners[key] in remote3 *)
  field remote_reads_test_res_3_8 : Option Bool = let x = True in Some {(Bool)} x
  
  (* owners[key] in remote3 *)
  field remote_reads_test_res_3_9 : Map Uint32 (Map (ByStr20 with end) Bool) =
    Emp (Uint32) (Map (ByStr20 with end) Bool)
  
  (* signatures of remote3 *)
  field remote_reads_test_res_3_10 : Bool = False
  
  (* exists of signatures[key] of remote3 *)
  field remote_reads_test_res_3_11 : Option (Map (ByStr20 with end) Bool) =
    None {(Map (ByStr20 with end) Bool)}
  
  (* signatures[key] of remote3 *)
  field remote_reads_test_res_3_12 : Bool = False
  
  (* exists signatures[key1][key2] of remote3 *)
  field remote_reads_test_res_3_13 : Option Bool = None {(Bool)}
  
  (* signatures[key1][key2] of remote3 *)
  field sender_balance_pre : Uint128 = Uint128 0
  
  field sender_balance_mid : Uint128 = Uint128 0
  
  field sender_balance_post : Uint128 = Uint128 0
  
  transition RemoteReadsTest
    (
      (* Any address in use *)
      remote1 : ByStr20 with end,
      (* Any contract address *)
      remote2 : ByStr20 with contract end,
      (* Address with various fields *)
      remote3 :
        ByStr20 with contract
          field admin : ByStr20 with end,
          field owners : Map (ByStr20 with end) Bool,
          field signatures : Map Uint32 (Map (ByStr20 with end) Bool),
          field transactionCount : Uint32
        end
    )
    tmp_1_1 <-& remote1._balance;
    remote_reads_test_res_1_1 := tmp_1_1;
    tmp_2_1 <-& remote2._balance;
    remote_reads_test_res_2_1 := tmp_2_1;
    tmp_3_1 <-& remote3._balance;
    remote_reads_test_res_3_1 := tmp_3_1;
    tmp_3_3 <-& remote3.transactionCount;
    remote_reads_test_res_3_3 := tmp_3_3;
    tmp_3_4 <-& remote3.admin;
    remote_reads_test_res_3_4 := tmp_3_4;
    tmp_3_5 <-& tmp_3_4._balance;
    remote_reads_test_res_3_5 := tmp_3_5;
    tmp_3_6 <-& remote3.owners;
    remote_reads_test_res_3_6 := tmp_3_6;
    tmp_3_7 <-& exists remote3.owners[_sender];
    remote_reads_test_res_3_7 := tmp_3_7;
    tmp_3_8 <-& remote3.owners[_sender];
    remote_reads_test_res_3_8 := tmp_3_8;
    tmp_3_9 <-& remote3.signatures;
    remote_reads_test_res_3_9 := tmp_3_9;
    x = Uint32 0;
    tmp_3_10 <-& exists remote3.signatures[x];
    remote_reads_test_res_3_10 := tmp_3_10;
    tmp_3_11 <-& remote3.signatures[x];
    remote_reads_test_res_3_11 := tmp_3_11;
    tmp_3_12 <-& exists remote3.signatures[x][_origin];
    remote_reads_test_res_3_12 := tmp_3_12;
    tmp_3_13 <-& remote3.signatures[x][_origin];
    remote_reads_test_res_3_13 := tmp_3_13
  end
  
  (* Test the dynamic typecheck of ADT values *)
  transition RemoteReadsADTTest
    (
      list1 : List (ByStr20 with end),
      list2 : List (ByStr20 with contract field f : Uint128 end),
      list3 : List (ByStr20 with contract field g : AddressADT end),
      pair1 : Pair (ByStr20 with end) AddressADT,
      adt1 : AddressADT,
      remote1 : ByStr20 with contract field h : Map Uint128 AddressADT end
    )
  
  end
  
  (* Test that outgoing messages and events use ByStr20 for type info, and not the full address type
     Also test that this is the case for EventInfo *)
  transition OutgoingMsgTest ()
    msg =
      { _tag : ""; _recipient : _sender; _amount : Uint128 0; param : cparam3 };
    msgs = let n = Nil {(Message)} in Cons {(Message)} msg n;
    send msgs;
    e1 = { _eventname : "TestEvent"; info : cparam2 };
    event e1;
    e2 = { _eventname : "TestEvent"; info : cparam3 };
    event e2
  end
  
  (* Test that exceptions use ByStr20 for type info, and not the full address type *)
  transition ExceptionTest ()
    e = { _exception : "TestException"; value : cparam3 };
    throw e
  end
  
  transition AssignTest ()
    x = Address2 cparam3;
    assign_test_8 := x;
    y = let n = Nil {(AddressADT)} in Cons {(AddressADT)} x n;
    assign_test_9 := y;
    z =
      let n = Emp (Uint128) (Map Uint128 AddressADT) in
      let sub_n = Emp (Uint128) (AddressADT) in
      let sub_k = Uint128 0 in
      let sub_res = builtin put sub_n sub_k x in
      builtin put n sub_k sub_res;
    assign_test_10 := z;
    k1 = Uint128 1;
    k2 = Uint128 42;
    assign_test_10[k1][k2] := x
  end
  
  (* Check that sender balance is deducted on acceptance *)
  transition SenderBalanceTest ()
    pre <-& _sender._balance;
    sender_balance_pre := pre;
    (* First accept should cause sender balance to decrease *)
    accept;
    mid <-& _sender._balance;
    sender_balance_mid := mid;
    (* Second accept should make no difference *)
    accept;
    post <-& _sender._balance;
    sender_balance_post := post
  end
  
