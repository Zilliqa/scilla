  $ scilla-fmt --json --deannot remote_state_reads.scilla
  {"smver":0,"libs":{"lname":["Ident",["SimpleLocal","RRLib"],null],"lentries":[["LibTyp",["Ident",["SimpleLocal","AddressADT"],null],[{"cname":["Ident",["SimpleLocal","Address1"],null],"c_arg_types":[["Address","Address"]]},{"cname":["Ident",["SimpleLocal","Address2"],null],"c_arg_types":[["Address","Address"]]}]]]},"elibs":[],"contr":{"cname":["Ident",["SimpleLocal","RRContract"],null],"cparams":[[["Ident",["SimpleLocal","cparam1"],null],["Address","Address"]],[["Ident",["SimpleLocal","cparam2"],null],["Address","Address"]],[["Ident",["SimpleLocal","cparam3"],null],["Address","Address"]]],"cconstraint":[["Literal",{"name":["SimpleLocal","True"],"types":[],"lits":[]}],null],"cfields":[[["Ident",["SimpleLocal","assign_test_1"],null],["Address","Address"],[["Var",["Ident",["SimpleLocal","cparam3"],null]],null]],[["Ident",["SimpleLocal","assign_test_2"],null],["Address","Address"],[["Var",["Ident",["SimpleLocal","cparam3"],null]],null]],[["Ident",["SimpleLocal","assign_test_3"],null],["Address","Address"],[["Var",["Ident",["SimpleLocal","cparam3"],null]],null]],[["Ident",["SimpleLocal","assign_test_4"],null],["Address","Address"],[["Var",["Ident",["SimpleLocal","cparam3"],null]],null]],[["Ident",["SimpleLocal","assign_test_5"],null],["Address","Address"],[["Var",["Ident",["SimpleLocal","cparam3"],null]],null]],[["Ident",["SimpleLocal","assign_test_6"],null],["Address","Address"],[["Var",["Ident",["SimpleLocal","cparam3"],null]],null]],[["Ident",["SimpleLocal","assign_test_7"],null],["Address","Address"],[["Var",["Ident",["SimpleLocal","cparam3"],null]],null]],[["Ident",["SimpleLocal","assign_test_8"],null],["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]],[["Constr",["Ident",["SimpleLocal","Address1"],null],[],[["Ident",["SimpleLocal","cparam1"],null]]],null]],[["Ident",["SimpleLocal","assign_test_9"],null],["ADT",["Ident",["SimpleLocal","List"],{"fname":"","lnum":0,"cnum":0}],[["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]]],[["Constr",["Ident",["SimpleLocal","Nil"],null],[["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]],[]],null]],[["Ident",["SimpleLocal","assign_test_10"],null],["MapType",["PrimType",["Uint_typ",["Bits128"]]],["MapType",["PrimType",["Uint_typ",["Bits128"]]],["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]]],[["Literal",{"mtype":[["PrimType",["Uint_typ",["Bits128"]]],["MapType",["PrimType",["Uint_typ",["Bits128"]]],["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]]],"data":[]}],null]],[["Ident",["SimpleLocal","remote_reads_test_res_1_1"],null],["PrimType",["Uint_typ",["Bits128"]]],[["Literal","0"],null]],[["Ident",["SimpleLocal","remote_reads_test_res_2_1"],null],["PrimType",["Uint_typ",["Bits128"]]],[["Literal","0"],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_1"],null],["PrimType",["Uint_typ",["Bits128"]]],[["Literal","0"],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_3"],null],["PrimType",["Uint_typ",["Bits32"]]],[["Literal","0"],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_4"],null],["Address","Address"],[["Var",["Ident",["SimpleLocal","cparam3"],null]],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_5"],null],["PrimType",["Uint_typ",["Bits128"]]],[["Literal","0"],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_6"],null],["MapType",["Address","Address"],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]],[["Literal",{"mtype":[["Address","Address"],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]],"data":[]}],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_7"],null],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]],[["Constr",["Ident",["SimpleLocal","True"],null],[],[]],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_8"],null],["ADT",["Ident",["SimpleLocal","Option"],{"fname":"","lnum":0,"cnum":0}],[["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]]],[["Let",["Ident",["SimpleLocal","x"],null],null,[["Constr",["Ident",["SimpleLocal","True"],null],[],[]],null],[["Constr",["Ident",["SimpleLocal","Some"],null],[["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]],[["Ident",["SimpleLocal","x"],null]]],null]],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_9"],null],["MapType",["PrimType",["Uint_typ",["Bits32"]]],["MapType",["Address","Address"],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]]],[["Literal",{"mtype":[["PrimType",["Uint_typ",["Bits32"]]],["MapType",["Address","Address"],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]]],"data":[]}],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_10"],null],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]],[["Constr",["Ident",["SimpleLocal","False"],null],[],[]],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_11"],null],["ADT",["Ident",["SimpleLocal","Option"],{"fname":"","lnum":0,"cnum":0}],[["MapType",["Address","Address"],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]]]],[["Constr",["Ident",["SimpleLocal","None"],null],[["MapType",["Address","Address"],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]]],[]],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_12"],null],["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]],[["Constr",["Ident",["SimpleLocal","False"],null],[],[]],null]],[["Ident",["SimpleLocal","remote_reads_test_res_3_13"],null],["ADT",["Ident",["SimpleLocal","Option"],{"fname":"","lnum":0,"cnum":0}],[["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]]],[["Constr",["Ident",["SimpleLocal","None"],null],[["ADT",["Ident",["SimpleLocal","Bool"],{"fname":"","lnum":0,"cnum":0}],[]]],[]],null]],[["Ident",["SimpleLocal","sender_balance_pre"],null],["PrimType",["Uint_typ",["Bits128"]]],[["Literal","0"],null]],[["Ident",["SimpleLocal","sender_balance_mid"],null],["PrimType",["Uint_typ",["Bits128"]]],[["Literal","0"],null]],[["Ident",["SimpleLocal","sender_balance_post"],null],["PrimType",["Uint_typ",["Bits128"]]],[["Literal","0"],null]]],"ccomps":[{"comp_type":["CompTrans"],"comp_name":["Ident",["SimpleLocal","RemoteReadsTest"],null],"comp_params":[[["Ident",["SimpleLocal","remote1"],null],["Address","Address"]],[["Ident",["SimpleLocal","remote2"],null],["Address","Address"]],[["Ident",["SimpleLocal","remote3"],null],["Address","Address"]]],"comp_body":[[["RemoteLoad",["Ident",["SimpleLocal","tmp_1_1"],null],["Ident",["SimpleLocal","remote1"],null],["Ident",["SimpleLocal","_balance"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_1_1"],null],["Ident",["SimpleLocal","tmp_1_1"],null]],null],[["RemoteLoad",["Ident",["SimpleLocal","tmp_2_1"],null],["Ident",["SimpleLocal","remote2"],null],["Ident",["SimpleLocal","_balance"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_2_1"],null],["Ident",["SimpleLocal","tmp_2_1"],null]],null],[["RemoteLoad",["Ident",["SimpleLocal","tmp_3_1"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","_balance"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_1"],null],["Ident",["SimpleLocal","tmp_3_1"],null]],null],[["RemoteLoad",["Ident",["SimpleLocal","tmp_3_3"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","transactionCount"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_3"],null],["Ident",["SimpleLocal","tmp_3_3"],null]],null],[["RemoteLoad",["Ident",["SimpleLocal","tmp_3_4"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","admin"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_4"],null],["Ident",["SimpleLocal","tmp_3_4"],null]],null],[["RemoteLoad",["Ident",["SimpleLocal","tmp_3_5"],null],["Ident",["SimpleLocal","tmp_3_4"],null],["Ident",["SimpleLocal","_balance"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_5"],null],["Ident",["SimpleLocal","tmp_3_5"],null]],null],[["RemoteLoad",["Ident",["SimpleLocal","tmp_3_6"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","owners"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_6"],null],["Ident",["SimpleLocal","tmp_3_6"],null]],null],[["RemoteMapGet",["Ident",["SimpleLocal","tmp_3_7"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","owners"],null],["Mutable"],[["Ident",["SimpleLocal","_sender"],null]],false],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_7"],null],["Ident",["SimpleLocal","tmp_3_7"],null]],null],[["RemoteMapGet",["Ident",["SimpleLocal","tmp_3_8"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","owners"],null],["Mutable"],[["Ident",["SimpleLocal","_sender"],null]],true],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_8"],null],["Ident",["SimpleLocal","tmp_3_8"],null]],null],[["RemoteLoad",["Ident",["SimpleLocal","tmp_3_9"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","signatures"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_9"],null],["Ident",["SimpleLocal","tmp_3_9"],null]],null],[["Bind",["Ident",["SimpleLocal","x"],null],[["Literal","0"],null]],null],[["RemoteMapGet",["Ident",["SimpleLocal","tmp_3_10"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","signatures"],null],["Mutable"],[["Ident",["SimpleLocal","x"],null]],false],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_10"],null],["Ident",["SimpleLocal","tmp_3_10"],null]],null],[["RemoteMapGet",["Ident",["SimpleLocal","tmp_3_11"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","signatures"],null],["Mutable"],[["Ident",["SimpleLocal","x"],null]],true],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_11"],null],["Ident",["SimpleLocal","tmp_3_11"],null]],null],[["RemoteMapGet",["Ident",["SimpleLocal","tmp_3_12"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","signatures"],null],["Mutable"],[["Ident",["SimpleLocal","x"],null],["Ident",["SimpleLocal","_origin"],null]],false],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_12"],null],["Ident",["SimpleLocal","tmp_3_12"],null]],null],[["RemoteMapGet",["Ident",["SimpleLocal","tmp_3_13"],null],["Ident",["SimpleLocal","remote3"],null],["Ident",["SimpleLocal","signatures"],null],["Mutable"],[["Ident",["SimpleLocal","x"],null],["Ident",["SimpleLocal","_origin"],null]],true],null],[["Store",["Ident",["SimpleLocal","remote_reads_test_res_3_13"],null],["Ident",["SimpleLocal","tmp_3_13"],null]],null]],"comp_return":null},{"comp_type":["CompTrans"],"comp_name":["Ident",["SimpleLocal","RemoteReadsADTTest"],null],"comp_params":[[["Ident",["SimpleLocal","list1"],null],["ADT",["Ident",["SimpleLocal","List"],{"fname":"","lnum":0,"cnum":0}],[["Address","Address"]]]],[["Ident",["SimpleLocal","list2"],null],["ADT",["Ident",["SimpleLocal","List"],{"fname":"","lnum":0,"cnum":0}],[["Address","Address"]]]],[["Ident",["SimpleLocal","list3"],null],["ADT",["Ident",["SimpleLocal","List"],{"fname":"","lnum":0,"cnum":0}],[["Address","Address"]]]],[["Ident",["SimpleLocal","pair1"],null],["ADT",["Ident",["SimpleLocal","Pair"],{"fname":"","lnum":0,"cnum":0}],[["Address","Address"],["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]]]],[["Ident",["SimpleLocal","adt1"],null],["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]],[["Ident",["SimpleLocal","remote1"],null],["Address","Address"]]],"comp_body":[],"comp_return":null},{"comp_type":["CompTrans"],"comp_name":["Ident",["SimpleLocal","OutgoingMsgTest"],null],"comp_params":[],"comp_body":[[["Bind",["Ident",["SimpleLocal","msg"],null],[["Message",[["_tag",["MLit",""]],["_recipient",["MVar",["Ident",["SimpleLocal","_sender"],null]]],["_amount",["MLit","0"]],["param",["MVar",["Ident",["SimpleLocal","cparam3"],null]]]]],null]],null],[["Bind",["Ident",["SimpleLocal","msgs"],null],[["Let",["Ident",["SimpleLocal","n"],null],null,[["Constr",["Ident",["SimpleLocal","Nil"],null],[["PrimType",["Msg_typ"]]],[]],null],[["Constr",["Ident",["SimpleLocal","Cons"],null],[["PrimType",["Msg_typ"]]],[["Ident",["SimpleLocal","msg"],null],["Ident",["SimpleLocal","n"],null]]],null]],null]],null],[["SendMsgs",["Ident",["SimpleLocal","msgs"],null]],null],[["Bind",["Ident",["SimpleLocal","e1"],null],[["Message",[["_eventname",["MLit","TestEvent"]],["info",["MVar",["Ident",["SimpleLocal","cparam2"],null]]]]],null]],null],[["CreateEvnt",["Ident",["SimpleLocal","e1"],null]],null],[["Bind",["Ident",["SimpleLocal","e2"],null],[["Message",[["_eventname",["MLit","TestEvent"]],["info",["MVar",["Ident",["SimpleLocal","cparam3"],null]]]]],null]],null],[["CreateEvnt",["Ident",["SimpleLocal","e2"],null]],null]],"comp_return":null},{"comp_type":["CompTrans"],"comp_name":["Ident",["SimpleLocal","ExceptionTest"],null],"comp_params":[],"comp_body":[[["Bind",["Ident",["SimpleLocal","e"],null],[["Message",[["_exception",["MLit","TestException"]],["value",["MVar",["Ident",["SimpleLocal","cparam3"],null]]]]],null]],null],[["Throw",["Ident",["SimpleLocal","e"],null]],null]],"comp_return":null},{"comp_type":["CompTrans"],"comp_name":["Ident",["SimpleLocal","AssignTest"],null],"comp_params":[],"comp_body":[[["Bind",["Ident",["SimpleLocal","x"],null],[["Constr",["Ident",["SimpleLocal","Address2"],null],[],[["Ident",["SimpleLocal","cparam3"],null]]],null]],null],[["Store",["Ident",["SimpleLocal","assign_test_8"],null],["Ident",["SimpleLocal","x"],null]],null],[["Bind",["Ident",["SimpleLocal","y"],null],[["Let",["Ident",["SimpleLocal","n"],null],null,[["Constr",["Ident",["SimpleLocal","Nil"],null],[["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]],[]],null],[["Constr",["Ident",["SimpleLocal","Cons"],null],[["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]],[["Ident",["SimpleLocal","x"],null],["Ident",["SimpleLocal","n"],null]]],null]],null]],null],[["Store",["Ident",["SimpleLocal","assign_test_9"],null],["Ident",["SimpleLocal","y"],null]],null],[["Bind",["Ident",["SimpleLocal","z"],null],[["Let",["Ident",["SimpleLocal","n"],null],null,[["Literal",{"mtype":[["PrimType",["Uint_typ",["Bits128"]]],["MapType",["PrimType",["Uint_typ",["Bits128"]]],["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]]],"data":[]}],null],[["Let",["Ident",["SimpleLocal","sub_n"],null],null,[["Literal",{"mtype":[["PrimType",["Uint_typ",["Bits128"]]],["ADT",["Ident",["SimpleLocal","AddressADT"],{"fname":"","lnum":0,"cnum":0}],[]]],"data":[]}],null],[["Let",["Ident",["SimpleLocal","sub_k"],null],null,[["Literal","0"],null],[["Let",["Ident",["SimpleLocal","sub_res"],null],null,[["Builtin",[["Builtin_put"],null],[],[["Ident",["SimpleLocal","sub_n"],null],["Ident",["SimpleLocal","sub_k"],null],["Ident",["SimpleLocal","x"],null]]],null],[["Builtin",[["Builtin_put"],null],[],[["Ident",["SimpleLocal","n"],null],["Ident",["SimpleLocal","sub_k"],null],["Ident",["SimpleLocal","sub_res"],null]]],null]],null]],null]],null]],null]],null],[["Store",["Ident",["SimpleLocal","assign_test_10"],null],["Ident",["SimpleLocal","z"],null]],null],[["Bind",["Ident",["SimpleLocal","k1"],null],[["Literal","1"],null]],null],[["Bind",["Ident",["SimpleLocal","k2"],null],[["Literal","42"],null]],null],[["MapUpdate",["Ident",["SimpleLocal","assign_test_10"],null],[["Ident",["SimpleLocal","k1"],null],["Ident",["SimpleLocal","k2"],null]],["Ident",["SimpleLocal","x"],null]],null]],"comp_return":null},{"comp_type":["CompTrans"],"comp_name":["Ident",["SimpleLocal","SenderBalanceTest"],null],"comp_params":[],"comp_body":[[["RemoteLoad",["Ident",["SimpleLocal","pre"],null],["Ident",["SimpleLocal","_sender"],null],["Ident",["SimpleLocal","_balance"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","sender_balance_pre"],null],["Ident",["SimpleLocal","pre"],null]],null],[["AcceptPayment"],null],[["RemoteLoad",["Ident",["SimpleLocal","mid"],null],["Ident",["SimpleLocal","_sender"],null],["Ident",["SimpleLocal","_balance"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","sender_balance_mid"],null],["Ident",["SimpleLocal","mid"],null]],null],[["AcceptPayment"],null],[["RemoteLoad",["Ident",["SimpleLocal","post"],null],["Ident",["SimpleLocal","_sender"],null],["Ident",["SimpleLocal","_balance"],null],["Mutable"]],null],[["Store",["Ident",["SimpleLocal","sender_balance_post"],null],["Ident",["SimpleLocal","post"],null]],null]],"comp_return":null}]}}
