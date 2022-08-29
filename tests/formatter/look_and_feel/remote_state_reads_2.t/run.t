scilla_version 0

contract RRContract (cparam : ByStr20 with contract field admin : ByStr20 end)

field remote_read_remote_read_res_1 : Uint128 = Uint128 0
field remote_read_remote_read_res_2 : Option Uint128 = None {Uint128}
field remote_read_remote_read_res_3 : ByStr20 = _this_address

field address_type_erasure_test_res_1 : Map Uint128 (ByStr20 with end) = Emp Uint128 (ByStr20 with end)

transition RemoteReadsOfRemoteRead(
end

transition RemoteReadsOfRemoteMap(
end

transition RemoteReadsContractParam()
end

transition AddressTypeErasureTest1()
end

transition AddressTypeErasureTest2()
end
