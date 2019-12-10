(*
  This file is part of scilla.

  Copyright (c) 2018 - present Zilliqa Research Pvt. Ltd.

  scilla is free software: you can redistribute it and/or modify it under the
  terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  scilla is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
  A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  scilla.  If not, see <http://www.gnu.org/licenses/>.
*)

(* The tests in this file are modelled after Ethereum's unit tests for zkSNARKs.
   https://github.com/ethereum/aleth/blob/master/test/unittests/libdevcrypto/LibSnark.cpp *)

open Core_kernel
open OUnit2
open Snark
open Integer256
open Syntax

(* Convert a decimal string to a binary string of 32 bytes. *)
let dec2bystr32 s =
  let b = Bytes.create 32 in
  Uint256.to_bytes_big_endian (Uint256.of_string s) b 0;
  Bytes.to_string b

let hex2bystr h =
  let b = Bystr.parse_hex h in
  Bystr.to_raw_bytes b

let group_order = dec2bystr32 "21888242871839275222246405745257275088548364400416034343698204186575808495617"
let group_order_m1 = dec2bystr32 "21888242871839275222246405745257275088548364400416034343698204186575808495616"

let add_helper p1 p2 =
  match alt_bn128_G1_add p1 p2 with
  | Some r -> r
  | None -> assert_failure "TestSnark: add_bn128_G1_add failed"

let mul_helper p s =
  match alt_bn128_G1_mul p s with
  | Some r -> r
  | None -> assert_failure "TestSnark: add_bn128_G1_mul failed"

let pairingprod_helper pairs =
  match alt_bn128_pairing_product pairs with
  | Some b -> b
  | None -> assert_failure "TestSnark: add_bn128_pairing_product failed"

let negateG1 p =
    match alt_bn128_G1_mul p group_order_m1 with
    | Some r -> r
    | None -> assert_failure "TestSnark: negateG1 failed"

let test_add_zero = test_case (fun _ ->
  (* "0 + 0 == 0" *)
  let zero_zero_p = {g1x = (dec2bystr32 "0"); g1y = (dec2bystr32 "0")} in
  match alt_bn128_G1_add zero_zero_p zero_zero_p with
  | Some result -> assert_bool "TestSnark failed: test_add_zero: incorrect result" (result = zero_zero_p)
  | None -> assert_failure "TestSnark failed: test_add_zero: failed."
)

let test_invalid = test_case (fun _ ->
  let (x, y) = 
    (dec2bystr32 "6851077925310461602867742977619883934042581405263014789956638244065803308498"),
    (dec2bystr32 "10336382210592135525880811046708757754106524561907815205241508542912494488506")
  in
  let (invalid_x, invalid_y) = (String.sub x ~pos:0 ~len:3) ^
    (((Char.to_int x.[3]) lxor 1) |> Char.of_int_exn |> String.of_char) ^
    (String.sub x ~pos:4 ~len:(String.length x - 4))
    , y
  in
  match alt_bn128_G1_add {g1x = x; g1y = y} {g1x = invalid_x; g1y = invalid_y} with
  | Some _ -> assert_failure "TestSnark failed: test_invalid: alt_bn128_add succeeded on invalid input"
  | None -> ()
)

let test_mul_add = test_case (fun _ ->
  let p =
    { g1x = (dec2bystr32 "6851077925310461602867742977619883934042581405263014789956638244065803308498");
      g1y = (dec2bystr32 "10336382210592135525880811046708757754106524561907815205241508542912494488506")}
  in
  let s = (dec2bystr32 "2") in
  match alt_bn128_G1_add p p, alt_bn128_G1_mul p s with
  | Some sum, Some prod ->
    assert_bool "TestSnark failed: test_mul_add: comparison failed" (prod = sum) 
  | _ -> assert_failure "TestSnark failed: test_mul_add: alt_bn128_(add/mul) failed"
)

let test_pairing = test_case (fun _ ->

  let vk_A = {
    g2x =
      hex2bystr("0x209dd15ebff5d46c4bd888e51a93cf99a7329636c63514396b4a452003a35bf7") ^
      hex2bystr("0x04bf11ca01483bfa8b34b43561848d28905960114c8ac04049af4b6315a41678");
    g2y =
      hex2bystr("0x2bb8324af6cfc93537a2ad1a445cfd0ca2a71acd7ac41fadbf933c2a51be344d") ^
      hex2bystr("0x120a2a4cf30c1bf9845f20c6fe39e07ea2cce61f0c9bb048165fe5e4de877550")
  } in
  let vk_B = {
    g1x = hex2bystr("0x2eca0c7238bf16e83e7a1e6c5d49540685ff51380f309842a98561558019fc02");
    g1y = hex2bystr("0x03d3260361bb8451de5ff5ecd17f010ff22f5c31cdf184e9020b06fa5997db84")
  } in
  let vk_C = {
    g2x =
      hex2bystr("0x2e89718ad33c8bed92e210e81d1853435399a271913a6520736a4729cf0d51eb") ^
      hex2bystr("0x01a9e2ffa2e92599b68e44de5bcf354fa2642bd4f26b259daa6f7ce3ed57aeb3");
    g2y =
      hex2bystr("0x14a9a87b789a58af499b314e13c3d65bede56c07ea2d418d6874857b70763713") ^
      hex2bystr("0x178fb49a2d6cd347dc58973ff49613a20757d0fcc22079f9abd10c3baee24590")
  } in
  let vk_gamma = {
    g2x =
      hex2bystr("0x25f83c8b6ab9de74e7da488ef02645c5a16a6652c3c71a15dc37fe3a5dcb7cb1") ^
      hex2bystr("0x22acdedd6308e3bb230d226d16a105295f523a8a02bfc5e8bd2da135ac4c245d");
    g2y =
      hex2bystr("0x065bbad92e7c4e31bf3757f1fe7362a63fbfee50e7dc68da116e67d600d9bf68") ^
      hex2bystr("0x06d302580dc0661002994e7cd3a7f224e7ddc27802777486bf80f40e4ca3cfdb")
  } in
  let vk_gammaBeta1 = {
    g1x = hex2bystr("0x15794ab061441e51d01e94640b7e3084a07e02c78cf3103c542bc5b298669f21");
    g1y = hex2bystr("0x14db745c6780e9df549864cec19c2daf4531f6ec0c89cc1c7436cc4d8d300c6d")
  } in
  let vk_gammaBeta2 = {
    g2x =
      hex2bystr("0x1f39e4e4afc4bc74790a4a028aff2c3d2538731fb755edefd8cb48d6ea589b5e") ^
      hex2bystr("0x283f150794b6736f670d6a1033f9b46c6f5204f50813eb85c8dc4b59db1c5d39");
    g2y =
      hex2bystr("0x140d97ee4d2b36d99bc49974d18ecca3e7ad51011956051b464d9e27d46cc25e") ^
      hex2bystr("0x0764bb98575bd466d32db7b15f582b2d5c452b36aa394b789366e5e3ca5aabd4")
  } in
  let vk_Z = {
    g2x = 
      hex2bystr("0x217cee0a9ad79a4493b5253e2e4e3a39fc2df38419f230d341f60cb064a0ac29") ^
      hex2bystr("0x0a3d76f140db8418ba512272381446eb73958670f00cf46f1d9e64cba057b53c");
    g2y =
      hex2bystr("0x26f64a8ec70387a13e41430ed3ee4a7db2059cc5fc13c067194bcc0cb49a9855") ^
      hex2bystr("0x2fd72bd9edb657346127da132e5b82ab908f5816c826acb499e22f2412d1a2d7")
  } in

  let vk_IC = [
    {
      g1x = hex2bystr("0x0aee46a7ea6e80a3675026dfa84019deee2a2dedb1bbe11d7fe124cb3efb4b5a");
      g1y = hex2bystr("0x044747b6e9176e13ede3a4dfd0d33ccca6321b9acd23bf3683a60adc0366ebaf")
    };
    {
      g1x = hex2bystr("0x1e39e9f0f91fa7ff8047ffd90de08785777fe61c0e3434e728fce4cf35047ddc");
      g1y = hex2bystr("0x2e0b64d75ebfa86d7f8f8e08abbe2e7ae6e0a1c0b34d028f19fa56e9450527cb")
    };
    {
      g1x = hex2bystr("0x1c36e713d4d54e3a9644dffca1fc524be4868f66572516025a61ca542539d43f");
      g1y = hex2bystr("0x042dcc4525b82dfb242b09cb21909d5c22643dcdbe98c4d082cc2877e96b24db")
    };
    {
      g1x = hex2bystr("0x17d5d09b4146424bff7e6fb01487c477bbfcd0cdbbc92d5d6457aae0b6717cc5");
      g1y = hex2bystr("0x02b5636903efbf46db9235bbe74045d21c138897fda32e079040db1a16c1a7a1")
    };
    {
      g1x = hex2bystr("0x0f103f14a584d4203c27c26155b2c955f8dfa816980b24ba824e1972d6486a5d");
      g1y = hex2bystr("0x0c4165133b9f5be17c804203af781bcf168da7386620479f9b885ecbcd27b17b")
    };
    {
      g1x = hex2bystr("0x232063b584fb76c8d07995bee3a38fa7565405f3549c6a918ddaa90ab971e7f8");
      g1y = hex2bystr("0x2ac9b135a81d96425c92d02296322ad56ffb16299633233e4880f95aafa7fda7")
    };
    {
      g1x = hex2bystr("0x09b54f111d3b2d1b2fe1ae9669b3db3d7bf93b70f00647e65c849275de6dc7fe");
      g1y = hex2bystr("0x18b2e77c63a3e400d6d1f1fbc6e1a1167bbca603d34d03edea231eb0ab7b14b4")
    };
    {
      g1x = hex2bystr("0x0c54b42137b67cc268cbb53ac62b00ecead23984092b494a88befe58445a244a");
      g1y = hex2bystr("0x18e3723d37fae9262d58b548a0575f59d9c3266db7afb4d5739555837f6b8b3e")
    };
    {
      g1x = hex2bystr("0x0a6de0e2240aa253f46ce0da883b61976e3588146e01c9d8976548c145fe6e4a");
      g1y = hex2bystr("0x04fbaa3a4aed4bb77f30ebb07a3ec1c7d77a7f2edd75636babfeff97b1ea686e")
    };
    {
      g1x= hex2bystr("0x111e2e2a5f8828f80ddad08f9f74db56dac1cc16c1cb278036f79a84cf7a116f");
      g1y = hex2bystr("0x1d7d62e192b219b9808faa906c5ced871788f6339e8d91b83ac1343e20a16b30")
    }
  ] in
  let proof_A = {
    g1x = dec2bystr32("12873740738727497448187997291915224677121726020054032516825496230827252793177");
    g1y = dec2bystr32("21804419174137094775122804775419507726154084057848719988004616848382402162497")
  } in
  let proof_Ap = {
    g1x = dec2bystr32("7742452358972543465462254569134860944739929848367563713587808717088650354556");
    g1y = dec2bystr32("7324522103398787664095385319014038380128814213034709026832529060148225837366")
  } in
  let proof_B = {
    g2x = 
      dec2bystr32("8176651290984905087450403379100573157708110416512446269839297438960217797614") ^
      dec2bystr32("15588556568726919713003060429893850972163943674590384915350025440408631945055");
    g2y =
      dec2bystr32("15347511022514187557142999444367533883366476794364262773195059233657571533367") ^
      dec2bystr32("4265071979090628150845437155927259896060451682253086069461962693761322642015")
  } in
  let proof_Bp = {
    g1x = dec2bystr32("2979746655438963305714517285593753729335852012083057917022078236006592638393");
    g1y = dec2bystr32("6470627481646078059765266161088786576504622012540639992486470834383274712950")
  } in
  let proof_C = {
    g1x = dec2bystr32("6851077925310461602867742977619883934042581405263014789956638244065803308498");
    g1y = dec2bystr32("10336382210592135525880811046708757754106524561907815205241508542912494488506")
  } in
  let proof_Cp = {
    g1x = dec2bystr32("12491625890066296859584468664467427202390981822868257437245835716136010795448");
    g1y = dec2bystr32("13818492518017455361318553880921248537817650587494176379915981090396574171686")
  } in
  let proof_H = {
    g1x = dec2bystr32("12091046215835229523641173286701717671667447745509192321596954139357866668225");
    g1y = dec2bystr32("14446807589950902476683545679847436767890904443411534435294953056557941441758")
  } in
  let proof_K = {
    g1x = dec2bystr32("21341087976609916409401737322664290631992568431163400450267978471171152600502");
    g1y = dec2bystr32("2942165230690572858696920423896381470344658299915828986338281196715687693170")
  } in
  let input = [
    dec2bystr32("13986731495506593864492662381614386532349950841221768152838255933892789078521");
    dec2bystr32("622860516154313070522697309645122400675542217310916019527100517240519630053");
    dec2bystr32("11094488463398718754251685950409355128550342438297986977413505294941943071569");
    dec2bystr32("6627643779954497813586310325594578844876646808666478625705401786271515864467");
    dec2bystr32("2957286918163151606545409668133310005545945782087581890025685458369200827463");
    dec2bystr32("1384290496819542862903939282897996566903332587607290986044945365745128311081");
    dec2bystr32("5613571677741714971687805233468747950848449704454346829971683826953541367271");
    dec2bystr32("9643208548031422463313148630985736896287522941726746581856185889848792022807");
    dec2bystr32("18066496933330839731877828156604");
  ] in
  let p2 = {
    g2x =
      dec2bystr32("11559732032986387107991004021392285783925812861821192530917403151452391805634") ^
      dec2bystr32("10857046999023057135944570762232829481370756359578518086990519993285655852781");
    g2y =
      dec2bystr32("4082367875863433681332203403145435568316851327593401208105741076214120093531") ^
      dec2bystr32("8495653923123431417604973247489272438418190587263600148770280649306958101930")
  } in

  (*  Compute the linear combination vk_x *)
  let vkx_init = { g1x = dec2bystr32("0"); g1y = dec2bystr32("0") } in
  let vkx' = List.fold2_exn (List.tl_exn vk_IC) input ~init:vkx_init ~f:(fun acc a b ->
      add_helper acc (mul_helper a b)
  ) in
  let vkx = add_helper vkx' (List.hd_exn vk_IC) in

  (* Now run the pairing checks. *)
  assert_bool "TestSnark: test_pairing1: alt_bn128_pairing_product incorrect result" 
    (pairingprod_helper [(proof_A, vk_A); (negateG1 proof_Ap, p2)]);

  assert_bool "TestSnark: test_pairing2: alt_bn128_pairing_product incorrect result"
    (pairingprod_helper [(vk_B, proof_B); (negateG1 proof_Bp,  p2)]);

  assert_bool "TestSnark: test_pairing3: alt_bn128_pairing_product incorrect result"
    (pairingprod_helper [(proof_C, vk_C); (negateG1 proof_Cp, p2)]);

  assert_bool "TestSnark: test_pairing4: alt_bn128_pairing_product incorrect result"
    (pairingprod_helper
      [
        (proof_K, vk_gamma);
        (negateG1 (add_helper vkx (add_helper proof_A proof_C)), vk_gammaBeta2);
        (negateG1 vk_gammaBeta1, proof_B)
      ]
    );

  assert_bool "TestSnark: test_pairing5: alt_bn128_pairing_product incorrect result"
    (pairingprod_helper
      [
        (add_helper vkx proof_A, proof_B);
        (negateG1 proof_H, vk_Z);
        (negateG1 proof_C, p2);
      ]
    );

  (* Just for the fun of it, try a wrong check. *)
  assert_bool "TestSnark: test_pairing6: alt_bn128_pairing_product incorrect result"
    (not (pairingprod_helper [(proof_A, vk_A); (proof_Ap, p2)]));
)

let test_pairing_null_input = test_case (fun _ ->

  (* TODO: Maybe the empty input should also be considered invalid? *)
  assert_bool "TestSnark: test_pairing_null_input1: failed"
  (pairingprod_helper([]));

  let g1_zeros = { g1x = dec2bystr32("0"); g1y = dec2bystr32("0") } in
  let g2_zeros = { 
    g2x = dec2bystr32("0") ^ dec2bystr32("0");
    g2y = dec2bystr32("0") ^ dec2bystr32("0");
  } in
  assert_bool "TestSnark: test_pairing_null_input2: failed"
    (pairingprod_helper [(g1_zeros, g2_zeros)]);

  (* Invalid length of input. *)
  let g1_zeros' = { g1x = (dec2bystr32("0") ^ dec2bystr32("0")); g1y = dec2bystr32("0") } in
  match alt_bn128_pairing_product [(g1_zeros', g2_zeros)] with
  | Some _ -> assert_failure "TestSnark: test_pairing_null_input3: failed"
  | None -> ()
)

let test_generate_random_points = test_case (fun _ ->

  let trivialPt = { g1x = dec2bystr32 "1"; g1y = dec2bystr32 "2" } in

  let input = dec2bystr32 "1" in
  let output = mul_helper trivialPt input in
  assert_bool "TestSnark: test_generate_random_points1: incorrect value"
    (eq_g1 output trivialPt);

  let input = dec2bystr32 "123454352435654643" in
  let a = mul_helper trivialPt input in
  assert_bool "TestSnark: test_generate_random_points2: incorrect value"
    ((encode_g1point_bytes a) =
    (hex2bystr "0x18b18acfb4c2c30276db5411368e7185b311dd124691610c5d3b74034e093dc9063c909c4720840cb5134cb9f59fa749755796819658d32efc0d288198f37266")
    );

  let input = dec2bystr32 "653179013456575642" in
  let b = mul_helper trivialPt input in
  assert_bool "TestSnark: test_generate_random_points3: incorrect value"
    ((encode_g1point_bytes b) =
    (hex2bystr "0x07c2b7f58a84bd6145f00c9c2bc0bb1a187f20ff2c92963a88019e7c6a014eed06614e20c147e940f2d70da3f74c9a17df361706a4485c742bd6788478fa17d7")
    );

  let c = add_helper a b in
  assert_bool "TestSnark: test_generate_random_points4: incorrect value"
    ((encode_g1point_bytes c) =
    (hex2bystr "0x2243525c5efd4b9c3d3c45ac0ca3fe4dd85e830a4ce6b65fa1eeaee202839703301d1d33be6da8e509df21cc35964723180eed7532537db9ae5e7d48f195c915")
    );

  let d = add_helper c a in
  assert_bool "TestSnark: test_generate_random_points5: incorrect value"
  ((encode_g1point_bytes d) =
  (hex2bystr "0x2bd3e6d0f3b142924f5ca7b49ce5b9d54c4703d7ae5648e61d02268b1a0a9fb721611ce0a6af85915e2f1d70300909ce2e49dfad4a4619c8390cae66cefdb204")
  );

  let input = dec2bystr32 "1230482048326178242" in
  let e = mul_helper d input in
  assert_bool "TestSnark: test_generate_random_points6: incorrect value"
  ((encode_g1point_bytes e) =
  (hex2bystr "0x070a8d6a982153cae4be29d434e8faef8a47b274a053f5a4ee2a6c9c13c31e5c031b8ce914eba3a9ffb989f9cdd5b0f01943074bf4f0f315690ec3cec6981afc")
  );

  (* Multiply by (p - 1). *)
  let input = dec2bystr32 "21888242871839275222246405745257275088696311157297823662689037894645226208582" in
  let f = mul_helper e input in
  assert_bool "TestSnark: test_generate_random_points7: incorrect value"
  ((encode_g1point_bytes f) =
  (hex2bystr "0x025a6f4181d2b4ea8b724290ffb40156eb0adb514c688556eb79cdea0752c2bb2eff3f31dea215f1eb86023a133a996eb6300b44da664d64251d05381bb8a02e")
  );

  (* Multiply by (p - 1) / 2. *)
  let input = dec2bystr32 "10944121435919637611123202872628637544348155578648911831344518947322613104291" in
  let output = mul_helper f input in
  assert_bool "TestSnark: test_generate_random_points8: incorrect value"
  ((encode_g1point_bytes output) =
  (hex2bystr "0x14789d0d4a730b354403b5fac948113739e276c23e0258d8596ee72f9cd9d3230af18a63153e0ec25ff9f2951dd3fa90ed0197bfef6e2a1a62b5095b9d2b4a27")
  );
)

let snark_tests _ = "snark_tests" >::: [
  test_add_zero;
  test_invalid;
  test_mul_add;
  test_pairing;
  test_pairing_null_input;
  test_generate_random_points;
]
