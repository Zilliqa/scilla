  $ scilla-fmt map_corners_test.scilla
  scilla_version 0
  (* A script to run all transitions here against a running IPC server is provided. *)
  (* Check out scripts/run_ipc_map_corner_tests.sh. *)
  
  import BoolUtils
  
  library MapCornersTest
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  
  (* The test sequence here is based on the unit tests in Test_ScillaIPCServer.cpp. *)
  (* Transitions "t*" are expected to succeed while "f*" are expected to fail. *)
  contract MapCornersTest ()
  
  
  field f_s1 : String = "420"
  
  field f_m1 : Map String String = Emp (String) (String)
  
  field f_m2 : Map String (Map String String) = Emp (String) (Map String String)
  
  field f_m3 : Map String (Map String (Map String String)) =
    Emp (String) (Map String (Map String String))
  
  field f_m : Map String (Map String String) = Emp (String) (Map String String)
  
  procedure fail (tname : String)
    e = { _exception : "Test Failed"; test_name : tname };
    throw e
  end
  
  procedure fail_msg (tname : String, msg : String)
    e = { _exception : "Test Failed"; test_name : tname; message : msg };
    throw e
  end
  
  procedure expected_fail (tname : String)
    e = { _exception : "Test failed as expected"; test_name : tname };
    throw e
  end
  
  procedure expected_fail_msg (tname : String, msg : String)
    e =
      { _exception : "Test failed as expected"; test_name : tname; message : msg };
    throw e
  end
  
  (* test_query_simple *)
  transition t1 ()
    (* Check existing value. *)
    tname = "t1";
    f <- f_s1;
    s = "420";
    t = builtin eq f s;
    match t with
    | False => fail tname
    | True =>
    end;
    (* Now store back a different value for next test. *)
    s2 = "421";
    f_s1 := s2
  end
  
  (* test_query_map_1 *)
  transition t2 ()
    (* Check existing value. *)
    tname = "t2";
    f <- f_s1;
    s = "421";
    t = builtin eq f s;
    match t with
    | False => fail tname
    | True =>
    end;
    (* Insert value for next test. *)
    key1 = "key1";
    val1 = "420";
    f_m1[key1] := val1
  end
  
  (* foo_test_query_map_1 *)
  transition t3 ()
    tname = "t3";
    s = "420";
    (* Fetch key1 and ensure value is "420" as set in t2. *)
    key1 = "key1";
    val1 <- f_m1[key1];
    match val1 with
    | Some val =>
      t = builtin eq val s;
      match t with
      | False =>
        m = "Incorrect value for key";
        fail_msg tname m
      | True =>
      end
    | None => fail tname
    end;
    (* Fetch "key2" and ensure not found. *)
    key2 = "key2";
    val2 <- f_m1[key2];
    match val2 with
    | Some _ =>
      m = "Rogue value found for key";
      fail_msg tname m
    | None =>
    end;
    (* Delete key1 for next test. *)
    delete f_m1[key1]
  end
  
  (* foo_test_query_map_1 *)
  transition t4 ()
    tname = "t4";
    (* Ensure key1 not present anymore. *)
    key1 = "key1";
    key1_found <- exists f_m1[key1];
    match key1_found with
    | True => fail tname
    | False =>
    end;
    (* Store data for next test. *)
    key1a = "key1a";
    key2a = "key2a";
    s = "420";
    f_m2[key1a][key2a] := s
  end
  
  (* test_query_map_2 *)
  transition t5 ()
    tname = "t5";
    s = "420";
    (* Ensure f_m2[key1a][key2a] has value "420". *)
    key1a = "key1a";
    key2a = "key2a";
    val <- f_m2[key1a][key2a];
    match val with
    | Some v =>
      t = builtin eq v s;
      match t with
      | True =>
      | False =>
        m = "Incorrect value fetched";
        fail_msg tname m
      end
    | None => fail tname
    end;
    (* Store data for next test. *)
    l_m2 =
      let e = Emp (String) (String) in
      let key2b = "key2b" in
      let s1 = "840" in
      let m1 = builtin put e key2b s1 in
      let key2c = "key2c" in
      let s2 = "841" in
      builtin put m1 key2c s2;
    key1b = "key1b";
    f_m2[key1b] := l_m2
  end
  
  (* test_query_map_2 *)
  transition t6 ()
    tname = "t6";
    (* We now expect the storage to contain: *)
    (* f_m2[key1a][key2a] : 420 *)
    (* f_m3[key1b][key2b] : 840 *)
    (* f_m2[key1b][key2c] : 841 *)
    key1a = "key1a";
    key2a = "key2a";
    c1 <- f_m2[key1a][key2a];
    match c1 with
    | Some c =>
      v = "420";
      eq = builtin eq c v;
      match eq with
      | True =>
      | False =>
        m = "Incorrect value for key1a,key2a";
        fail_msg tname m
      end
    | None =>
      m = "key1a,key2a not found";
      fail_msg tname m
    end;
    key1b = "key1b";
    key2b = "key2b";
    c1 <- f_m2[key1b][key2b];
    match c1 with
    | Some c =>
      v = "840";
      eq = builtin eq c v;
      match eq with
      | True =>
      | False =>
        m = "Incorrect value for key1b,key2b";
        fail_msg tname m
      end
    | None =>
      m = "key1b,key2b not found";
      fail_msg tname m
    end;
    key1b = "key1b";
    key2c = "key2c";
    c1 <- f_m2[key1b][key2c];
    match c1 with
    | Some c =>
      v = "841";
      eq = builtin eq c v;
      match eq with
      | True =>
      | False =>
        m = "Incorrect value for key1b,key2c";
        fail_msg tname m
      end
    | None =>
      m = "key1b,key2c not found";
      fail_msg tname m
    end;
    (* Delete key1b for next test. *)
    delete f_m2[key1b]
  end
  
  (* test_query_delete_to_empty *)
  transition t7 ()
    tname = "t7";
    (* We now expect the storage to contain: *)
    (* f_m2[key1a][key2a] : 420 *)
    key1a = "key1a";
    key2a = "key2a";
    c1 <- f_m2[key1a][key2a];
    match c1 with
    | Some c =>
      v = "420";
      eq = builtin eq c v;
      match eq with
      | True =>
      | False =>
        m = "Incorrect value for key1a,key2a";
        fail_msg tname m
      end
    | None =>
      m = "key1a,key2a not found";
      fail_msg tname m
    end;
    (* And _not_ contain f_m2[key1b] *)
    key1b = "key1b";
    c1 <- f_m2[key1b];
    match c1 with
    | Some _ =>
      m = "Fail: deleted value for key1b found!";
      fail_msg tname m
    | None =>
    end;
    (* and _not_ contain f_m2[key1b][key2b] *)
    key1b = "key1b";
    key2b = "key2b";
    c1 <- f_m2[key1b][key2b];
    match c1 with
    | Some _ =>
      m = "Fail: deleted value for key1b,key2b found!";
      fail_msg tname m
    | None =>
    end;
    (* and _not_ contain f_m2[key1b][key2d] (which never existed) *)
    key1b = "key1b";
    key2d = "key2d";
    c1 <- f_m2[key1b][key2d];
    match c1 with
    | Some _ =>
      m = "Fail: deleted value for key1b,key2d found!";
      fail_msg tname m
    | None =>
    end;
    (* Add f_m2[key1b][key2c] back again, with a different value. *)
    key1b = "key1b";
    key2c = "key2c";
    s = "121";
    f_m2[key1b][key2c] := s
  end
  
  (* test_query_map_2 *)
  transition t8 ()
    tname = "t8";
    (* We now expect the storage to contain: *)
    (* f_m2[key1a][key2a] : 420 *)
    (* f_m2[key1b][key2c] : 121 *)
    key1a = "key1a";
    key2a = "key2a";
    c1 <- f_m2[key1a][key2a];
    match c1 with
    | Some c =>
      v = "420";
      eq = builtin eq c v;
      match eq with
      | True =>
      | False =>
        m = "Incorrect value for key1a,key2a";
        fail_msg tname m
      end
    | None =>
      m = "key1a,key2a not found";
      fail_msg tname m
    end;
    key1b = "key1b";
    key2c = "key2c";
    c1 <- f_m2[key1b][key2c];
    match c1 with
    | Some c =>
      v = "121";
      eq = builtin eq c v;
      match eq with
      | True =>
      | False =>
        m = "Incorrect value for key1b,key2c";
        fail_msg tname m
      end
    | None =>
      m = "key1b,key2c not found";
      fail_msg tname m
    end;
    (* For the next test, we replace f_m1 with an empty map. *)
    em = Emp (String) (String);
    f_m1 := em
  end
  
  (* test_query_empty_map *)
  transition t9 ()
    tname = "t9";
    (* Verify that f_m1 is empty. *)
    m1 <- f_m1;
    m1_size = builtin size m1;
    zero = Uint32 0;
    is_empty = builtin eq m1_size zero;
    match is_empty with
    | True =>
    | False => fail tname
    end;
    (* Insert one key for next test. *)
    key1a = "key1a";
    val = "420";
    m1 = builtin put m1 key1a val;
    f_m1 := m1
  end
  
  (* test_query_delete_to_empty *)
  transition t10 ()
    tname = "t10";
    (* We now expect the storage to contain: *)
    (* f_m1[key1a] : 420 *)
    key1a = "key1a";
    m1 <- f_m1;
    c1 = builtin get m1 key1a;
    match c1 with
    | Some c =>
      v = "420";
      eq = builtin eq c v;
      match eq with
      | True =>
      | False =>
        m = "Incorrect value for key1a";
        fail_msg tname m
      end
    | None =>
      m = "key1a not found";
      fail_msg tname m
    end;
    (* Delete key in a map to make it empty and then query the map *)
    delete f_m1[key1a]
  end
  
  (* test_query_delete_to_empty *)
  transition t11 ()
    tname = "t11";
    (* f_m1 should be empty. *)
    m1 <- f_m1;
    m1_size = builtin size m1;
    zero = Uint32 0;
    is_empty = builtin eq m1_size zero;
    match is_empty with
    | True =>
    | False => fail tname
    end;
    (* Insert an empty map into f_m2 for next test. *)
    e2 = Emp (String) (Map String String);
    f_m2 := e2
  end
  
  (* test_query_empty_map_2 *)
  transition t12 ()
    tname = "t12";
    (* f_m2 should be empty. *)
    m2 <- f_m2;
    m2_size = builtin size m2;
    zero = Uint32 0;
    is_empty = builtin eq m2_size zero;
    match is_empty with
    | True =>
    | False => fail tname
    end;
    (* Insert f_m2[key1a] = Emp *)
    e1 = Emp (String) (String);
    key1a = "key1a";
    f_m2[key1a] := e1
  end
  
  (* test_query_empty_map_2 *)
  transition t13 ()
    tname = "t13";
    (* f_m2[key1a] must be empty *)
    key1a = "key1a";
    mo <- f_m2[key1a];
    match mo with
    | Some m =>
      m_size = builtin size m;
      zero = Uint32 0;
      is_empty = builtin eq m_size zero;
      match is_empty with
      | True =>
      | False =>
        msg = "Submap should have been empty";
        fail_msg tname msg
      end
    | None => fail tname
    end;
    (* Insert entries for next test *)
    m3 = Emp (String) (String);
    m2 =
      let key2a = "key2a" in
      let e = Emp (String) (Map String String) in
      builtin put e key2a m3;
    m3 =
      let e = Emp (String) (Map String (Map String String)) in
      builtin put e key1a m2;
    f_m3 := m3
  end
  
  (* test_query_empty_map_3 *)
  transition t14 ()
    tname = "t14";
    (* f_m3 should be singleton *)
    m3 <- f_m3;
    m3_size = builtin size m3;
    one = Uint32 1;
    is_empty = builtin eq m3_size one;
    match is_empty with
    | True =>
    | False => fail tname
    end;
    (* Now insert something into f_m, whose name is a proper prefix of f_m3 *)
    e = Emp (String) (Map String String);
    f_m := e
  end
  
  (* test_query_empty_map_3 *)
  transition t15 ()
    tname = "t15";
    (* f_m3 should be [key1a][key2a] = [] *)
    m3 <- f_m3;
    m3_size = builtin size m3;
    one = Uint32 1;
    is_one = builtin eq m3_size one;
    match is_one with
    | True =>
    | False =>
      err = "Incorrect size of f_m3";
      fail_msg tname err
    end;
    key1a = "key1a";
    key2a = "key2a";
    m2o = builtin get m3 key1a;
    match m2o with
    | Some m2 =>
      m2_size = builtin size m2;
      is_one_1 = builtin eq m2_size one;
      match is_one_1 with
      | True =>
        m1o = builtin get m2 key2a;
        match m1o with
        | Some m1 =>
          m1_size = builtin size m1;
          zero = Uint32 0;
          is_empty = builtin eq m1_size zero;
          match is_empty with
          | True =>
          | False =>
            err = "Incorrect size of f_m3[key1a][key2a]";
            fail_msg tname err
          end
        | None =>
          err = "Unexpected empty m1";
          fail_msg tname err
        end
      | False =>
        err = "Incorrect size of f_m3[key1]";
        fail_msg tname err
      end
    | None =>
      err = "Unexpected empty m2";
      fail_msg tname err
    end;
    (* For the next test, insert a non-empty nested map into f_m2 *)
    key1b = "key1b";
    key2b = "key2b";
    key1c = "key1c";
    key2c = "key2c";
    key1d = "key1d";
    key2d = "key2d";
    v1 = "420";
    v2 = "421";
    v3 = "422";
    v4 = "423";
    m2 = Emp (String) (Map String String);
    m1 = Emp (String) (String);
    m2_full =
      let m21 = builtin put m1 key2a v1 in
      let m22 = builtin put m1 key2b v2 in
      let m23 = builtin put m1 key2c v3 in
      let m24 = builtin put m1 key2d v4 in
      let m11 = builtin put m2 key1a m21 in
      let m12 = builtin put m11 key1b m22 in
      let m13 = builtin put m12 key1c m23 in
      let m14 = builtin put m13 key1d m24 in
      m14;
    f_m2 := m2_full
  end
  
  (* test_query_update_fetch_nested *)
  transition t16 ()
    tname = "t16";
    (* Compare the entries *)
    (* f_m2[key1a][key1a] = "420" *)
    (* f_m2[key1b][key1b] = "421" *)
    (* f_m2[key1c][key1c] = "422" *)
    (* f_m2[key1d][key1d] = "423" *)
    key1a = "key1a";
    key2a = "key2a";
    key1b = "key1b";
    key2b = "key2b";
    key1c = "key1c";
    key2c = "key2c";
    key1d = "key1d";
    key2d = "key2d";
    t1 <- f_m2[key1a][key2a];
    t2 <- f_m2[key1b][key2b];
    t3 <- f_m2[key1c][key2c];
    t4 <- f_m2[key1d][key2d];
    v1 = "420";
    v2 = "421";
    v3 = "422";
    v4 = "423";
    b1 =
      match t1 with
      | Some t1v => builtin eq t1v v1
      | None => False
      end;
    b2 =
      match t2 with
      | Some t2v => builtin eq t2v v2
      | None => False
      end;
    b3 =
      match t3 with
      | Some t3v => builtin eq t3v v3
      | None => False
      end;
    b4 =
      match t4 with
      | Some t4v => builtin eq t4v v4
      | None => False
      end;
    b = let a1 = andb b1 b2 in let a2 = andb b3 b4 in andb a1 a2;
    match b with
    | True =>
    | False => fail tname
    end;
    (* Insert an empty string key. *)
    m1 =
      let k = "" in
      let v = "420" in
      let e = Emp (String) (String) in
      builtin put e k v;
    f_m1 := m1
  end
  
  (* test_query_empty_key *)
  transition t17 ()
    tname = "t17";
    (* We now expect the storage to contain: *)
    (* f_m1[""] : 420 *)
    key = "";
    found <- exists f_m1[key];
    match found with
    | True =>
    | False => fail tname
    end;
    (* delete key "" for next test *)
    delete f_m1[key]
  end
  
  (* test_query_empty_key *)
  transition t18 ()
    tname = "t18";
    (* We now expect the storage to NOT contain: *)
    (* f_m1[""] *)
    key = "";
    found <- exists f_m1[key];
    match found with
    | True => fail tname
    | False =>
    end
  end
  
  (* Set field f_s1 and fail. *)
  transition f1 ()
    tname = "f1";
    s = "422";
    f_s1 := s;
    expected_fail tname
  end
  
  (* Check no change to f_s1. *)
  procedure p1 (tname : String)
    s <- f_s1;
    f_s1_original = "421";
    not_changed = builtin eq f_s1_original s;
    match not_changed with
    | False =>
      msg = "f_s1 changed in a failing transition f1";
      fail_msg tname msg
    | True =>
    end
  end
  
  (* Ensure no change to f_s1 after f1 *)
  transition t19 ()
    tname = "t19";
    p1 tname
  end
  
  (* A call back transition that fails. *)
  transition callback_expected_fail (tname : String)
    expected_fail tname
  end
  
  (* Set field f_s1 and fail via chaincall to callback_expected_fail. *)
  transition f2 ()
    tname = "f2";
    s = "422";
    f_s1 := s;
    m =
      {
        _tag : "callback_expected_fail";
        _recipient : _this_address;
        _amount : Uint128 0;
        tname : tname
      };
    ms = one_msg m;
    send ms
  end
  
  (* Ensure no change to f_s1 after f2. *)
  transition t20 ()
    tname = "t20";
    p1 tname
  end
  
  (* Set f_m1["foo1"] and fail. *)
  transition f3 ()
    tname = "f3";
    key = "foo1";
    val = "bar1";
    f_m1[key] := val;
    expected_fail tname
  end
  
  (* Ensure f_m1["foo1"] doesn't exist. *)
  procedure p2 (tname : String)
    key = "foo1";
    found <- exists f_m1[key];
    match found with
    | True => fail tname
    | False =>
    end
  end
  
  (* Ensure f3 was reverted. *)
  transition t21 ()
    tname = "t21";
    p2 tname
  end
  
  (* Set f_m1["foo1"] and fail via chaincall to callback_expected_fail. *)
  transition f4 ()
    tname = "f4";
    key = "foo1";
    val = "bar1";
    f_m1[key] := val;
    m =
      {
        _tag : "callback_expected_fail";
        _recipient : _this_address;
        _amount : Uint128 0;
        tname : tname
      };
    ms = one_msg m;
    send ms
  end
  
  (* Ensure f4 was reverted. *)
  transition t22 ()
    tname = "t22";
    p2 tname
  end
  
  (* f_m2["key1a"]["key2a"] has a key, let's delete it and fail. *)
  transition f5 ()
    tname = "f5";
    key1 = "key1a";
    key2 = "key2a";
    delete f_m2[key1][key2];
    expected_fail tname
  end
  
  (* Ensure f_m2["key1a"]["key2a"] exists. *)
  procedure p3 (tname : String)
    key1 = "key1a";
    key2 = "key2a";
    valexp = "420";
    val <- f_m2[key1][key2];
    match val with
    | Some val =>
      eq = builtin eq val valexp;
      match eq with
      | True =>
      | False => fail tname
      end
    | None => fail tname
    end
  end
  
  (* Ensure f5 was reverted. *)
  transition t23 ()
    tname = "t23";
    p3 tname
  end
  
  (* f_m2["key1a"]["key2a"] has a key, let's delete it and fail via chaincall. *)
  transition f6 ()
    tname = "f6";
    key1 = "key1a";
    key2 = "key2a";
    delete f_m2[key1][key2];
    m =
      {
        _tag : "callback_expected_fail";
        _recipient : _this_address;
        _amount : Uint128 0;
        tname : tname
      };
    ms = one_msg m;
    send ms
  end
  
  (* Ensure f6 was reverted. *)
  transition t24 ()
    tname = "t24";
    p3 tname
  end
  
