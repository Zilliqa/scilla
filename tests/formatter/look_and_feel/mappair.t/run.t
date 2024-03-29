  $ scilla-fmt mappair.scilla
  scilla_version 0
  
  import
    ListUtils
    PairUtils
    NatUtils
  
  library Test
  
  let no_msg = Nil {(Message)}
  
  let one_msg =
    fun (msg : Message) =>
      let nil_msg = Nil {(Message)} in
      Cons {(Message)} msg nil_msg
  
  let flip_obool =
    fun (ob : Option Bool) =>
      let t = True in
      let f = False in
      match ob with
      | None => Some {(Bool)} t
      | Some b =>
        match b with
        | True => Some {(Bool)} f
        | False => Some {(Bool)} t
        end
      end
  
  let fst_f = @fst (List Int64) (Option Bool)
  
  let snd_f = @snd (List Int64) (Option Bool)
  
  
  contract Test (owner : ByStr20)
  
  
  field gmap : Map ByStr20 (Pair Int32 Int32) = Emp (ByStr20) (Pair Int32 Int32)
  
  field gpair : Pair (List Int64) (Option Bool) =
    let el = Nil {(Int64)} in
    let n = None {(Bool)} in
    Pair {(List Int64) (Option Bool)} el n
  
  field llist : List (List Int64) = Nil {(List Int64)}
  
  field plist : List (Option Int32) = Nil {(Option Int32)}
  
  field gnat : Nat = Zero
  
  transition testMapPair ()
    is_owner = builtin eq owner _sender;
    match is_owner with
    | False =>
      one = Int32 1;
      two = Int32 2;
      p = Pair {(Int32) (Int32)} one two;
      gmap[_sender] := p;
      send no_msg
    | True =>
      three = Int32 3;
      four = Int32 4;
      p = Pair {(Int32) (Int32)} three four;
      gmap[_sender] := p;
      send no_msg
    end
  end
  
  transition addNumToList (num : Int64)
    p <- gpair;
    (* get first of pair = List (Int64) *)
    l1 = fst_f p;
    (* get second of pair = Option (Bool) *)
    b = snd_f p;
    (* have fun: flip the boolean *)
    bflip = flip_obool b;
    (* append num to the list *)
    l2 = Cons {(Int64)} num l1;
    (* Form updated pair *)
    new_p = Pair {(List Int64) (Option Bool)} l2 bflip;
    gpair := new_p;
    len = let my_list_length = @list_length (Int64) in my_list_length l2;
    msg =
      { _tag : ""; _recipient : _sender; _amount : Uint128 0; listLength : len };
    msgs = one_msg msg;
    send msgs
  end
  
  transition incNat ()
    n <- gnat;
    m = Succ n;
    gnat := m;
    i = nat_to_int m;
    msg = { _tag : ""; _recipient : _sender; _amount : Uint128 0; nat : i };
    msgs = one_msg msg;
    send msgs
  end
  
  transition lflatten ()
    n <- llist;
    lfl = @list_flatten (Int64);
    m = lfl n;
    len = let my_list_length = @list_length (Int64) in my_list_length m;
    msg =
      { _tag : ""; _recipient : _sender; _amount : Uint128 0; listLength : len };
    msgs = one_msg msg;
    send msgs
  end
  
  transition optlist ()
    n <- plist;
    len = let my_list_length = @list_length (Option Int32) in my_list_length n;
    msg =
      { _tag : ""; _recipient : _sender; _amount : Uint128 0; listLength : len };
    msgs = one_msg msg;
    send msgs
  end
  
  transition redef_warn (b : Bool)
    x = Uint32 0;
    match b with
    | True => x = Uint32 1
    | False =>
    end;
    e = { _eventname : "Foo"; x : x };
    event e
  end
  
  transition print_sender_origin ()
    e = { _eventname : "Source"; _sender : _sender; _origin : _origin };
    event e
  end
  
