  $ scilla-fmt address_list_traversal.scilla
  scilla_version 0
  
  import ListUtils
  
  library AddressListTraversalLib
  
  let f :
    (ByStr20 with contract end) ->
    (ByStr20 with contract end) ->
    (ByStr20 with contract end) ->
    List (ByStr20 with contract end) =
    fun (x : ByStr20 with contract end) =>
      fun (y : ByStr20 with contract end) =>
        fun (z : ByStr20 with contract end) =>
          let n = Nil {(ByStr20 with contract end)} in
          let c1 = Cons {(ByStr20 with contract end)} x n in
          let c2 = Cons {(ByStr20 with contract end)} y c1 in
          Cons {(ByStr20 with contract end)} z c2
  
  
  contract AddressListTraversal (cparam : ByStr20 with end)
  
  
  field res_list : List (ByStr20 with end) = Nil {(ByStr20 with end)}
  
  field res_mem : Bool = False
  
  transition Test1
    (
      param1 : ByStr20 with contract field f : Uint32 end,
      param2 : ByStr20 with contract field g : Uint128 end,
      param3 : ByStr20 with contract end
    )
    l = f param1 param2 param3;
    fold = @list_foldl (ByStr20 with end) (List (ByStr20 with end));
    iter_f =
      fun (acc : List (ByStr20 with end)) =>
        fun (x : ByStr20 with end) =>
          Cons {(ByStr20 with end)} x acc;
    init = Nil {(ByStr20 with end)};
    res = fold iter_f init l;
    res_list := res
  end
  
  transition Test2
    (
      param1 : ByStr20 with contract field f : Uint32 end,
      param2 : ByStr20 with contract field g : Uint128 end,
      param3 : ByStr20 with contract end
    )
    l = f param1 param2 param3;
    mem = @list_mem (ByStr20 with end);
    eq_x =
      fun (x : ByStr20 with end) =>
        fun (y : ByStr20 with end) =>
          builtin eq x y;
    res = mem eq_x _sender l;
    res_mem := res
  end
  
