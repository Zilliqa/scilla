  $ scilla-fmt map_as_cparam.scilla
  scilla_version 0
  
  contract T
    (
      x :
        Map
          (ByStr20 with contract field f : Uint128 end)
          (ByStr20 with contract field g : Bool end)
    )
  
  
  field f :
    Map
      (ByStr20 with contract field f : Uint128 end)
      (ByStr20 with contract field g : Bool end) = x
  
  
  
