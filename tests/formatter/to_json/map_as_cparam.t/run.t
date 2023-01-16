  $ scilla-fmt --json --deannot --human-readable map_as_cparam.scilla
  {
    "smver": 0,
    "libs": null,
    "elibs": [],
    "contr": {
      "cname": [ "Ident", [ "SimpleLocal", "T" ], null ],
      "cparams": [
        [
          [ "Ident", [ "SimpleLocal", "x" ], null ],
          [ "MapType", [ "Address", "Address" ], [ "Address", "Address" ] ]
        ]
      ],
      "cconstraint": [
        [
          "Literal",
          { "name": [ "SimpleLocal", "True" ], "types": [], "lits": [] }
        ],
        null
      ],
      "cfields": [
        [
          [ "Ident", [ "SimpleLocal", "f" ], null ],
          [ "MapType", [ "Address", "Address" ], [ "Address", "Address" ] ],
          [ [ "Var", [ "Ident", [ "SimpleLocal", "x" ], null ] ], null ]
        ]
      ],
      "ccomps": []
    }
  }
