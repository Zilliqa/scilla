  $ scilla-fmt --json --deannot --human-readable timestamp.scilla
  {
    "smver": 0,
    "libs": null,
    "elibs": [],
    "contr": {
      "cname": [ "Ident", [ "SimpleLocal", "HelloWorld" ], null ],
      "cparams": [],
      "cconstraint": [
        [
          "Literal",
          { "name": [ "SimpleLocal", "True" ], "types": [], "lits": [] }
        ],
        null
      ],
      "cfields": [],
      "ccomps": [
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "EventTimestamp" ], null ],
          "comp_params": [],
          "comp_body": [
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "bnum" ], null ],
                [ [ "Literal", "100" ], null ]
              ],
              null
            ],
            [
              [
                "ReadFromBC",
                [ "Ident", [ "SimpleLocal", "ts" ], null ],
                [ "Timestamp", [ "Ident", [ "SimpleLocal", "bnum" ], null ] ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "e" ], null ],
                [
                  [
                    "Message",
                    [
                      [ "_eventname", [ "MLit", "TS" ] ],
                      [
                        "timestamp",
                        [ "MVar", [ "Ident", [ "SimpleLocal", "ts" ], null ] ]
                      ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [ "CreateEvnt", [ "Ident", [ "SimpleLocal", "e" ], null ] ], null
            ]
          ]
        }
      ]
    }
  }
