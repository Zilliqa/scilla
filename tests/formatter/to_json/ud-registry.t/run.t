  $ scilla-fmt --json --deannot --human-readable ud-registry.scilla
  {
    "smver": 0,
    "libs": {
      "lname": [ "Ident", [ "SimpleLocal", "RegistryLib" ], null ],
      "lentries": [
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "zeroUint64" ], null ],
          null,
          [ [ "Literal", "0" ], null ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "zeroByStr20" ], null ],
          null,
          [
            [
              "Literal",
              "\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "zeroByStr32" ], null ],
          null,
          [
            [
              "Literal",
              "\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000"
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "nilByStr20" ], null ],
          null,
          [
            [
              "Constr",
              [ "Ident", [ "SimpleLocal", "Nil" ], null ],
              [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ],
              []
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "nilMessage" ], null ],
          null,
          [
            [
              "Constr",
              [ "Ident", [ "SimpleLocal", "Nil" ], null ],
              [ [ "PrimType", [ "Msg_typ" ] ] ],
              []
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "oneMsg" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "msg" ], null ],
              [ "PrimType", [ "Msg_typ" ] ],
              [
                [
                  "Constr",
                  [ "Ident", [ "SimpleLocal", "Cons" ], null ],
                  [ [ "PrimType", [ "Msg_typ" ] ] ],
                  [
                    [ "Ident", [ "SimpleLocal", "msg" ], null ],
                    [ "Ident", [ "SimpleLocal", "nilMessage" ], null ]
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "eqByStr20" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "bs1" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "bs2" ], null ],
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [
                    [
                      "Builtin",
                      [ [ "Builtin_eq" ], null ],
                      [],
                      [
                        [ "Ident", [ "SimpleLocal", "bs1" ], null ],
                        [ "Ident", [ "SimpleLocal", "bs2" ], null ]
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "listByStr20Contains" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "list" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "List" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
              ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "bs" ], null ],
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [
                    [
                      "Let",
                      [ "Ident", [ "SimpleLocal", "listMemByStr20" ], null ],
                      null,
                      [
                        [
                          "TApp",
                          [ "Ident", [ "SimpleLocal", "list_mem" ], null ],
                          [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
                        ],
                        null
                      ],
                      [
                        [
                          "App",
                          [
                            "Ident", [ "SimpleLocal", "listMemByStr20" ], null
                          ],
                          [
                            [ "Ident", [ "SimpleLocal", "eqByStr20" ], null ],
                            [ "Ident", [ "SimpleLocal", "bs" ], null ],
                            [ "Ident", [ "SimpleLocal", "list" ], null ]
                          ]
                        ],
                        null
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "listByStr20Excludes" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "list" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "List" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
              ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "bs" ], null ],
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [
                    [
                      "Let",
                      [ "Ident", [ "SimpleLocal", "b" ], null ],
                      null,
                      [
                        [
                          "App",
                          [
                            "Ident",
                            [ "SimpleLocal", "listByStr20Contains" ],
                            null
                          ],
                          [
                            [ "Ident", [ "SimpleLocal", "list" ], null ],
                            [ "Ident", [ "SimpleLocal", "bs" ], null ]
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "App",
                          [ "Ident", [ "SimpleLocal", "negb" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "b" ], null ] ]
                        ],
                        null
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "listByStr20FilterOut" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "list" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "List" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
              ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "bs" ], null ],
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [
                    [
                      "Let",
                      [ "Ident", [ "SimpleLocal", "listByStr20Filter" ], null ],
                      null,
                      [
                        [
                          "TApp",
                          [ "Ident", [ "SimpleLocal", "list_filter" ], null ],
                          [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
                        ],
                        null
                      ],
                      [
                        [
                          "Let",
                          [ "Ident", [ "SimpleLocal", "fn" ], null ],
                          null,
                          [
                            [
                              "Fun",
                              [ "Ident", [ "SimpleLocal", "v" ], null ],
                              [ "PrimType", [ "Bystrx_typ", 20 ] ],
                              [
                                [
                                  "Let",
                                  [ "Ident", [ "SimpleLocal", "b" ], null ],
                                  null,
                                  [
                                    [
                                      "Builtin",
                                      [ [ "Builtin_eq" ], null ],
                                      [],
                                      [
                                        [
                                          "Ident", [ "SimpleLocal", "v" ], null
                                        ],
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "bs" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    null
                                  ],
                                  [
                                    [
                                      "App",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "negb" ],
                                        null
                                      ],
                                      [
                                        [
                                          "Ident", [ "SimpleLocal", "b" ], null
                                        ]
                                      ]
                                    ],
                                    null
                                  ]
                                ],
                                null
                              ]
                            ],
                            null
                          ],
                          [
                            [
                              "App",
                              [
                                "Ident",
                                [ "SimpleLocal", "listByStr20Filter" ],
                                null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "fn" ], null ],
                                [ "Ident", [ "SimpleLocal", "list" ], null ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "xandb" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "b1" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "Bool" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                []
              ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "b2" ], null ],
                  [
                    "ADT",
                    [
                      "Ident",
                      [ "SimpleLocal", "Bool" ],
                      { "fname": "", "lnum": 0, "cnum": 0 }
                    ],
                    []
                  ],
                  [
                    [
                      "MatchExpr",
                      [ "Ident", [ "SimpleLocal", "b1" ], null ],
                      [
                        [
                          [
                            "Constructor",
                            [ "Ident", [ "SimpleLocal", "True" ], null ],
                            []
                          ],
                          [
                            [
                              "MatchExpr",
                              [ "Ident", [ "SimpleLocal", "b2" ], null ],
                              [
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "True" ], null
                                    ],
                                    []
                                  ],
                                  [
                                    [
                                      "Constr",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "True" ],
                                        null
                                      ],
                                      [],
                                      []
                                    ],
                                    null
                                  ]
                                ],
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "False" ], null
                                    ],
                                    []
                                  ],
                                  [
                                    [
                                      "Constr",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "False" ],
                                        null
                                      ],
                                      [],
                                      []
                                    ],
                                    null
                                  ]
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        [
                          [
                            "Constructor",
                            [ "Ident", [ "SimpleLocal", "False" ], null ],
                            []
                          ],
                          [
                            [
                              "MatchExpr",
                              [ "Ident", [ "SimpleLocal", "b2" ], null ],
                              [
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "True" ], null
                                    ],
                                    []
                                  ],
                                  [
                                    [
                                      "Constr",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "False" ],
                                        null
                                      ],
                                      [],
                                      []
                                    ],
                                    null
                                  ]
                                ],
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "False" ], null
                                    ],
                                    []
                                  ],
                                  [
                                    [
                                      "Constr",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "True" ],
                                        null
                                      ],
                                      [],
                                      []
                                    ],
                                    null
                                  ]
                                ]
                              ]
                            ],
                            null
                          ]
                        ]
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "eAdminSet" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "address" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "isApproved" ], null ],
                  [
                    "ADT",
                    [
                      "Ident",
                      [ "SimpleLocal", "Bool" ],
                      { "fname": "", "lnum": 0, "cnum": 0 }
                    ],
                    []
                  ],
                  [
                    [
                      "Message",
                      [
                        [ "_eventname", [ "MLit", "AdminSet" ] ],
                        [
                          "address",
                          [
                            "MVar",
                            [ "Ident", [ "SimpleLocal", "address" ], null ]
                          ]
                        ],
                        [
                          "isApproved",
                          [
                            "MVar",
                            [ "Ident", [ "SimpleLocal", "isApproved" ], null ]
                          ]
                        ]
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "eApprovedFor" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "user" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "operator" ], null ],
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [
                    [
                      "Fun",
                      [ "Ident", [ "SimpleLocal", "isApproved" ], null ],
                      [
                        "ADT",
                        [
                          "Ident",
                          [ "SimpleLocal", "Bool" ],
                          { "fname": "", "lnum": 0, "cnum": 0 }
                        ],
                        []
                      ],
                      [
                        [
                          "Message",
                          [
                            [ "_eventname", [ "MLit", "ApprovedFor" ] ],
                            [
                              "user",
                              [
                                "MVar",
                                [ "Ident", [ "SimpleLocal", "user" ], null ]
                              ]
                            ],
                            [
                              "operator",
                              [
                                "MVar",
                                [
                                  "Ident", [ "SimpleLocal", "operator" ], null
                                ]
                              ]
                            ],
                            [
                              "isApproved",
                              [
                                "MVar",
                                [
                                  "Ident",
                                  [ "SimpleLocal", "isApproved" ],
                                  null
                                ]
                              ]
                            ]
                          ]
                        ],
                        null
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "eApproved" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "address" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ],
              [
                [
                  "Message",
                  [
                    [ "_eventname", [ "MLit", "Approved" ] ],
                    [
                      "address",
                      [
                        "MVar", [ "Ident", [ "SimpleLocal", "address" ], null ]
                      ]
                    ]
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "eNewRegistrar" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "address" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ],
              [
                [
                  "Message",
                  [
                    [ "_eventname", [ "MLit", "NewRegistrar" ] ],
                    [
                      "address",
                      [
                        "MVar", [ "Ident", [ "SimpleLocal", "address" ], null ]
                      ]
                    ]
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "eNewDomain" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "parent" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "label" ], null ],
                  [ "PrimType", [ "String_typ" ] ],
                  [
                    [
                      "Message",
                      [
                        [ "_eventname", [ "MLit", "NewDomain" ] ],
                        [
                          "parent",
                          [
                            "MVar",
                            [ "Ident", [ "SimpleLocal", "parent" ], null ]
                          ]
                        ],
                        [
                          "label",
                          [
                            "MVar",
                            [ "Ident", [ "SimpleLocal", "label" ], null ]
                          ]
                        ]
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "eConfigured" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "node" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "owner" ], null ],
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [
                    [
                      "Fun",
                      [ "Ident", [ "SimpleLocal", "resolver" ], null ],
                      [ "PrimType", [ "Bystrx_typ", 20 ] ],
                      [
                        [
                          "Message",
                          [
                            [ "_eventname", [ "MLit", "Configured" ] ],
                            [
                              "node",
                              [
                                "MVar",
                                [ "Ident", [ "SimpleLocal", "node" ], null ]
                              ]
                            ],
                            [
                              "owner",
                              [
                                "MVar",
                                [ "Ident", [ "SimpleLocal", "owner" ], null ]
                              ]
                            ],
                            [
                              "resolver",
                              [
                                "MVar",
                                [
                                  "Ident", [ "SimpleLocal", "resolver" ], null
                                ]
                              ]
                            ]
                          ]
                        ],
                        null
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "eError" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "msg" ], null ],
              [ "PrimType", [ "String_typ" ] ],
              [
                [
                  "Message",
                  [
                    [ "_eventname", [ "MLit", "Error" ] ],
                    [
                      "msg",
                      [ "MVar", [ "Ident", [ "SimpleLocal", "msg" ], null ] ]
                    ]
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibTyp",
          [ "Ident", [ "SimpleLocal", "Record" ], null ],
          [
            {
              "cname": [ "Ident", [ "SimpleLocal", "Record" ], null ],
              "c_arg_types": [
                [ "PrimType", [ "Bystrx_typ", 20 ] ],
                [ "PrimType", [ "Bystrx_typ", 20 ] ]
              ]
            }
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "recordMemberOwner" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "Option" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                [
                  [
                    "ADT",
                    [
                      "Ident",
                      [ "SimpleLocal", "Record" ],
                      { "fname": "", "lnum": 0, "cnum": 0 }
                    ],
                    []
                  ]
                ]
              ],
              [
                [
                  "MatchExpr",
                  [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                  [
                    [
                      [
                        "Constructor",
                        [ "Ident", [ "SimpleLocal", "None" ], null ],
                        []
                      ],
                      [
                        [
                          "Var",
                          [ "Ident", [ "SimpleLocal", "zeroByStr20" ], null ]
                        ],
                        null
                      ]
                    ],
                    [
                      [
                        "Constructor",
                        [ "Ident", [ "SimpleLocal", "Some" ], null ],
                        [
                          [
                            "Binder",
                            [ "Ident", [ "SimpleLocal", "record" ], null ]
                          ]
                        ]
                      ],
                      [
                        [
                          "MatchExpr",
                          [ "Ident", [ "SimpleLocal", "record" ], null ],
                          [
                            [
                              [
                                "Constructor",
                                [ "Ident", [ "SimpleLocal", "Record" ], null ],
                                [
                                  [
                                    "Binder",
                                    [
                                      "Ident", [ "SimpleLocal", "owner" ], null
                                    ]
                                  ],
                                  [
                                    "Binder",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "resolver" ],
                                      null
                                    ]
                                  ]
                                ]
                              ],
                              [
                                [
                                  "Var",
                                  [ "Ident", [ "SimpleLocal", "owner" ], null ]
                                ],
                                null
                              ]
                            ]
                          ]
                        ],
                        null
                      ]
                    ]
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "parentLabelToNode" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "parent" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "label" ], null ],
                  [ "PrimType", [ "String_typ" ] ],
                  [
                    [
                      "Let",
                      [ "Ident", [ "SimpleLocal", "labelHash" ], null ],
                      null,
                      [
                        [
                          "Builtin",
                          [ [ "Builtin_sha256hash" ], null ],
                          [],
                          [ [ "Ident", [ "SimpleLocal", "label" ], null ] ]
                        ],
                        null
                      ],
                      [
                        [
                          "Let",
                          [ "Ident", [ "SimpleLocal", "nodeInput" ], null ],
                          null,
                          [
                            [
                              "Builtin",
                              [ [ "Builtin_concat" ], null ],
                              [],
                              [
                                [ "Ident", [ "SimpleLocal", "parent" ], null ],
                                [
                                  "Ident", [ "SimpleLocal", "labelHash" ], null
                                ]
                              ]
                            ],
                            null
                          ],
                          [
                            [
                              "Builtin",
                              [ [ "Builtin_sha256hash" ], null ],
                              [],
                              [
                                [
                                  "Ident", [ "SimpleLocal", "nodeInput" ], null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "getIsOAO" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "sender" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ],
              [
                [
                  "Fun",
                  [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [
                    [
                      "Fun",
                      [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                      [
                        "ADT",
                        [
                          "Ident",
                          [ "SimpleLocal", "Option" ],
                          { "fname": "", "lnum": 0, "cnum": 0 }
                        ],
                        [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
                      ],
                      [
                        [
                          "Fun",
                          [
                            "Ident", [ "SimpleLocal", "maybeOperators" ], null
                          ],
                          [
                            "ADT",
                            [
                              "Ident",
                              [ "SimpleLocal", "Option" ],
                              { "fname": "", "lnum": 0, "cnum": 0 }
                            ],
                            [
                              [
                                "ADT",
                                [
                                  "Ident",
                                  [ "SimpleLocal", "List" ],
                                  { "fname": "", "lnum": 0, "cnum": 0 }
                                ],
                                [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
                              ]
                            ]
                          ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "isOwner" ], null ],
                              null,
                              [
                                [
                                  "Builtin",
                                  [ [ "Builtin_eq" ], null ],
                                  [],
                                  [
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "sender" ],
                                      null
                                    ],
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "recordOwner" ],
                                      null
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "Let",
                                  [
                                    "Ident",
                                    [ "SimpleLocal", "isApproved" ],
                                    null
                                  ],
                                  null,
                                  [
                                    [
                                      "MatchExpr",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "maybeApproved" ],
                                        null
                                      ],
                                      [
                                        [
                                          [
                                            "Constructor",
                                            [
                                              "Ident",
                                              [ "SimpleLocal", "None" ],
                                              null
                                            ],
                                            []
                                          ],
                                          [
                                            [
                                              "Constr",
                                              [
                                                "Ident",
                                                [ "SimpleLocal", "False" ],
                                                null
                                              ],
                                              [],
                                              []
                                            ],
                                            null
                                          ]
                                        ],
                                        [
                                          [
                                            "Constructor",
                                            [
                                              "Ident",
                                              [ "SimpleLocal", "Some" ],
                                              null
                                            ],
                                            [
                                              [
                                                "Binder",
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "approved" ],
                                                  null
                                                ]
                                              ]
                                            ]
                                          ],
                                          [
                                            [
                                              "Builtin",
                                              [ [ "Builtin_eq" ], null ],
                                              [],
                                              [
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "sender" ],
                                                  null
                                                ],
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "approved" ],
                                                  null
                                                ]
                                              ]
                                            ],
                                            null
                                          ]
                                        ]
                                      ]
                                    ],
                                    null
                                  ],
                                  [
                                    [
                                      "Let",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "isOperator" ],
                                        null
                                      ],
                                      null,
                                      [
                                        [
                                          "MatchExpr",
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "maybeOperators" ],
                                            null
                                          ],
                                          [
                                            [
                                              [
                                                "Constructor",
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "None" ],
                                                  null
                                                ],
                                                []
                                              ],
                                              [
                                                [
                                                  "Constr",
                                                  [
                                                    "Ident",
                                                    [ "SimpleLocal", "False" ],
                                                    null
                                                  ],
                                                  [],
                                                  []
                                                ],
                                                null
                                              ]
                                            ],
                                            [
                                              [
                                                "Constructor",
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "Some" ],
                                                  null
                                                ],
                                                [
                                                  [
                                                    "Binder",
                                                    [
                                                      "Ident",
                                                      [
                                                        "SimpleLocal",
                                                        "operators"
                                                      ],
                                                      null
                                                    ]
                                                  ]
                                                ]
                                              ],
                                              [
                                                [
                                                  "App",
                                                  [
                                                    "Ident",
                                                    [
                                                      "SimpleLocal",
                                                      "listByStr20Contains"
                                                    ],
                                                    null
                                                  ],
                                                  [
                                                    [
                                                      "Ident",
                                                      [
                                                        "SimpleLocal",
                                                        "operators"
                                                      ],
                                                      null
                                                    ],
                                                    [
                                                      "Ident",
                                                      [
                                                        "SimpleLocal", "sender"
                                                      ],
                                                      null
                                                    ]
                                                  ]
                                                ],
                                                null
                                              ]
                                            ]
                                          ]
                                        ],
                                        null
                                      ],
                                      [
                                        [
                                          "Let",
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "b1" ],
                                            null
                                          ],
                                          null,
                                          [
                                            [
                                              "App",
                                              [
                                                "Ident",
                                                [ "SimpleLocal", "orb" ],
                                                null
                                              ],
                                              [
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "isOwner" ],
                                                  null
                                                ],
                                                [
                                                  "Ident",
                                                  [
                                                    "SimpleLocal", "isApproved"
                                                  ],
                                                  null
                                                ]
                                              ]
                                            ],
                                            null
                                          ],
                                          [
                                            [
                                              "App",
                                              [
                                                "Ident",
                                                [ "SimpleLocal", "orb" ],
                                                null
                                              ],
                                              [
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "b1" ],
                                                  null
                                                ],
                                                [
                                                  "Ident",
                                                  [
                                                    "SimpleLocal", "isOperator"
                                                  ],
                                                  null
                                                ]
                                              ]
                                            ],
                                            null
                                          ]
                                        ],
                                        null
                                      ]
                                    ],
                                    null
                                  ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ]
      ]
    },
    "elibs": [
      [ [ "Ident", [ "SimpleLocal", "BoolUtils" ], null ], null ],
      [ [ "Ident", [ "SimpleLocal", "ListUtils" ], null ], null ]
    ],
    "contr": {
      "cname": [ "Ident", [ "SimpleLocal", "Registry" ], null ],
      "cparams": [
        [
          [ "Ident", [ "SimpleLocal", "initialOwner" ], null ],
          [ "PrimType", [ "Bystrx_typ", 20 ] ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "rootNode" ], null ],
          [ "PrimType", [ "Bystrx_typ", 32 ] ]
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
          [ "Ident", [ "SimpleLocal", "records" ], null ],
          [
            "MapType",
            [ "PrimType", [ "Bystrx_typ", 32 ] ],
            [
              "ADT",
              [
                "Ident",
                [ "SimpleLocal", "Record" ],
                { "fname": "", "lnum": 0, "cnum": 0 }
              ],
              []
            ]
          ],
          [
            [
              "Let",
              [ "Ident", [ "SimpleLocal", "empty" ], null ],
              null,
              [
                [
                  "Literal",
                  {
                    "mtype": [
                      [ "PrimType", [ "Bystrx_typ", 32 ] ],
                      [
                        "ADT",
                        [
                          "Ident",
                          [ "SimpleLocal", "Record" ],
                          { "fname": "", "lnum": 0, "cnum": 0 }
                        ],
                        []
                      ]
                    ],
                    "data": []
                  }
                ],
                null
              ],
              [
                [
                  "Let",
                  [ "Ident", [ "SimpleLocal", "rootRecord" ], null ],
                  null,
                  [
                    [
                      "Constr",
                      [ "Ident", [ "SimpleLocal", "Record" ], null ],
                      [],
                      [
                        [ "Ident", [ "SimpleLocal", "initialOwner" ], null ],
                        [ "Ident", [ "SimpleLocal", "zeroByStr20" ], null ]
                      ]
                    ],
                    null
                  ],
                  [
                    [
                      "Builtin",
                      [ [ "Builtin_put" ], null ],
                      [],
                      [
                        [ "Ident", [ "SimpleLocal", "empty" ], null ],
                        [ "Ident", [ "SimpleLocal", "rootNode" ], null ],
                        [ "Ident", [ "SimpleLocal", "rootRecord" ], null ]
                      ]
                    ],
                    null
                  ]
                ],
                null
              ]
            ],
            null
          ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "registrar" ], null ],
          [ "PrimType", [ "Bystrx_typ", 20 ] ],
          [
            [ "Var", [ "Ident", [ "SimpleLocal", "zeroByStr20" ], null ] ],
            null
          ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "approvals" ], null ],
          [
            "MapType",
            [ "PrimType", [ "Bystrx_typ", 32 ] ],
            [ "PrimType", [ "Bystrx_typ", 20 ] ]
          ],
          [
            [
              "Literal",
              {
                "mtype": [
                  [ "PrimType", [ "Bystrx_typ", 32 ] ],
                  [ "PrimType", [ "Bystrx_typ", 20 ] ]
                ],
                "data": []
              }
            ],
            null
          ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "operators" ], null ],
          [
            "MapType",
            [ "PrimType", [ "Bystrx_typ", 20 ] ],
            [
              "ADT",
              [
                "Ident",
                [ "SimpleLocal", "List" ],
                { "fname": "", "lnum": 0, "cnum": 0 }
              ],
              [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
            ]
          ],
          [
            [
              "Literal",
              {
                "mtype": [
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [
                    "ADT",
                    [
                      "Ident",
                      [ "SimpleLocal", "List" ],
                      { "fname": "", "lnum": 0, "cnum": 0 }
                    ],
                    [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
                  ]
                ],
                "data": []
              }
            ],
            null
          ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "admins" ], null ],
          [
            "ADT",
            [
              "Ident",
              [ "SimpleLocal", "List" ],
              { "fname": "", "lnum": 0, "cnum": 0 }
            ],
            [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ]
          ],
          [
            [
              "Constr",
              [ "Ident", [ "SimpleLocal", "Cons" ], null ],
              [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ],
              [
                [ "Ident", [ "SimpleLocal", "initialOwner" ], null ],
                [ "Ident", [ "SimpleLocal", "nilByStr20" ], null ]
              ]
            ],
            null
          ]
        ]
      ],
      "ccomps": [
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "setAdmin" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "address" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "isApproved" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "Bool" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                []
              ]
            ]
          ],
          "comp_body": [
            [
              [
                "Load",
                [ "Ident", [ "SimpleLocal", "currentAdmins" ], null ],
                [ "Ident", [ "SimpleLocal", "admins" ], null ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isSenderAdmin" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "listByStr20Contains" ], null ],
                    [
                      [ "Ident", [ "SimpleLocal", "currentAdmins" ], null ],
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isSenderAdmin" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "needsToChange" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "b" ], null ],
                              null,
                              [
                                [
                                  "App",
                                  [
                                    "Ident",
                                    [ "SimpleLocal", "listByStr20Excludes" ],
                                    null
                                  ],
                                  [
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "currentAdmins" ],
                                      null
                                    ],
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "address" ],
                                      null
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [ "Ident", [ "SimpleLocal", "xandb" ], null ],
                                  [
                                    [ "Ident", [ "SimpleLocal", "b" ], null ],
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "isApproved" ],
                                      null
                                    ]
                                  ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MatchStmt",
                          [ "Ident", [ "SimpleLocal", "needsToChange" ], null ],
                          [
                            [
                              [
                                "Constructor",
                                [ "Ident", [ "SimpleLocal", "True" ], null ],
                                []
                              ],
                              [
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "newAdmins" ],
                                      null
                                    ],
                                    [
                                      [
                                        "MatchExpr",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "isApproved" ],
                                          null
                                        ],
                                        [
                                          [
                                            [
                                              "Constructor",
                                              [
                                                "Ident",
                                                [ "SimpleLocal", "True" ],
                                                null
                                              ],
                                              []
                                            ],
                                            [
                                              [
                                                "Constr",
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "Cons" ],
                                                  null
                                                ],
                                                [
                                                  [
                                                    "PrimType",
                                                    [ "Bystrx_typ", 20 ]
                                                  ]
                                                ],
                                                [
                                                  [
                                                    "Ident",
                                                    [
                                                      "SimpleLocal", "address"
                                                    ],
                                                    null
                                                  ],
                                                  [
                                                    "Ident",
                                                    [
                                                      "SimpleLocal",
                                                      "currentAdmins"
                                                    ],
                                                    null
                                                  ]
                                                ]
                                              ],
                                              null
                                            ]
                                          ],
                                          [
                                            [
                                              "Constructor",
                                              [
                                                "Ident",
                                                [ "SimpleLocal", "False" ],
                                                null
                                              ],
                                              []
                                            ],
                                            [
                                              [
                                                "App",
                                                [
                                                  "Ident",
                                                  [
                                                    "SimpleLocal",
                                                    "listByStr20FilterOut"
                                                  ],
                                                  null
                                                ],
                                                [
                                                  [
                                                    "Ident",
                                                    [
                                                      "SimpleLocal",
                                                      "currentAdmins"
                                                    ],
                                                    null
                                                  ],
                                                  [
                                                    "Ident",
                                                    [
                                                      "SimpleLocal", "address"
                                                    ],
                                                    null
                                                  ]
                                                ]
                                              ],
                                              null
                                            ]
                                          ]
                                        ]
                                      ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Store",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "admins" ],
                                      null
                                    ],
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "newAdmins" ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [ "Ident", [ "SimpleLocal", "e" ], null ],
                                    [
                                      [
                                        "App",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "eAdminSet" ],
                                          null
                                        ],
                                        [
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "address" ],
                                            null
                                          ],
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "isApproved" ],
                                            null
                                          ]
                                        ]
                                      ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "CreateEvnt",
                                    [ "Ident", [ "SimpleLocal", "e" ], null ]
                                  ],
                                  null
                                ]
                              ]
                            ],
                            [ [ "Wildcard" ], [] ]
                          ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "False" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [ "Literal", "Sender not root node owner" ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "eError" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ]
                    ]
                  ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "approve" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "node" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "address" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "recordMemberOwner" ], null ],
                    [ [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ] ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isSenderNodeOwner" ], null ],
                [
                  [
                    "Builtin",
                    [ [ "Builtin_eq" ], null ],
                    [],
                    [
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                      [ "Ident", [ "SimpleLocal", "recordOwner" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isSenderNodeOwner" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "MapGet",
                          [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                          [ "Ident", [ "SimpleLocal", "approvals" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          true
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [
                            "Ident",
                            [ "SimpleLocal", "currentlyApproved" ],
                            null
                          ],
                          [
                            [
                              "MatchExpr",
                              [
                                "Ident",
                                [ "SimpleLocal", "maybeApproved" ],
                                null
                              ],
                              [
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "None" ], null
                                    ],
                                    []
                                  ],
                                  [
                                    [
                                      "Var",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "zeroByStr20" ],
                                        null
                                      ]
                                    ],
                                    null
                                  ]
                                ],
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "Some" ], null
                                    ],
                                    [
                                      [
                                        "Binder",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "approved" ],
                                          null
                                        ]
                                      ]
                                    ]
                                  ],
                                  [
                                    [
                                      "Var",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "approved" ],
                                        null
                                      ]
                                    ],
                                    null
                                  ]
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "needsToChange" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "b" ], null ],
                              null,
                              [
                                [
                                  "Builtin",
                                  [ [ "Builtin_eq" ], null ],
                                  [],
                                  [
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "currentlyApproved" ],
                                      null
                                    ],
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "address" ],
                                      null
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [ "Ident", [ "SimpleLocal", "negb" ], null ],
                                  [ [ "Ident", [ "SimpleLocal", "b" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MatchStmt",
                          [ "Ident", [ "SimpleLocal", "needsToChange" ], null ],
                          [
                            [
                              [
                                "Constructor",
                                [ "Ident", [ "SimpleLocal", "True" ], null ],
                                []
                              ],
                              [
                                [
                                  [
                                    "MapUpdate",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "approvals" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "node" ],
                                        null
                                      ]
                                    ],
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "address" ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [ "Ident", [ "SimpleLocal", "e" ], null ],
                                    [
                                      [
                                        "App",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "eApproved" ],
                                          null
                                        ],
                                        [
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "address" ],
                                            null
                                          ]
                                        ]
                                      ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "CreateEvnt",
                                    [ "Ident", [ "SimpleLocal", "e" ], null ]
                                  ],
                                  null
                                ]
                              ]
                            ],
                            [ [ "Wildcard" ], [] ]
                          ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "False" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [ [ "Literal", "Sender not node owner" ], null ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "eError" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ]
                    ]
                  ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "approveFor" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "address" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "isApproved" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "Bool" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                []
              ]
            ]
          ],
          "comp_body": [
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ],
                [ "Ident", [ "SimpleLocal", "operators" ], null ],
                [ [ "Ident", [ "SimpleLocal", "_sender" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "currentOperators" ], null ],
                [
                  [
                    "MatchExpr",
                    [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ],
                    [
                      [
                        [
                          "Constructor",
                          [ "Ident", [ "SimpleLocal", "None" ], null ],
                          []
                        ],
                        [
                          [
                            "Var",
                            [ "Ident", [ "SimpleLocal", "nilByStr20" ], null ]
                          ],
                          null
                        ]
                      ],
                      [
                        [
                          "Constructor",
                          [ "Ident", [ "SimpleLocal", "Some" ], null ],
                          [
                            [
                              "Binder",
                              [ "Ident", [ "SimpleLocal", "ops" ], null ]
                            ]
                          ]
                        ],
                        [
                          [
                            "Var", [ "Ident", [ "SimpleLocal", "ops" ], null ]
                          ],
                          null
                        ]
                      ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "needsToChange" ], null ],
                [
                  [
                    "Let",
                    [ "Ident", [ "SimpleLocal", "b" ], null ],
                    null,
                    [
                      [
                        "App",
                        [
                          "Ident",
                          [ "SimpleLocal", "listByStr20Excludes" ],
                          null
                        ],
                        [
                          [
                            "Ident",
                            [ "SimpleLocal", "currentOperators" ],
                            null
                          ],
                          [ "Ident", [ "SimpleLocal", "address" ], null ]
                        ]
                      ],
                      null
                    ],
                    [
                      [
                        "App",
                        [ "Ident", [ "SimpleLocal", "xandb" ], null ],
                        [
                          [ "Ident", [ "SimpleLocal", "b" ], null ],
                          [ "Ident", [ "SimpleLocal", "isApproved" ], null ]
                        ]
                      ],
                      null
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "needsToChange" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "newOperators" ], null ],
                          [
                            [
                              "MatchExpr",
                              [
                                "Ident", [ "SimpleLocal", "isApproved" ], null
                              ],
                              [
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "True" ], null
                                    ],
                                    []
                                  ],
                                  [
                                    [
                                      "Constr",
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "Cons" ],
                                        null
                                      ],
                                      [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ],
                                      [
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "address" ],
                                          null
                                        ],
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "currentOperators" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    null
                                  ]
                                ],
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "False" ], null
                                    ],
                                    []
                                  ],
                                  [
                                    [
                                      "App",
                                      [
                                        "Ident",
                                        [
                                          "SimpleLocal", "listByStr20FilterOut"
                                        ],
                                        null
                                      ],
                                      [
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "currentOperators" ],
                                          null
                                        ],
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "address" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    null
                                  ]
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MapUpdate",
                          [ "Ident", [ "SimpleLocal", "operators" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "_sender" ], null ] ],
                          [ "Ident", [ "SimpleLocal", "newOperators" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "App",
                              [
                                "Ident",
                                [ "SimpleLocal", "eApprovedFor" ],
                                null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                                [ "Ident", [ "SimpleLocal", "address" ], null ],
                                [
                                  "Ident",
                                  [ "SimpleLocal", "isApproved" ],
                                  null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [ [ "Wildcard" ], [] ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "configureNode" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "node" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "owner" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "resolver" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                [ "Ident", [ "SimpleLocal", "approvals" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "recordMemberOwner" ], null ],
                    [ [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ] ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ],
                [ "Ident", [ "SimpleLocal", "operators" ], null ],
                [ [ "Ident", [ "SimpleLocal", "recordOwner" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isSenderOAO" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "getIsOAO" ], null ],
                    [
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                      [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                      [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                      [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isSenderOAO" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "Record" ], null ],
                              [],
                              [
                                [ "Ident", [ "SimpleLocal", "owner" ], null ],
                                [
                                  "Ident", [ "SimpleLocal", "resolver" ], null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MapUpdate",
                          [ "Ident", [ "SimpleLocal", "records" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "App",
                              [
                                "Ident", [ "SimpleLocal", "eConfigured" ], null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "node" ], null ],
                                [ "Ident", [ "SimpleLocal", "owner" ], null ],
                                [
                                  "Ident", [ "SimpleLocal", "resolver" ], null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Message",
                                  [
                                    [
                                      "_tag", [ "MLit", "onConfigureSuccess" ]
                                    ],
                                    [
                                      "node",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "node" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "owner",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "owner" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [ "_amount", [ "MLit", "0" ] ],
                                    [
                                      "_recipient",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "_sender" ],
                                          null
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "oneMsg" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "SendMsgs",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "False" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Literal",
                                  "Sender not node owner, approved or operator"
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "eError" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Message",
                                  [
                                    [
                                      "_tag", [ "MLit", "onConfigureFailure" ]
                                    ],
                                    [
                                      "node",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "node" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "owner",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "recordOwner" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [ "_amount", [ "MLit", "0" ] ],
                                    [
                                      "_recipient",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "_sender" ],
                                          null
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "oneMsg" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "SendMsgs",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ]
                        ],
                        null
                      ]
                    ]
                  ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [
            "Ident", [ "SimpleLocal", "configureResolver" ], null
          ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "node" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "resolver" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                [ "Ident", [ "SimpleLocal", "approvals" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "recordMemberOwner" ], null ],
                    [ [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ] ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ],
                [ "Ident", [ "SimpleLocal", "operators" ], null ],
                [ [ "Ident", [ "SimpleLocal", "recordOwner" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isSenderOAO" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "getIsOAO" ], null ],
                    [
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                      [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                      [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                      [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isSenderOAO" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "Record" ], null ],
                              [],
                              [
                                [
                                  "Ident",
                                  [ "SimpleLocal", "recordOwner" ],
                                  null
                                ],
                                [
                                  "Ident", [ "SimpleLocal", "resolver" ], null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MapUpdate",
                          [ "Ident", [ "SimpleLocal", "records" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "App",
                              [
                                "Ident", [ "SimpleLocal", "eConfigured" ], null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "node" ], null ],
                                [
                                  "Ident",
                                  [ "SimpleLocal", "recordOwner" ],
                                  null
                                ],
                                [
                                  "Ident", [ "SimpleLocal", "resolver" ], null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "False" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Literal",
                                  "Sender not node owner, approved or operator"
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "eError" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ]
                    ]
                  ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "transfer" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "node" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "owner" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                [ "Ident", [ "SimpleLocal", "approvals" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "recordMemberOwner" ], null ],
                    [ [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ] ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ],
                [ "Ident", [ "SimpleLocal", "operators" ], null ],
                [ [ "Ident", [ "SimpleLocal", "recordOwner" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isSenderOAO" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "getIsOAO" ], null ],
                    [
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                      [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                      [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                      [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isSenderOAO" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "MapUpdate",
                          [ "Ident", [ "SimpleLocal", "approvals" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          null
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "Record" ], null ],
                              [],
                              [
                                [ "Ident", [ "SimpleLocal", "owner" ], null ],
                                [
                                  "Ident",
                                  [ "SimpleLocal", "zeroByStr20" ],
                                  null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MapUpdate",
                          [ "Ident", [ "SimpleLocal", "records" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "App",
                              [
                                "Ident", [ "SimpleLocal", "eConfigured" ], null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "node" ], null ],
                                [ "Ident", [ "SimpleLocal", "owner" ], null ],
                                [
                                  "Ident",
                                  [ "SimpleLocal", "zeroByStr20" ],
                                  null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Message",
                                  [
                                    [ "_tag", [ "MLit", "onTransferSuccess" ] ],
                                    [
                                      "node",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "node" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "owner",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "owner" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [ "_amount", [ "MLit", "0" ] ],
                                    [
                                      "_recipient",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "_sender" ],
                                          null
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "oneMsg" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "SendMsgs",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "False" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Literal",
                                  "Sender not node owner, approved or operator"
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "eError" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Message",
                                  [
                                    [ "_tag", [ "MLit", "onTransferFailure" ] ],
                                    [
                                      "node",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "node" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "owner",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "owner" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [ "_amount", [ "MLit", "0" ] ],
                                    [
                                      "_recipient",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "_sender" ],
                                          null
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "oneMsg" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "SendMsgs",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ]
                        ],
                        null
                      ]
                    ]
                  ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "assign" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "parent" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "label" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "owner" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "parent" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                [ "Ident", [ "SimpleLocal", "approvals" ], null ],
                [ [ "Ident", [ "SimpleLocal", "parent" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "recordMemberOwner" ], null ],
                    [ [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ] ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ],
                [ "Ident", [ "SimpleLocal", "operators" ], null ],
                [ [ "Ident", [ "SimpleLocal", "recordOwner" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isSenderOAO" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "getIsOAO" ], null ],
                    [
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                      [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                      [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                      [ "Ident", [ "SimpleLocal", "maybeOperators" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isSenderOAO" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "node" ], null ],
                          [
                            [
                              "App",
                              [
                                "Ident",
                                [ "SimpleLocal", "parentLabelToNode" ],
                                null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "parent" ], null ],
                                [ "Ident", [ "SimpleLocal", "label" ], null ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MapGet",
                          [ "Ident", [ "SimpleLocal", "recordExists" ], null ],
                          [ "Ident", [ "SimpleLocal", "records" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          false
                        ],
                        null
                      ],
                      [
                        [
                          "MatchStmt",
                          [ "Ident", [ "SimpleLocal", "recordExists" ], null ],
                          [
                            [
                              [
                                "Constructor",
                                [ "Ident", [ "SimpleLocal", "False" ], null ],
                                []
                              ],
                              [
                                [
                                  [
                                    "Bind",
                                    [ "Ident", [ "SimpleLocal", "e" ], null ],
                                    [
                                      [
                                        "App",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "eNewDomain" ],
                                          null
                                        ],
                                        [
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "parent" ],
                                            null
                                          ],
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "label" ],
                                            null
                                          ]
                                        ]
                                      ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "CreateEvnt",
                                    [ "Ident", [ "SimpleLocal", "e" ], null ]
                                  ],
                                  null
                                ]
                              ]
                            ],
                            [ [ "Wildcard" ], [] ]
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MapUpdate",
                          [ "Ident", [ "SimpleLocal", "approvals" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          null
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "Record" ], null ],
                              [],
                              [
                                [ "Ident", [ "SimpleLocal", "owner" ], null ],
                                [
                                  "Ident",
                                  [ "SimpleLocal", "zeroByStr20" ],
                                  null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MapUpdate",
                          [ "Ident", [ "SimpleLocal", "records" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "App",
                              [
                                "Ident", [ "SimpleLocal", "eConfigured" ], null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "node" ], null ],
                                [ "Ident", [ "SimpleLocal", "owner" ], null ],
                                [
                                  "Ident",
                                  [ "SimpleLocal", "zeroByStr20" ],
                                  null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Message",
                                  [
                                    [ "_tag", [ "MLit", "onAssignSuccess" ] ],
                                    [
                                      "parent",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "parent" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "label",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "label" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "owner",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "owner" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [ "_amount", [ "MLit", "0" ] ],
                                    [
                                      "_recipient",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "_sender" ],
                                          null
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "oneMsg" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "SendMsgs",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "False" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Literal",
                                  "Sender not parent owner, approved or operator"
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "eError" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Message",
                                  [
                                    [ "_tag", [ "MLit", "onAssignFailure" ] ],
                                    [
                                      "parent",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "parent" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "label",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "label" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "owner",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "recordOwner" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [ "_amount", [ "MLit", "0" ] ],
                                    [
                                      "_recipient",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "_sender" ],
                                          null
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "oneMsg" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "SendMsgs",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ]
                        ],
                        null
                      ]
                    ]
                  ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "bestow" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "label" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "owner" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "resolver" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "Load",
                [ "Ident", [ "SimpleLocal", "currentAdmins" ], null ],
                [ "Ident", [ "SimpleLocal", "admins" ], null ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "node" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "parentLabelToNode" ], null ],
                    [
                      [ "Ident", [ "SimpleLocal", "rootNode" ], null ],
                      [ "Ident", [ "SimpleLocal", "label" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "recordExists" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                false
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Load",
                [ "Ident", [ "SimpleLocal", "currentRegistrar" ], null ],
                [ "Ident", [ "SimpleLocal", "registrar" ], null ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isOk" ], null ],
                [
                  [
                    "Let",
                    [ "Ident", [ "SimpleLocal", "isSenderAdmin" ], null ],
                    null,
                    [
                      [
                        "App",
                        [
                          "Ident",
                          [ "SimpleLocal", "listByStr20Contains" ],
                          null
                        ],
                        [
                          [ "Ident", [ "SimpleLocal", "currentAdmins" ], null ],
                          [ "Ident", [ "SimpleLocal", "_sender" ], null ]
                        ]
                      ],
                      null
                    ],
                    [
                      [
                        "Let",
                        [
                          "Ident", [ "SimpleLocal", "isSenderRegistrar" ], null
                        ],
                        null,
                        [
                          [
                            "Builtin",
                            [ [ "Builtin_eq" ], null ],
                            [],
                            [
                              [
                                "Ident",
                                [ "SimpleLocal", "currentRegistrar" ],
                                null
                              ],
                              [ "Ident", [ "SimpleLocal", "_sender" ], null ]
                            ]
                          ],
                          null
                        ],
                        [
                          [
                            "Let",
                            [ "Ident", [ "SimpleLocal", "isOkSender" ], null ],
                            null,
                            [
                              [
                                "App",
                                [ "Ident", [ "SimpleLocal", "orb" ], null ],
                                [
                                  [
                                    "Ident",
                                    [ "SimpleLocal", "isSenderRegistrar" ],
                                    null
                                  ],
                                  [
                                    "Ident",
                                    [ "SimpleLocal", "isSenderAdmin" ],
                                    null
                                  ]
                                ]
                              ],
                              null
                            ],
                            [
                              [
                                "Let",
                                [
                                  "Ident",
                                  [ "SimpleLocal", "recordOwner" ],
                                  null
                                ],
                                null,
                                [
                                  [
                                    "App",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "recordMemberOwner" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "maybeRecord" ],
                                        null
                                      ]
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Let",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "recordIsUnowned" ],
                                      null
                                    ],
                                    null,
                                    [
                                      [
                                        "Builtin",
                                        [ [ "Builtin_eq" ], null ],
                                        [],
                                        [
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "recordOwner" ],
                                            null
                                          ],
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "zeroByStr20" ],
                                            null
                                          ]
                                        ]
                                      ],
                                      null
                                    ],
                                    [
                                      [
                                        "Let",
                                        [
                                          "Ident",
                                          [
                                            "SimpleLocal",
                                            "recordIsOwnedByRegistrar"
                                          ],
                                          null
                                        ],
                                        null,
                                        [
                                          [
                                            "Builtin",
                                            [ [ "Builtin_eq" ], null ],
                                            [],
                                            [
                                              [
                                                "Ident",
                                                [
                                                  "SimpleLocal", "recordOwner"
                                                ],
                                                null
                                              ],
                                              [
                                                "Ident",
                                                [
                                                  "SimpleLocal",
                                                  "currentRegistrar"
                                                ],
                                                null
                                              ]
                                            ]
                                          ],
                                          null
                                        ],
                                        [
                                          [
                                            "Let",
                                            [
                                              "Ident",
                                              [
                                                "SimpleLocal",
                                                "isRegistrarSenderAndOwned"
                                              ],
                                              null
                                            ],
                                            null,
                                            [
                                              [
                                                "App",
                                                [
                                                  "Ident",
                                                  [ "SimpleLocal", "andb" ],
                                                  null
                                                ],
                                                [
                                                  [
                                                    "Ident",
                                                    [
                                                      "SimpleLocal",
                                                      "recordIsOwnedByRegistrar"
                                                    ],
                                                    null
                                                  ],
                                                  [
                                                    "Ident",
                                                    [
                                                      "SimpleLocal",
                                                      "isSenderRegistrar"
                                                    ],
                                                    null
                                                  ]
                                                ]
                                              ],
                                              null
                                            ],
                                            [
                                              [
                                                "Let",
                                                [
                                                  "Ident",
                                                  [
                                                    "SimpleLocal",
                                                    "isOkRecordOwner"
                                                  ],
                                                  null
                                                ],
                                                null,
                                                [
                                                  [
                                                    "App",
                                                    [
                                                      "Ident",
                                                      [ "SimpleLocal", "orb" ],
                                                      null
                                                    ],
                                                    [
                                                      [
                                                        "Ident",
                                                        [
                                                          "SimpleLocal",
                                                          "recordIsUnowned"
                                                        ],
                                                        null
                                                      ],
                                                      [
                                                        "Ident",
                                                        [
                                                          "SimpleLocal",
                                                          "isRegistrarSenderAndOwned"
                                                        ],
                                                        null
                                                      ]
                                                    ]
                                                  ],
                                                  null
                                                ],
                                                [
                                                  [
                                                    "App",
                                                    [
                                                      "Ident",
                                                      [ "SimpleLocal", "andb" ],
                                                      null
                                                    ],
                                                    [
                                                      [
                                                        "Ident",
                                                        [
                                                          "SimpleLocal",
                                                          "isOkSender"
                                                        ],
                                                        null
                                                      ],
                                                      [
                                                        "Ident",
                                                        [
                                                          "SimpleLocal",
                                                          "isOkRecordOwner"
                                                        ],
                                                        null
                                                      ]
                                                    ]
                                                  ],
                                                  null
                                                ]
                                              ],
                                              null
                                            ]
                                          ],
                                          null
                                        ]
                                      ],
                                      null
                                    ]
                                  ],
                                  null
                                ]
                              ],
                              null
                            ]
                          ],
                          null
                        ]
                      ],
                      null
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isOk" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "MatchStmt",
                          [ "Ident", [ "SimpleLocal", "recordExists" ], null ],
                          [
                            [
                              [
                                "Constructor",
                                [ "Ident", [ "SimpleLocal", "False" ], null ],
                                []
                              ],
                              [
                                [
                                  [
                                    "Bind",
                                    [ "Ident", [ "SimpleLocal", "e" ], null ],
                                    [
                                      [
                                        "App",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "eNewDomain" ],
                                          null
                                        ],
                                        [
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "rootNode" ],
                                            null
                                          ],
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "label" ],
                                            null
                                          ]
                                        ]
                                      ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "CreateEvnt",
                                    [ "Ident", [ "SimpleLocal", "e" ], null ]
                                  ],
                                  null
                                ]
                              ]
                            ],
                            [ [ "Wildcard" ], [] ]
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "Record" ], null ],
                              [],
                              [
                                [ "Ident", [ "SimpleLocal", "owner" ], null ],
                                [
                                  "Ident", [ "SimpleLocal", "resolver" ], null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "MapUpdate",
                          [ "Ident", [ "SimpleLocal", "records" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                          [ "Ident", [ "SimpleLocal", "newRecord" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "App",
                              [
                                "Ident", [ "SimpleLocal", "eConfigured" ], null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "node" ], null ],
                                [ "Ident", [ "SimpleLocal", "owner" ], null ],
                                [
                                  "Ident", [ "SimpleLocal", "resolver" ], null
                                ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "False" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [ [ "Literal", "Sender admin" ], null ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "eError" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ]
                    ]
                  ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "setRegistrar" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "address" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "Load",
                [ "Ident", [ "SimpleLocal", "currentAdmins" ], null ],
                [ "Ident", [ "SimpleLocal", "admins" ], null ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isOk" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "listByStr20Contains" ], null ],
                    [
                      [ "Ident", [ "SimpleLocal", "currentAdmins" ], null ],
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isOk" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "App",
                              [
                                "Ident",
                                [ "SimpleLocal", "eNewRegistrar" ],
                                null
                              ],
                              [
                                [ "Ident", [ "SimpleLocal", "address" ], null ]
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CreateEvnt",
                          [ "Ident", [ "SimpleLocal", "e" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Store",
                          [ "Ident", [ "SimpleLocal", "registrar" ], null ],
                          [ "Ident", [ "SimpleLocal", "address" ], null ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [ [ "Wildcard" ], [] ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "register" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "parent" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "label" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "node" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "parentLabelToNode" ], null ],
                    [
                      [ "Ident", [ "SimpleLocal", "parent" ], null ],
                      [ "Ident", [ "SimpleLocal", "label" ], null ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                [ "Ident", [ "SimpleLocal", "approvals" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "recordMemberOwner" ], null ],
                    [ [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ] ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "approved" ], null ],
                [
                  [
                    "MatchExpr",
                    [ "Ident", [ "SimpleLocal", "maybeApproved" ], null ],
                    [
                      [
                        [
                          "Constructor",
                          [ "Ident", [ "SimpleLocal", "None" ], null ],
                          []
                        ],
                        [
                          [
                            "Var",
                            [ "Ident", [ "SimpleLocal", "zeroByStr20" ], null ]
                          ],
                          null
                        ]
                      ],
                      [
                        [
                          "Constructor",
                          [ "Ident", [ "SimpleLocal", "Some" ], null ],
                          [
                            [
                              "Binder",
                              [ "Ident", [ "SimpleLocal", "approved" ], null ]
                            ]
                          ]
                        ],
                        [
                          [
                            "Var",
                            [ "Ident", [ "SimpleLocal", "approved" ], null ]
                          ],
                          null
                        ]
                      ]
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "Load",
                [ "Ident", [ "SimpleLocal", "currentRegistrar" ], null ],
                [ "Ident", [ "SimpleLocal", "registrar" ], null ]
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "isOk" ], null ],
                [
                  [
                    "Let",
                    [ "Ident", [ "SimpleLocal", "isRecordUnowned" ], null ],
                    null,
                    [
                      [
                        "Builtin",
                        [ [ "Builtin_eq" ], null ],
                        [],
                        [
                          [ "Ident", [ "SimpleLocal", "recordOwner" ], null ],
                          [ "Ident", [ "SimpleLocal", "zeroByStr20" ], null ]
                        ]
                      ],
                      null
                    ],
                    [
                      [
                        "Let",
                        [ "Ident", [ "SimpleLocal", "isUnapproved" ], null ],
                        null,
                        [
                          [
                            "Builtin",
                            [ [ "Builtin_eq" ], null ],
                            [],
                            [
                              [ "Ident", [ "SimpleLocal", "approved" ], null ],
                              [
                                "Ident", [ "SimpleLocal", "zeroByStr20" ], null
                              ]
                            ]
                          ],
                          null
                        ],
                        [
                          [
                            "App",
                            [ "Ident", [ "SimpleLocal", "andb" ], null ],
                            [
                              [
                                "Ident",
                                [ "SimpleLocal", "isRecordUnowned" ],
                                null
                              ],
                              [
                                "Ident",
                                [ "SimpleLocal", "isUnapproved" ],
                                null
                              ]
                            ]
                          ],
                          null
                        ]
                      ],
                      null
                    ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "isOk" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
                      []
                    ],
                    [
                      [ [ "AcceptPayment" ], null ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "m" ], null ],
                              null,
                              [
                                [
                                  "Message",
                                  [
                                    [ "_tag", [ "MLit", "register" ] ],
                                    [
                                      "_amount",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "_amount" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "_recipient",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "currentRegistrar" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "origin",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "_sender" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "node",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "node" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "parent",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "parent" ],
                                          null
                                        ]
                                      ]
                                    ],
                                    [
                                      "label",
                                      [
                                        "MVar",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "label" ],
                                          null
                                        ]
                                      ]
                                    ]
                                  ]
                                ],
                                null
                              ],
                              [
                                [
                                  "App",
                                  [
                                    "Ident", [ "SimpleLocal", "oneMsg" ], null
                                  ],
                                  [ [ "Ident", [ "SimpleLocal", "m" ], null ] ]
                                ],
                                null
                              ]
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "SendMsgs",
                          [ "Ident", [ "SimpleLocal", "msgs" ], null ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "False" ], null ],
                      []
                    ],
                    []
                  ]
                ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [
            "Ident", [ "SimpleLocal", "onResolverConfigured" ], null
          ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "node" ], null ],
              [ "PrimType", [ "Bystrx_typ", 32 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [ "Ident", [ "SimpleLocal", "records" ], null ],
                [ [ "Ident", [ "SimpleLocal", "node" ], null ] ],
                true
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "maybeRecord" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "None" ], null ],
                      []
                    ],
                    []
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "Some" ], null ],
                      [
                        [
                          "Binder",
                          [ "Ident", [ "SimpleLocal", "record" ], null ]
                        ]
                      ]
                    ],
                    [
                      [
                        [
                          "MatchStmt",
                          [ "Ident", [ "SimpleLocal", "record" ], null ],
                          [
                            [
                              [
                                "Constructor",
                                [ "Ident", [ "SimpleLocal", "Record" ], null ],
                                [
                                  [
                                    "Binder",
                                    [
                                      "Ident", [ "SimpleLocal", "owner" ], null
                                    ]
                                  ],
                                  [
                                    "Binder",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "resolver" ],
                                      null
                                    ]
                                  ]
                                ]
                              ],
                              [
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident", [ "SimpleLocal", "isOk" ], null
                                    ],
                                    [
                                      [
                                        "Builtin",
                                        [ [ "Builtin_eq" ], null ],
                                        [],
                                        [
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "resolver" ],
                                            null
                                          ],
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "_sender" ],
                                            null
                                          ]
                                        ]
                                      ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "MatchStmt",
                                    [
                                      "Ident", [ "SimpleLocal", "isOk" ], null
                                    ],
                                    [
                                      [
                                        [
                                          "Constructor",
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "True" ],
                                            null
                                          ],
                                          []
                                        ],
                                        [
                                          [
                                            [
                                              "Bind",
                                              [
                                                "Ident",
                                                [ "SimpleLocal", "e" ],
                                                null
                                              ],
                                              [
                                                [
                                                  "App",
                                                  [
                                                    "Ident",
                                                    [
                                                      "SimpleLocal",
                                                      "eConfigured"
                                                    ],
                                                    null
                                                  ],
                                                  [
                                                    [
                                                      "Ident",
                                                      [ "SimpleLocal", "node" ],
                                                      null
                                                    ],
                                                    [
                                                      "Ident",
                                                      [
                                                        "SimpleLocal", "owner"
                                                      ],
                                                      null
                                                    ],
                                                    [
                                                      "Ident",
                                                      [
                                                        "SimpleLocal",
                                                        "resolver"
                                                      ],
                                                      null
                                                    ]
                                                  ]
                                                ],
                                                null
                                              ]
                                            ],
                                            null
                                          ],
                                          [
                                            [
                                              "CreateEvnt",
                                              [
                                                "Ident",
                                                [ "SimpleLocal", "e" ],
                                                null
                                              ]
                                            ],
                                            null
                                          ]
                                        ]
                                      ],
                                      [
                                        [
                                          "Constructor",
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "False" ],
                                            null
                                          ],
                                          []
                                        ],
                                        []
                                      ]
                                    ]
                                  ],
                                  null
                                ]
                              ]
                            ]
                          ]
                        ],
                        null
                      ]
                    ]
                  ]
                ]
              ],
              null
            ]
          ]
        }
      ]
    }
  }
