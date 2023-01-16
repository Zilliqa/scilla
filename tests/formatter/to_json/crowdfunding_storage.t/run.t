  $ scilla-fmt --json --deannot --human-readable crowdfunding_storage.scilla
  {
    "smver": 0,
    "libs": {
      "lname": [ "Ident", [ "SimpleLocal", "CrowdfundingStorage" ], null ],
      "lentries": [
        [
          "LibTyp",
          [ "Ident", [ "SimpleLocal", "Error" ], null ],
          [
            {
              "cname": [
                "Ident", [ "SimpleLocal", "GovernorIsNotSetYet" ], null
              ],
              "c_arg_types": []
            },
            {
              "cname": [
                "Ident", [ "SimpleLocal", "SenderIsNotGovernor" ], null
              ],
              "c_arg_types": []
            },
            {
              "cname": [
                "Ident", [ "SimpleLocal", "SenderIsNotContractOwner" ], null
              ],
              "c_arg_types": []
            }
          ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "make_error" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "result" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "Error" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                []
              ],
              [
                [
                  "Let",
                  [ "Ident", [ "SimpleLocal", "result_code" ], null ],
                  null,
                  [
                    [
                      "MatchExpr",
                      [ "Ident", [ "SimpleLocal", "result" ], null ],
                      [
                        [
                          [
                            "Constructor",
                            [
                              "Ident",
                              [ "SimpleLocal", "GovernorIsNotSetYet" ],
                              null
                            ],
                            []
                          ],
                          [ [ "Literal", "-1" ], null ]
                        ],
                        [
                          [
                            "Constructor",
                            [
                              "Ident",
                              [ "SimpleLocal", "SenderIsNotGovernor" ],
                              null
                            ],
                            []
                          ],
                          [ [ "Literal", "-2" ], null ]
                        ],
                        [
                          [
                            "Constructor",
                            [
                              "Ident",
                              [ "SimpleLocal", "SenderIsNotContractOwner" ],
                              null
                            ],
                            []
                          ],
                          [ [ "Literal", "-3" ], null ]
                        ]
                      ]
                    ],
                    null
                  ],
                  [
                    [
                      "Message",
                      [
                        [ "_exception", [ "MLit", "Error" ] ],
                        [
                          "code",
                          [
                            "MVar",
                            [ "Ident", [ "SimpleLocal", "result_code" ], null ]
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
        ]
      ]
    },
    "elibs": [],
    "contr": {
      "cname": [ "Ident", [ "SimpleLocal", "CrowdfundingStorage" ], null ],
      "cparams": [
        [
          [ "Ident", [ "SimpleLocal", "contract_owner" ], null ],
          [ "PrimType", [ "Bystrx_typ", 20 ] ]
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
          [ "Ident", [ "SimpleLocal", "backers" ], null ],
          [
            "MapType",
            [ "PrimType", [ "Bystrx_typ", 20 ] ],
            [ "PrimType", [ "Uint_typ", [ "Bits128" ] ] ]
          ],
          [
            [
              "Literal",
              {
                "mtype": [
                  [ "PrimType", [ "Bystrx_typ", 20 ] ],
                  [ "PrimType", [ "Uint_typ", [ "Bits128" ] ] ]
                ],
                "data": []
              }
            ],
            null
          ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "governor" ], null ],
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
              "Constr",
              [ "Ident", [ "SimpleLocal", "None" ], null ],
              [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ],
              []
            ],
            null
          ]
        ]
      ],
      "ccomps": [
        {
          "comp_type": [ "CompProc" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "Throw" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "error" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "Error" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                []
              ]
            ]
          ],
          "comp_body": [
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "e" ], null ],
                [
                  [
                    "App",
                    [ "Ident", [ "SimpleLocal", "make_error" ], null ],
                    [ [ "Ident", [ "SimpleLocal", "error" ], null ] ]
                  ],
                  null
                ]
              ],
              null
            ],
            [ [ "Throw", [ "Ident", [ "SimpleLocal", "e" ], null ] ], null ]
          ]
        },
        {
          "comp_type": [ "CompProc" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "RequireGovernor" ], null ],
          "comp_params": [],
          "comp_body": [
            [
              [
                "Load",
                [ "Ident", [ "SimpleLocal", "optional_governor" ], null ],
                [ "Ident", [ "SimpleLocal", "governor" ], null ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "optional_governor" ], null ],
                [
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "None" ], null ],
                      []
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "e" ], null ],
                          [
                            [
                              "Constr",
                              [
                                "Ident",
                                [ "SimpleLocal", "GovernorIsNotSetYet" ],
                                null
                              ],
                              [],
                              []
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CallProc",
                          [ "Ident", [ "SimpleLocal", "Throw" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "e" ], null ] ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "Some" ], null ],
                      [
                        [
                          "Binder", [ "Ident", [ "SimpleLocal", "gov" ], null ]
                        ]
                      ]
                    ],
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "is_governor" ], null ],
                          [
                            [
                              "Builtin",
                              [ [ "Builtin_eq" ], null ],
                              [],
                              [
                                [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                                [ "Ident", [ "SimpleLocal", "gov" ], null ]
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
                          [ "Ident", [ "SimpleLocal", "is_governor" ], null ],
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
                                        "Constr",
                                        [
                                          "Ident",
                                          [
                                            "SimpleLocal",
                                            "SenderIsNotGovernor"
                                          ],
                                          null
                                        ],
                                        [],
                                        []
                                      ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "CallProc",
                                    [
                                      "Ident", [ "SimpleLocal", "Throw" ], null
                                    ],
                                    [
                                      [ "Ident", [ "SimpleLocal", "e" ], null ]
                                    ]
                                  ],
                                  null
                                ]
                              ]
                            ],
                            [
                              [
                                "Constructor",
                                [ "Ident", [ "SimpleLocal", "True" ], null ],
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
        },
        {
          "comp_type": [ "CompProc" ],
          "comp_name": [
            "Ident", [ "SimpleLocal", "RequireContractOwner" ], null
          ],
          "comp_params": [],
          "comp_body": [
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "is_contract_owner" ], null ],
                [
                  [
                    "Builtin",
                    [ [ "Builtin_eq" ], null ],
                    [],
                    [
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                      [ "Ident", [ "SimpleLocal", "contract_owner" ], null ]
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
                [ "Ident", [ "SimpleLocal", "is_contract_owner" ], null ],
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
                              "Constr",
                              [
                                "Ident",
                                [ "SimpleLocal", "SenderIsNotContractOwner" ],
                                null
                              ],
                              [],
                              []
                            ],
                            null
                          ]
                        ],
                        null
                      ],
                      [
                        [
                          "CallProc",
                          [ "Ident", [ "SimpleLocal", "Throw" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "e" ], null ] ]
                        ],
                        null
                      ]
                    ]
                  ],
                  [
                    [
                      "Constructor",
                      [ "Ident", [ "SimpleLocal", "True" ], null ],
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
          "comp_name": [ "Ident", [ "SimpleLocal", "SetBackersKey" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "key" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "val" ], null ],
              [ "PrimType", [ "Uint_typ", [ "Bits128" ] ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "CallProc",
                [ "Ident", [ "SimpleLocal", "RequireGovernor" ], null ],
                []
              ],
              null
            ],
            [
              [
                "MapUpdate",
                [ "Ident", [ "SimpleLocal", "backers" ], null ],
                [ [ "Ident", [ "SimpleLocal", "key" ], null ] ],
                [ "Ident", [ "SimpleLocal", "val" ], null ]
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "DeleteBackersKey" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "key" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "CallProc",
                [ "Ident", [ "SimpleLocal", "RequireGovernor" ], null ],
                []
              ],
              null
            ],
            [
              [
                "MapUpdate",
                [ "Ident", [ "SimpleLocal", "backers" ], null ],
                [ [ "Ident", [ "SimpleLocal", "key" ], null ] ],
                null
              ],
              null
            ]
          ]
        },
        {
          "comp_type": [ "CompTrans" ],
          "comp_name": [
            "Ident", [ "SimpleLocal", "SetStorageGovernor" ], null
          ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "new_governor" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "CallProc",
                [ "Ident", [ "SimpleLocal", "RequireContractOwner" ], null ],
                []
              ],
              null
            ],
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "gov" ], null ],
                [
                  [
                    "Constr",
                    [ "Ident", [ "SimpleLocal", "Some" ], null ],
                    [ [ "PrimType", [ "Bystrx_typ", 20 ] ] ],
                    [ [ "Ident", [ "SimpleLocal", "new_governor" ], null ] ]
                  ],
                  null
                ]
              ],
              null
            ],
            [
              [
                "Store",
                [ "Ident", [ "SimpleLocal", "governor" ], null ],
                [ "Ident", [ "SimpleLocal", "gov" ], null ]
              ],
              null
            ]
          ]
        }
      ]
    }
  }
