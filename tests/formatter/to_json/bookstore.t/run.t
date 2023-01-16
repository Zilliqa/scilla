  $ scilla-fmt --json --deannot --human-readable bookstore.scilla
  {
    "smver": 0,
    "libs": {
      "lname": [ "Ident", [ "SimpleLocal", "BookStore" ], null ],
      "lentries": [
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "one_msg" ], null ],
          null,
          [
            [
              "Fun",
              [ "Ident", [ "SimpleLocal", "msg" ], null ],
              [ "PrimType", [ "Msg_typ" ] ],
              [
                [
                  "Let",
                  [ "Ident", [ "SimpleLocal", "nil_msg" ], null ],
                  null,
                  [
                    [
                      "Constr",
                      [ "Ident", [ "SimpleLocal", "Nil" ], null ],
                      [ [ "PrimType", [ "Msg_typ" ] ] ],
                      []
                    ],
                    null
                  ],
                  [
                    [
                      "Constr",
                      [ "Ident", [ "SimpleLocal", "Cons" ], null ],
                      [ [ "PrimType", [ "Msg_typ" ] ] ],
                      [
                        [ "Ident", [ "SimpleLocal", "msg" ], null ],
                        [ "Ident", [ "SimpleLocal", "nil_msg" ], null ]
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
          [ "Ident", [ "SimpleLocal", "code_success" ], null ],
          null,
          [ [ "Literal", "0" ], null ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "code_book_not_found" ], null ],
          null,
          [ [ "Literal", "1" ], null ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "code_not_authorized" ], null ],
          null,
          [ [ "Literal", "2" ], null ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "code_invalid_params" ], null ],
          null,
          [ [ "Literal", "3" ], null ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "code_bookid_exist" ], null ],
          null,
          [ [ "Literal", "4" ], null ]
        ],
        [
          "LibVar",
          [ "Ident", [ "SimpleLocal", "code_store_not_open" ], null ],
          null,
          [ [ "Literal", "5" ], null ]
        ],
        [
          "LibTyp",
          [ "Ident", [ "SimpleLocal", "Book" ], null ],
          [
            {
              "cname": [ "Ident", [ "SimpleLocal", "Book" ], null ],
              "c_arg_types": [
                [ "PrimType", [ "String_typ" ] ],
                [ "PrimType", [ "String_typ" ] ]
              ]
            }
          ]
        ],
        [
          "LibTyp",
          [ "Ident", [ "SimpleLocal", "Member" ], null ],
          [
            {
              "cname": [ "Ident", [ "SimpleLocal", "Member" ], null ],
              "c_arg_types": [
                [ "PrimType", [ "String_typ" ] ],
                [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ]
              ]
            }
          ]
        ]
      ]
    },
    "elibs": [],
    "contr": {
      "cname": [ "Ident", [ "SimpleLocal", "BookStore" ], null ],
      "cparams": [
        [
          [ "Ident", [ "SimpleLocal", "owner" ], null ],
          [ "PrimType", [ "Bystrx_typ", 20 ] ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "store_name" ], null ],
          [ "PrimType", [ "String_typ" ] ]
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
          [ "Ident", [ "SimpleLocal", "members" ], null ],
          [
            "MapType",
            [ "PrimType", [ "Bystrx_typ", 20 ] ],
            [
              "ADT",
              [
                "Ident",
                [ "SimpleLocal", "Member" ],
                { "fname": "", "lnum": 0, "cnum": 0 }
              ],
              []
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
                      [ "SimpleLocal", "Member" ],
                      { "fname": "", "lnum": 0, "cnum": 0 }
                    ],
                    []
                  ]
                ],
                "data": []
              }
            ],
            null
          ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "is_store_open" ], null ],
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
            [ "Constr", [ "Ident", [ "SimpleLocal", "True" ], null ], [], [] ],
            null
          ]
        ],
        [
          [ "Ident", [ "SimpleLocal", "bookInventory" ], null ],
          [
            "MapType",
            [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ],
            [
              "ADT",
              [
                "Ident",
                [ "SimpleLocal", "Book" ],
                { "fname": "", "lnum": 0, "cnum": 0 }
              ],
              []
            ]
          ],
          [
            [
              "Literal",
              {
                "mtype": [
                  [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ],
                  [
                    "ADT",
                    [
                      "Ident",
                      [ "SimpleLocal", "Book" ],
                      { "fname": "", "lnum": 0, "cnum": 0 }
                    ],
                    []
                  ]
                ],
                "data": []
              }
            ],
            null
          ]
        ]
      ],
      "ccomps": [
        {
          "comp_type": [ "CompProc" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "EmitMemberEvent" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "status" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "Bool" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                []
              ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "status_code" ], null ],
              [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "msg" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "status" ], null ],
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
                              "Message",
                              [
                                [
                                  "_eventname", [ "MLit", "AddMemberSuccess" ]
                                ],
                                [
                                  "code",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status_code" ],
                                      null
                                    ]
                                  ]
                                ],
                                [
                                  "message",
                                  [
                                    "MVar",
                                    [ "Ident", [ "SimpleLocal", "msg" ], null ]
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
                              "Message",
                              [
                                [
                                  "_eventname", [ "MLit", "AddMemberFailure" ]
                                ],
                                [
                                  "code",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status_code" ],
                                      null
                                    ]
                                  ]
                                ],
                                [
                                  "message",
                                  [
                                    "MVar",
                                    [ "Ident", [ "SimpleLocal", "msg" ], null ]
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
          "comp_type": [ "CompProc" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "EmitBookEvent" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "status" ], null ],
              [
                "ADT",
                [
                  "Ident",
                  [ "SimpleLocal", "Bool" ],
                  { "fname": "", "lnum": 0, "cnum": 0 }
                ],
                []
              ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "status_code" ], null ],
              [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "event_action" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "book_id" ], null ],
              [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "status" ], null ],
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
                              "Message",
                              [
                                [
                                  "_eventname", [ "MLit", "BookEventSuccess" ]
                                ],
                                [
                                  "code",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status_code" ],
                                      null
                                    ]
                                  ]
                                ],
                                [
                                  "action",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "event_action" ],
                                      null
                                    ]
                                  ]
                                ],
                                [
                                  "id",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "book_id" ],
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
                              "Message",
                              [
                                [
                                  "_eventname", [ "MLit", "BookEventFailure" ]
                                ],
                                [
                                  "code",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status_code" ],
                                      null
                                    ]
                                  ]
                                ],
                                [
                                  "action",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "event_action" ],
                                      null
                                    ]
                                  ]
                                ],
                                [
                                  "id",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "book_id" ],
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
          "comp_name": [ "Ident", [ "SimpleLocal", "OpenStore" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "is_open" ], null ],
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
                "Bind",
                [ "Ident", [ "SimpleLocal", "is_authorized" ], null ],
                [
                  [
                    "Builtin",
                    [ [ "Builtin_eq" ], null ],
                    [],
                    [
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                      [ "Ident", [ "SimpleLocal", "owner" ], null ]
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
                [ "Ident", [ "SimpleLocal", "is_authorized" ], null ],
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
                          "Store",
                          [ "Ident", [ "SimpleLocal", "is_store_open" ], null ],
                          [ "Ident", [ "SimpleLocal", "is_open" ], null ]
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
                                [ "_eventname", [ "MLit", "OpenStore" ] ],
                                [
                                  "status",
                                  [
                                    "MVar",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "is_open" ],
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
                          [ "Ident", [ "SimpleLocal", "status" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "False" ], null ],
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
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msg" ], null ],
                          [ [ "Literal", "Unauthorised Transaction" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "CallProc",
                          [
                            "Ident", [ "SimpleLocal", "EmitMemberEvent" ], null
                          ],
                          [
                            [ "Ident", [ "SimpleLocal", "status" ], null ],
                            [
                              "Ident",
                              [ "SimpleLocal", "code_not_authorized" ],
                              null
                            ],
                            [ "Ident", [ "SimpleLocal", "msg" ], null ]
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
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "AddMember" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "name" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "member_address" ], null ],
              [ "PrimType", [ "Bystrx_typ", 20 ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "member_type" ], null ],
              [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "Bind",
                [ "Ident", [ "SimpleLocal", "is_authorized" ], null ],
                [
                  [
                    "Builtin",
                    [ [ "Builtin_eq" ], null ],
                    [],
                    [
                      [ "Ident", [ "SimpleLocal", "_sender" ], null ],
                      [ "Ident", [ "SimpleLocal", "owner" ], null ]
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
                [ "Ident", [ "SimpleLocal", "is_authorized" ], null ],
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
                          [ "Ident", [ "SimpleLocal", "valid_type" ], null ],
                          [
                            [
                              "Let",
                              [ "Ident", [ "SimpleLocal", "three" ], null ],
                              null,
                              [ [ "Literal", "3" ], null ],
                              [
                                [
                                  "Builtin",
                                  [ [ "Builtin_lt" ], null ],
                                  [],
                                  [
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "member_type" ],
                                      null
                                    ],
                                    [
                                      "Ident", [ "SimpleLocal", "three" ], null
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
                          [ "Ident", [ "SimpleLocal", "valid_type" ], null ],
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
                                      [ "SimpleLocal", "new_member" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Constr",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "Member" ],
                                          null
                                        ],
                                        [],
                                        [
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "name" ],
                                            null
                                          ],
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "member_type" ],
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
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "members" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "member_address" ],
                                        null
                                      ]
                                    ],
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "new_member" ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status" ],
                                      null
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
                                  null
                                ],
                                [
                                  [
                                    "CallProc",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "EmitMemberEvent" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "status" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "code_success" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "name" ],
                                        null
                                      ]
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
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status" ],
                                      null
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
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [ "Ident", [ "SimpleLocal", "msg" ], null ],
                                    [
                                      [ "Literal", "Invalid membership type" ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "CallProc",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "EmitMemberEvent" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "status" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [
                                          "SimpleLocal", "code_invalid_params"
                                        ],
                                        null
                                      ],
                                      [
                                        "Ident", [ "SimpleLocal", "msg" ], null
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
                          [ "Ident", [ "SimpleLocal", "status" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "False" ], null ],
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
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "msg" ], null ],
                          [ [ "Literal", "Unauthorised Transaction" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "CallProc",
                          [
                            "Ident", [ "SimpleLocal", "EmitMemberEvent" ], null
                          ],
                          [
                            [ "Ident", [ "SimpleLocal", "status" ], null ],
                            [
                              "Ident",
                              [ "SimpleLocal", "code_not_authorized" ],
                              null
                            ],
                            [ "Ident", [ "SimpleLocal", "msg" ], null ]
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
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "AddBook" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "book_title" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "author" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "book_id" ], null ],
              [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "Load",
                [ "Ident", [ "SimpleLocal", "is_open" ], null ],
                [ "Ident", [ "SimpleLocal", "is_store_open" ], null ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "is_open" ], null ],
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
                          [
                            "Ident", [ "SimpleLocal", "does_book_exist" ], null
                          ],
                          [ "Ident", [ "SimpleLocal", "bookInventory" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "book_id" ], null ] ],
                          false
                        ],
                        null
                      ],
                      [
                        [
                          "MatchStmt",
                          [
                            "Ident", [ "SimpleLocal", "does_book_exist" ], null
                          ],
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
                                      [ "SimpleLocal", "action" ],
                                      null
                                    ],
                                    [ [ "Literal", "Add" ], null ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status" ],
                                      null
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
                                  null
                                ],
                                [
                                  [
                                    "CallProc",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "EmitBookEvent" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "status" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "code_bookid_exist" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "action" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "book_id" ],
                                        null
                                      ]
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
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "new_book" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Constr",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "Book" ],
                                          null
                                        ],
                                        [],
                                        [
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "book_title" ],
                                            null
                                          ],
                                          [
                                            "Ident",
                                            [ "SimpleLocal", "author" ],
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
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "bookInventory" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "book_id" ],
                                        null
                                      ]
                                    ],
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "new_book" ],
                                      null
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "action" ],
                                      null
                                    ],
                                    [ [ "Literal", "Add" ], null ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status" ],
                                      null
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
                                  null
                                ],
                                [
                                  [
                                    "CallProc",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "EmitBookEvent" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "status" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "code_success" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "action" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "book_id" ],
                                        null
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
                          [ "Ident", [ "SimpleLocal", "action" ], null ],
                          [ [ "Literal", "Add" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "status" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "False" ], null ],
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
                          [ "Ident", [ "SimpleLocal", "EmitBookEvent" ], null ],
                          [
                            [ "Ident", [ "SimpleLocal", "status" ], null ],
                            [
                              "Ident",
                              [ "SimpleLocal", "code_store_not_open" ],
                              null
                            ],
                            [ "Ident", [ "SimpleLocal", "action" ], null ],
                            [ "Ident", [ "SimpleLocal", "book_id" ], null ]
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
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "RemoveBook" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "book_id" ], null ],
              [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "Load",
                [ "Ident", [ "SimpleLocal", "is_open" ], null ],
                [ "Ident", [ "SimpleLocal", "is_store_open" ], null ]
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "is_open" ], null ],
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
                          [ "Ident", [ "SimpleLocal", "get_book" ], null ],
                          [ "Ident", [ "SimpleLocal", "bookInventory" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "book_id" ], null ] ],
                          true
                        ],
                        null
                      ],
                      [
                        [
                          "MatchStmt",
                          [ "Ident", [ "SimpleLocal", "get_book" ], null ],
                          [
                            [
                              [
                                "Constructor",
                                [ "Ident", [ "SimpleLocal", "Some" ], null ],
                                [
                                  [
                                    "Constructor",
                                    [
                                      "Ident", [ "SimpleLocal", "Book" ], null
                                    ],
                                    [
                                      [
                                        "Binder",
                                        [
                                          "Ident",
                                          [ "SimpleLocal", "book_title" ],
                                          null
                                        ]
                                      ],
                                      [ "Wildcard" ]
                                    ]
                                  ]
                                ]
                              ],
                              [
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "action" ],
                                      null
                                    ],
                                    [ [ "Literal", "Remove" ], null ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status" ],
                                      null
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
                                  null
                                ],
                                [
                                  [
                                    "CallProc",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "EmitBookEvent" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "status" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "code_success" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "action" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "book_id" ],
                                        null
                                      ]
                                    ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "MapUpdate",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "bookInventory" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "book_id" ],
                                        null
                                      ]
                                    ],
                                    null
                                  ],
                                  null
                                ]
                              ]
                            ],
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
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "book_title" ],
                                      null
                                    ],
                                    [ [ "Literal", "Error: Not Found" ], null ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "action" ],
                                      null
                                    ],
                                    [ [ "Literal", "Remove" ], null ]
                                  ],
                                  null
                                ],
                                [
                                  [
                                    "Bind",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "status" ],
                                      null
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
                                  null
                                ],
                                [
                                  [
                                    "CallProc",
                                    [
                                      "Ident",
                                      [ "SimpleLocal", "EmitBookEvent" ],
                                      null
                                    ],
                                    [
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "status" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [
                                          "SimpleLocal", "code_book_not_found"
                                        ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "action" ],
                                        null
                                      ],
                                      [
                                        "Ident",
                                        [ "SimpleLocal", "book_id" ],
                                        null
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
                          [ "Ident", [ "SimpleLocal", "action" ], null ],
                          [ [ "Literal", "Add" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "status" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "False" ], null ],
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
                          [ "Ident", [ "SimpleLocal", "EmitBookEvent" ], null ],
                          [
                            [ "Ident", [ "SimpleLocal", "status" ], null ],
                            [
                              "Ident",
                              [ "SimpleLocal", "code_store_not_open" ],
                              null
                            ],
                            [ "Ident", [ "SimpleLocal", "action" ], null ],
                            [ "Ident", [ "SimpleLocal", "book_id" ], null ]
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
          "comp_type": [ "CompTrans" ],
          "comp_name": [ "Ident", [ "SimpleLocal", "UpdateBook" ], null ],
          "comp_params": [
            [
              [ "Ident", [ "SimpleLocal", "book_id" ], null ],
              [ "PrimType", [ "Uint_typ", [ "Bits32" ] ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "book_title" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ],
            [
              [ "Ident", [ "SimpleLocal", "author" ], null ],
              [ "PrimType", [ "String_typ" ] ]
            ]
          ],
          "comp_body": [
            [
              [
                "MapGet",
                [ "Ident", [ "SimpleLocal", "does_book_exist" ], null ],
                [ "Ident", [ "SimpleLocal", "bookInventory" ], null ],
                [ [ "Ident", [ "SimpleLocal", "book_id" ], null ] ],
                false
              ],
              null
            ],
            [
              [
                "MatchStmt",
                [ "Ident", [ "SimpleLocal", "does_book_exist" ], null ],
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
                          [ "Ident", [ "SimpleLocal", "action" ], null ],
                          [ [ "Literal", "Update" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "status" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "False" ], null ],
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
                          [ "Ident", [ "SimpleLocal", "EmitBookEvent" ], null ],
                          [
                            [ "Ident", [ "SimpleLocal", "status" ], null ],
                            [
                              "Ident",
                              [ "SimpleLocal", "code_book_not_found" ],
                              null
                            ],
                            [ "Ident", [ "SimpleLocal", "action" ], null ],
                            [ "Ident", [ "SimpleLocal", "book_id" ], null ]
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
                    [
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "new_book" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "Book" ], null ],
                              [],
                              [
                                [
                                  "Ident",
                                  [ "SimpleLocal", "book_title" ],
                                  null
                                ],
                                [ "Ident", [ "SimpleLocal", "author" ], null ]
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
                          [ "Ident", [ "SimpleLocal", "bookInventory" ], null ],
                          [ [ "Ident", [ "SimpleLocal", "book_id" ], null ] ],
                          [ "Ident", [ "SimpleLocal", "new_book" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "action" ], null ],
                          [ [ "Literal", "Update" ], null ]
                        ],
                        null
                      ],
                      [
                        [
                          "Bind",
                          [ "Ident", [ "SimpleLocal", "status" ], null ],
                          [
                            [
                              "Constr",
                              [ "Ident", [ "SimpleLocal", "True" ], null ],
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
                          [ "Ident", [ "SimpleLocal", "EmitBookEvent" ], null ],
                          [
                            [ "Ident", [ "SimpleLocal", "status" ], null ],
                            [
                              "Ident", [ "SimpleLocal", "code_success" ], null
                            ],
                            [ "Ident", [ "SimpleLocal", "action" ], null ],
                            [ "Ident", [ "SimpleLocal", "book_id" ], null ]
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
