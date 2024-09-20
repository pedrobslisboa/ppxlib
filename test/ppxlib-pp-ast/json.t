ppxlib-pp-ast as a --json flag that pretty prints the AST in JSON format.

Consider the following .ml file:

  $ cat > test.ml << EOF
  > let x = 2
  > let y = true
  > let z =
  >  fun x ->
  >  x
  > EOF

This is how it's printed without the flag:

  $ ppxlib-pp-ast test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr = Pexp_constant (Pconst_integer ( "2", None))
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "y"
          ; pvb_expr = Pexp_construct ( Lident "true", None)
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ; Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "z"
          ; pvb_expr =
              Pexp_fun ( Nolabel, None, Ppat_var "x", Pexp_ident (Lident "x"))
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ]

Now how it's printed with the flag:

  $ ppxlib-pp-ast --json test.ml
  [
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": { "Ppat_var": "x" },
            "pvb_expr": {
              "Pexp_constant": { "Pconst_integer": [ "2", "None" ] }
            },
            "pvb_attributes": "__attrs",
            "pvb_loc": "__loc"
          }
        ]
      ]
    },
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": { "Ppat_var": "y" },
            "pvb_expr": { "Pexp_construct": [ { "Lident": "true" }, "None" ] },
            "pvb_attributes": "__attrs",
            "pvb_loc": "__loc"
          }
        ]
      ]
    },
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": { "Ppat_var": "z" },
            "pvb_expr": {
              "Pexp_fun": [
                "Nolabel",
                "None",
                { "Ppat_var": "x" },
                { "Pexp_ident": { "Lident": "x" } }
              ]
            },
            "pvb_attributes": "__attrs",
            "pvb_loc": "__loc"
          }
        ]
      ]
    }
  ]

You can compase with other flags, for example --show-locs to display location:

  $ ppxlib-pp-ast --json --show-locs --full-locs test.ml
  [
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": {
              "ppat_desc": {
                "Ppat_var": {
                  "txt": "x",
                  "loc": {
                    "loc_start": {
                      "pos_fname": "test.ml",
                      "pos_lnum": 1,
                      "pos_bol": 0,
                      "pos_cnum": 4
                    },
                    "loc_end": {
                      "pos_fname": "test.ml",
                      "pos_lnum": 1,
                      "pos_bol": 0,
                      "pos_cnum": 5
                    },
                    "loc_ghost": false
                  }
                }
              },
              "ppat_loc": {
                "loc_start": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 1,
                  "pos_bol": 0,
                  "pos_cnum": 4
                },
                "loc_end": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 1,
                  "pos_bol": 0,
                  "pos_cnum": 5
                },
                "loc_ghost": false
              },
              "ppat_loc_stack": "__lstack",
              "ppat_attributes": "__attrs"
            },
            "pvb_expr": {
              "pexp_desc": {
                "Pexp_constant": { "Pconst_integer": [ "2", "None" ] }
              },
              "pexp_loc": {
                "loc_start": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 1,
                  "pos_bol": 0,
                  "pos_cnum": 8
                },
                "loc_end": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 1,
                  "pos_bol": 0,
                  "pos_cnum": 9
                },
                "loc_ghost": false
              },
              "pexp_loc_stack": "__lstack",
              "pexp_attributes": "__attrs"
            },
            "pvb_attributes": "__attrs",
            "pvb_loc": {
              "loc_start": {
                "pos_fname": "test.ml",
                "pos_lnum": 1,
                "pos_bol": 0,
                "pos_cnum": 0
              },
              "loc_end": {
                "pos_fname": "test.ml",
                "pos_lnum": 1,
                "pos_bol": 0,
                "pos_cnum": 9
              },
              "loc_ghost": false
            }
          }
        ]
      ]
    },
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": {
              "ppat_desc": {
                "Ppat_var": {
                  "txt": "y",
                  "loc": {
                    "loc_start": {
                      "pos_fname": "test.ml",
                      "pos_lnum": 2,
                      "pos_bol": 10,
                      "pos_cnum": 14
                    },
                    "loc_end": {
                      "pos_fname": "test.ml",
                      "pos_lnum": 2,
                      "pos_bol": 10,
                      "pos_cnum": 15
                    },
                    "loc_ghost": false
                  }
                }
              },
              "ppat_loc": {
                "loc_start": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 2,
                  "pos_bol": 10,
                  "pos_cnum": 14
                },
                "loc_end": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 2,
                  "pos_bol": 10,
                  "pos_cnum": 15
                },
                "loc_ghost": false
              },
              "ppat_loc_stack": "__lstack",
              "ppat_attributes": "__attrs"
            },
            "pvb_expr": {
              "pexp_desc": {
                "Pexp_construct": [
                  {
                    "txt": { "Lident": "true" },
                    "loc": {
                      "loc_start": {
                        "pos_fname": "test.ml",
                        "pos_lnum": 2,
                        "pos_bol": 10,
                        "pos_cnum": 18
                      },
                      "loc_end": {
                        "pos_fname": "test.ml",
                        "pos_lnum": 2,
                        "pos_bol": 10,
                        "pos_cnum": 22
                      },
                      "loc_ghost": false
                    }
                  },
                  "None"
                ]
              },
              "pexp_loc": {
                "loc_start": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 2,
                  "pos_bol": 10,
                  "pos_cnum": 18
                },
                "loc_end": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 2,
                  "pos_bol": 10,
                  "pos_cnum": 22
                },
                "loc_ghost": false
              },
              "pexp_loc_stack": "__lstack",
              "pexp_attributes": "__attrs"
            },
            "pvb_attributes": "__attrs",
            "pvb_loc": {
              "loc_start": {
                "pos_fname": "test.ml",
                "pos_lnum": 2,
                "pos_bol": 10,
                "pos_cnum": 10
              },
              "loc_end": {
                "pos_fname": "test.ml",
                "pos_lnum": 2,
                "pos_bol": 10,
                "pos_cnum": 22
              },
              "loc_ghost": false
            }
          }
        ]
      ]
    },
    {
      "Pstr_value": [
        "Nonrecursive",
        [
          {
            "pvb_pat": {
              "ppat_desc": {
                "Ppat_var": {
                  "txt": "z",
                  "loc": {
                    "loc_start": {
                      "pos_fname": "test.ml",
                      "pos_lnum": 3,
                      "pos_bol": 23,
                      "pos_cnum": 27
                    },
                    "loc_end": {
                      "pos_fname": "test.ml",
                      "pos_lnum": 3,
                      "pos_bol": 23,
                      "pos_cnum": 28
                    },
                    "loc_ghost": false
                  }
                }
              },
              "ppat_loc": {
                "loc_start": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 3,
                  "pos_bol": 23,
                  "pos_cnum": 27
                },
                "loc_end": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 3,
                  "pos_bol": 23,
                  "pos_cnum": 28
                },
                "loc_ghost": false
              },
              "ppat_loc_stack": "__lstack",
              "ppat_attributes": "__attrs"
            },
            "pvb_expr": {
              "pexp_desc": {
                "Pexp_fun": [
                  "Nolabel",
                  "None",
                  {
                    "ppat_desc": {
                      "Ppat_var": {
                        "txt": "x",
                        "loc": {
                          "loc_start": {
                            "pos_fname": "test.ml",
                            "pos_lnum": 4,
                            "pos_bol": 31,
                            "pos_cnum": 36
                          },
                          "loc_end": {
                            "pos_fname": "test.ml",
                            "pos_lnum": 4,
                            "pos_bol": 31,
                            "pos_cnum": 37
                          },
                          "loc_ghost": false
                        }
                      }
                    },
                    "ppat_loc": {
                      "loc_start": {
                        "pos_fname": "test.ml",
                        "pos_lnum": 4,
                        "pos_bol": 31,
                        "pos_cnum": 36
                      },
                      "loc_end": {
                        "pos_fname": "test.ml",
                        "pos_lnum": 4,
                        "pos_bol": 31,
                        "pos_cnum": 37
                      },
                      "loc_ghost": false
                    },
                    "ppat_loc_stack": "__lstack",
                    "ppat_attributes": "__attrs"
                  },
                  {
                    "pexp_desc": {
                      "Pexp_ident": {
                        "txt": { "Lident": "x" },
                        "loc": {
                          "loc_start": {
                            "pos_fname": "test.ml",
                            "pos_lnum": 5,
                            "pos_bol": 41,
                            "pos_cnum": 42
                          },
                          "loc_end": {
                            "pos_fname": "test.ml",
                            "pos_lnum": 5,
                            "pos_bol": 41,
                            "pos_cnum": 43
                          },
                          "loc_ghost": false
                        }
                      }
                    },
                    "pexp_loc": {
                      "loc_start": {
                        "pos_fname": "test.ml",
                        "pos_lnum": 5,
                        "pos_bol": 41,
                        "pos_cnum": 42
                      },
                      "loc_end": {
                        "pos_fname": "test.ml",
                        "pos_lnum": 5,
                        "pos_bol": 41,
                        "pos_cnum": 43
                      },
                      "loc_ghost": false
                    },
                    "pexp_loc_stack": "__lstack",
                    "pexp_attributes": "__attrs"
                  }
                ]
              },
              "pexp_loc": {
                "loc_start": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 4,
                  "pos_bol": 31,
                  "pos_cnum": 32
                },
                "loc_end": {
                  "pos_fname": "test.ml",
                  "pos_lnum": 5,
                  "pos_bol": 41,
                  "pos_cnum": 43
                },
                "loc_ghost": false
              },
              "pexp_loc_stack": "__lstack",
              "pexp_attributes": "__attrs"
            },
            "pvb_attributes": "__attrs",
            "pvb_loc": {
              "loc_start": {
                "pos_fname": "test.ml",
                "pos_lnum": 3,
                "pos_bol": 23,
                "pos_cnum": 23
              },
              "loc_end": {
                "pos_fname": "test.ml",
                "pos_lnum": 5,
                "pos_bol": 41,
                "pos_cnum": 43
              },
              "loc_ghost": false
            }
          }
        ]
      ]
    }
  ]
