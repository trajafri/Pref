module CpserTests
  ( cpserTestList
  ) where

import Data.Map as M
import Exp
import Test.HUnit
import Transform.CPS

-- TODO: Use parser here instead of making the ast by hand.
errorMsg = " cpsed incorrectly"

allTests =
  [ TestCase $ assertEqual (show test ++ errorMsg) e (cpser test)
  | (e, test) <-
      [ (NLiteral 1, NLiteral 1)
      , (SLiteral "S", SLiteral "S")
      , (Id "x", Id "x")
      , (Lambda ["x", "k"] (App (Id "k") [Id "x"]), Lambda ["x"] (Id "x"))
      , ( Lambda
            ["x", "k"]
            (App (Id "a") [Id "b", Lambda ["arg0"] (App (Id "k") [Id "arg0"])])
        , Lambda ["x"] (App (Id "a") [Id "b"]))
      , ( Lambda
            ["x", "k"]
            (App
               (Id "b")
               [ Id "c"
               , Lambda
                   ["arg0"]
                   (App
                      (Id "a")
                      [Id "arg0", Lambda ["arg1"] (App (Id "k") [Id "arg1"])])
               ])
        , Lambda ["x"] (App (Id "a") [App (Id "b") [Id "c"]]))
      , ( Lambda
            ["x", "k"]
            (App
               (Id "a")
               [ Id "b"
               , Lambda
                   ["arg0"]
                   (App
                      (Id "e")
                      [ Id "f"
                      , Lambda
                          ["arg1"]
                          (App
                             (Id "d")
                             [ Id "arg1"
                             , Lambda
                                 ["arg2"]
                                 (App
                                    (Id "c")
                                    [ Id "arg2"
                                    , Lambda
                                        ["arg3"]
                                        (App
                                           (Id "func")
                                           [ Id "arg0"
                                           , Id "arg3"
                                           , Lambda
                                               ["arg4"]
                                               (App (Id "k") [Id "arg4"])
                                           ])
                                    ])
                             ])
                      ])
               ])
        , Lambda
            ["x"]
            (App
               (Id "func")
               [ App (Id "a") [Id "b"]
               , App (Id "c") [App (Id "d") [App (Id "e") [Id "f"]]]
               ]))
        -- Example below is bin-to-decimal from C311's cpsing assignment
      , ( Lambda ["n", "k"] $
          App
            (Id "empty")
            [ Id "n"
            , Lambda ["arg0"] $
              If
                (Id "arg0")
                (App (Id "k") [NLiteral 0])
                (App
                   (Id "car")
                   [ Id "n"
                   , Lambda ["arg0"] $
                     (App
                        (Id "cdr")
                        [ Id "n"
                        , Lambda ["arg1"] $
                          App
                            (Id "bin-to-decimal")
                            [ Id "arg1"
                            , Lambda ["arg2"] $
                              App
                                (Id "*")
                                [ NLiteral 2
                                , Id "arg2"
                                , Lambda ["arg3"] $
                                  App
                                    (Id "+")
                                    [ Id "arg0"
                                    , Id "arg3"
                                    , Lambda ["arg4"] $ App (Id "k") [Id "arg4"]
                                    ]
                                ]
                            ]
                        ])
                   ])
            ]
        , Lambda ["n"] $
          If
            (App (Id "empty") [Id "n"])
            (NLiteral 0)
            (App
               (Id "+")
               [ App (Id "car") [Id "n"]
               , App
                   (Id "*")
                   [ NLiteral 2
                   , App (Id "bin-to-decimal") [App (Id "cdr") [Id "n"]]
                   ]
               ]))
      ]
  ]

cpserTestList =
  TestList [TestLabel ("test " ++ show i) t | (i, t) <- zip [1,2 ..] allTests]
