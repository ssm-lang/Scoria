module Regression.FiveForkerSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun0"
  , args  = []
  , funs  = fromList
    [ ( "fun0"
      , Procedure
        { name      = "fun0"
        , arguments = []
        , body      =
          [ NewRef (Fresh "ref1") (Ref TInt32) (Lit TInt32 (LInt32 0))
          , NewRef (Fresh "ref2") (Ref TUInt64) (Lit TUInt64 (LUInt64 0))
          , NewRef (Fresh "ref4") (Ref TUInt64) (Lit TUInt64 (LUInt64 0))
          , Fork
            [ ( "fun5"
              , [ Right ("ref1", Ref TInt32)
                , Right ("ref2", Ref TUInt64)
                , Right ("ref4", Ref TUInt64)
                , Left
                  (BOp TInt32
                       (Lit TInt32 (LInt32 144))
                       (Lit TInt32 (LInt32 202))
                       OPlus
                  )
                , Left
                  (BOp TBool
                       (Lit TInt32 (LInt32 141))
                       (Lit TInt32 (LInt32 68))
                       OLT
                  )
                , Left
                  (BOp TInt32
                       (Lit TInt32 (LInt32 210))
                       (Lit TInt32 (LInt32 71))
                       OMinus
                  )
                ]
              )
            ]
          ]
        }
      )
    , ("fun2", Procedure { name = "fun2", arguments = [], body = [] })
    , ( "fun5"
      , Procedure
        { name      = "fun5"
        , arguments = [ ("ref1", Ref TInt32)
                      , ("ref2", Ref TUInt64)
                      , ("ref4", Ref TUInt64)
                      , ("var5", TInt32)
                      , ("var6", TBool)
                      , ("var7", TInt32)
                      ]
        , body      =
          [ If
            (Var TBool "var6")
            [ Fork
              [ ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref2", Ref TUInt64)
                  , Right ("ref4", Ref TUInt64)
                  , Left
                    (UOpE
                      TInt32
                      (BOp
                        TInt32
                        (BOp TInt32
                             (Lit TInt32 (LInt32 208))
                             (Var TInt32 "var7")
                             OMinus
                        )
                        (BOp TInt32
                             (Var TInt32 "var7")
                             (Var TInt32 "var5")
                             OPlus
                        )
                        OTimes
                      )
                      Neg
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var5") OPlus)
                      (BOp TInt32 (Var TInt32 "var7") (Var TInt32 "var7") OPlus)
                      OLT
                    )
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OPlus)
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OPlus)
                      OPlus
                    )
                  ]
                )
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref4", Ref TUInt64)
                  , Right ("ref4", Ref TUInt64)
                  , Left
                    (BOp TInt32 (Var TInt32 "var7") (Var TInt32 "var5") OMinus)
                  , Left
                    (BOp TBool
                         (Lit TBool (LBool True))
                         (Lit TBool (LBool True))
                         OEQ
                    )
                  , Left (Var TInt32 "var7")
                  ]
                )
              , ("fun2", [])
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref2", Ref TUInt64)
                  , Right ("ref2", Ref TUInt64)
                  , Left (Var TInt32 "var7")
                  , Left
                    (BOp
                      TBool
                      (UOpE TInt32 (Lit TInt32 (LInt32 19)) Neg)
                      (BOp TInt32 (Var TInt32 "var7") (Var TInt32 "var7") OTimes
                      )
                      OLT
                    )
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OPlus)
                      (BOp TInt32 (Var TInt32 "var7") (Var TInt32 "var5") OPlus)
                      OMinus
                    )
                  ]
                )
              , ("fun2", [])
              ]
            , Fork
              [ ("fun2", [])
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref4", Ref TUInt64)
                  , Right ("ref2", Ref TUInt64)
                  , Left (Var TInt32 "var5")
                  , Left (Lit TBool (LBool True))
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 0))
                           (Var TInt32 "var5")
                           OPlus
                      )
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OTimes
                      )
                      OPlus
                    )
                  ]
                )
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref4", Ref TUInt64)
                  , Right ("ref4", Ref TUInt64)
                  , Left
                    (BOp TInt32 (Var TInt32 "var7") (Var TInt32 "var5") OMinus)
                  , Left
                    (BOp
                      TBool
                      (BOp TBool
                           (Lit TBool (LBool True))
                           (Lit TBool (LBool True))
                           OEQ
                      )
                      (BOp TBool (Var TInt32 "var7") (Var TInt32 "var7") OLT)
                      OEQ
                    )
                  , Left
                    (BOp TInt32
                         (Lit TInt32 (LInt32 80))
                         (Var TInt32 "var7")
                         OMinus
                    )
                  ]
                )
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref4", Ref TUInt64)
                  , Right ("ref2", Ref TUInt64)
                  , Left
                    (BOp TInt32
                         (Var TInt32 "var5")
                         (Lit TInt32 (LInt32 25))
                         OPlus
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32
                           (Lit TInt32 (LInt32 158))
                           (Lit TInt32 (LInt32 25))
                           OPlus
                      )
                      (BOp TInt32
                           (Lit TInt32 (LInt32 148))
                           (Var TInt32 "var7")
                           OPlus
                      )
                      OLT
                    )
                  , Left
                    (UOpE
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 77))
                           (Var TInt32 "var5")
                           OTimes
                      )
                      Neg
                    )
                  ]
                )
              , ("fun2", [])
              ]
            , If
              (BOp TBool (Var TInt32 "var7") (Var TInt32 "var7") OLT)
              [ After
                (Lit TUInt64 (LUInt64 1112))
                ("ref1", Ref TInt32)
                (BOp
                  TInt32
                  (BOp TInt32
                       (Lit TInt32 (LInt32 81))
                       (Lit TInt32 (LInt32 38))
                       OMinus
                  )
                  (BOp TInt32 (Var TInt32 "var7") (Var TInt32 "var7") OTimes)
                  OTimes
                )
              , Fork
                [ ( "fun5"
                  , [ Right ("ref1", Ref TInt32)
                    , Right ("ref4", Ref TUInt64)
                    , Right ("ref4", Ref TUInt64)
                    , Left
                      (BOp TInt32
                           (Lit TInt32 (LInt32 41))
                           (Lit TInt32 (LInt32 16))
                           OPlus
                      )
                    , Left
                      (BOp
                        TBool
                        (BOp TBool
                             (Var TInt32 "var7")
                             (Lit TInt32 (LInt32 206))
                             OLT
                        )
                        (BOp TBool
                             (Lit TInt32 (LInt32 0))
                             (Var TInt32 "var5")
                             OEQ
                        )
                        OEQ
                      )
                    , Left
                      (UOpE
                        TInt32
                        (BOp
                          TInt32
                          (BOp TInt32
                               (Lit TInt32 (LInt32 91))
                               (Var TInt32 "var5")
                               OMinus
                          )
                          (UOpE TInt32 (Lit TInt32 (LInt32 113)) Neg)
                          OTimes
                        )
                        Neg
                      )
                    ]
                  )
                , ( "fun5"
                  , [ Right ("ref1", Ref TInt32)
                    , Right ("ref4", Ref TUInt64)
                    , Right ("ref2", Ref TUInt64)
                    , Left
                      (UOpE
                        TInt32
                        (BOp TInt32
                             (Var TInt32 "var5")
                             (Lit TInt32 (LInt32 158))
                             OTimes
                        )
                        Neg
                      )
                    , Left (Var TBool "var6")
                    , Left (Var TInt32 "var5")
                    ]
                  )
                , ("fun2", [])
                , ("fun2", [])
                , ("fun2", [])
                ]
              ]
              [ Fork
                  [ ("fun2", [])
                  , ( "fun5"
                    , [ Right ("ref1", Ref TInt32)
                      , Right ("ref2", Ref TUInt64)
                      , Right ("ref2", Ref TUInt64)
                      , Left (Var TInt32 "var7")
                      , Left
                        (BOp TBool (Var TInt32 "var5") (Var TInt32 "var5") OLT)
                      , Left
                        (BOp TInt32
                             (Var TInt32 "var5")
                             (Var TInt32 "var5")
                             OMinus
                        )
                      ]
                    )
                  , ( "fun5"
                    , [ Right ("ref1", Ref TInt32)
                      , Right ("ref4", Ref TUInt64)
                      , Right ("ref4", Ref TUInt64)
                      , Left
                        (BOp
                          TInt32
                          (BOp TInt32
                               (Var TInt32 "var5")
                               (Var TInt32 "var7")
                               OPlus
                          )
                          (BOp TInt32
                               (Lit TInt32 (LInt32 179))
                               (Var TInt32 "var5")
                               OTimes
                          )
                          OPlus
                        )
                      , Left
                        (BOp
                          TBool
                          (BOp TInt32
                               (Var TInt32 "var5")
                               (Lit TInt32 (LInt32 19))
                               OMinus
                          )
                          (BOp TInt32
                               (Var TInt32 "var7")
                               (Var TInt32 "var5")
                               OTimes
                          )
                          OEQ
                        )
                      , Left
                        (BOp TInt32
                             (Lit TInt32 (LInt32 156))
                             (Lit TInt32 (LInt32 181))
                             OTimes
                        )
                      ]
                    )
                  , ("fun2", [])
                  , ("fun2", [])
                  ]
              ]
            , Wait [("ref1", Ref TInt32), ("ref2", Ref TUInt64)]
            ]
            [ Fork
              [ ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref4", Ref TUInt64)
                  , Right ("ref2", Ref TUInt64)
                  , Left (Var TInt32 "var7")
                  , Left (Lit TBool (LBool True))
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var5") OPlus)
                      (BOp TInt32
                           (Lit TInt32 (LInt32 24))
                           (Var TInt32 "var5")
                           OPlus
                      )
                      OMinus
                    )
                  ]
                )
              , ("fun2", [])
              , ("fun2", [])
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref4", Ref TUInt64)
                  , Right ("ref2", Ref TUInt64)
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 211))
                           (Var TInt32 "var7")
                           OMinus
                      )
                      (UOpE TInt32 (Var TInt32 "var5") Neg)
                      OTimes
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32
                           (Lit TInt32 (LInt32 208))
                           (Lit TInt32 (LInt32 201))
                           OMinus
                      )
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OTimes
                      )
                      OLT
                    )
                  , Left
                    (BOp TInt32 (Var TInt32 "var7") (Var TInt32 "var5") OMinus)
                  ]
                )
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref2", Ref TUInt64)
                  , Right ("ref4", Ref TUInt64)
                  , Left (Lit TInt32 (LInt32 62))
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32
                           (Lit TInt32 (LInt32 143))
                           (Lit TInt32 (LInt32 3))
                           OTimes
                      )
                      (BOp TInt32
                           (Lit TInt32 (LInt32 155))
                           (Var TInt32 "var7")
                           OPlus
                      )
                      OEQ
                    )
                  , Left (Lit TInt32 (LInt32 106))
                  ]
                )
              ]
            , GetRef (Fresh "v2") TUInt64 ("ref4", Ref TUInt64)
            , Fork
              [ ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref2", Ref TUInt64)
                  , Right ("ref4", Ref TUInt64)
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var5") OTimes
                      )
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OTimes
                      )
                      OMinus
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32
                           (Var TInt32 "var7")
                           (Lit TInt32 (LInt32 60))
                           OTimes
                      )
                      (BOp TInt32
                           (Var TInt32 "var5")
                           (Lit TInt32 (LInt32 205))
                           OPlus
                      )
                      OLT
                    )
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 33))
                           (Lit TInt32 (LInt32 208))
                           OPlus
                      )
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OTimes
                      )
                      OTimes
                    )
                  ]
                )
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref2", Ref TUInt64)
                  , Right ("ref2", Ref TUInt64)
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 12))
                           (Var TInt32 "var7")
                           OMinus
                      )
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var5") OPlus)
                      OTimes
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TBool
                           (Lit TBool (LBool True))
                           (Lit TBool (LBool True))
                           OEQ
                      )
                      (BOp TBool (Var TInt32 "var7") (Var TInt32 "var7") OLT)
                      OEQ
                    )
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 57))
                           (Var TInt32 "var7")
                           OPlus
                      )
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OMinus
                      )
                      OMinus
                    )
                  ]
                )
              , ( "fun5"
                , [ Right ("ref1", Ref TInt32)
                  , Right ("ref2", Ref TUInt64)
                  , Right ("ref4", Ref TUInt64)
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 12))
                           (Var TInt32 "var5")
                           OTimes
                      )
                      (UOpE TInt32 (Var TInt32 "var7") Neg)
                      OMinus
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32 (Var TInt32 "var5") (Var TInt32 "var7") OMinus
                      )
                      (BOp TInt32
                           (Var TInt32 "var7")
                           (Lit TInt32 (LInt32 147))
                           OTimes
                      )
                      OLT
                    )
                  , Left
                    (BOp TInt32 (Var TInt32 "var7") (Var TInt32 "var5") OPlus)
                  ]
                )
              , ("fun2", [])
              , ("fun2", [])
              ]
            ]
          , GetRef (Fresh "v4") TInt32 ("ref1", Ref TInt32)
          , Wait [("ref2", Ref TUInt64)]
          , Wait [("ref1", Ref TInt32)]
          , GetRef (Fresh "v5") TUInt64 ("ref4", Ref TUInt64)
          , GetRef (Fresh "v6") TUInt64 ("ref2", Ref TUInt64)
          , After (Lit TUInt64 (LUInt64 3525))
                  ("ref2", Ref TUInt64)
                  (Lit TUInt64 (LUInt64 167))
          , GetRef (Fresh "v11") TUInt64 ("ref2", Ref TUInt64)
          , Wait [("ref1", Ref TInt32)]
          , Wait [("ref4", Ref TUInt64)]
          , After
            (Lit TUInt64 (LUInt64 4696))
            ("ref2", Ref TUInt64)
            (BOp
              TUInt64
              (BOp TUInt64
                   (Lit TUInt64 (LUInt64 7582))
                   (Var TUInt64 "v11")
                   OMinus
              )
              (BOp TUInt64
                   (Lit TUInt64 (LUInt64 17605))
                   (Lit TUInt64 (LUInt64 63057))
                   OTimes
              )
              OMinus
            )
          , Fork [("fun2", [])]
          , Fork [("fun2", [])]
          ]
        }
      )
    ]
  }

spec :: H.Spec
spec = T.semanticIncorrectSpec "FiveForker" p
