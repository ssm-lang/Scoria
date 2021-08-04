{- | It's not very clear what this example tests, but it did trigger a bug where
the order of forked child processes differed between the interpreter and the
compiled code..
-}
module Regression.FiveForkerSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = Ident "fun0" Nothing
  , args  = []
  , funs  = fromList
    [ ( Ident "fun0" Nothing
      , Procedure
        { name      = Ident "fun0" Nothing
        , arguments = []
        , body      =
          [ NewRef (Ident "ref1" Nothing) TInt32 (Lit TInt32 (LInt32 0))
          , NewRef (Ident "ref2" Nothing) TUInt64 (Lit TUInt64 (LUInt64 0))
          , NewRef (Ident "ref4" Nothing) TUInt64 (Lit TUInt64 (LUInt64 0))
          , Fork
            [ ( Ident "fun5" Nothing
              , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
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
    , (Ident "fun2" Nothing, Procedure { name = Ident "fun2" Nothing, arguments = [], body = [] })
    , ( Ident "fun5" Nothing
      , Procedure
        { name      = Ident "fun5" Nothing
        , arguments = [ (Ident "ref1" Nothing, Ref TInt32)
                      , (Ident "ref2" Nothing, Ref TUInt64)
                      , (Ident "ref4" Nothing, Ref TUInt64)
                      , ((Ident "var5" Nothing), TInt32)
                      , ((Ident "var6" Nothing), TBool)
                      , ((Ident "var7" Nothing), TInt32)
                      ]
        , body      =
          [ If
            (Var TBool (Ident "var6" Nothing))
            [ Fork
              [ ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Left
                    (UOpE
                      TInt32
                      (BOp
                        TInt32
                        (BOp TInt32
                             (Lit TInt32 (LInt32 208))
                             (Var TInt32 (Ident "var7" Nothing))
                             OMinus
                        )
                        (BOp TInt32
                             (Var TInt32 (Ident "var7" Nothing))
                             (Var TInt32 (Ident "var5" Nothing))
                             OPlus
                        )
                        OTimes
                      )
                      Neg
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OPlus)
                      (BOp TInt32 (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OPlus)
                      OLT
                    )
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OPlus)
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OPlus)
                      OPlus
                    )
                  ]
                )
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic ((Ident "ref1" Nothing), Ref TInt32))
                  , Right (Dynamic ((Ident "ref4" Nothing), Ref TUInt64))
                  , Right (Dynamic ((Ident "ref4" Nothing), Ref TUInt64))
                  , Left
                    (BOp TInt32 (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OMinus)
                  , Left
                    (BOp TBool
                         (Lit TBool (LBool True))
                         (Lit TBool (LBool True))
                         OEQ
                    )
                  , Left (Var TInt32 (Ident "var7" Nothing))
                  ]
                )
              , (Ident "fun2" Nothing, [])
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Left (Var TInt32 (Ident "var7" Nothing))
                  , Left
                    (BOp
                      TBool
                      (UOpE TInt32 (Lit TInt32 (LInt32 19)) Neg)
                      (BOp TInt32 (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OTimes
                      )
                      OLT
                    )
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OPlus)
                      (BOp TInt32 (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OPlus)
                      OMinus
                    )
                  ]
                )
              , (Ident "fun2" Nothing, [])
              ]
            , Fork
              [ (Ident "fun2" Nothing, [])
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Left (Var TInt32 (Ident "var5" Nothing))
                  , Left (Lit TBool (LBool True))
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 0))
                           (Var TInt32 (Ident "var5" Nothing))
                           OPlus
                      )
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OTimes
                      )
                      OPlus
                    )
                  ]
                )
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Left
                    (BOp TInt32 (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OMinus)
                  , Left
                    (BOp
                      TBool
                      (BOp TBool
                           (Lit TBool (LBool True))
                           (Lit TBool (LBool True))
                           OEQ
                      )
                      (BOp TBool (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OLT)
                      OEQ
                    )
                  , Left
                    (BOp TInt32
                         (Lit TInt32 (LInt32 80))
                         (Var TInt32 (Ident "var7" Nothing))
                         OMinus
                    )
                  ]
                )
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Left
                    (BOp TInt32
                         (Var TInt32 (Ident "var5" Nothing))
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
                           (Var TInt32 (Ident "var7" Nothing))
                           OPlus
                      )
                      OLT
                    )
                  , Left
                    (UOpE
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 77))
                           (Var TInt32 (Ident "var5" Nothing))
                           OTimes
                      )
                      Neg
                    )
                  ]
                )
              , (Ident "fun2" Nothing, [])
              ]
            , If
              (BOp TBool (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OLT)
              [ After
                (Lit TUInt64 (LUInt64 1112))
                (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                (BOp
                  TInt32
                  (BOp TInt32
                       (Lit TInt32 (LInt32 81))
                       (Lit TInt32 (LInt32 38))
                       OMinus
                  )
                  (BOp TInt32 (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OTimes)
                  OTimes
                )
              , Fork
                [ ( Ident "fun5" Nothing
                  , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                    , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                    , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
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
                             (Var TInt32 (Ident "var7" Nothing))
                             (Lit TInt32 (LInt32 206))
                             OLT
                        )
                        (BOp TBool
                             (Lit TInt32 (LInt32 0))
                             (Var TInt32 (Ident "var5" Nothing))
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
                               (Var TInt32 (Ident "var5" Nothing))
                               OMinus
                          )
                          (UOpE TInt32 (Lit TInt32 (LInt32 113)) Neg)
                          OTimes
                        )
                        Neg
                      )
                    ]
                  )
                , ( Ident "fun5" Nothing
                  , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                    , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                    , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                    , Left
                      (UOpE
                        TInt32
                        (BOp TInt32
                             (Var TInt32 (Ident "var5" Nothing))
                             (Lit TInt32 (LInt32 158))
                             OTimes
                        )
                        Neg
                      )
                    , Left (Var TBool (Ident "var6" Nothing))
                    , Left (Var TInt32 (Ident "var5" Nothing))
                    ]
                  )
                , (Ident "fun2" Nothing, [])
                , (Ident "fun2" Nothing, [])
                , (Ident "fun2" Nothing, [])
                ]
              ]
              [ Fork
                  [ (Ident "fun2" Nothing, [])
                  , ( Ident "fun5" Nothing
                    , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                      , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                      , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                      , Left (Var TInt32 (Ident "var7" Nothing))
                      , Left
                        (BOp TBool (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OLT)
                      , Left
                        (BOp TInt32
                             (Var TInt32 (Ident "var5" Nothing))
                             (Var TInt32 (Ident "var5" Nothing))
                             OMinus
                        )
                      ]
                    )
                  , ( Ident "fun5" Nothing
                    , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                      , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                      , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                      , Left
                        (BOp
                          TInt32
                          (BOp TInt32
                               (Var TInt32 (Ident "var5" Nothing))
                               (Var TInt32 (Ident "var7" Nothing))
                               OPlus
                          )
                          (BOp TInt32
                               (Lit TInt32 (LInt32 179))
                               (Var TInt32 (Ident "var5" Nothing))
                               OTimes
                          )
                          OPlus
                        )
                      , Left
                        (BOp
                          TBool
                          (BOp TInt32
                               (Var TInt32 (Ident "var5" Nothing))
                               (Lit TInt32 (LInt32 19))
                               OMinus
                          )
                          (BOp TInt32
                               (Var TInt32 (Ident "var7" Nothing))
                               (Var TInt32 (Ident "var5" Nothing))
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
                  , (Ident "fun2" Nothing, [])
                  , (Ident "fun2" Nothing, [])
                  ]
              ]
            , Wait [Dynamic (Ident "ref1" Nothing, Ref TInt32), Dynamic (Ident "ref2" Nothing, Ref TUInt64)]
            ]
            [ Fork
              [ ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Left (Var TInt32 (Ident "var7" Nothing))
                  , Left (Lit TBool (LBool True))
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OPlus)
                      (BOp TInt32
                           (Lit TInt32 (LInt32 24))
                           (Var TInt32 (Ident "var5" Nothing))
                           OPlus
                      )
                      OMinus
                    )
                  ]
                )
              , (Ident "fun2" Nothing, [])
              , (Ident "fun2" Nothing, [])
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 211))
                           (Var TInt32 (Ident "var7" Nothing))
                           OMinus
                      )
                      (UOpE TInt32 (Var TInt32 (Ident "var5" Nothing)) Neg)
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
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OTimes
                      )
                      OLT
                    )
                  , Left
                    (BOp TInt32 (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OMinus)
                  ]
                )
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
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
                           (Var TInt32 (Ident "var7" Nothing))
                           OPlus
                      )
                      OEQ
                    )
                  , Left (Lit TInt32 (LInt32 106))
                  ]
                )
              ]
            , Fork
              [ ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OTimes
                      )
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OTimes
                      )
                      OMinus
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32
                           (Var TInt32 (Ident "var7" Nothing))
                           (Lit TInt32 (LInt32 60))
                           OTimes
                      )
                      (BOp TInt32
                           (Var TInt32 (Ident "var5" Nothing))
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
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OTimes
                      )
                      OTimes
                    )
                  ]
                )
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 12))
                           (Var TInt32 (Ident "var7" Nothing))
                           OMinus
                      )
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OPlus)
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
                      (BOp TBool (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OLT)
                      OEQ
                    )
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 57))
                           (Var TInt32 (Ident "var7" Nothing))
                           OPlus
                      )
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OMinus
                      )
                      OMinus
                    )
                  ]
                )
              , ( Ident "fun5" Nothing
                , [ Right (Dynamic (Ident "ref1" Nothing, Ref TInt32))
                  , Right (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  , Right (Dynamic (Ident "ref4" Nothing, Ref TUInt64))
                  , Left
                    (BOp
                      TInt32
                      (BOp TInt32
                           (Lit TInt32 (LInt32 12))
                           (Var TInt32 (Ident "var5" Nothing))
                           OTimes
                      )
                      (UOpE TInt32 (Var TInt32 (Ident "var7" Nothing)) Neg)
                      OMinus
                    )
                  , Left
                    (BOp
                      TBool
                      (BOp TInt32 (Var TInt32 (Ident "var5" Nothing)) (Var TInt32 (Ident "var7" Nothing)) OMinus
                      )
                      (BOp TInt32
                           (Var TInt32 (Ident "var7" Nothing))
                           (Lit TInt32 (LInt32 147))
                           OTimes
                      )
                      OLT
                    )
                  , Left
                    (BOp TInt32 (Var TInt32 (Ident "var7" Nothing)) (Var TInt32 (Ident "var5" Nothing)) OPlus)
                  ]
                )
              , (Ident "fun2" Nothing, [])
              , (Ident "fun2" Nothing, [])
              ]
            ]
          , Wait [Dynamic (Ident "ref2" Nothing, Ref TUInt64)]
          , Wait [Dynamic (Ident "ref1" Nothing, Ref TInt32)]
          , After (Lit TUInt64 (LUInt64 3525))
                  (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
                  (Lit TUInt64 (LUInt64 167))
          , Wait [(Dynamic (Ident "ref1" Nothing, Ref TInt32))]
          , Wait [(Dynamic (Ident "ref4" Nothing, Ref TUInt64))]
          , After
            (Lit TUInt64 (LUInt64 4696))
            (Dynamic (Ident "ref2" Nothing, Ref TUInt64))
            (BOp
              TUInt64
              (BOp TUInt64
                   (Lit TUInt64 (LUInt64 7582))
                   (UOpR TUInt64 (Dynamic (Ident "ref2" Nothing, Ref TUInt64)) Deref)
                   OMinus
              )
              (BOp TUInt64
                   (Lit TUInt64 (LUInt64 17605))
                   (Lit TUInt64 (LUInt64 63057))
                   OTimes
              )
              OMinus
            )
          , Fork [(Ident "fun2" Nothing, [])]
          , Fork [(Ident "fun2" Nothing, [])]
          ]
        }
      )
    ]
  , global_references = []}

spec :: H.Spec
spec = T.semanticIncorrectSpec "FiveForker" p
