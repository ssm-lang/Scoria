{- | This module exposes a pretty printer of programs. The programs are turned into
a representation that looks a lot like pseudo-code/C-code.

As an example, the program

@
Program
  { entry = "fun1"
  , args = [ Right ("ref2",Ref TUInt64)
           , Right ("ref4",Ref TInt32)
           , Right ("ref5",Ref TBool)
           , Right ("ref8",Ref TInt64)
           , Right ("ref9",Ref TInt64)
           ]
  , funs = fromList
      [ ("fun1",Procedure
          { name = "fun1"
          , arguments = [ ("ref2",Ref TUInt64)
                        , ("ref4",Ref TInt32)
                        , ("ref5",Ref TBool)
                        , ("ref8",Ref TInt64)
                        , ("ref9",Ref TInt64)
                        ]
          , body = [ Fork [ ("fun1", [ Right ("ref2",Ref TUInt64)
                                     , Right ("ref4",Ref TInt32)
                                     , Right ("ref5",Ref TBool)
                                     , Right ("ref9",Ref TInt64)
                                     , Right ("ref8",Ref TInt64)
                                     ])
                          , ("fun1", [ Right ("ref2",Ref TUInt64)
                                     , Right ("ref4",Ref TInt32)
                                     , Right ("ref5",Ref TBool)
                                     , Right ("ref9",Ref TInt64)
                                     , Right ("ref8",Ref TInt64)
                                     ])
                          ]
                   , GetRef (Fresh "v0") TUInt64 ("ref2",Ref TUInt64)
                   ]
          })
      ]
  }
@

is pretty-printed to the string

@
entrypoint:
  fun1(ref2, ref4, ref5, ref8, ref9)

fun1(*uint64 ref2, *int ref4, *bool ref5, *int64 ref8, *int64 ref9) {
  fork [ fun1(ref2, ref4, ref5, ref9, ref8)
       , fun1(ref2, ref4, ref5, ref9, ref8)
       ]
  uint64 v0 = *ref2
}
@

which is at least slightly more readable.-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module SSM.Pretty
    ( -- * Prettyprint SSM programs
      prettySSM
      -- * Prettyprint low level programs
    , prettyProgram
    ) where

--import SSM.Core.Syntax ( SSM )
import SSM.Core.Program
import SSM.Pretty.Syntax ( prettyProgram )
import SSM.Core.Backend
import SSM.Frontend.Compile

prettySSM :: Compile PrettyPrint () -> String
prettySSM p = prettyProgram $ toProgram p
