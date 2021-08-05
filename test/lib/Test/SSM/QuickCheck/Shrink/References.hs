module Test.SSM.QuickCheck.Shrink.References
    ( refs ) where

import SSM.Core.Syntax

import Test.SSM.QuickCheck.Util

import Data.Maybe

refs :: Program -> [Program]
refs = transformProcedures removeAllDeclaredRefs

{- | Given a procedure, this function will return all successful transformations of the
procedure where a transformation is defined as the act of removing one of the declared
references from the program. -}
removeAllDeclaredRefs :: Procedure -> [Procedure]
removeAllDeclaredRefs p = removeRefs p (allRefs p)

removeRefs :: Procedure -> [Reference] -> [Procedure]
removeRefs p refs = 
  let ps = for refs $ \ref -> do
        -- references given to the procedure as an argument are in scope in the procedure
        let initialrefs = map (\(n,t) -> makeDynamicRef n t) $ filter (isReference . snd) $ arguments p
        -- rewrite the procedure body & update the procedure
            body'           = removeVars [refIdent ref] initialrefs (body p)
        return $ p { body = body' }
      
      -- filter out the successful shrinkings and return them
      ps' = filter isJust ps
  in map fromJust ps'

-- | All declared references in a program (expect procedure arguments)
allRefs :: Procedure -> [Reference]
allRefs p = refs $ body p
  where
    refs :: [Stm] -> [Reference]
    refs xs = concat $ for xs $ \x -> case x of
      NewRef n t e -> [makeDynamicRef n t]
      If _ thn els -> refs thn ++ refs els
      While _ bdy  -> refs bdy
      _            -> []
