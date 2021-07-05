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
        let initialrefs = filter (isReference . snd) $ arguments p
        -- rewrite the procedure body & update the procedure
        body'           <- removeRef' ref [] initialrefs (body p)
        return $ p { body = body' }
      
      -- filter out the successful shrinkings and return them
      ps' = filter isJust ps
  in map fromJust ps'
  
  where
    -- | Tries to remove a reference from a procedure. If successful it returns the new
    -- procedure, and if not it will return Nothing.
    removeRef' :: Reference    -- ^ Reference to remove
               -> [(Name, Type)]   -- ^ Variables that are no longer valid
               -> [Reference]  -- ^ References that are valid
               -> [Stm]        -- ^ Procedure body to transform
               -> Maybe [Stm]
    removeRef' _   _    _    []     = Just []
    removeRef' ref vars refs (x:xs) = case x of

      NewRef n t e -> let r = (getVarName n, t) in
        if r == ref
          then removeRef' ref vars refs xs
          else let stm = NewRef n t (rewriteExp vars ref e)
               in (:) stm <$> removeRef' ref vars (r:refs) xs

      GetRef n t r -> if r == ref
        then removeRef' ref ((n,t):vars) refs xs
        else (:) x <$> removeRef' ref vars refs xs

      SetRef r e -> if r == ref
        then removeRef' ref vars refs xs
        else let stm = SetRef r (rewriteExp vars ref e)
             in (:) stm <$> removeRef' ref vars refs xs

      SetLocal n t e -> if (n,t) `elem` vars
        then removeRef' ref vars refs xs
        else let stm = SetLocal n t (rewriteExp vars ref e)
             in (:) stm <$> removeRef' ref vars refs xs

      If c thn els -> do
        let c' = rewriteExp vars ref c
        thn'  <- removeRef' ref vars refs thn
        els'  <- removeRef' ref vars refs els
        let stm = If c' thn' els'
        (:) stm <$> removeRef' ref vars refs xs

      While c bdy -> do
        let c'  = rewriteExp vars ref c
        bdy'   <- removeRef' ref vars refs bdy
        let stm = While c' bdy'
        (:) stm <$> removeRef' ref vars refs xs

      Skip -> (:) Skip <$> removeRef' ref vars refs xs

      After d r v -> if r == ref
        then removeRef' ref vars refs xs
        else let stm = After (rewriteExp vars ref d) r (rewriteExp vars ref v)
             in (:) stm <$> removeRef' ref vars refs xs

      Wait references -> let references' = filter ((/=) ref) references in
        if null references'
          -- if the `Wait` statement now becomes empty, just remove it all together.
          then removeRef' ref vars refs xs
          else (:) (Wait references') <$> removeRef' ref vars refs xs

      Fork procs -> case sequence (map removeRefFromFork procs) of
        Just procs' -> (:) (Fork procs') <$> removeRef' ref vars refs xs
        Nothing     -> Nothing

      where
        {- | Transform an expression into one that contains no invalid variables.
        The list @vars@ contain variables that are no longer valid. -}
        deleteVars :: [(Name, Type)] -> SSMExp -> SSMExp
        deleteVars vars e = case e of
          {- If an expression is using a variable that is no longer valid, we replace it
          with a literal that has the same type. -}
          Var t n        -> if n `elem` map (getVarName . fst) vars
                              then defaultValue t
                              else e
          BOp t e1 e2 op -> BOp t (deleteVars vars e1) (deleteVars vars e2) op
          UOpE t e op    -> UOpE t (deleteVars vars e) op
          otherwise      -> otherwise
          where
            -- Returns a literal representing some default value of a type
            defaultValue :: Type -> SSMExp
            defaultValue TUInt8  = Lit TUInt8  $ LUInt8  1
            defaultValue TUInt64 = Lit TUInt64 $ LUInt64 1
            defaultValue TInt32  = Lit TInt32  $ LInt32  1
            defaultValue TInt64  = Lit TInt64  $ LInt64  1
            defaultValue TBool   = Lit TBool   $ LBool True
            defaultValue (Ref _) = error "expression contains reference - ill-typed"

        {- | Takes a reference and an expression and removes any occurence of the
        reference with a literal instead. Currently references only appear in the
        changed operator which we knows evaluates to a Bool, so this transformation
        is easy. Might become trickier if we add more variants later? -}
        deleteRef :: Reference -> SSMExp -> SSMExp
        deleteRef r e = case e of
          BOp t e1 e2 op -> BOp t (deleteRef r e1) (deleteRef r e2) op
          UOpR t r op    -> case op of
            Changed -> Lit TBool $ LBool True -- new default value if we remove it
          otherwise      -> otherwise

        {- | Takes a list of invalid variables and the reference that we are trying to
        remove, and an expression, and returns the same expression after transforming
        it by removing any occurence of the invalid variables or references. -}
        rewriteExp :: [(Name, Type)] -> Reference -> SSMExp -> SSMExp
        rewriteExp vars r e = deleteRef r (deleteVars vars e)

        -- | Try to remove a reference from a procedure call. This is done by replacing
        -- it with an arbitrary reference in scope of the same type.
        removeRefFromFork :: (String, [Either SSMExp Reference])
                          -> Maybe (String, [Either SSMExp Reference])
        removeRefFromFork (n, args) = do
          args' <- sequence $ for args $ \a -> case a of
                Left e  -> Just $ Left $ rewriteExp vars ref e
                Right r -> if r == ref
                             then replaceRef $ snd r
                             else Just $ Right r
          return (n, args')
        
        -- | Try to replace a reference with a reference of the same type, if one
        -- exists. NOTE: The choice to take the head here is arbitrary. Discuss
        -- with Koen.
        replaceRef :: Type -> Maybe (Either SSMExp Reference)
        replaceRef t = case filter ((==) t . snd) refs of
            []    -> Nothing
            (x:_) -> Just $ Right x

-- | All declared references in a program (expect procedure arguments)
allRefs :: Procedure -> [(String, Type)]
allRefs p = refs $ body p
  where
    refs :: [Stm] -> [(String, Type)]
    refs xs = concat $ for xs $ \x -> case x of
      NewRef n t e -> [(getVarName n,t)]
      If _ thn els -> refs thn ++ refs els
      While _ bdy  -> refs bdy
      _            -> []
