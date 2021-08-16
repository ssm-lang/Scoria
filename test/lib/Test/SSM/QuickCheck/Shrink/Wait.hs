module Test.SSM.QuickCheck.Shrink.Wait
    ( waits
    ) where

import           SSM.Core.Syntax
import           SSM.Util.HughesList     hiding ( (++) )

import           Test.SSM.QuickCheck.Util

import           Data.List

{- | Shrinks a program into several sub programs by making every wait statement
one reference smaller, if the wait statement has more than 1 reference. -}
waits :: Program -> [Program]
waits = transformProcedures shrinkWaitProcedure

shrinkWaitProcedure :: Procedure -> [Procedure]
shrinkWaitProcedure p =
    [ p { body = body' } | body' <- shrinkWaits (emptyHughes, (body p)) ]

{- | Takes the body of a procedure and returns all variants of that body where each
variant contains a shrunken wait statement. -}
shrinkWaits :: (Hughes Stm, [Stm]) -> [[Stm]]
shrinkWaits (_, []) = []
shrinkWaits (front, If c thn els : xs) =
    let
        front' = fromHughes front
        thns =
            [ front' ++ (If c thn' els : xs)
            | thn' <- shrinkWaits (emptyHughes, thn)
            ]
        elss =
            [ front' ++ (If c thn els' : xs)
            | els' <- shrinkWaits (emptyHughes, els)
            ]
    in
        thns ++ elss ++ shrinkWaits (snoc front (If c thn els), xs)
shrinkWaits (front, While c bdy : xs) =
    let front' = fromHughes front
        curr =
            [ front' ++ (While c bdy' : xs)
            | bdy' <- shrinkWaits (emptyHughes, bdy)
            ]
    in  curr ++ shrinkWaits (snoc front (While c bdy), xs)
shrinkWaits (front, x : xs) = case isWait (x : xs) of
    Just (waitstm, rest) ->
        let front' = fromHughes front
            curr =
                (front' ++ rest)
                    : [ front' ++ waitstm' ++ rest
                      | waitstm' <- removeOneSensitizepair waitstm
                      ]
        in  curr ++ shrinkWaits (front <> toHughes waitstm, rest)
    Nothing -> shrinkWaits (snoc front x, xs)

{- | The input is a wait statement, in the form of a sequence of constituent statements.
Wait statement = [... sensitize statements ... yield ... desensitize statements ...].
Returns all variants of this wait statement where one reference has been removed in each
variant.

@
> removeOneSensitizePair [ Sensitize r1
                         , Sensitize r2
                         , Sensitize r3
                         , Yield
                         , Desensitize r1
                         , Desensitize r2
                         , Desensitize r3
                         ]
[ Sensitize r2
, Sensitize r3
, Yield
, Desensitize r2
, Desensitize r3
]
:
[ Sensitize r1
, Sensitize r3
, Yield
, Desensitize r1
, Desensitize r3
]
:
[ Sensitize r1
, Sensitize r2
, Yield
, Desensitize r1
, Desensitize r2
]
:
[]
@
-}
removeOneSensitizepair :: [Stm] -> [[Stm]]
removeOneSensitizepair []       = []
removeOneSensitizepair (x : xs) = case x of
    Sensitize _ ->
        let (sensitizes, (yield : desensitizes)) =
                takeWhileAndRest isSensitize (x : xs)
        in  if length sensitizes == 1
                then []
                else if isYield yield
                    then
                        [ sensitizes' ++ [yield] ++ desensitizes'
                        | (sensitizes', desensitizes') <- removeAllIths
                            sensitizes
                            desensitizes
                        ]
                    else []
    If c thn els -> undefined
    While c bdy  -> undefined
    _            -> undefined
  where
    -- | Get the @Reference@ out of a @Sensitize@ or @Desensitize@-statement.
    stmRef :: Stm -> Reference
    stmRef (Sensitize   r) = r
    stmRef (Desensitize r) = r
    stmRef _               = error "shrinking error - wait"

    {- | Returns all sublists acquired by removing all pairwise elements.
    
    @
    > removeAllIths [1,2,3] [4,5,6]
    ([2,3],[5,6]) : ([1,3],[4,6]) : ([2,3],[5,6]) : []
    @
    -}
    removeAllIths :: [a] -> [b] -> [([a], [b])]
    removeAllIths xs ys = go [] [] xs ys
      where
        go _ _ [] _  = []
        go _ _ _  [] = []
        go hx hy (x : xs) (y : ys) =
            (hx ++ xs, hy ++ ys) : go (hx ++ [x]) (hy ++ [y]) xs ys

{- | If the sequence of statements begin with a wait statement (which is made up of some
sensitize statements, a yield statement and a corresponding number of desensitize
statements), return @Just (the wait statement, the rest of the statements)@, and
otherwise returns @Nothing@.

@
> isWait [Sensitize r, Yield, Desensitize r, stm1, stm2, ...]
Just ([Sensitize r, Yield, Desensitize], [stm1, stm2, ...])
@
-}
isWait :: [Stm] -> Maybe ([Stm], [Stm])
isWait stmts = if isSensitize $ head stmts
    then
        let (sensitizes  , (x : xs)) = takeWhileAndRest isSensitize stmts
            (desensitizes, rest    ) = takeWhileAndRest isDesensitize xs
        in  if isYield x && length sensitizes == length desensitizes
                then Just (sensitizes ++ [x] ++ desensitizes, rest)
                else Nothing
    else Nothing

-- | Is the statement a @Sensitize@ statement?
isSensitize :: Stm -> Bool
isSensitize (Sensitize _) = True
isSensitize _             = False

-- | Is the statement a @Desensitize@ statement?
isDesensitize :: Stm -> Bool
isDesensitize (Desensitize _) = True
isDesensitize _               = False

-- | Is the statement a @Yield@ statement?
isYield :: Stm -> Bool
isYield Yield = True
isYield _     = False
