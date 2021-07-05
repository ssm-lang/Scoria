module Test.SSM.QuickCheck.Shrink.Expressions where

import SSM.Core.Syntax

import Test.SSM.QuickCheck.Util
    ( transformProcedures, distributeMutate )

{- | Shrink the expressions in a program. Each program in the output has at most
and at least one expression shrunk. -}
expressions :: Program -> [Program]
expressions = transformProcedures shrinkExpInProcedure

{- | Take a procedure and produce a list of mutated procedures, where each mutation has
one expression shrunk. -}
shrinkExpInProcedure :: Procedure -> [Procedure]
shrinkExpInProcedure p =
    [ p { body = body' }
    | body' <- distributeMutate (body p) (shrinkExpInStatement shrinkExp)
    ]
    -- When we implement e.g constant folding we can add that shrinking step by
    -- commenting out this and replacing -your folder- with a function
    -- @SSMExp -> [SSMExp]@ which does the folding, if possible. Empty list would
    -- signify unsuccessful folding.
    {- ++ [ p { body = body' }
       | body' <- distributeMutate (body p) (shrinkExpInStatement -your folder-) ]
    -}

{- | Shrink expressions in statements. Meant to be used as a parameter to
distributeMutate along with the body of a procedure. -}
shrinkExpInStatement :: (SSMExp -> [SSMExp]) -> Stm -> [Stm]
shrinkExpInStatement shrinkexp stm = case stm of
    NewRef n t e   -> [ NewRef n t e'   | e' <- shrinkexp e ]
    SetRef r e     -> [ SetRef r e'     | e' <- shrinkexp e ]
    SetLocal n t e -> [ SetLocal n t e' | e' <- shrinkexp e ]
    -- don't shrink the delay for fear of producing an illegal delay (< now)
    After d r v    -> [ After d r v'    | v' <- shrinkexp v ]
    If c thn els   -> [ If c' thn els   | c' <- shrinkexp c ] ++
                      [ If c thn' els
                      | thn' <- distributeMutate thn (shrinkExpInStatement shrinkexp) ] ++
                      [ If c thn els'
                      | els' <- distributeMutate els (shrinkExpInStatement shrinkexp) ] ++
                      []
    While c bdy    -> [ While c' bdy    | c' <- shrinkexp c ] ++
                      [ While c bdy'
                      | bdy' <- distributeMutate bdy (shrinkExpInStatement shrinkexp) ]
    Fork procs     -> [ Fork procs' | procs' <- distributeMutate procs shrinkCall ]
    _              -> []
  where

      {- | Shrink a procedure call into many procedure calls, where at most one proedure
      call has been shrunk per resulting call. -}
      shrinkCall :: (String, [Either SSMExp Reference])
                 -> [(String, [Either SSMExp Reference])]
      shrinkCall (fun, args) = [ (fun, args') | args' <- shrinkArgs args ]

      {- | Shrink one list of arguments into many list of arguments, where at most one
      argument has been shrunk per resulting list. -}
      shrinkArgs :: [Either SSMExp Reference] -> [[Either SSMExp Reference]]
      shrinkArgs args = distributeMutate args mutateArg
        where
            mutateArg (Left e)  = [ Left e' | e' <- shrinkexp e]
            mutateArg (Right _) = []

{- | Shrink an SSM expression. This is simply a syntactic division of the tree into
sub-trees. Variables, literals, unary operators can not be shrunk, and less than
comparisons can not be shrunk. Less than's can not be shrunk as the type of the entire
less-than expression is @Bool@, but the operands are of some numeric type.
If equality checks for equality of booleans, replacing the equality check for either
or the arguments would still produce a type safe expression.

tl;dr - Turn an expression of type @t@ into a list of sub-expressions, all of which
has type @t@. Syntactic split of AST. -}
shrinkExp :: SSMExp -> [SSMExp]
shrinkExp e = case e of
    Var t n        -> []
    Lit t l        -> []
    UOpE t e op    -> []
    UOpR t r op    -> []
    BOp t e1 e2 op -> case op of
        OPlus  -> [e1,e2]
        OMinus -> [e1,e2]
        OTimes -> [e1,e2]
        OLT    -> []
        OEQ    -> if (expType e1) == TBool then [e1,e2] else []
