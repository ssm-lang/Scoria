{- | The Reference module exposes the `Reference` type that is used internally. This type
is not exposed to the end user in any way. The only parts of the compiler that depends
on this type is the backend-parts (C generation, pretty-printer, interpreter etc). -}
module SSM.Core.Reference where

import           SSM.Core.Ident                 ( Ident(identName) )
import           SSM.Core.Type                  ( Type )

-- * References

-- | References have a name and a type
type Ref = (Ident, Type)

-- | References in our language. They are either dynamic or static.
data Reference
    {- | A Dynamic reference will be dynamically allocated and deallocated as a program
    is running. It will reside in an activation record in the generated C-code. -}
    = Dynamic Ref
    {- | A static reference is allocated in the global scope of things, and does not
    reside in an activation record in the generated C-code. It can be referenced from any
    context. -}
    | Static Ref
    deriving (Eq, Show, Read)

-- * Destructing references

-- | Type of a reference
refType :: Reference -> Type
refType (Dynamic (_, t)) = t
refType (Static  (_, t)) = t

-- | Name of a reference
refName :: Reference -> String
refName = identName . refIdent

-- | Return the `Ident` of a `Reference`
refIdent :: Reference -> Ident
refIdent (Dynamic (n, _)) = n
refIdent (Static  (n, _)) = n

-- * Constructing references

-- | Create a dynamic reference
makeDynamicRef :: Ident -> Type -> Reference
makeDynamicRef name typ = Dynamic (name, typ)

-- | Create a static reference
makeStaticRef :: Ident -> Type -> Reference
makeStaticRef name typ = Static (name, typ)

-- * Modifying references

-- | Rename a reference
renameRef :: Reference -> Ident -> Reference
renameRef (Dynamic (_, t)) n = Dynamic (n, t)
renameRef (Static  (_, t)) n = Static (n, t)

-- * Reference predicates

-- | Returns @True@ if a reference is a dynamic reference
isDynamic :: Reference -> Bool
isDynamic (Dynamic _) = True
isDynamic _           = False

-- | Returns @True@ if a reference is a static reference
isStatic :: Reference -> Bool
isStatic = not . isDynamic
