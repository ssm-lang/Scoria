{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module SSM.Frontend.NewPeripheral.GPIO where

import           SSM.Core             ( C
                                      , Ident
                                      , dereference
                                      , Type
                                      , makeStaticRef
                                      , refName
                                      , refType
                                      , IsPeripheral(..)
                                      , Peripheral(..)
                                      , makeIdent
                                      )
import           SSM.Core.Backend

import           SSM.Backend.C.Identifiers
import           SSM.Backend.C.Types  ( svt_
                                      , initialize_
                                      , assign_
                                      )

import           SSM.Frontend.Ref     ( Ref(..) )
import           SSM.Frontend.Compile
import           SSM.Frontend.Syntax

import           Data.Proxy           ( Proxy(Proxy) )
import           Data.Word            ( Word8 )
import qualified Data.Map as Map

import           Control.Monad.State ( MonadState(put, get) )

import           Language.C.Quote.GCC ( cedecl, cexp, citem, citems )
import qualified Language.C.Syntax as C

-- | The GPIO datatype represents the GPIO pins we have requested from the environment
data GPIOP = GPIOP { pins :: Map.Map Word8 (Ident, Type)}
  deriving (Show, Eq)

-- | Create an empty GPIO peripheral
emptyGPIO :: GPIOP
emptyGPIO = GPIOP { pins = Map.empty }

instance IsPeripheral C GPIOP where
    declareReference _ t id i gpio = gpio { pins = Map.insert i (id,t) (pins gpio) }

    declaredReferences _ gpio =
        map (uncurry makeStaticRef) $ Map.elems $ pins gpio
    
    globalDeclarations p gpio =
        flip map (declaredReferences p gpio) $ \ref ->
            [cedecl| $ty:(svt_ $ dereference $ refType ref) $id:(refName ref); |]

    staticInitialization p gpio = flip concatMap (declaredReferences p gpio) $ \ref ->
        let bt     = dereference $ refType ref
            init   = initialize_ bt [cexp| &$id:(refName ref)|]
            assign = assign_ bt [cexp| &$id:(refName ref)|] [cexp|0|] [cexp|0|]
        in [citems| $exp:init; $exp:assign; |]

type GPIO = Bool

{- | Populates the GPIO pripheral with a new reference.

Parameters:

  1. @Word8@ that identifies the GPIO pin on the board
  2. The name of the reference

Returns: The @Ref LED@ that represents the newly created reference. -}
insertGPIO :: forall backend
           .  IsPeripheral backend GPIOP
           => Word8 -> Ident -> Compile backend (Ref GPIO)
insertGPIO i id = do
    st <- get

    -- fetch the GPIO peripheral and populate it with the new reference
    let maybegpio = Map.lookup "gpio" (peripherals st)
        emptyperi = Peripheral @backend emptyGPIO
        m         = maybe emptyperi (\x -> x) maybegpio
        m'        = declareReference (Proxy @backend) typ id i m

    -- modify the @CompileSt@ to contain the updated GPIO peripheral
    put $ st { peripherals = Map.insert "gpio" m' (peripherals st)}

    -- create the reference and return it
    let ref = makeStaticRef id typ
    return $ Ptr ref
  where
      -- | GPIO pins have a binary state, so treating them like @Bool@s seems reasonable
      typ :: Type
      typ = Ref TBool

class GPIOHandler backend where
    make_handler :: proxy backend -> Ref GPIO -> Word8 -> OutputHandler backend

instance GPIOHandler C where
    make_handler _ (Ptr r) i = OutputHandler $ \k cs ->
        let (prio,dep) = pdep k cs priority_at_root depth_at_root
        in [citem| $id:initialize_static_output_device( $id:top_parent
                                                      , $exp:prio
                                                      , $exp:dep
                                                      , &$id:(refName r).sv
                                                      , $uint:i);  |]

{- | Ask the GPIO peripheral for a GPIO pin identified by the @Word8@, and
get the reference and handler back. The reference is what is used to interact
with the GPIO, and the handler must be `schedule`d in order to actually
perform the IO output actions. -}
gpio :: forall backend .
        (IsPeripheral backend GPIOP, GPIOHandler backend)
     => Word8 -> Compile backend (Ref GPIO, OutputHandler backend)
gpio i = do
    n <- fresh
    let id = makeIdent n

    ref <- insertGPIO i id

    let handler = make_handler (Proxy @backend) ref i

    return (ref, handler)
