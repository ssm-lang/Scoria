{-| This module exposes the GPIO peripheral. The GPIO peripheral enables a developer
to write programs that use GPIO pins either as high/low output or high/low input. -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module SSM.Frontend.Peripheral.GPIO
  ( -- * SUpporting GPIO
    SupportGPIO
    -- * Output GPIO
  , GPIO
  , output
    -- * Input GPIO
  , input
  , Switch
    -- * Controlling GPIO
  , high
  , low
  )
  where

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
                                      , Handler(..)
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
import           SSM.Frontend.Language

import           Data.Proxy           ( Proxy(Proxy) )
import           Data.Word            ( Word8 )
import qualified Data.Map as Map

import           Control.Monad.State ( MonadState(put, get) )

import           Language.C.Quote.GCC ( cedecl, cexp, citem, citems )
import qualified Language.C.Syntax as C

---------- GPIO Output ----------

-- | The GPIO datatype represents the GPIO pins we have requested from the environment
data GPIOutput = GPIOutput { output_ :: Map.Map Word8 (Ident, Type)}
  deriving (Show, Eq)

-- | Create an empty GPIO peripheral
emptyGPIOutput :: GPIOutput
emptyGPIOutput = GPIOutput { output_ = Map.empty }

instance IsPeripheral C GPIOutput where
    declareReference _ t id i gpio = gpio { output_ = Map.insert i (id,t) (output_ gpio) }

    declaredReferences _ gpio =
        map (uncurry makeStaticRef) $ Map.elems $ output_ gpio
    
    globalDeclarations p gpio = []

    staticInitialization p gpio = []

gpioutputkey :: String
gpioutputkey = "gpioutput"

-- | GPIO output pins have a binary state
type GPIO = Bool

{- | Populates the GPIO pripheral with a new reference.

Parameters:

  1. @Word8@ that identifies the GPIO pin on the board
  2. The name of the reference

Returns: The @Ref LED@ that represents the newly created reference. -}
insertGPIOutput :: forall backend
           .  IsPeripheral backend GPIOutput
           => Word8 -> Ident -> Compile backend (Ref GPIO)
insertGPIOutput i id = do
    st <- get

    -- fetch the GPIO peripheral and populate it with the new reference
    let maybegpio = Map.lookup gpioutputkey (peripherals st)
        emptyperi = Peripheral @backend emptyGPIOutput
        m         = maybe emptyperi (\x -> x) maybegpio
        m'        = declareReference (Proxy @backend) typ id i m

    -- modify the @CompileSt@ to contain the updated GPIO peripheral
    put $ st { peripherals = Map.insert gpioutputkey m' (peripherals st)}

    -- create the reference and return it
    let ref = makeStaticRef id typ
    return $ Ptr ref
  where
      -- | GPIO pins have a binary state, so treating them like @Bool@s seems reasonable
      typ :: Type
      typ = mkReference $ typeOf $ Proxy @Bool

class GPIOHandler backend where
    make_handler :: proxy backend -> Ref GPIO -> Word8 -> OutputHandler backend

instance GPIOHandler C where
    make_handler _ (Ptr r) i =
        let sched k cs = let (prio, dep) = pdep k cs priority_at_root depth_at_root
                         in [[citem| $id:initialize_static_output_device(
                                                     $id:top_parent,
                                                     $exp:prio,
                                                     $exp:dep,
                                                     &$id:(refName r).sv,
                                                     $uint:i);|]]
            pretty = concat ["initialize_static_output_device(", refName r, ", ", show i, ")"]
        in Handler sched pretty
        
{- | Ask the GPIO peripheral for an output pin that can take the value high or low.
The pin is identified by the @Word8@ parameter.

The output is

  1. A reference that controls the pin
  2. A handler than when scheduled will make sure that the IO is actualized

 -}
output :: forall backend .
       (IsPeripheral backend GPIOutput, GPIOHandler backend)
       => Word8 -> Compile backend (Ref GPIO, OutputHandler backend)
output i = do
    n <- fresh
    let id = makeIdent ("output" <> show n)

    ref <- insertGPIOutput i id

    let handler = make_handler (Proxy @backend) ref i

    return (ref, handler)

----------- GPIO Input ----------

data GPInputO = GPInputO { input_ :: Map.Map Word8 (Ident, Type) }
  deriving (Show, Eq)

emptyGPInputO :: GPInputO
emptyGPInputO = GPInputO { input_ = Map.empty }

instance IsPeripheral C GPInputO where
    declareReference _ t id i gpio = gpio { input_ = Map.insert i (id,t) (input_ gpio) }

    declaredReferences _ gpio =
        map (uncurry makeStaticRef) $ Map.elems $ input_ gpio
    
    globalDeclarations p gpio = []

    staticInitialization p gpio = flip concatMap (Map.toList (input_ gpio)) $
      \(i,(id,t)) ->
        let 
            bt     = dereference t
            ref    = makeStaticRef id t
            bind   = [cexp| $id:initialize_static_input_device(
                                    (typename ssm_sv_t *) &$id:(refName ref).sv,
                                    $uint:i) |]
        in [citems|  $exp:bind; |]

-- | GPIO input pins have a binary state
type Switch = Bool

gpinputokey :: String
gpinputokey = "gpinputo"

insertGPInputO :: forall backend
           .  IsPeripheral backend GPInputO
           => Word8 -> Ident -> Compile backend (Ref Switch)
insertGPInputO i id = do
    st <- get

    -- fetch the GPIO peripheral and populate it with the new reference
    let maybegpio = Map.lookup gpinputokey (peripherals st)
        emptyperi = Peripheral @backend emptyGPInputO
        m         = maybe emptyperi (\x -> x) maybegpio
        m'        = declareReference (Proxy @backend) typ id i m

    -- modify the @CompileSt@ to contain the updated GPIO peripheral
    put $ st { peripherals = Map.insert gpinputokey m' (peripherals st)}

    -- create the reference and return it
    let ref = makeStaticRef id typ
    return $ Ptr ref
  where
      -- | GPIO pins have a binary state, so treating them like @Bool@s seems reasonable
      typ :: Type
      typ = mkReference $ typeOf $ Proxy @Bool

{- | Ask the GPIO peripheral for an input pin that can take the value high or low.
The pin is identified by the @Word8@ parameter.

The output is a reference that is written to by the GPIO driver when an input is received
 -}
input :: forall backend .
      (IsPeripheral backend GPInputO, GPIOHandler backend)
      => Word8 -> Compile backend (Ref Switch)
input i = do
    n <- fresh
    let id = makeIdent ("input" <> show n)

    ref <- insertGPInputO i id

    return ref

-- | pin state high
high :: Exp Bool
high = true

-- | pin state low
low :: Exp Bool
low = false

{- | A backend that satisfies the @SupportGPIO@ constraint fully supports both input and
output GPIO pins. -}
type SupportGPIO backend = ( IsPeripheral backend GPIOutput
                           , IsPeripheral backend GPInputO
                           , GPIOHandler backend
                           )
