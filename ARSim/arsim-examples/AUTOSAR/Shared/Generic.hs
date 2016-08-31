{-# LANGUAGE DeriveDataTypeable #-}

-- | Generic components / skeletons. 
-- 
-- Provides some basic generic components/skeletons, such as Johan's sequencer
-- and the generic feedthrough component. 
-- 
-- NOTE: Most things in here are largely unused at this point.
module AUTOSAR.Shared.Generic
  ( -- * Sequencer skeleton
    SeqState(..), Ticks, Limit, Index
  , sequencer
    -- * Feedthrough component
  , Feedthrough(..)
  , feedthrough, feedthroughT 
    -- * Signal routing
  , Switch(..)
  , switchRoute
    -- * Signal conversion
  , Trigger(..)
  , trigger
  ) where

import AUTOSAR.ARSim      hiding (void)
import Control.Monad

-- * Sequencer skeleton
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Johan's generic sequencer.

-- | `SeqState` current tick (ms).
type Ticks = Int

-- | `SeqState` tick limit (ms).
type Limit = Int

-- | `SeqState` index.
type Index = Int

-- | A sequencer state is either @Stopped@ or @Running ticks limit index@, where 
-- @limit@ is the step size in milliseconds, @ticks@ is the time in milliseconds
-- since the last step, and @index@ is the new step index. A sequencer can be 
-- started and stopped by means of an exported boolean service operation.
data SeqState 
  = Stopped 
  | Running Ticks Limit Index 
  deriving (Typeable, Data)

-- | A generic skeleton for creating sequencer components; i.e.,components that 
-- produce sequences of observable effects at programmable points in time. The 
-- skeleton is parametric in the actual step function (which takes takes a step 
-- index as a parameter, performs any observable effects, and returns a new 
-- new sequencer state). 
sequencer :: Data a 
          => RTE c SeqState 
          -- ^ Initial state/sequencer setup program
          -> (Index -> RTE c SeqState) 
          -- ^ Step function
          -> (a -> RTE c SeqState)
          -- ^ Controller
          -> Atomic c (ClientServerOperation a () Provided c)
          -- ^ Boolean service operation (for start/stop)
sequencer setup step ctrl = do
  excl <- exclusiveArea
  state <- interRunnableVariable Stopped
  runnable Concurrent [InitEvent] $ 
    do rteEnter excl
       s <- setup
       rteIrvWrite state s
       rteExit excl
  runnable (MinInterval 0) [TimingEvent 1e-3] $ 
    do rteEnter excl
       Ok s <- rteIrvRead state
       s' <- case s of
              Stopped                 -> return s
              Running ticks limit i   -- Intended invariant: ticks < limit
                  | ticks + 1 < limit -> return (Running (ticks + 1) limit i)
                  | otherwise         -> step i
       rteIrvWrite state s'
       rteExit excl
  onoff <- providedPort
  serverRunnable (MinInterval 0) [OperationInvokedEvent onoff] $ \on -> 
    do rteEnter excl
       s <- ctrl on
       rteIrvWrite state s
       rteExit excl
       return ()
  return onoff

-- * Feedthrough component 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The feedthrough component serves as a skeleton for components of signals
-- which are continuations in the RTE monad. Any function @f :: a -> b@ can be
-- lifted to such a function using @g = return . f@.

-- | The `Feedthrough` component requires an input and provides an output from
-- which modified data is available.
data Feedthrough a b = Feedthrough
  { feedIn  :: DataElem Unqueued a Required
  , feedOut :: DataElem Unqueued b Provided
  }

-- | Task assigned feedthrough component.
feedthroughT :: (Data a, Data b)
             => [Task]
             -> (a -> b)
             -> b
             -> AUTOSAR (Feedthrough a b)
feedthroughT tasks f init = atomic $ do
  feedIn  <- requiredPort
  feedOut <- providedPort
  comSpec feedOut (InitValue init)

  runnableT tasks (MinInterval 0) [DataReceivedEvent feedIn] $
    do Ok t <- rteRead feedIn
       rteWrite feedOut (f t)
  return $ sealBy Feedthrough feedIn feedOut

-- | Feedthrough component.
-- * Initial value not useful anymore.
feedthrough :: (Data a, Data b)
            => (a -> b)
            -- ^ Signal manipulation
            -> b 
            -- ^ Initial value for output
            -> AUTOSAR (Feedthrough a b)
feedthrough = feedthroughT [] 

-- * Signal routing
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Offers the two-input switch which selects between two inputs. The switch is 
-- controlled by an exported @Bool@ operation.
--
-- NOTE: Unsure if we have to constantly call @rteRead@ on both inputs.

-- | A two input switch with a control operation.
data Switch a = Switch 
  { switchLeft  :: DataElem Unqueued  a   Required
  , switchRight :: DataElem Unqueued  a   Required
  , switchOut   :: DataElem Unqueued  a   Provided
  , switchOp    :: ClientServerOp Bool () Provided
  }

-- | Two input switch. By default, the @switchLeft@ input is passed through.
-- Calling the control operation @switchOp@ with @False@ sets input to
-- @switchRight@.
switchRoute :: Data a => AUTOSAR (Switch a)
switchRoute = atomic $ do
     switchLeft  <- requiredPort
     switchRight <- requiredPort
     switchOut   <- providedPort
     switchOp    <- providedPort

     lock  <- exclusiveArea
     state <- interRunnableVariable True

     -- Change the switch state
     serverRunnable (MinInterval 0) [OperationInvokedEvent switchOp] $ \sw ->
       do rteEnter lock
          rteIrvWrite state sw
          rteExit lock
          return ()

     -- Feedthrough mechanism. Use /one/ runnable.
     runnable (MinInterval 0) [DataReceivedEvent switchLeft] $
       do rteEnter lock
          Ok flag <- rteIrvRead state
          Ok val  <- rteRead switchLeft
          when flag $ void $ rteWrite switchOut val
          rteExit lock
     runnable (MinInterval 0) [DataReceivedEvent switchRight] $ 
       do rteEnter lock
          Ok flag <- rteIrvRead state
          Ok val  <- rteRead switchRight
          unless flag $ void $ rteWrite switchOut val
          rteExit lock
     
     return $ sealBy Switch switchLeft switchRight switchOut switchOp

-- * Trigger
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Monitor changes in a discrete signal to call a @ClientServerOperation@. 

-- | Trigger.
data Trigger a b = Trigger
  { input :: DataElem Unqueued a Required
  , op    :: ClientServerOp b () Required 
  }

-- | Let the changes in a discrete signal trigger a call to a  
-- @ClientServerOperation@. @'trigger' step f init@ applies the mapping @f@ to
-- the signal before feeding it to the @rteCall@. 
trigger :: (Data a, Data b, Eq a)
        => Time
        -- ^ Sample time resolution
        -> (a -> b)
        -- ^ Mapping 
        -> a
        -- ^ Initial value
        -> AUTOSAR (Trigger a b)
trigger step f init = atomic $ do
     state <- interRunnableVariable init
     input <- requiredPort
     op    <- requiredPort 
     
     runnable (MinInterval 0) [TimingEvent step] $ 
       do Ok v0 <- rteIrvRead state
          Ok v1 <- rteRead input
          when (v0 /= v1) $ 
            do rteCall op (f v1)
               rteIrvWrite state v1
               return ()
     return $ sealBy Trigger input op

