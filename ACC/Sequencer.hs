{- Johans generic AUTOSAR sequencer skeleton.
 

-} 

{-# LANGUAGE DeriveDataTypeable #-}

module Sequencer 
  ( -- * Sequencer state 
    SeqState(..)
    -- * Types
  , Ticks, Limit, Index
    -- * Sequencer skeleton
  , sequencer
  ) where

import NewARSim

-- * Sequencer skeleton
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

type Ticks = Int
type Limit = Int
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

