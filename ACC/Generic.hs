{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Generic components / skeletons. 
-- 
-- Provides some basic generic components/skeletons, such as Johan's sequencer
-- and the generic feedthrough component.
module Generic
  ( -- * Sequencer skeleton
    SeqState(..), Ticks, Limit, Index
  , sequencer
    -- * Feedthrough component
  , Feedthrough(..)
  , feedthrough
  ) where

import NewARSim

-- * Sequencer skeleton
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
data Feedthrough c a b = Feedthrough
  { feedIn  :: DataElement Queued   a Required c
  , feedOut :: DataElement Unqueued b Provided c
  }

-- | Feedthrough component.
feedthrough :: (Data a, Data b)
            => (a -> RTE c b)
            -- ^ Monadic signal manipulation
            -> b 
            -- ^ Initial value for output
            -> Atomic c (Feedthrough c a b)
feedthrough act init = 
  do feedIn  <- requiredPort
     feedOut <- providedPort

     comSpec feedIn  (QueueLength 1)
     comSpec feedOut (InitValue init)

     -- Perform some sort of normalisation here
     runnable (MinInterval 0) [DataReceivedEvent feedIn] $
       do Ok t <- rteReceive feedIn
          rteWrite feedOut =<< act t
     return Feedthrough {..}

