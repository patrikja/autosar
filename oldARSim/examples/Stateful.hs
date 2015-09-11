{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Test.QuickCheck hiding (Prop, exhaustive)
import System.Random hiding (next)
import Control.Monad
import Data.List
import Prelude hiding (until)

-- Input to the warning system.
data Input = Input {
  driver :: Bool,    -- Driver belted
  passenger :: Bool, -- Passenger belted
  speed :: Speed
  } deriving Show

-- The state of the warning system.
data State = State {
  alarm :: Bool      -- Was the alarm going off previously?
  }

warning :: Input -> State -> (Bool, State)
warning input state = (alarmed, State alarmed)
  where
    alarmed = speed input >= minSpeed &&
              not (driver input && passenger input)
    minSpeed
      | alarm state = 2
      | otherwise = 5

initialState :: State
initialState = State False

-- Run the warning system on a sequence of inputs.
run :: [Input] -> [Bool]
run = tail . map fst . scanl run1 (False, initialState)
  where
    run1 (_, state) input = warning input state

-- Random generation.
newtype Speed = Speed Double deriving (Eq, Ord, Show, Num, Random, Fractional)

instance Arbitrary Speed where
  arbitrary =
    oneof [
      choose (0, 200),
      choose (0, 7)
      ]

  shrink (Speed x) =
    [ Speed x' | x' <- shrink x ] ++
    [ Speed (-x) | x < 0 ] ++
    [ Speed (x / 2) | x < 0 || x > 0 ] ++
    [ Speed (x - 1) | x > 0 ] ++
    [ Speed (x + 1) | x < 0 ]

instance Arbitrary Input where
  arbitrary = liftM3 Input arbitrary arbitrary arbitrary
  shrink (Input x y z) = [ Input x' y' z' | (x', y', z') <- shrink (x, y, z) ]

-- A little LTL-ish property language.
-- Prop a b: properties over traces of input->output pairs
-- where the inputs have type a and the results have type b
type Prop a b =
  -- Events that have happened in the past
  -- (used by "previously" combinator)
  [(a,b)] ->
  -- Trace
  [(a,b)] -> Bool

-- Turn an LTL property into a QuickCheck property.
test :: Prop Input Bool -> Property
test p = property (\inp -> p [] (zip inp (run inp)))

-- The usual LTLish combinators.
false, true :: Prop a b
false _ _ = False
true _ _ = True

nott :: Prop a b -> Prop a b
nott p past present = not (p past present)

(&&&), (|||), (-->), (<-->) :: Prop a b -> Prop a b -> Prop a b
(&&&) p q past present = p past present && q past present
(|||) p q past present = p past present || q past present
p --> q = nott p ||| q
p <--> q = (p --> q) &&& (q --> p)

end :: Prop a b
end _ [] = True
end _ _ = False

now :: (a -> b -> Bool) -> Prop a b
now p _ [] = False
now p _ ((x,y):_) = p x y

next :: Prop a b -> Prop a b
next p _ [] = False
next p past (now:future) = p (now:past) future

until :: Prop a b -> Prop a b -> Prop a b
p `until` q = q ||| (p &&& next (p `until` q))

wuntil :: Prop a b -> Prop a b -> Prop a b
p `wuntil` q = p `until` (end ||| q)

always :: Prop a b -> Prop a b
always p = p `wuntil` false

eventually :: Prop a b -> Prop a b
eventually p = true `until` p

-- Reverse the flow of time :)
previously :: Prop a b -> Prop a b
previously p past present = p present past

-- Three properties about the alarm system.
prop1 =
  (unbelted &&& speedIs (>= 5)) -->
  (alarmed `until` (nott (unbelted &&& speedIs (>= 2))))

prop2 =
  alarmed --> unbelted

prop3 =
  alarmed --> speedIs (>= 5) ||| (speedIs (>= 2) &&& previously alarmed)

alarmed = now $ \_ alarm -> alarm
inputIs p = now $ \input _ -> p input
speedIs p = inputIs (p . speed)
driverBelted = inputIs driver
passengerBelted = inputIs passenger
unbelted = nott (driverBelted &&& passengerBelted)

-- A quick hack:
-- check that a property is "exhaustive" i.e.
-- always specifies a definite value for the output.
exhaustive :: Prop Input Bool -> Property
exhaustive p = test (always q)
  where q past [] = True
        q past ((x,y):future) =
          always p [] (reverse past ++ [(x,True)]) /=
          always p [] (reverse past ++ [(x,False)])