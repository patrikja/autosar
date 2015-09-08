import Test.QuickCheck

prop_reverse :: [Int] -> Property
prop_reverse xs =
  collect (length xs) (reverse (reverse xs) == xs)

warning :: Double -> Bool -> Bool -> Bool
warning speed driverBelted passengerBelted =
  (speed >= 5 && not driverBelted) ||
  (speed >= 4.1 && not passengerBelted)

newtype Speed = Speed Double deriving Show

instance Arbitrary Speed where
  arbitrary =
    fmap Speed . oneof $ [
      choose (0, 200),
      choose (3, 7)
      ]

  shrink (Speed x) = map Speed (shrink x)

prop_warning :: Speed -> Bool
prop_warning (Speed speed) =
  warning speed False False ==
  warning speed False True

prop_swap :: Speed -> Bool -> Bool -> Bool
prop_swap (Speed speed) driverBelted passengerBelted =
  warning speed driverBelted passengerBelted ==
  warning speed passengerBelted driverBelted
