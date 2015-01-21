{-#LANGUAGE GADTs, DeriveDataTypeable, StandaloneDeriving #-}
module Dynamics(Value, toValue, value, value', Data, Typeable) where

import Data.Dynamic
import Data.Data
import Data.Generics hiding (gshow,gshows)
import Data.Maybe

toValue :: Data a => a -> Value
toValue = Value

data Value where
  Value :: Data a => a -> Value
  deriving (Typeable)

-- deriving instance Data Value

instance Data.Data.Data Value where
  gfoldl k_a1Pp z_a1Pq (Value a1_a1Pr)
    = (z_a1Pq Value `k_a1Pp` a1_a1Pr)
  gunfold cbr pure _ = cbr (pure (Value :: () -> Value)) where
  toConstr (Value _) = cValue
  dataTypeOf _ = tValue

tValue :: Data.Data.DataType
cValue :: Data.Data.Constr
tValue
  = Data.Data.mkDataType "Dynamics.Value" [cValue]
cValue
  = Data.Data.mkConstr Dynamics.tValue "Value" [] Data.Data.Prefix


instance Show Value where
  show (Value a) = gshow a

value :: Data a => Value -> Maybe a
value (Value a) = cast a

value' :: Data a => Value -> a
value' = fromJust . value






--Test
run = mapM_ print [ Value (5.0 :: Double)
                  , Value "Hello"
                  , Value (Just (True,[False])) 
                  , Value (Value (Value True))]

-- | Generic show: an alternative to \"deriving Show\"
gshow :: Data a => a -> String
gshow x = gshows False x ""

-- | Generic shows
gshows :: Data a => Bool -> a -> ShowS

-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
-- Updated to have fewer parenthesis
gshows p = ( \t -> let subs = gmapQ ((showChar ' ' .) . gshows True) t in
              showParen (p && not (null subs)) $
              (showString . showConstr . toConstr $ t) . (foldr (.) id subs)
         ) `extQ` (shows :: String -> ShowS)


