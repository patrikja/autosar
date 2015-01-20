{-#LANGUAGE GADTs#-}
module Dynamics(Value, toValue, value, value', Data, Typeable) where

import Data.Dynamic
import Data.Data
import Data.Generics hiding (gshow,gshows)
import Data.Maybe

toValue :: Data a => a -> Value
toValue = Value

data Value where
  Value :: Data a => a -> Value
  
instance Show Value where
  show (Value a) = gshow a

value :: Data a => Value -> Maybe a
value (Value a) = cast a

value' :: Data a => Value -> a
value' = fromJust . value




--Test
run = mapM_ print [Value (5.0 :: Double),Value "Hello", Value (Just (True,[False])) ]

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


