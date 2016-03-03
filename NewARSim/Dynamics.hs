{-
Copyright (c) 2014-2016, Johan Nordlander, Jonas Duregård, Michał Pałka,
                         Patrik Jansson and Josef Svenningsson
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
   * Neither the name of the Chalmers University of Technology nor the names of its
     contributors may be used to endorse or promote products derived from this
     software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-#LANGUAGE GADTs, DeriveDataTypeable, StandaloneDeriving #-}
module Dynamics(Value, toValue, value, value', Data.Data.Data, Data.Data.Typeable) where

import qualified Data.Data
import qualified Data.Generics
import Data.Maybe

toValue :: Data.Data.Data a => a -> Value
toValue = Value

data Value where
  Value :: Data.Data.Data a => a -> Value
  deriving (Data.Data.Typeable)

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

value :: Data.Data.Data a => Value -> Maybe a
value (Value a) = Data.Data.cast a

value' :: Data.Data.Data a => Value -> a
value' = fromJust . value






--Test
run = mapM_ print [ Value (5.0 :: Double)
                  , Value "Hello"
                  , Value (Just (True,[False]))
                  , Value (Value (Value True))]

-- | Generic show: an alternative to \"deriving Show\"
gshow :: Data.Data.Data a => a -> String
gshow x = gshows False x ""

-- | Generic shows
gshows :: Data.Data.Data a => Bool -> a -> ShowS

-- This is a prefix-show using surrounding "(" and ")",
-- where we recurse into subterms with gmapQ.
-- Updated to have fewer parenthesis
gshows p = ( \t -> let subs = Data.Data.gmapQ ((showChar ' ' .) . gshows True) t in
              showParen (p && not (null subs)) $
              (showString . Data.Data.showConstr . Data.Data.toConstr $ t) . (foldr (.) id subs)
         ) `Data.Generics.extQ` (shows :: String -> ShowS)
