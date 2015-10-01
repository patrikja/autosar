{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module TestGenPut where
import Datatypes
import GenPut

deriving instance Generic   (DataElement q a r c)
deriving instance Serialize (DataElement q a r c)
deriving instance Generic   (ClientServerOperation a b r c)
deriving instance Serialize (ClientServerOperation a b r c)
deriving instance Generic   (ServerComSpec a b)
deriving instance Serialize (ServerComSpec a b)
deriving instance Generic   (ClientComSpec)
deriving instance Serialize (ClientComSpec)

instance Serialize Int where put = putInt
instance Serialize Bool where put = putBool
-- instance Serialize String where put = error "TBD"

putBool :: Bool -> [Bin]
putBool True = [I]
putBool False = [O]

putInt :: Int -> [Bin]
putInt (-4) = [I,O,O]
putInt (-3) = [I,O,I]
putInt (-2) = [I,I,O]
putInt (-1) = [I,I,I]
putInt   0  = [O,O,O]
putInt   1  = [O,O,I]
putInt   2  = [O,I,O]
putInt   3  = [O,I,I]

-- derive instance Generic (UnqueuedSenderComSpec a)

-- instance Serialize a => Serialize (UnqueuedSenderComSpec a)
-- instance Serialize (IFace2 c)

deriving instance Serialize a => Serialize (Maybe a)
deriving instance (Serialize a, Serialize b) => Serialize (a, b)

val1 :: (Maybe (Int, Bool), Bool)
val1 = (Just (3, False), True)

test1 :: [Bin]
test1 = put val1

deriving instance Generic   (MyPort r c)
deriving instance Serialize (MyPort r c)

val2 :: MyPort Int ()
val2 = MyPort (DE 1) (DE 2)

test2 :: [Bin]
test2 = put val2
