{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module SpeculationStub where
data AR c a
instance Functor      (AR c)
instance Applicative  (AR c)
instance Monad        (AR c)
data ClientComSpec         i o = ClientComSpec
data ClientServerOperation i o
data DataElementQueued   a
data DataElementUnqueued a

data Event           c = DataReceivedEvent (DataElementQueued Boolean) -- TODO ?
data ServerEvent a b c = OperationInvokedEvent (ClientServerOperation a b)

data QueuedReceiverComSpec   a = QueuedReceiverComSpec   {length    :: Int}
data QueuedSenderComSpec     a = QueuedSenderComSpec     {initQSCS  :: a}
data UnqueuedReceiverComSpec a = UnqueuedReceiverComSpec {init      :: a}
data ServerComSpec a b         = ServerComSpec           {lengthSCS :: Int}

data PPort c a
data RPort c a


type Boolean = Bool

data RTE c a
instance Functor      (RTE c)
instance Applicative  (RTE c)
instance Monad        (RTE c)

-- portOfPairsFrompairOfPorts :: (RPort c a1, RPort c a2) -> RPort c (a1, a2)
-- portOfPairsFrompairOfPorts = error "portOfPairsFrompairOfPorts: stub"

-- type family RPort c a
-- type instance RPort c (a,b) = (RPort c a, RPort c b)
-- data family RPort c a
-- data instance RPort c (a,b) = RPort2 {unRPort2 :: (RPort c a, RPort c b)}


----------------

required1    :: (UnqueuedReceiverComSpec a, QueuedReceiverComSpec b) ->
               AR c (RPort c (DataElementUnqueued a, DataElementQueued b))
required1 = error "required: stub"

elementsOfPPort2 :: PPort c (a, b) -> (a, b)
elementsOfPPort2 = error "elementsOfPPort2: stub"

elementsOfPort2 :: RPort c (a, b) -> (a, b)
elementsOfPort2 = error "elementsOfPort2: stub"

elementsOfPort1 :: RPort c a -> a
elementsOfPort1 = error "elementsOfPort1: stub"

elementsOfPPort1 :: PPort c a -> a
elementsOfPPort1 = error "elementsOfPPort1: stub"


required2    :: (ClientComSpec a1 b1, ClientComSpec a2 b2) ->
                AR c (RPort c (ClientServerOperation a1 b1, ClientServerOperation a2 b2))
required2 = error "required': stub"

----------------

provided1    :: QueuedSenderComSpec a ->
                AR c (PPort c (DataElementQueued a))
provided1 = error "provided: stub"
provided2    :: (ServerComSpec a1 b1, ServerComSpec a2 b2) ->
                AR c (PPort c (ClientServerOperation a1 b1, ClientServerOperation a2 b2))
provided2 = error "provided2: stub"

provided'    :: (QueuedSenderComSpec a) ->
                AR c (PPort c (DataElementQueued a))
provided' = error "provided': stub"


composition :: (forall c . AR c a) -> AR c' a
composition = error "composition: stub"

rte_Read    :: DataElementUnqueued a -> RTE c a
rte_Read = error "rte_Read: stub"

rte_Receive :: DataElementQueued a -> RTE c a
rte_Receive = error "rte_Receive: stub"

rte_Send    :: DataElementQueued a -> a -> RTE c ()
rte_Send = error "rte_Send: stub"

rte_Call    :: ClientServerOperation a b -> a -> RTE c b
rte_Call = error "rte_Call: stub"

runnable        :: [Event           c] ->       RTE c b  -> AR c ()
runnable = error "runnable: stub"

-- TODO: is c correctly matched with RTE and AR?
serverRunnable  :: [ServerEvent a b c] -> (a -> RTE c b) -> AR c ()
serverRunnable = error "serverRunnable: stub"

serverRunnable2  :: [ServerEvent a b c] -> (a -> RTE c b) -> AR c ()
serverRunnable2 = error "serverRunnable2: stub"

serverRunnable3  :: [ServerEvent a b c] -> (a -> RTE c b) -> AR c ()
serverRunnable3 = error "serverRunnable3: stub"
