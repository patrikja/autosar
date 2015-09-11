
type A  = ( DataElementUnqueued Int, 
            DataElementQueued Boolean )
type B  = ( ClientServerOperation (Int,String) Int, 
            ClientServerOperation (String,String,Boolean) Double )
type C  = ( DataElementQueued Double )

comp1 :: AR c (RPort () A, RPort () B)
comp1 = atomic $ do
    a@(x, y) <- required (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=32})
    b@(m, n) <- required (ClientComSpec{}, ClientComSpec{})
    runnable [DataReceivedEvent y] $ do
        v <- rte_Read    x
        w <- rte_Receive y
        r <- rte_Call    m (v,"")
        s <- rte_Call    n ("a","b",w)
        return ()
    return (a, b)

comp2 :: AR c (RPort () A, PPort () B, PPort () C)
comp2 = atomic $ do
    a@(x, y) <- required (UnqueuedReceiverComSpec{init=7}, QueuedReceiverComSpec{length=8})
    b@(m, n) <- provided (ServerComSpec{length=10}, ServerComSpec{length=2})
    c@(z) <- provided (QueuedSenderComSpec{init=3.14})
    serverRunnable [OperationInvokedEvent m] $ \(i,s) -> do
        v <- rte_Read a x
        _ <- rte_Send b z (v/i)
        return (2*v/i)
    serverRunnable [OperationInvokedEvent n] $ \(s,r,f) -> do
        ...
    return (a, b, c)
    
comp3 :: AR c (RPort () A, PPort () C)
comp3 = composition $ do
    (a1, b1) <- comp1
    (a2, b2, c2) <- comp2
    connect b1 b2
    a <- delegate [a1, a2]
    return (a, c2)

--------------

required    :: (UnqueuedReceiverComSpec a, QueuedReceiverComSpec b) -> 
                AR c (RPort c (DataElementUnqueued a, DataElementQueued b))
required    :: (ClientComSpec a1 b1, ClientComSpec a2 b2) ->
                AR c (RPort c (ClientServerOperation a1 b1, ClientServerOperation a2 b2))

provided    :: (ServerComSpec a1 b1, ServerComSpec a2 b2) ->
                AR c (PPort c (ClientServerOperation a1 b1, ClientServerOperation a2 b2))
provided    :: (QueuedSenderComSpec a) ->
                AR c (PPort c (DataElementQueued a))

atomic      :: (forall c . AR c (RPort c A, RPort c B))            -> AR c' (RPort () A, RPort () B)
atomic      :: (forall c . AR c (RPort c A, PPort c B), PPort c C) -> AR c' (RPort () A, PPort () B, PPort () C)

composition :: (forall c . AR c a) -> AR c' a

connect     :: RPort () B -> PPort () B -> AR c ()

delegate    :: [RPort () A] -> AR c (RPort () A)
delegate    :: [PPort () C] -> AR c (PPort () C)

rte_Read    :: RPort c (DataElementUnqueued a) -> RTE c a
rte_Receive :: RPort c (DataElementQueued a) -> RTE c a
rte_Send    :: PPort c (DataElementQueued a) -> a -> RTE c ()
rte_Call    :: RPort c (ClientServerOperation a b) -> a -> RTE c b

runnable        :: [Event           c] ->       RTE c b  -> AR c ()
serverRunnable  :: [ServerEvent a b c] -> (a -> RTE c b) -> AR c ()