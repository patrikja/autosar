
type PortA  = ( DataElement Unqueued Int, 
                DataElement Queued Boolean )
type PortB  = ( ClientServerOperation (Int,String) Int, 
                ClientServerOperation (String,String,Boolean) Double )
type PortC  = ( DataElement Queued Double )

comp1 :: (Required PortA, Required PortB)
comp1 = atomic $ do
    a@(x, y) <- required (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=32})
    b@(m, n) <- required (ClientComSpec{}, ClientComSpec{})
    runnable [DataReceivedEvent x] $ do
        ...
    runnable [DataReceivedEvent y]$ do
        ...
    return (a, b)

comp2 :: (Required PortA, Provided PortB, Provided PortC)
comp2 = atomic $ do
    a@(x, y) <- required (UnqueuedReceiverComSpec{init=7}, QueuedReceiverComSpec{length=8})
    b@(m, n) <- provided (ServerComSpec{length=10}, ServerComSpec{length=2})
    c@(z) <- provided (QueuedSenderComSpec{init=3.14})
    runnable [DataReceivedEventy] $ do
        ...
    serverRunnable [OperationInvokedEvent m] $ \(i,s) -> do
        ...
    serverRunnable [OperationInvokedEvent n] $ \(s,r,f) -> do
        ...
    return (a, b, c)
    
comp3 :: (Required PortA, Provided PortC)
comp3 = composition $ do
    (a1, b1) <- comp1
    (a2, b2, c2) <- comp2
    connect b1 b2
    a <- delegate [a1, a2]
    c <- delegate [c2]
    return (a, c)

--------------

connect :: Interface a => RPort a -> PPort a -> AR c ()

delegate :: Interface a => [RPort a] -> AR c (RPort a)
delegate :: Interface a => [PPort a] -> AR c (PPort a)

instance Interface (DataElement q1 a1)
instance Interface (DataElement q1 a1, DataElement q2 a2)
instance Interface (DataElement q1 a1, DataElement q2 a2, DataElement q3 a3)
...
instance Interface (ClientServerOperation a1 r1)
instance Interface (ClientServerOperation a1 r1, ClientServerOperation a2 r2)
instance Interface (ClientServerOperation a1 r1, ClientServerOperation a2 r2, ClientServerOperation a3 r3)
...

required :: Interface a => RPortComSpec a -> AR c (RPort a)

provided :: Interface a => PPortComSpec a -> AR c (PPort a)

