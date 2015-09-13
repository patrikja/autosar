{-# LANGUAGE RankNTypes #-}
import SpeculationStub
import Prelude hiding (init, length)

type A  = ( DataElementUnqueued Int,
            DataElementQueued Boolean )
type B  = ( ClientServerOperation (Int,String) Int,
            ClientServerOperation (String,String,Boolean) Double )
type C  = ( DataElementQueued Double )

comp1 :: AR c (RPort () A, RPort () B)
comp1 = atomic2 $ do
    a@(x, y) <- required1 (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=32})
    b@(m, n) <- required2 (ClientComSpec{}, ClientComSpec{})
    runnable [DataReceivedEvent y] $ do
        v <- rte_Read    x   -- x :: RPort c (DataElementUnqueued a)
        w <- rte_Receive y
        r <- rte_Call    m (v,"")
        s <- rte_Call    n ("a","b",w)
        return ()
    return (portOfPairsFrompairOfPorts a, portOfPairsFrompairOfPorts b)

{-
comp2 :: AR c (RPort () A, PPort () B, PPort () C)
comp2 = atomic3 $ do
    a@(x, y) <- required1 (UnqueuedReceiverComSpec{init=7}, QueuedReceiverComSpec{length=8})
    b@(m, n) <- provided (ServerComSpec{length=10}, ServerComSpec{length=2})
    c@(z) <- provided (QueuedSenderComSpec{init=3.14})
    serverRunnable2 [OperationInvokedEvent m] $ \(i,s) -> do
        v <- rte_Read a x
        _ <- rte_Send b z (v/i)
        return (2*v/i)
    serverRunnable3 [OperationInvokedEvent n] $ \(s,r,f) -> do
    -- some computation ...
    return (a, b, c)


comp3 :: AR c (RPort () A, PPort () C)
comp3 = composition $ do
    (a1, b1) <- comp1
    (a2, b2, c2) <- comp2
    connect b1 b2
    a <- delegate [a1, a2]
    return (a, c2)
-}

--------------


connect     :: RPort () B -> PPort () B -> AR c ()
connect = error "connect: stub"

-- TODO: unify to one operation "atomic"
atomic2     :: (forall c . AR c (RPort c A, RPort c B))            -> AR c' (RPort () A, RPort () B)
atomic3     :: (forall c . AR c (RPort c A, PPort c B, PPort c C)) -> AR c' (RPort () A, PPort () B, PPort () C)
atomic2 = error "atomic2: stub"
atomic3 = error "atomic3: stub"

delegate    :: [RPort () A] -> AR c (RPort () A)
delegate'   :: [PPort () C] -> AR c (PPort () C)
delegate = error "delegate: stub"
delegate' = error "delegate': stub"
