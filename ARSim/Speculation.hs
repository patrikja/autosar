{-# LANGUAGE RankNTypes, PartialTypeSignatures, ScopedTypeVariables #-}
import SpeculationStub
import Prelude hiding (init, length)

type A  = ( DataElementUnqueued Int,
            DataElementQueued Boolean )
type B  = ( ClientServerOperation (Int,String) Int,
            ClientServerOperation (String,String,Boolean) Double )
type C  = ( DataElementQueued Double )


comp1 :: AR c (RPort () A, RPort () B)
comp1 = atomic2 $ do
    a <- required1 (UnqueuedReceiverComSpec{init=0}, QueuedReceiverComSpec{length=32})
    let (x, y) = elementsOfPort2 a
    b <- required2 (ClientComSpec{}, ClientComSpec{})
    let (m, n) = elementsOfPort2 b
    runnable [DataReceivedEvent y] $ do
        v <- rte_Read    x   -- x :: DataElementUnqueued a
        w <- rte_Receive y
        r <- rte_Call    m (v,"")
        s <- rte_Call    n ("a","b",w)
        return ()
    return (a, b)


comp2 :: AR c (RPort () A, PPort () B, PPort () C)
comp2 = atomic3 $ do
    a <- required1 (UnqueuedReceiverComSpec{init=7}, QueuedReceiverComSpec{length=8})
    let (x, y) = elementsOfPort2 a
    b <- provided2 (ServerComSpec{lengthSCS=10}, ServerComSpec{lengthSCS=2})
    let (m, n) = elementsOfPPort2 b
    c <- provided1 (QueuedSenderComSpec{initQSCS=3.14})
    let -- dummy = c :: PPort c1 C
        z :: C
        z = elementsOfPPort1 c
    serverRunnable2 [OperationInvokedEvent m] $ \(i::Int,s) -> do
        v <- rte_Read x
        let vDivi = fromIntegral v/fromIntegral i
        _ <- rte_Send z vDivi
        return (2*v)
    serverRunnable3 [OperationInvokedEvent n] $ \(s,r,f) -> do
      -- some computation ...
      return (read s * pi)
    return (a, b, c)

comp3 :: AR c (RPort () A, PPort () C)
comp3 = composition $ do
    (a1, b1) <- comp1
    (a2, b2, c2) <- comp2
    connect b1 b2
    a <- delegate [a1, a2]
    return (a, c2)

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
