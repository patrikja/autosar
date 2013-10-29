module ABS where
        
import ARSim
import System.Random

instance (Valuable a, Valuable b) => Valuable (a,b) where
        toVal (a,b)             = VArray [toVal a, toVal b]
        fromVal (VArray [a, b]) = (fromVal a, fromVal b)

data SeqState = Stopped | Running Int Int Int

instance Valuable SeqState where
        toVal Stopped           = Void
        toVal (Running t l a)   = VArray [VInt t, VInt l, VInt a]
        fromVal Void            = Stopped
        fromVal (VArray [VInt t, VInt l, VInt a])
                                = Running t l a
        
seq_tick step excl state = do
        rte_enter excl
        Ok s <- rte_irvRead state
        s' <- case s of
                Stopped                 -> return s
                Running ticks limit a 
                        | ticks < limit -> return (Running (ticks+1) limit a)
                        | otherwise     -> step a
        rte_irvWrite state s'
        rte_exit excl

seq_ctrl step excl state on = do
        rte_enter excl
        s <- step (if on then 0 else -1)
        rte_irvWrite state s
        rte_exit excl
        return ()

sequencer step = do
        excl <- exclusiveArea
        state <- interRunnableVariable Stopped
        runnable (MinInterval 0) [Timed 0.001] (seq_tick step excl state)
        ctrl <- providedOperation
        serverRunnable (MinInterval 0) [ctrl] (seq_ctrl step excl state)
        return (seal ctrl)



relief_step valve wheelacc 0 = do
        Ok a <- rte_read wheelacc
        if a < 0 then do
                rte_write valve True
                return (Running 0 (-(round a)*10) 1)
         else
                return (Running 0 5 0)
relief_step valve wheelacc 1 = do
        rte_write valve False
        return (Running 0 5 0)
relief_step valve wheelacc n = do
        rte_write valve False
        return Stopped

relief :: AR c (PE Bool (), RE Double (), PO Bool () ())
relief = component $ do
        valve <- providedDataElement
        wheelacc <- requiredDataElement
        ctrl <- sequencer $ relief_step valve wheelacc
        return (seal valve, seal wheelacc, ctrl)

pressure_step valve wheelacc 0 = do
        rte_write valve True
        return (Running 0 100 1)
pressure_step valve wheelacc 20 = do
        rte_write valve True
        return Stopped
pressure_step valve wheelacc n | even n = do
        rte_write valve True
        Ok a <- rte_read wheelacc
        return (Running 0 (round a*50) (n+1))
pressure_step valve wheelacc n | odd n = do
        rte_write valve False
        return (Running 0 20 (n+1))
pressure_step valve wheelacc n | n < 0 = do
        rte_write valve False
        return Stopped

pressure :: AR c (PE Bool (), RE Double (), PO Bool () ())
pressure = component $ do
        valve <- providedDataElement
        wheelacc <- requiredDataElement
        ctrl <- sequencer $ pressure_step valve wheelacc
        return (seal valve, seal wheelacc, ctrl)


control mem pressure_seq relief_seq param = do
        Ok slip <- rte_receive param
        Ok slip' <- rte_irvRead mem
        case (slip < 0.8, slip' < 0.8) of
                (True, False) -> do
                        rte_call pressure_seq False
                        rte_call relief_seq True
                        return ()
                (False, True) -> do
                        rte_call relief_seq False
                        rte_call pressure_seq True
                        return ()
                _ ->    return ()
        rte_irvWrite mem slip

controller :: AR c (RO Bool () (), RO Bool () (), RQ Double ())
controller = component $ do
        mem <- interRunnableVariable 1.0
        pressure_seq <- requiredOperation
        relief_seq <- requiredOperation
        param <- requiredQueueElement 10
        runnable (MinInterval 0) [ReceiveQ param] (control mem pressure_seq relief_seq param)
        return (seal pressure_seq, seal relief_seq, seal param)

loop velocities controllers = do
        velos <- mapM (\w -> do Ok v <- rte_read w; return v) velocities
        let v0 = minimum velos
        mapM (\(v,c) -> rte_send c (v/v0)) (velos `zip` controllers)

main_loop :: AR c ([RE Double ()], [PQ Double ()])
main_loop = component $ do
        velocities <- mapM (const requiredDataElement) [1..4]
        controllers <- mapM (const providedQueueElement) [1..4]
        runnable (MinInterval 0) [Timed 0.01] (loop velocities controllers)
        return (map seal velocities, map seal controllers)
        
sensor n = do
        vel <- source ("V" ++ show n) (vel_sim n)
        acc <- source ("A" ++ show n) (acc_sim n)
        return (vel, acc)
        
actuator n = do
        rlf_v <- sink ("R" ++ show n)
        prs_v <- sink ("P" ++ show n)
        return (rlf_v, prs_v)

wheel :: Int -> AR c (PO Bool () (), PO Bool () (), PE Double ())        
wheel n = do
        (rlf_valve, rlf_acc, rlf_ctrl) <- relief
        (prs_valve, prs_acc, prs_ctrl) <- pressure
        (vel, acc) <- sensor n
        (rlf_v, prs_v) <- actuator n
        connect rlf_valve rlf_v
        connect prs_valve prs_v
        connect acc rlf_acc
        connect acc prs_acc
        return (prs_ctrl, rlf_ctrl, vel)

system = do
        (prov_rlf_seqs, prov_prs_seqs, prov_vels) <- fmap unzip3 $ mapM wheel [1..4]
        (reqd_rlf_seqs, reqd_prs_seqs, reqd_ctrls) <- fmap unzip3 $ mapM (const controller) [1..4]
        (reqd_vels, prov_ctrls) <- main_loop
        mapM (uncurry connect) (reqd_rlf_seqs `zip` prov_rlf_seqs)
        mapM (uncurry connect) (reqd_prs_seqs `zip` prov_prs_seqs)
        mapM (uncurry connect) (prov_vels `zip` reqd_vels)
        mapM (uncurry connect) (prov_ctrls `zip` reqd_ctrls)
        
vel_sim 1 = []
vel_sim 2 = []
vel_sim 3 = []
vel_sim 4 = []

acc_sim 1 = []
acc_sim 2 = []
acc_sim 3 = []
acc_sim 4 = []
