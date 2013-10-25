module Main where

import ARSim

r1 op           = do Ok 49 <- rte_call op (7::Int); return ()

c1              = do rop <- requiredOperation
                     runnable Concurrent [Init] (r1 rop)
                     return (seal rop)

r2 x            = do return (x*x)

c2              = do pop <- providedOperation
                     serverRunnable Concurrent [pop] r2
                     return (seal pop)

test            = do rop <- component c1
                     pop <- component c2
                     connect rop pop
                             
main            = putTrace $ fst $ (simulationHead test)
