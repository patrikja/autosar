{-
Copyright (c) 2014-2015, Johan Nordlander, Jonas Duregård, Michał Pałka,
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

module Main where

import ARSim
import Test.QuickCheck.Random
import System.Random

r1 re op pe     = do Ok x <- rte_read re; Ok y <- rte_call op x; rte_write pe y; return ()

c1              = do re <- requiredDataElement
                     rop <- requiredOperation
                     pe <- providedDataElement
                     runnable Concurrent [ReceiveE re] (r1 re rop pe)
                     return (seal re, seal rop, seal pe)

r2 x            = do return (x*x)

c2              = do pop <- providedOperation
                     serverRunnable Concurrent [pop] r2
                     return (seal pop)

test            = do src <- source [(0.0,5),(0.0,7::Int)]
                     snk <- sink
                     (re,rop,pe) <- component c1
                     pop <- component c2
                     connect rop pop
                     connect src re
                     connect pe snk
           
-- Occasionally gives pattern match failure (in r1)           
main            = do s <- newQCGen; putTraceLabels (fst (simulationRand s test))

