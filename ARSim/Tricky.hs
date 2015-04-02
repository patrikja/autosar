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

module Tricky where

import NewARSim

-- This module should not typecheck due to a leaked inter-runnable variable

rmethod rport excl irv = do
        Ok v <- rteRead rport
        rteEnter excl
        Ok s <- rteIrvRead irv
        rteIrvWrite irv (s+v)
        rteExit excl

wmethod pport peek = do
        x <- peek
        rteWrite pport (7+x)

peek irv = do
        Ok v <- rteIrvRead irv
        return v

tricky = do (rp,peek)   <- component $ do rport <- requiredDataElement
                                          excl <- exclusiveArea
                                          irv <- interRunnableVariable (0::Int)
                                          runnable Concurrent [ReceiveE rport] (rmethod rport excl irv)
                                          return (seal rport, peek irv) -- leaks out irv!
            pp          <- component $ do pport <- providedDataElement
                                          runnable Concurrent [Timed 1.0] (wmethod pport peek)
                                          return (seal pport)
            connect pp rp
