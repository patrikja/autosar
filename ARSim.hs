module ARSim where

import ARText hiding (Exit)

data StdReturn  = Void
                | Ok Value
                | NO_DATA
                | NEVER_RECEIVED
                | INVALID
                | UNCONNECTED
                | TIMEOUT
                | IN_EXCLUSIVE_AREA
                deriving (Eq,Ord)

type Cont       = StdReturn -> Code

data Code       = Send PortName ElemName Value Cont
                | Receive PortName ElemName Cont
                | Write PortName ElemName Value Cont
                | Read PortName ElemName Cont
                | IsUpdated PortName ElemName Cont
                | Invalidate PortName ElemName Cont
                | Call PortName OpName Value Cont
                | IrvWrite VarName Value Code
                | IrvRead VarName Cont
                | Enter ExclName Code
                | Exit ExclName Code
                | Terminate Value

type Time       = Double

type Client     = (InstName,PortName,OpName)

data Act        = Idle
                | Pending
                | Serving [Client] [Value]

data P          = Run   (InstName,RunName) Time Act Int
                | RInst (InstName,RunName) (Maybe Client) [ExclName] Code
                | Excl  (InstName,ExclName) Bool
                | Irv   (InstName,VarName) Value
                | Timer (InstName,RunName) Time
                | QElem (InstName,PortName,ElemName) Int [Value]
                | DElem (InstName,PortName,ElemName) Bool Value
                | Op    (InstName,PortName,OpName) [Value]


data Label      = ENTER (InstName,ExclName)
                | EXIT (InstName,ExclName)
                | IRVR (InstName,VarName) StdReturn
                | IRVW (InstName,VarName) Value
                
                | PASS

step procs                              = explore [] labels1 procs
  where labels0                         = map may_say procs
        labels1                         = map (respond procs) labels0
        respond procs label             = foldl may_hear label procs

explore pre (PASS:labels) (p:post)      = explore (p:pre) labels post
explore pre (l:labels) (p:post)         = commit l pre p post : explore (p:pre) labels post
explore _ _ _                           = []

commit l pre p post                     = commit' l pre (say l p ++ map (hear l) post)
  where commit' l [] post               = post
        commit' l (p:pre) post          = commit' l pre (hear l p : post)


may_say (RInst (i,r) c ex (Enter x code))               = ENTER (i,x)
may_say (RInst (i,r) c (x:ex) (Exit y code)) | x==y     = EXIT (i,x)
may_say (RInst (i,r) c ex (IrvRead s cont))             = IRVR (i,s) Void
may_say (RInst (i,r) c ex (IrvWrite s v code))          = IRVW (i,s) v
may_say _                                               = PASS

may_hear (ENTER a) (Excl b True) | a==b                 = ENTER a
may_hear (ENTER a) (Excl b _) | a==b                    = PASS
may_hear (EXIT a) (Excl b False) | a==b                 = EXIT a
may_hear (EXIT a) (Excl b _) | a==b                     = PASS
may_hear (IRVR a res) (Irv b v) | a==b                  = IRVR a (max res (Ok v))
may_hear (IRVW a v) (Irv b _) | a==b                    = IRVW a v
may_hear label _                                        = label

say (ENTER _) (RInst a c ex (Enter x code))             = [RInst a c (x:ex) code]
say (EXIT _) (RInst a c (_:ex) (Exit x code))           = [RInst a c ex code]
say (IRVR _ res) (RInst a c ex (IrvRead _ cont))        = [RInst a c ex (cont res)]
say (IRVW _ _) (RInst a c ex (IrvWrite _ _ code))       = [RInst a c ex code]

hear (ENTER a) (Excl b True) | a==b                     = Excl b False
hear (EXIT a) (Excl b False) | a==b                     = Excl b True
hear (IRVR a _) (Irv b v)                               = Irv b v
hear (IRVW a v) (Irv b _) | a==b                        = Irv b v
hear label proc                                         = proc
