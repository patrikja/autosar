module ARSim where

import ARText hiding (Exit)

data StdReturn  = Void
                | Ok Value
                | NO_DATA
                | NEVER_RECEIVED
                | LIMIT
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
                | Result PortName OpName Cont
                | IrvWrite VarName Value Code
                | IrvRead VarName Cont
                | Enter ExclName Code
                | Exit ExclName Code
                | Terminate StdReturn

type Time       = Double

type Client     = (InstName,PortName,OpName)

data Act        = Idle
                | Pending
                | Serving [Client] [Value]

data P          = Run   (InstName,RunName) Time Act Int
                | RInst (InstName,RunName) (Maybe Client) [ExclName] Code
                | Excl  (InstName,ExclName) Bool
                | Irv   (InstName,VarName) Value
                | Timer (InstName,RunName) Time Time
                | QElem (InstName,PortName,ElemName) Int [Value]
                | DElem (InstName,PortName,ElemName) Bool StdReturn
                | Op    (InstName,PortName,OpName) [Value]


data Label      = ENTER (InstName,ExclName)
                | EXIT (InstName,ExclName)
                | IRVR (InstName,VarName) StdReturn
                | IRVW (InstName,VarName) Value
                | RCV (InstName,PortName,ElemName) StdReturn
                | SND (InstName,PortName,ElemName) Value StdReturn
                | RD (InstName,PortName,ElemName) StdReturn
                | WR (InstName,PortName,ElemName) Value
                | UP (InstName,PortName,ElemName) StdReturn
                | INV (InstName,PortName,ElemName)
                | CALL (InstName,PortName,OpName) Value StdReturn
                | RES (InstName,PortName,OpName) StdReturn
                | RET (InstName,PortName,OpName) Value
                | NEW (InstName,RunName)
                | TERM (InstName,RunName)
                | TICK (InstName,RunName)
                | DELTA Time
                | PASS

step m procs                            = explore m [] labels1 procs
  where labels0                         = map (may_say m) procs
        labels1                         = map (respond procs) labels0
        respond procs label             = foldl (may_hear m) label procs

explore m pre (PASS:labels) (p:post)    = explore m (p:pre) labels post
explore m pre (l:labels) (p:post)       = commit m l pre p post : explore m (p:pre) labels post
explore m _ _ _                         = []

commit m l pre p post                   = commit' l pre (say m l p ++ map (hear m l) post)
  where commit' l [] post               = post
        commit' l (p:pre) post          = commit' l pre (hear m l p : post)


may_say m (RInst (i,r) c ex (Enter x code))             = ENTER (i,x)
may_say m (RInst (i,r) c (x:ex) (Exit y code)) | x==y   = EXIT (i,x)
may_say m (RInst (i,r) c ex (IrvRead s cont))           = IRVR (i,s) Void
may_say m (RInst (i,r) c ex (IrvWrite s v code))        = IRVW (i,s) v
may_say m (RInst (i,r) c ex (Receive p e cont))         = RCV (i,p,e) Void
may_say m (RInst (i,r) c ex (Send p e v cont))          = SND (i,p,e) v Void
may_say m (RInst (i,r) c ex (Read p e cont))            = RD (i,p,e) Void
may_say m (RInst (i,r) c ex (Write p e v cont))         = WR (i,p,e) v
may_say m (RInst (i,r) c ex (IsUpdated p e cont))       = UP (i,p,e) Void
may_say m (RInst (i,r) c ex (Invalidate p e cont))      = INV (i,p,e)
may_say m (RInst (i,r) c ex (Call p o v cont))          = CALL (i,p,o) v Void
may_say m (RInst (i,r) c ex (Result p o cont))          = RES (i,p,o) Void
may_say m (RInst (i,r) (Just a) ex (Terminate (Ok v)))  = RET a v
may_say m (RInst (i,r) Nothing [] (Terminate Void))     = TERM (i,r)
may_say m (Run a 0.0 Pending n)                         = NEW a
may_say m (Run a t act n)                               = DELTA t
may_say m (Timer a 0.0 t)                               = TICK a
may_say m (Timer a t t0)                                = DELTA t
may_say m _                                             = PASS

may_hear m PASS _                                       = PASS
may_hear m (ENTER a) (Excl b True) | a==b               = ENTER a
may_hear m (ENTER a) (Excl b _) | a==b                  = PASS
may_hear m (EXIT a) (Excl b False) | a==b               = EXIT a
may_hear m (EXIT a) (Excl b _) | a==b                   = PASS
may_hear m (IRVR a res) (Irv b v) | a==b                = IRVR a (max res (Ok v))
may_hear m (IRVW a v) (Irv b _) | a==b                  = IRVW a v
may_hear m (RCV a res) (QElem b n (v:vs)) | a==b        = RCV a (max res (Ok v))
may_hear m (RCV a res) (QElem b n []) | a==b            = RCV a (max res NO_DATA)
may_hear m (SND a v res) (QElem b n vs) 
        | connect m a b && length vs < n                = SND a v (max res Void)
        | connect m a b                                 = SND a v (max res LIMIT)
may_hear m (SND a v res) (Run b _ _ _) | trig    m a b  = SND a v res
may_hear m (RD a res) (DElem b u v) | a==b              = RD a (max res v)
may_hear m (WR a v) (DElem b _ _) | connect m a b       = WR a v
may_hear m (WR a v) (Run b _ _ _) | trig m a b          = WR a v
may_hear m (UP a res) (DElem b u _) | a==b              = UP a (max res (Ok (VBool u)))
may_hear m (INV a) (DElem b _ _) | connect m a b        = INV a
may_hear m (CALL a v res) (Run b t (Serving cs vs) n)
        | trig m a b && a `notElem` cs                  = CALL a v (max res Void)
        | trig m a b                                    = CALL a v (max res LIMIT)
may_hear m (RES a res) (Op b (v:vs)) | a==b             = RES a (max res (Ok v))
may_hear m (RES a res) (Op b []) | a==b                 = RES a (max res NO_DATA)
may_hear m (RET a v) (Op b vs) | a==b                   = RET a v
may_hear m (TERM a) (Run b t act n) | a==b              = TERM a
may_hear m (TICK a) (Run b _ _ _) | a==b                = TICK a
may_hear m (DELTA d) (Run _ t _ _) | d <= t             = DELTA d
                                   | d > t              = PASS
may_hear m (DELTA d) (Timer _ t _) | d <= t             = DELTA d
                                   | d > t              = PASS
may_hear m label _                                      = label

connect m a b = undefined
trig m a b = undefined
async m a b = undefined
sync m a b = undefined
minstart m a = undefined
impl m a = undefined

say m (ENTER _) (RInst a c ex (Enter x code))           = [RInst a c (x:ex) code]
say m (EXIT _) (RInst a c (_:ex) (Exit x code))         = [RInst a c ex code]
say m (IRVR _ res) (RInst a c ex (IrvRead _ cont))      = [RInst a c ex (cont res)]
say m (IRVW _ _) (RInst a c ex (IrvWrite _ _ code))     = [RInst a c ex code]
say m (RCV _ res) (RInst a c ex (Receive _ _ cont))     = [RInst a c ex (cont res)]
say m (SND _ _ res) (RInst a c ex (Send _ _ _ cont))    = [RInst a c ex (cont res)]
say m (RD _ res) (RInst a c ex (Read _ _ cont))         = [RInst a c ex (cont res)]
say m (WR _ _) (RInst a c ex (Write _ _ _ cont))        = [RInst a c ex (cont Void)]
say m (UP _ res) (RInst a c ex (IsUpdated _ _ cont))    = [RInst a c ex (cont res)]
say m (INV _) (RInst a c ex (Invalidate _ _ cont))      = [RInst a c ex (cont Void)]
say m (CALL _ _ res) (RInst a c ex (Call p o _ cont))
        | async m (p,o) a || res /= Void                = [RInst a c ex (cont res)]
        | sync m (p,o) a                                = [RInst a c ex (Result p o cont)]
say m (RES _ res) (RInst a c ex (Result p o cont))      = [RInst a c ex (cont res)]
say m (RET _ _) (RInst a _ ex (Terminate _))            = [RInst a Nothing ex (Terminate Void)]
say m (TERM _) (RInst _ _ _ _)                          = []
say m (NEW _) (Run a _ Pending n)                       = [Run a (minstart m a) Idle (n+1),
                                                           RInst a Nothing [] (impl m a Void)]
say m (NEW _) (Run a _ (Serving (c:cs) (v:vs)) n)       = [Run a (minstart m a) (Serving cs vs) (n+1),
                                                           RInst a (Just c) [] (impl m a (Ok v))]
say m (DELTA d) (Run a t act n)                         = [Run a (t-d) act n]
say m (TICK _) (Timer a _ t)                            = [Timer a t t]
say m (DELTA d) (Timer a t t0)                          = [Timer a (t-d) t0]

hear m (ENTER a) (Excl b True) | a==b                   = Excl b False
hear m (EXIT a) (Excl b False) | a==b                   = Excl b True
hear m (IRVR a _) (Irv b v)                             = Irv b v
hear m (IRVW a v) (Irv b _) | a==b                      = Irv b v
hear m (RCV a _) (QElem b n (v:vs)) | a==b              = QElem b n vs
hear m (RCV a _) (QElem b n []) | a==b                  = QElem b n []
hear m (SND a v _) (QElem b n vs) 
        | connect m a b && length vs < n                = QElem b n (vs++[v])
        | connect m a b                                 = QElem b n vs
hear m (SND a _ _) (Run b t _ n) | trig m a b           = Run b t Pending n
hear m (RD a _) (DElem b _ v) | a==b                    = DElem b False v
hear m (WR a v) (DElem b _ _) | connect m a b           = DElem b True (Ok v)
hear m (WR a _) (Run b t _ n) | trig m a b              = Run b t Pending n
hear m (UP a _) (DElem b u v) | a==b                    = DElem b u v
hear m (INV a) (DElem b _ _) | connect m a b            = DElem b True NO_DATA
hear m (CALL a v _) (Run b t (Serving cs vs) n)
        | trig m a b && a `notElem` cs                  = Run b t (Serving (cs++[a]) (vs++[v])) n
        | trig m a b                                    = Run b t (Serving cs vs) n
hear m (RES a _) (Op b (v:vs)) | a==b                   = Op b vs
hear m (RES a _) (Op b []) | a==b                       = Op b []
hear m (RET a v) (Op b vs) | a==b                       = Op b (vs++[v])
hear m (TERM a) (Run b t act n) | a==b                  = Run b t act (n-1)
hear m (TICK a) (Run b t _ n) | a==b                    = Run b t Pending n
hear m (DELTA d) (Run b t act n)                        = Run b (t-d) act n
hear m (DELTA d) (Timer b t t0)                         = Timer b (t-d) t0
hear m label proc                                       = proc
