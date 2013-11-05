{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE FlexibleContexts #-}
module ARComp where

import Language.C.Syntax
import Language.C.Quote.C
import Language.C.Smart
import Data.Loc

import Text.PrettyPrint.Mainland
import Language.C.Parser

import Data.Proxy
import Data.Maybe
import Data.Supply

import System.IO.Unsafe

------------
-- Runnables
------------

class Typable a where
  getType :: f a -> Type

class Typable (Internal a) => Valuable a where
  type Internal a
  toExpr :: a -> Expr (Internal a)
--  toExpr' :: (b ~ Internal a) => a -> f b -> Expr (Internal a)

instance Typable () where
  getType _ = [cty| void |]

instance Typable Int where
  getType _ = [cty| int |]

instance Typable Bool where
  getType _ = [cty| int |]

instance Valuable () where
  type Internal () = ()
  toExpr () = Void

instance Valuable Bool where
  type Internal Bool = Bool
  toExpr = EBool

instance Valuable Int where
  type Internal Int = Int
  toExpr = EInt

newtype RunM a = RunM { runRunM :: (forall b. (a -> Supply Int -> Code b)
                                                 -> Supply Int -> Code b) }

instance Monad RunM where
  return a = RunM (\k -> k a)
  RunM m >>= f = RunM (\k -> m (\a -> runRunM (f a) k))

data Code a where
  Send       :: PQ c a -> Expr (Internal a) ->   Code (StdRet ())
  Receive    :: RQ c a ->                        Code (StdRet a)
  Write      :: PE c a -> Expr (Internal a) ->   Code (StdRet ())
  Read       :: RE c a ->                        Code (StdRet a)
  IsUpdated  :: RE c a ->                        Code (StdRet Bool)
  Invalidate :: PE c a ->                        Code (StdRet ())
  Call       :: RO c a b -> Expr (Internal a) -> Code (StdRet b)
  Result     :: RO c a b ->                      Code (StdRet b)
  IrvWrite   :: IV c a -> Expr (Internal a) ->   Code (StdRet ())
  IrvRead    :: IV c a ->                        Code (StdRet a)
  Enter      :: EX c ->                          Code (StdRet ())
  Exit       :: EX c ->                          Code (StdRet ())
  Terminate  :: StdReturn ->                     Code ()
  Bind       :: Code a -> Maybe V -> Maybe (V,Type) -> Code b
             -> Code b
  Skip       ::                                  Code ()

  Loop       :: Code a ->                        Code ()
  IfC        :: Expr Bool -> Code a -> Code a -> Code a
  NewVar     :: Type -> Expr a ->                Code (Var a)
  Assgn      :: Var a  -> Expr a ->              Code ()

liftCode :: (Typable a, Valuable a) => Code (StdRet a) -> RunM (StdRet a)
liftCode c = RunM (\k s -> let (a,b,d) = split3 s
                               i   = supplyValue a
                               j   = supplyValue b
                               var = EVar (V i)
                               ret = EVar (V j) 
                               std = StdRet Proxy ret var
                           in
                    Bind c (Just j) (Just (i,getType std)) (k std d))

liftVoid :: Code (StdRet ()) -> RunM (StdRet ())
liftVoid c = RunM (\k s -> let (a,b) = split2 s
                               i     = supplyValue a
                               ret   = EVar (V i) in
                    Bind c (Just i) Nothing (k (StdRet Proxy ret Void) b))

terminate :: StdReturn -> RunM ()
terminate ret = RunM (\k i -> Bind (Terminate ret) Nothing Nothing (k () i))

liftStm :: Code () -> RunM ()
liftStm c = RunM (\k -> Bind c Nothing Nothing . k ())

data Expr a where
  Void    :: Expr ()
  EBool   :: Bool -> Expr Bool
  EInt    :: Int -> Expr Int
  EReal   :: Double -> Expr Double
  EString :: String -> Expr String
  EArray  :: [Expr a] -> Expr [a]

  EBinop  :: Binop a b c -> Expr a -> Expr b -> Expr c

  EVar    :: Var a -> Expr a

newtype Var a = V Int

data Binop a b c where
  BAdd  :: Num a => Binop a a a
  BSub  :: Num a => Binop a a a
  BMul  :: Num a => Binop a a a
  BDiv  :: Integral a => Binop a a a
  (:/)  :: Fractional a => Binop a a a
  
  BEq   :: Eq a => Binop a a Bool
  BNEq  :: Eq a => Binop a a Bool
  
  BLT   :: Ord a => Binop a a Bool
  BGT   :: Ord a => Binop a a Bool
  BLTE  :: Ord a => Binop a a Bool
  BGTE  :: Ord a => Binop a a Bool

  Ix   :: Binop [a] Int a

data StdRet a  = forall b . b ~ Internal a =>
                 StdRet (Proxy b) (Expr Int) (Expr (Internal a))
type StdReturn = StdRet ()

instance (Num a, Hack a) => Num (Expr a) where
  (+) = EBinop BAdd
  (-) = EBinop BSub
  (*) = EBinop BMul
  fromInteger = hack
  abs = error "abs: unimplemented"
  signum = error "signum: unimplemented"

class Hack a where
  hack :: Integer -> Expr a

instance Hack Int where
  hack = EInt . fromInteger

instance Typable a => Valuable (Expr a) where
  type Internal (Expr a) = a
  toExpr e = e

rte_send        :: Valuable a => PQ c a -> a -> RunM (StdRet ())
rte_send pq v = liftVoid (Send pq (toExpr v))

rte_receive     :: (Valuable a, Typable a) => RQ c a -> RunM (StdRet a)
rte_receive rq = liftCode (Receive rq)

rte_write       :: Valuable a => PE c a -> a -> RunM (StdRet ())
rte_write pe v = liftVoid (Write pe (toExpr v))

rte_read        :: (Valuable a, Typable a) => RE c a -> RunM (StdRet a)
rte_read re = liftCode (Read re)

rte_isUpdated   :: (Valuable a, Typable a) => RE c a -> RunM (StdRet Bool)
rte_isUpdated re = liftCode (IsUpdated re)

rte_invalidate  :: Valuable a => PE c a -> RunM (StdRet ())
rte_invalidate pe = liftVoid (Invalidate pe)

rte_call        :: (Valuable a, Valuable b, Typable b) => RO c a b -> a -> RunM (StdRet b)
rte_call ro v = liftCode (Call ro (toExpr v))

rte_callAsync   :: (Valuable a, Valuable b, Typable b) => RO c a b -> a -> RunM (StdRet b)
rte_callAsync ro v = liftCode (Call ro (toExpr v))

rte_result      :: (Valuable a, Valuable b, Typable b) => RO c a b -> RunM (StdRet b)
rte_result ro = liftCode (Result ro)

rte_irvWrite    :: Valuable a => IV c a -> a -> RunM (StdRet ())
rte_irvWrite iv v = liftVoid (IrvWrite iv (toExpr v))

rte_irvRead     :: (Typable a, Valuable a) => IV c a -> RunM (StdRet a)
rte_irvRead iv = liftCode (IrvRead iv)

rte_enter       :: EX c -> RunM (StdRet ())
rte_enter ex = liftVoid (Enter ex)

rte_exit        :: EX c -> RunM (StdRet ())
rte_exit ex = liftVoid (Exit ex)

get_res :: StdRet a -> Expr (Internal a)
get_res (StdRet _ _ v) = v

(=:) :: Var a -> Expr a -> RunM ()
var =: e = liftStm (Assgn var e)

new_var :: (Typable a , Valuable a) => Expr a -> RunM (Var a)
new_var e = RunM (\k s -> let (t,u) = split2 s
                              i  = supplyValue t
                              v  = V i
                              ty = getType e
                          in
                   Bind (NewVar ty e) Nothing (Just (i,ty)) (k v u))

loop :: RunM a -> RunM ()
loop (RunM body) =
  RunM (\k s -> let (t,u) = split2 s in
                Bind (Loop (body (\_ _ -> Skip) t)) Nothing Nothing (k () u))

compile :: RunM a -> Code ()
compile (RunM m) = m (\_ _ -> Skip) (unsafePerformIO newNumSupply)

--------------------
-- C code generation
--------------------

type V = Int

runnableC :: Code a -> [BlockItem]
runnableC Skip = []
-- The two cases below shouldn't happen because the RunM monad will only 
-- generate binds in normal form. I include thes case anyway for completeness.
runnableC (Bind (Bind c1 r1 v1 c2) r2 v2 c3) = runnableC (Bind c1 r1 v1 (Bind c2 r2 v2 c3))
runnableC (Bind Skip _ _ c3) = runnableC c3
runnableC (Bind (Terminate (StdRet _ ret _)) _ _ _) = [ BlockStm [cstm| return $(exprToExp ret); |] ]
runnableC (Bind (NewVar t e) _ (Just (v,_)) c) =
  BlockDecl [cdecl| $ty:t $id:var = $(exprToExp e) ; |]
  : runnableC c
  where var = Id (mkVar v) noLoc
runnableC (Bind (IfC e ct ce) _ _ c) = 
  BlockStm (If (exprToExp e) (block ct) (Just (block ce)) noLoc)
  : runnableC c
runnableC (Bind (Loop c) _ _ d) =
  BlockStm (While [cexp|1|] (block c) noLoc)
  : runnableC d
runnableC (Bind (Assgn (V v) e) _ _ c) =
  BlockStm [cstm| $id:(Id (mkVar v) noLoc) = $(exprToExp e) ; |]
  : runnableC c
runnableC (Bind c1 (Just s) arg c2) =
  [ BlockDecl [cdecl| $ty:t $id:(mkVar v); |] | (v,t) <- maybeToList arg]
  ++
  [BlockDecl [cdecl| typename Std_ReturnType $id:(r) = $(rteCall c1 var); |] ]
  ++ runnableC c2
  where var = maybeToList $ fmap 
              (\(v,_) -> UnOp AddrOf (Var (Id (mkVar v) noLoc) noLoc) noLoc) arg
        r   = Id (mkVar s) noLoc

block :: Code a -> Stm
block c = Block (runnableC c) noLoc

mkVar :: Int -> String
mkVar v = "var" ++ show v

rteCall :: Code a -> [Exp] -> Exp
rteCall (Send (PQ (_,n)) e)     _ = [cexp| Rte_Send( $args:([fromIntegral n,exprToExp e]) )  |]
rteCall (Receive (RQ (_,n)))    a = [cexp| Rte_Receive( $args:([fromIntegral n]++a)) |]
rteCall (Write (PE (_,n)) e)    _ = [cexp| Rte_Write( $args:([fromIntegral n,exprToExp e]) ) |]
rteCall (Read (RE (_,n)))       a = [cexp| Rte_Read( $args:([fromIntegral n]++a) ) |]
rteCall (IsUpdated (RE (_,n)))  a = [cexp| Rte_IsUpdated( $args:([fromIntegral n]++a) ) |]
rteCall (Invalidate (PE (_,n))) _ = [cexp| Rte_Invalidate( $args:([n]) ) |]
rteCall (Call (RO (_,n)) e)     a = [cexp| Rte_Call( $args:([fromIntegral n,exprToExp e]++a) ) |]
rteCall (Result (RO (_,n)))     a = [cexp| Rte_Result( $args:([fromIntegral n]++a) ) |]
rteCall (IrvWrite (IV (_,n)) e) _ = [cexp| Rte_IrvWrite( $args:([fromIntegral n,exprToExp e]) ) |]
rteCall (IrvRead (IV (_,n)))    a = [cexp| Rte_IrvRead( $args:([fromIntegral n]++a) ) |]
rteCall (Enter (EX (_,n)))      _ = [cexp| Rte_Enter( $args:([n]) ) |]
rteCall (Exit  (EX (_,n)))      _ = [cexp| Rte_Exit ( $args:([n]) ) |]
rteCall _ _ = error "rteCall only deals with calls to the runtime system"

exprToExp :: Expr a -> Exp
exprToExp (Void)           = [cexp|0|] -- error "exprToExp (Void): Unimplemented"
exprToExp (EBool True)     = [cexp|1|]
exprToExp (EBool False)    = [cexp|0|]
exprToExp (EInt i)         = fromIntegral i
exprToExp (EReal d)        = error "exprToExp (EReal d): Unimplemented"
exprToExp (EString s)      = Const (StringConst [] s noLoc) noLoc
exprToExp (EBinop Ix a b)  = Index (exprToExp a) (exprToExp b) noLoc
exprToExp (EBinop bop a b) = exprToExp a -: binop bop :- exprToExp b
  where binop bop a b = BinOp (toBop bop) a b noLoc
        toBop BAdd = Add
        toBop BSub = Sub
        toBop BMul = Mul
        toBop (:/) = Div
        toBop BEq  = Eq
        toBop BNEq = Ne
        toBop BLT  = Lt
        toBop BGT  = Gt
        toBop BLTE = Le
        toBop BGTE = Ge
exprToExp (EVar (V v)) = Var (Id (mkVar v) noLoc) noLoc
exprToExp (EArray arr) = error "exprToExp (EArray arr): Unimplemented"

--------------------
-- Copied from ARSim
--------------------

-- Phatom type arguments to make the use more type safe
newtype RE c a          = RE (InstName, ElemName)
newtype PE c a          = PE (InstName, ElemName)
newtype RQ c a          = RQ (InstName, ElemName)
newtype PQ c a          = PQ (InstName, ElemName)
newtype RO c a b        = RO (InstName, OpName)
newtype PO c a b        = PO (InstName, OpName)
newtype IV c a          = IV (InstName, VarName)
newtype EX c            = EX (InstName, ExclName)

type Name       = Int
type InstName   = Name
type OpName     = Name
type ElemName   = Name
type VarName    = Name
type ExclName   = Name
type RunName    = Name

--------------------
-- Infix expressions
--------------------

infixr 0 -:, :-
data Infix f y = f :- y
x -:f:- y = x `f` y

-----------
-- Examples
-----------

icompile run = putStrLn $ pretty 0 $ ppr $ runnableC $ compile run

icompileF run = putStrLn $ pretty 0 $ ppr $
                Func (DeclSpec [] [] (Tvoid noLoc) noLoc) 
                     (Id "foo" noLoc)
                     (DeclRoot noLoc)
                     (Params [] False noLoc)
                     (runnableC $ compile run)
                     noLoc

r1 pqe          = do rte_send pqe (EInt 123)
                     return ()

test1 = r1 (PQ (1,2))
        

r2 :: (Valuable a, Typable a) => RQ c a -> RunM ()
r2 rqe          = do rte_receive rqe; return ()

test2 = r2 (RQ (3,4) :: RQ (Expr Int) ())

-- Ticket dispenser.

dispR irv = do v <- rte_irvRead irv
               rte_irvWrite irv (get_res v + 1)
               return v

r3 rop pqe = do
  i <- new_var (0 :: Expr Int)
  loop $ do
    res <- rte_call rop ()
    rte_send pqe (get_res res)
    i =: (EVar i + 1)

test3 = r3 (RO (1,2) :: RO c () Int) (PQ (3,4))

{-
r2 i  = do Ok v <- rte_call rop ()
           rte_send pqe v
           r2 (i+1)
-}
