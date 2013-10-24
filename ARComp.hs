{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE QuasiQuotes #-}
module ARComp where

import Language.C.Syntax
import Language.C.Quote.C
import Language.C.Smart
import Data.Loc

import Text.PrettyPrint.Mainland
import Language.C.Parser

------------
-- Runnables
------------

class Valuable a where
  type Internal a
  toExpr :: a -> Expr (Internal a)

newtype RunM a = RunM { runRunM :: (forall b. (a -> Int -> Code b) -> Int -> Code b) }

instance Monad RunM where
  return a = RunM (\k -> k a)
  RunM m >>= f = RunM (\k -> m (\a -> runRunM (f a) k))

data Code a where
  Send       :: PQ a c -> Expr (Internal a) ->   Code (StdRet ())
  Receive    :: RQ a c ->                        Code (StdRet a)
  Write      :: PE a c -> Expr (Internal a) ->   Code (StdRet ())
  Read       :: RE a c ->                        Code (StdRet a)
  IsUpdated  :: RE a c ->                        Code (StdRet Bool)
  Invalidate :: PE a c ->                        Code (StdRet ())
  Call       :: RO a b c -> Expr (Internal a) -> Code (StdRet b)
  Result     :: RO a b c ->                      Code (StdRet b)
  IrvWrite   :: IV a b -> Expr (Internal a) ->   Code (StdRet ())
  IrvRead    :: IV a b ->                        Code (StdRet a)
  Enter      :: EX c ->                          Code (StdRet ())
  Exit       :: EX c ->                          Code (StdRet ())
  Terminate  :: StdReturn ->                     Code ()
  Bind       :: Code a -> Maybe V -> Code b
             -> Code b
  Skip       ::                                  Code ()

liftCode :: Code (StdRet a) -> RunM (StdRet a)
liftCode c = RunM (\k i -> let var = EVar (V i) in
                    Bind c (Just i) (k var (i+1)))

terminate :: StdReturn -> RunM ()
terminate ret = RunM (\k i -> Bind (Terminate ret) Nothing (k () i))

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

type StdRet a  = Expr Int
type StdReturn = StdRet ()

instance Num a => Num (Expr a) where
  (+) = EBinop BAdd
  (-) = EBinop BSub
  (*) = EBinop BMul
  fromInteger = error "fromInteger: unimplemented"
  abs = error "abs: unimplemented"
  signum = error "signum: unimplemented"

instance Valuable (Expr a) where
  type Internal (Expr a) = a
  toExpr e = e

rte_send        :: Valuable a => PQ a c -> a -> RunM (StdRet ())
rte_send pq v = liftCode (Send pq (toExpr v))

rte_receive     :: Valuable a => RQ a c -> RunM (StdRet a)
rte_receive rq = liftCode (Receive rq)

rte_write       :: Valuable a => PE a c -> a -> RunM (StdRet ())
rte_write pe v = liftCode (Write pe (toExpr v))

rte_read        :: Valuable a => RE a c -> RunM (StdRet a)
rte_read re = liftCode (Read re)

rte_isUpdated   :: Valuable a => RE a c -> RunM (StdRet Bool)
rte_isUpdated re = liftCode (IsUpdated re)

rte_invalidate  :: Valuable a => PE a c -> RunM (StdRet ())
rte_invalidate pe = liftCode (Invalidate pe)

rte_call        :: (Valuable a, Valuable b) => RO a b c -> a -> RunM (StdRet b)
rte_call ro v = liftCode (Call ro (toExpr v))

rte_callAsync   :: (Valuable a, Valuable b) => RO a b c -> a -> RunM (StdRet b)
rte_callAsync ro v = liftCode (Call ro (toExpr v))

rte_result      :: (Valuable a, Valuable b) => RO a b c -> RunM (StdRet b)
rte_result ro = liftCode (Result ro)

rte_irvWrite    :: Valuable a => IV a c -> a -> RunM (StdRet ())
rte_irvWrite iv v = liftCode (IrvWrite iv (toExpr v))

rte_irvRead     :: Valuable a => IV a c -> RunM (StdRet a)
rte_irvRead iv = liftCode (IrvRead iv)

rte_enter       :: EX c -> RunM (StdRet ())
rte_enter ex = liftCode (Enter ex)

rte_exit        :: EX c -> RunM (StdRet ())
rte_exit ex = liftCode (Exit ex)

compile :: RunM () -> Code ()
compile (RunM m) = m (\() _ -> Skip) 0

--------------------
-- C code generation
--------------------

type V = Int

runnableC :: Code a -> [BlockItem]
runnableC Skip = []
runnableC (Bind (Bind c1 v1 c2) v2 c3) = runnableC (Bind c1 v1 (Bind c2 v2 c3))
runnableC (Bind (Terminate ret) _ _) = [ BlockStm [cstm| return $(exprToExp ret); |] ]
runnableC (Bind c1 Nothing c2)  = BlockStm (Exp (Just (codeToC c1)) noLoc) : runnableC c2
runnableC (Bind c1 (Just v) c2) = BlockStm (Exp (Just (Assign var JustAssign e noLoc)) noLoc)
                                 : runnableC c2
  where var = Var (Id (mkVar v) noLoc) noLoc
        e   = codeToC c1

mkVar v = "var" ++ show v

codeToC :: Code a -> Exp
codeToC (Send (PQ (_,n)) e)     = [cexp| Rte_Send( $args:([fromIntegral n,exprToExp e]) )  |]
codeToC (Receive (RQ (_,n)))    = [cexp| Rte_Receive( $args:([n])) |]
codeToC (Write (PE (_,n)) e)    = [cexp| Rte_Write( $args:([fromIntegral n,exprToExp e]) ) |]
codeToC (Read (RE (_,n)))       = [cexp| Rte_Read( $args:([n]) ) |]
codeToC (IsUpdated (RE (_,n)))  = [cexp| Rte_IsUpdated( $args:([n]) ) |]
codeToC (Invalidate (PE (_,n))) = [cexp| Rte_Invalidate( $args:([n]) ) |]
codeToC (Call (RO (_,n)) e)     = [cexp| Rte_Call( $args:([fromIntegral n,exprToExp e]) ) |]
codeToC (Result (RO (_,n)))     = [cexp| Rte_Result( $args:([n]) ) |]
codeToC (IrvWrite (IV (_,n)) e) = [cexp| Rte_IrvWrite( $args:([fromIntegral n,exprToExp e]) ) |]
codeToC (Enter (EX (_,n)))      = [cexp| Rte_Enter( $args:([n]) ) |]
codeToC (Exit  (EX (_,n)))      = [cexp| Rte_Exit ( $args:([n]) ) |]
codeToC (Terminate ret)         = error "codeToC (Terminate ret): should not happen!"
codeToC (Bind c1 _ c2)          = error "codeToC (Bind c1 _ c2): should not happen!"
codeToC (Skip)                  = error "codeToC (Skip): should not happen!"

exprToExp :: Expr a -> Exp
exprToExp (Void)           = error "exprToExp (Void): Unimplemented"
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
newtype RE a c          = RE (InstName, ElemName)
newtype PE a c          = PE (InstName, ElemName)
newtype RQ a c          = RQ (InstName, ElemName)
newtype PQ a c          = PQ (InstName, ElemName)
newtype RO a b c        = RO (InstName, OpName)
newtype PO a b c        = PO (InstName, OpName)
newtype IV a c          = IV (InstName, VarName)
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
        

r2 :: Valuable a => RQ a c -> RunM ()
r2 rqe          = do rte_receive rqe; return ()

test2 = r2 (RQ (3,4) :: RQ (Expr Int) ())
