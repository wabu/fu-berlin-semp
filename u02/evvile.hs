import Control.Monad
import Control.Monad.Error

type Symbol = String

data Term 
    = I Int
    | S Symbol
    | Term :+: Term 
    | Term :-: Term
    | Term :*: Term
    | Term :/: Term
    | Term :%: Term
    | ReadT
    deriving (Eq, Show, Read)
infixl 5 :+:
infixl 5 :-:
infixl 6 :*:
infixl 6 :/:
infixl 6 :%:

data Logical 
    = B Bool
    | Not Logical
    | Term :=: Term
    | Term :<: Term
    | Term :>: Term
    | Term :<=: Term
    | Term :>=: Term
    | Term :/=: Term
    | ReadB
    deriving (Eq, Show, Read)
infix 2 :=:
infix 2 :<:
infix 2 :>:
infix 2 :<=:
infix 2 :>=:
infix 2 :/=:

data Expr = Skip 
    | Symbol ::=: Term
    | Expr :.: Expr
    | If Logical Expr Expr
    | While Logical Expr
    | OutputB Logical
    | OutputT Term
    deriving (Eq, Show, Read)
infixr 0 :.:
infix 1 ::=:

data Const = Ci Int | Cb Bool
    deriving (Eq, Show, Read)


divprog :: Expr
divprog = 
    "x" ::=: ReadT:.:
    "y" ::=: ReadT:.:
    "g" ::=: I 0:.:
    While (S"x" :>=: S"y") (
        "g" ::=: S"g" :+: I 1:.:
        "x" ::=: S"x" :-: S"y"
    ):.:
    OutputT(S"g"):.:
    OutputT(S"x")


type Input = [Const]
type Output = [Const]
type Memory = [(Symbol, Int)]

type State = (Memory, Input, Output)

newtype StateM a  = R (State -> (a,State))
instance Monad StateM where
    return a = R $ \s -> (a, s)
    (R r) >>= k  = R $ \s -> let (a, s') = r s
                                 (R r') = k a
                              in r' s'

get :: Symbol -> (StateM (Maybe Int))
get sym = R (\(s@(m,_,_)) -> (lookup sym m, s))

set :: Symbol -> Int -> StateM ()
set sym val = R (\(m,is,os) -> (,) () ((sym,val):m, is, os))

input :: (StateM (Maybe Const))
input = R $ \(m,is,os) -> case is of 
    i:is -> (Just i, (m,is,os))
    _    -> (Nothing, (m,is,os))

output :: Const -> (StateM ())
output o = R $ \(m,is,os) -> ((), (m,is,o:os))

type Computation a = ErrorT String StateM a

evalT :: Term -> Computation Int
evalT (I n) = return n
evalT (S s) =
    do  v <- lift $ get s
        case v of
            Just n  -> return n
            Nothing -> throwError $ "free var " ++ s

evalT (t1 :+: t2) = applyT (+) t1 t2
evalT (t1 :-: t2) = applyT (-) t1 t2
evalT (t1 :*: t2) = applyT (*) t1 t2
evalT (t1 :/: t2) = applyT div t1 t2
evalT (t1 :%: t2) = applyT mod t1 t2

evalT ReadT =
    do  c <- lift $ input 
        case c of
            Just (Ci n) -> return n
            Just _      -> throwError "not a number"
            Nothing     -> throwError "segfault"

applyT :: (Int -> Int -> a) -> Term -> Term -> Computation a
applyT f t1 t2 = 
    do a1 <- evalT t1
       a2 <- evalT t2
       return (f a1 a2)
        
evalB :: Logical -> Computation Bool
evalB (B w)   = return w
evalB (Not b) = liftM not $ evalB b
evalB (t1 :=: t2) = applyT (==) t1 t2
evalB (t1 :<: t2) = applyT (<) t1 t2
evalB (t1 :>: t2) = applyT (>) t1 t2
evalB (t1 :<=: t2) = applyT (<=) t1 t2
evalB (t1 :>=: t2) = applyT (>=) t1 t2
evalB (t1 :/=: t2) = applyT (/=) t1 t2
evalB ReadB = 
    do  i <- lift $ input 
        case i of
            Just (Cb n) -> return n
            Just _      -> throwError "not a boolean"
            Nothing     -> throwError "segfault"

evalC :: Expr -> Computation ()
evalC Skip = return ()
evalC (i ::=: t) = evalT t   >>= lift . (set i)
evalC (c1 :.: c2) = evalC c1 >> evalC c2
evalC (If b ct cf) = 
    do  w <- evalB b
        evalC (if w then ct else cf)

evalC while@(While b c) = 
    do  w <- evalB b
        if w then evalC (c :.: while)
             else return ()
evalC (OutputB b) = evalB b >>= lift . output . Cb
evalC (OutputT t) = evalT t >>= lift . output . Ci

runC s0 (R c) = c s0

eval :: Expr -> Input -> Output
eval c args = case r of 
        Right () -> reverse os
        Left (msg ) -> error $ 
            "error in computation: " ++ msg 
            ++ "\n\tmem: " ++ show mem
            ++ "\n\tin:  " ++ show is
            ++ "\n\tout: " ++ show os
    where (r, (mem,is,os)) = runC ([],args,[]) $ runErrorT $ evalC c
    -- eval in new environment and return output

    -- eval with maped/unmaped number input/ouput to Konst expressions
eval' c = map unK . eval c . map Ci
    where unK (Ci i) = i
