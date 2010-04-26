-- Aufgabe 1
type Z = Int
type W = Bool
type I = String

data T = Z Z | I I 
    | T :+: T 
    | T :-: T
    | T :*: T
    | T :/: T
    | T :%: T
    | ReadT
    deriving (Eq, Show, Read)

data B = W W
    | Not B
    | T :=: T
    | T :<: T
    | T :>: T
    | T :<=: T
    | T :>=: T
    | T :/=: T
    | ReadB
    deriving (Eq, Show, Read)

data C = Skip 
    | I ::=: T 
    | C :.: C
    | If B C C
    | While B C
    | OutputB B
    | OutputT T
    deriving (Eq, Show, Read)

data K = K Z | B W
    deriving (Eq, Show, Read)


-- Aufgabe 2
divprog :: C
divprog = 
    ("x" ::=: ReadT) :.:
    ("y" ::=: ReadT) :.:
    ("g" ::=: Z(0)) :.:
    (While ((I "x") :>=: (I "y"))
        (("g" ::=: ((I "g") :+: (Z 1))) :.:
         ("x" ::=: ((I "x") :-: (I "y"))))) :.:
    (OutputT (I "g")) :.:
    (OutputT (I "x"))


-- Aufgabe 3
type Input = [K]
type Output = [K]
type Memory = [(I, Z)]

type State = (Memory, Input, Output)

reduceT :: T -> (Z -> Z -> a) -> T -> State -> (a, State)
reduceT t1 f t2 s = (f a1 a2, s'')
    where (a1, s')  = evalT t1 s
          (a2, s'') = evalT t2 s'

evalT :: T -> State -> (Z, State)
evalT (Z z) s = (z, s)
evalT (I i) s@(m,_,_) = case lookup i m of
    Just z -> (z,s)
    Nothing -> error("free var " ++ i)
evalT (t1 :+: t2) s = reduceT t1 (+) t2 s
evalT (t1 :-: t2) s = reduceT t1 (-) t2 s
evalT (t1 :*: t2) s = reduceT t1 (*) t2 s
evalT (t1 :/: t2) s = reduceT t1 div t2 s
evalT (t1 :%: t2) s = reduceT t1 mod t2 s
evalT ReadT (m,is,os) = case is of
    (K z):is' -> (z, (m,is',os))
    i:_      -> error("not num " ++ (show i))
    _        -> error("segfault")

evalB :: B -> State -> (W, State)
evalB (W w) s = (w,s)
evalB (Not b) s = (not w, s')
    where (w,s') = evalB b s
evalB (t1 :=: t2) s = reduceT t1 (==) t2 s
evalB (t1 :<: t2) s = reduceT t1 (<) t2 s
evalB (t1 :>: t2) s = reduceT t1 (>) t2 s
evalB (t1 :<=: t2) s = reduceT t1 (<=) t2 s
evalB (t1 :>=: t2) s = reduceT t1 (>=) t2 s
evalB (t1 :/=: t2) s = reduceT t1 (/=) t2 s
evalB ReadB (m,is,os) = case is of
    (B z):is' -> (z, (m,is',os))
    i:_      -> error("not bool " ++ (show i))
    _        -> error("segfault")

evalC :: C -> State -> State
evalC (i ::=: t) s = ((i,z):m, is, os)
    where (z, (m,is,os)) = evalT t s
evalC (c1 :.: c2) s = evalC c2 s'
    where s' = evalC c1 s
evalC (If b ct cf) s = evalC (if w then ct else cf) s
    where (w,s') = evalB b s
evalC while@(While b c) s = if w then evalC while s'' else s'
    where (w,s') = evalB b s
          s'' = evalC c s'
evalC (OutputB b) s = (m', is', (B w):os')
    where (w, (m',is',os')) = evalB b s
evalC (OutputT t) s = (m', is', (K z):os')
    where (z, (m',is',os')) = evalT t s

eval c is = os
    where (_,_,os) = evalC c ([], is, [])
-- eval divprog [(K 20), (K 3)] == [K 2,K 6]
