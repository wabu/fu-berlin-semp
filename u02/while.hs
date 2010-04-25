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

data K = KZ Z | KW W
    deriving (Eq, Show, Read)


-- Aufgabe 2
divprog :: C
divprog = 
    ("x" ::=: ReadT) :.:
    ("y" ::=: ReadT) :.:
    ("g" ::=: Z(0)) :.:
    (While ((I "x") :<=: (I "y"))
        ("g" ::=: ((I "g") :+: (Z 1))) :.:
        ("x" ::=: ((I "x") :-: (I "y")))) :.:
    (OutputT (I "g")) :.:
    (OutputT (I "x"))


-- Aufgabe 3
type Input = [K]
type Output = [K]
type Memory = [(I, Z)]

reduceT :: T -> (Z -> Z -> a) -> T -> Memory -> Input -> a
reduceT t1 f t2 s i = f (evalT t1 s i) (evalT t2 s i)

evalT :: T -> Memory -> Input -> Z
evalT (Z z) _ _ = z
evalT (I i) s _ = case lookup i s of
    Just z -> z
    Nothing -> error("free var " ++ i)
evalT (t1 :+: t2) s i = reduceT t1 (+) t2 s i
evalT (t1 :-: t2) s i = reduceT t1 (-) t2 s i
evalT (t1 :*: t2) s i = reduceT t1 (*) t2 s i
evalT (t1 :/: t2) s i = reduceT t1 div t2 s i
evalT (t1 :%: t2) s i = reduceT t1 mod t2 s i
evalT ReadT _ is = case is of
    (KZ z):_ -> z
    i:_      -> error("not num " ++ (show i))
    _        -> error("segfault")

evalB :: B -> Memory -> Input -> W
evalB (W w) _ _ = w
evalB (Not b) s i = not $ evalB b s i
evalB (t1 :=: t2) s i = reduceT t1 (==) t2 s i
evalB (t1 :<: t2) s i = reduceT t1 (<) t2 s i
evalB (t1 :>: t2) s i = reduceT t1 (>) t2 s i
evalB (t1 :<=: t2) s i = reduceT t1 (<=) t2 s i
evalB (t1 :>=: t2) s i = reduceT t1 (>=) t2 s i
evalB (t1 :/=: t2) s i = reduceT t1 (/=) t2 s i
evalB ReadB _ is = case is of
    (KW w):_ -> w
    i:_      -> error("not bool " ++ (show i))
    _        -> error("segfault")

