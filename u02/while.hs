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


type Input = [K]
type Output = [K]
type Speicher = [(I, Z)]

evalT :: T -> Speicher -> Input -> Z
evalT (Z z) _ _ = z
evalT (I i) s _ = case lookup i s of
    Just z -> z
    Nothing -> error("free var " ++ i)
evalT (t1 :+: t2) s i = (evalT t1 s i) + (evalT t2 s i)
evalT (t1 :-: t2) s i = (evalT t1 s i) - (evalT t2 s i)
evalT (t1 :*: t2) s i = (evalT t1 s i) * (evalT t2 s i)
evalT (t1 :/: t2) s i = (evalT t1 s i) `div` (evalT t2 s i)
evalT (t1 :%: t2) s i = (evalT t1 s i) `mod` (evalT t2 s i)
evalT ReadT _ [] = error("segfault")
evalT ReadT _ ((KZ z):_) = z
evalT ReadT _ (i:_) = error("no number " ++ (show i))

evalB :: B -> Speicher -> Input -> W
evalB (W w) _ _ = w
evalB (Not b) s i = not $ evalB b s i
evalB (t1 :=: t2) s i = (evalT t1 s i) == (evalT t2 s i)
evalB (t1 :<: t2) s i = (evalT t1 s i) < (evalT t2 s i)
evalB (t1 :>: t2) s i = (evalT t1 s i) > (evalT t2 s i)
evalB (t1 :<=: t2) s i = (evalT t1 s i) <= (evalT t2 s i)
evalB (t1 :>=: t2) s i = (evalT t1 s i) >= (evalT t2 s i)
evalB (t1 :/=: t2) s i = (evalT t1 s i) /= (evalT t2 s i)
evalB ReadB _ [] = error("segfault")
evalB ReadB _ ((KW w):_) = w
evalB ReadB _ (i:_) = error("no bool " ++ (show i))
