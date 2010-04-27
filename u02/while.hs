---------------
-- Aufgabe 1 --
---------------
    -- straight forward ADT for (T)erm, (B)oolean and (C)ommand expressions
    -- TODO nicer Show+Read

type Z = Int
type W = Bool
type I = String

data T 
    = Z Z   -- zahl
    | I I   -- identifier
    | T :+: T 
    | T :-: T
    | T :*: T
    | T :/: T
    | T :%: T
    | ReadT
    deriving (Eq, Show, Read)

data B 
    = W W   -- wahreithswert
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
    | I ::=: T  -- :=
    | C :.: C   -- ; 
    | If B C C
    | While B C
    | OutputB B
    | OutputT T
    deriving (Eq, Show, Read)

data K = K Z | B W
    deriving (Eq, Show, Read)


---------------
-- Aufgabe 2 --
---------------
    -- ugly syntax

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


---------------
-- Aufgabe 3 --
---------------
    -- see book 41f: only haskell notation
    -- eval? gets a ? and the current state and returns the value of ? and the next state
    -- EXMAPLE eval' divprog [20, 3]
    -- TODO use Error monand instead of error calls

type Input = [K]
type Output = [K]
type Memory = [(I, Z)]

type State = (Memory, Input, Output)
type Out a = (a, State)

evalT :: T -> State -> Out Z
evalT (Z z) s = (z, s)
evalT (I i) s@(m,_,_) = case lookup i m of
    Just z -> (z,s)
    Nothing -> error("free var " ++ i)
evalT (t1 :+: t2) s = applyT (+) t1 t2 s
evalT (t1 :-: t2) s = applyT (-) t1 t2 s
evalT (t1 :*: t2) s = applyT (*) t1 t2 s
evalT (t1 :/: t2) s = applyT div t1 t2 s
evalT (t1 :%: t2) s = applyT mod t1 t2 s
evalT ReadT (m,is,os) = case is of
    (K z):is' -> (z, (m,is',os))
    i:_      -> error("not num " ++ (show i))
    _        -> error("segfault")

applyT :: (Z -> Z -> a) -> T -> T -> State -> (a, State)
applyT f t1 t2 s = (f a1 a2, s'')
    where (a1, s')  = evalT t1 s
          (a2, s'') = evalT t2 s'


evalB :: B -> State -> Out W
evalB (W w) s = (w,s)
evalB (Not b) s = (not w, s')
    where (w,s') = evalB b s
evalB (t1 :=: t2) s = applyT (==) t1 t2 s
evalB (t1 :<: t2) s = applyT (<) t1 t2 s
evalB (t1 :>: t2) s = applyT (>) t1 t2 s
evalB (t1 :<=: t2) s = applyT (<=) t1 t2 s
evalB (t1 :>=: t2) s = applyT (>=) t1 t2 s
evalB (t1 :/=: t2) s = applyT (/=) t1 t2 s
evalB ReadB (m,is,os) = case is of
    (B z):is' -> (z, (m,is',os))
    i:_      -> error("not bool " ++ (show i))
    _        -> error("segfault")

void :: State -> Out ()
void = (,) ()

evalC :: C -> State -> Out ()
evalC (i ::=: t) s = void ((i,z):m, is, os)
    where (z, (m,is,os)) = evalT t s
evalC (c1 :.: c2) s = evalC c2 s'
    where (_, s') = evalC c1 s
evalC (If b ct cf) s = evalC (if w then ct else cf) s
    where (w, s') = evalB b s
evalC while@(While b c) s = if w then evalC while s'' else void s'
    where (w, s') = evalB b s
          (_, s'')= evalC c s'
evalC (OutputB b) s = void (m', is', (B w):os')
    where (w, (m',is',os')) = evalB b s
evalC (OutputT t) s = void (m', is', (K z):os')
    where (z, (m',is',os')) = evalT t s

eval c is = reverse os
    where (_, (_,_,os)) = evalC c ([], is, [])

eval' c = map unK . eval c . map K
    where unK (K i) = i

---------------
-- Aufgabe 4 --
---------------
    -- EXAMPLE compile'' divprog
    -- TODO lift compile to a labeling monad

type Label = String
    -- instructions of our simple stack machine
data Inst 
    = Read          -- push input
    | Const Int     -- push const
    | Output        -- output pop
    | Plus | Minus | Mult | Div | Mod       -- push (pop `op` pop)
    | Nt | Eq | Gt | Lt | Geq | Leq | Neq   -- push (pop `op` pop)
    | Set I         -- mem[i] = pop
    | Get I         -- push mem[i]
    | Label Label   -- label code
    | Branch Label  -- if pop then jump to label
    | Jump Label    -- jump to label 
    deriving Show

genLabel s = s

    -- outputs reversed code so we can add instuctions with :
    -- so read from right to left :-P
codeT :: T ->  [Inst]
codeT (Z i) = Const i : []
codeT (I i) = Get i : []
codeT (t1 :+: t2) = Plus  : codeT t2 ++ codeT t1
codeT (t1 :-: t2) = Minus : codeT t2 ++ codeT t1
codeT (t1 :*: t2) = Mult  : codeT t2 ++ codeT t1
codeT (t1 :/: t2) = Div   : codeT t2 ++ codeT t1
codeT (t1 :%: t2) = Mod   : codeT t2 ++ codeT t1
codeT ReadT = Read : []

codeB :: B ->  [Inst]
codeB (W True) = Const 1 : []
codeB (W False) = Const 0 : []
codeB (Not b) = Nt : codeB b
codeB (t1 :=: t2) = Eq : codeT t2 ++ codeT t1
codeB (t1 :<: t2) = Lt : codeT t2 ++ codeT t1
codeB (t1 :>: t2) = Gt : codeT t2 ++ codeT t1
codeB (t1 :<=: t2) = Leq : codeT t2 ++ codeT t1
codeB (t1 :>=: t2) = Geq : codeT t2 ++ codeT t1
codeB (t1 :/=: t2) = Neq : codeT t2 ++ codeT t1
codeB ReadB = Read : []

codeC :: C -> [Inst]
codeC (i ::=: t)   = Set i : codeT t
codeC (c1 :.: c2)  = codeC c2 ++ codeC c1
codeC (If b ct cf) = Label end : codeC ct ++ Label true : Jump end : codeC ct ++ Branch true : codeB b
    where true = genLabel "true"
          end  = genLabel "end"
codeC (While b c) =  Label end : Jump cond : codeC c ++ Branch end : codeB (Not b) ++ Label cond : []
    where cond = genLabel "cond"
          end  = genLabel "end"
codeC (OutputB b) = Output : codeB b
codeC (OutputT t) = Output : codeT t

compile :: C -> [Inst]
compile = reverse . codeC

showProg :: [Inst] -> String
showProg = foldr ((++).(++"\n")) "" . map show

compile' = showProg . compile
compile'' = putStr . compile'

main = do 
        putStrLn $ show divprog
        putStrLn $ "\n> eval' divprog [20,3]"
        putStrLn $ show $ eval' divprog [20,3]
        putStrLn $ "\n> compile' divprog"
        putStrLn $ compile' divprog
