module Wskea where

import While 

data Control
    = T Term
    | L Logical
    | E Expr
    | I Int
    | Op (Int -> Int -> Int)
    | LNot
    | LOp (Int -> Int -> Bool)
    | Assign | Branch | Loop | Out
    deriving Show

instance Show (Int -> Int -> Int) where
    show _ = ""
instance Show (Int -> Int -> Bool) where
    show _ = ""
instance Show (Symbol -> Int) where
    show _ = "mem"

data Stack
    = V Int
    | W Bool
    | U Symbol
    deriving (Show, Eq, Read)

type Memory = Symbol -> Int
type WSKEA = ( [Stack] , Memory, [Control] , [Const], [Const] )

delta :: WSKEA -> WSKEA

-- terms

delta (    w, s, T (N n):k, e, a) = 
      (V n:w, s,         k, e, a)

delta (        w, s, T (S x):k, e, a) = 
      (V (s x):w, s,         k, e, a)

delta (w, s, T         (t1:+:t2):k, e, a) = 
      (w, s, T t1:T t2:Op (+)   :k, e, a)
delta (w, s, T         (t1:-:t2):k, e, a) = 
      (w, s, T t1:T t2:Op (-)   :k, e, a)
delta (w, s, T         (t1:*:t2):k, e, a) = 
      (w, s, T t1:T t2:Op (*)   :k, e, a)
delta (w, s, T         (t1:/:t2):k, e, a) = 
      (w, s, T t1:T t2:Op div   :k, e, a)
delta (w, s, T         (t1:%:t2):k, e, a) = 
      (w, s, T t1:T t2:Op mod   :k, e, a)

delta (  V n2  : V n1 :w, s, Op op:k, e, a) = 
      (V ( n1 `op` n2):w, s,       k, e, a)

delta (    w, s, T ReadT:k, Cn n:e, a) = 
      (V n:w, s,         k,      e, a)

-- logical

delta (       w, s, L (B b):k, e, a) = 
      (W b:w, s,            k, e, a)

delta (w, s, L (Not b):k, e, a) = 
      (w, s, L b: LNot:k, e, a)

delta (W b      :w, s, LNot:k, e, a) =
      (W (not b):w, s,      k, e, a)

delta (w, s, L          (t1:=:t2):k, e, a) =
      (w, s, T t1:T t2:LOp (==)  :k, e, a)
delta (w, s, L          (t1:<:t2):k, e, a) =
      (w, s, T t1:T t2:LOp (<)   :k, e, a)
delta (w, s, L          (t1:>:t2):k, e, a) =
      (w, s, T t1:T t2:LOp (>)   :k, e, a)
delta (w, s, L          (t1:<=:t2):k, e, a) =
      (w, s, T t1:T t2:LOp (<=)   :k, e, a)
delta (w, s, L          (t1:>=:t2):k, e, a) =
      (w, s, T t1:T t2:LOp (>=)   :k, e, a)
delta (w, s, L          (t1:/=:t2):k, e, a) =
      (w, s, T t1:T t2:LOp (/=)   :k, e, a)

delta (  V n2  : V n1 :w, s, LOp op:k, e, a) = 
      (W ( n1 `op` n2):w, s,        k, e, a)

delta (    w, s, L ReadB:k, Cb b:e, a) = 
      (W b:w, s,         k,      e, a)

-- expr

delta (w, s, E Skip:k, e, a) =
      (w, s,        k, e, a)
delta (    w, s, E (x::=:t):k, e, a) =
      (U x:w, s, T t:Assign:k, e, a)
delta (V n:U x:w, s , Assign:k, e, a) =
      (        w, s',        k, e, a)
            where s' sym = if sym == x then n else s sym

delta (w, s, E (c1:.:c2):k, e, a) =
      (w, s, E c1: E c2 :k, e, a)

delta (w, s,     E(If l       et   ef):k, e, a) =
      (w, s, L l : Branch : E et:E ef :k, e, a)

delta (W True:w, s, Branch:E et:E _:k, e, a) =
      (       w, s,        E et    :k, e, a)
delta (W False:w, s, Branch:E _:E ef:k, e, a) =
      (        w, s,            E ef:k, e, a)

delta (w, s,        E(While l  loop)      :k, e, a) =
      (w, s,    L l : Loop : E loop : L l :k, e, a)
delta (W True:w, s,   Loop : E loop : L l :k ,e, a) =
      (w, s, E loop:E(While l  loop)      :k, e, a)
delta (W False:w, s,  Loop : E loop : L l :k ,e, a) =
      (w, s,                               k, e, a)

delta (w, s, E(OutputT t):k, e, a) =
      (w, s, T t:Out     :k, e, a)
delta (w, s, E(OutputB b):k, e, a) =
      (w, s, L b:Out     :k, e, a)
delta ((V n):w, s, Out:k, e,      a) =
      (      w, s,     k, e, Cn n:a)
delta ((W b):w, s, Out:k, e,      a) =
      (      w, s,     k, e, Cb b:a)


start :: Expr -> [Const] -> WSKEA
start e is = ([], \s -> error "segfault", [E e], is, [])

run :: WSKEA -> [Const]
run (_, _, [], _, a) = a
run w = run $ delta w

run' e = reverse . map unbox . run . start e . map Cn 
    where unbox (Cn n) = n

-- EXAMPLE: run' divprog [20, 3]
