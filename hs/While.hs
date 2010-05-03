module While where

type Symbol = String

data Term 
    = N Int         -- number
    | S Symbol      -- symbol
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
    = B Bool        -- bool
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
    | Symbol ::=: Term -- :=
    | Expr :.: Expr -- ; 
    | If Logical Expr Expr
    | While Logical Expr
    | OutputB Logical
    | OutputT Term
    deriving (Eq, Show, Read)
infixr 0 :.:
infix 1 ::=:

data Const = Cn Int | Cb Bool
    deriving (Eq, Show, Read)

divprog :: Expr
divprog = 
    "x" ::=: ReadT:.:
    "y" ::=: ReadT:.:
    "g" ::=: N 0:.:
    While (S"x" :>=: S"y") (
        "g" ::=: S"g" :+: N 1:.:
        "x" ::=: S"x" :-: S"y"
    ):.:
    OutputT(S"g"):.:
    OutputT(S"x")
