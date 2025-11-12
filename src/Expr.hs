module Expr where

-- Dados algébricos para expressões matemáticas
data Expr 
    = Num Double                    -- Número com decimais (ex: 42)
    | Add Expr Expr                 -- Adição (ex: 2 + 3)
    | Sub Expr Expr                 -- Subtração (ex: 5 - 2)
    | Mul Expr Expr                 -- Multiplicação (ex: 3 * 4)
    | Div Expr Expr                 -- Divisão (ex: 10 / 2)
    deriving (Show, Eq)

-- Função de avaliação com pattern matching
eval :: Expr -> Double
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2

-- Exemplos de uso:
-- eval (Add (Num 2) (Num 3))           → 5.0
-- eval (Mul (Num 4) (Sub (Num 5) (Num 2)))  → 12.0