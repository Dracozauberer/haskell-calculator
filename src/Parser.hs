module Parser where

import Expr
import Text.Read (readMaybe)

-- Parser simples para expressões básicas
-- Exemplo: "2 + 3" vira Add (Num 2) (Num 3)
parseExpr :: String -> Maybe Expr
parseExpr str = 
    let tokens = words str  -- separa por espaços
    in case tokens of
        -- Apenas um número: "42"
        [n] -> Num <$> readMaybe n
        
        -- Operação binária: "2 + 3"
        [n1, op, n2] -> do
            num1 <- readMaybe n1
            num2 <- readMaybe n2
            case op of
                "+" -> Just $ Add (Num num1) (Num num2)
                "-" -> Just $ Sub (Num num1) (Num num2)
                "*" -> Just $ Mul (Num num1) (Num num2)
                "/" -> Just $ Div (Num num1) (Num num2)
                _   -> Nothing
        
        -- Caso contrário: inválido
        _ -> Nothing