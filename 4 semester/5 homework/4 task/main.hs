module Main where

data Expr = Number Double| Plus Expr Expr | Minus Expr Expr | Multi Expr Expr | Div Expr Expr | X Double
    deriving(Eq, Show)

main = do
        putStrLn $ showExpr $ simplifyEnd $ derivative test
    
    

test = Minus (Multi (X 1) (X 1)) (X 2) -- x*x - 2x

derivative :: Expr -> Expr
derivative (Number _)    = Number 0
derivative (X n)         = Number n
derivative (Plus e1 e2)  = Plus (derivative e1) (derivative e2)
derivative (Minus e1 e2) = Minus (derivative e1) (derivative e2)
derivative (Multi e1 e2) = Plus (Multi (derivative e1) e2) (Multi e1 (derivative e2))
derivative (Div e1 e2)   = Minus (Div (derivative e1) e2) (Div (Multi e1 (derivative e2)) (Multi e2 e2))


showExpr :: Expr -> String
showExpr (Number n)| n >= 0  = show n
                   | n < 0   = "(" ++ show n ++ ")"
showExpr (X n)| n == 1       = "x"
              | n > 0        =  show n ++ "x"
              | n == 0       = "0"
              | n == (-1)    = "(-x)"
              | n < 0        = "(" ++ show n ++ "x)"
showExpr (Plus e1 e2)        = "(" ++ showExpr e1 ++ " + " ++ showExpr e2 ++ ")"
showExpr (Minus e1 e2)       = "(" ++ showExpr e1 ++ " - " ++ showExpr e2 ++ ")"
showExpr (Multi e1 e2)       = "(" ++ showExpr e1 ++ " * " ++ showExpr e2 ++ ")"
showExpr (Div e1 e2)         = "(" ++ showExpr e1 ++ " / " ++ showExpr e2 ++ ")"

simplify :: Expr -> Expr
simplify (X 0) = Number 0
simplify (Plus (Number 0) e)           = simplify e
simplify (Plus e (Number 0))           = simplify e
simplify (Plus (Number a) (Number b))  = Number (a + b)
simplify (Plus (X a) (X b))            = simplify $ X (a + b)
simplify (Plus e1 e2)                  = Plus (simplify e1) (simplify e2)
simplify (Minus (Number 0) e)          = Multi (Number (-1)) (simplify e)
simplify (Minus e (Number 0))          = simplify e
simplify (Minus (Number a) (Number b)) = Number (a - b)
simplify (Minus (X a) (X b))           = simplify $ X (a - b)
simplify (Minus e1 e2)                 = Minus (simplify e1) (simplify e2)
simplify (Multi (Number 1) e)          = simplify e
simplify (Multi e (Number 1))          = simplify e
simplify (Multi (Number 0) e)          = Number 0
simplify (Multi e (Number 0))          = Number 0
simplify (Multi (X k) (Number a))      = simplify $ X (k * a)
simplify (Multi (Number a) (X k))      = simplify $ X (k * a)
simplify (Multi (Number a) (Number b)) = Number (a * b)
simplify (Multi (X a) (X b))           = Multi (simplify(X (a * b))) (X 1)
simplify (Multi e1 e2)                 = Multi (simplify e1) (simplify e2)
simplify (Div e (Number 1))            = simplify e
simplify (Div (Number 0) e)            = Number 0
simplify (Div e (Number 0))            = error "Division by zero"
simplify (Div (X k) (Number a))        = simplify $ X (k / a)
simplify (Div (Number a) (X k))        = simplify $ X (k / a)
simplify (Div (Number a) (Number b))   = Number (a / b)
simplify (Div (X a) (X b))             = Number (a / b)
simplify (Div e1 e2)                   = Div (simplify e1) (simplify e2)
simplify e                             = e

simplifyEnd e | simplify e == e = e
              | otherwise = simplifyEnd $ simplify e