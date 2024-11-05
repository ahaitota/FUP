module Hw3 where
 
type Symbol = String
data Expr = Var Symbol
          | App Expr Expr
          | Lambda Symbol Expr deriving Eq
 
instance Show Expr where
    show (Var x) = x
    show (App x y) = "(" ++ show x ++ " " ++ show y ++ ")"
    show (Lambda s exp) = "(\\" ++ s ++ "." ++ show exp ++ ")"
 
counter :: Int -> String
counter n = "a" ++ show n
 
reduction :: Expr -> Expr
reduction (App (Lambda x y) z) = substitute y x z 0 
reduction (App e1 e2) = App (reduction e1) (reduction e2) 
reduction (Var x) = Var x
reduction (Lambda x e) = Lambda x (reduction e) 
 
checkFree :: Symbol -> Expr -> Bool
checkFree y (Var z) = y == z
checkFree y (App z1 z2) = checkFree y z1 || checkFree y z2
checkFree y (Lambda z e) = z /= y && checkFree y e
 
substitute :: Expr -> Symbol -> Expr -> Int -> Expr
substitute (Var y) x z n = if y == x then z else Var y
substitute (App e1 e2) x z n = App (substitute e1 x z n) (substitute e2 x z n)
substitute (Lambda y e) x z n | y == x = Lambda y e 
                              | not (checkFree y z) = Lambda y (substitute e x z n) 
                              | otherwise = Lambda (counter n) (substitute (substitute e y (Var (counter n)) (n+1)) x z (n+1)) 
 
eval :: Expr -> Expr
eval expr =
    let reducedExpr = reduction expr
    in if expr == reducedExpr
       then reducedExpr 
       else eval reducedExpr
