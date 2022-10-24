data Exp = ENum Integer
         | ENeg Exp
         | EPlus Exp Exp
         | EMinus Exp Exp
         | EMult Exp Exp
         | EDiv Exp Exp

instance Show Exp where
    show (ENum n) = show n
    show (ENeg e) = "-{" ++ show e ++ "}"
    show (EPlus e1 e2) = "{" ++ show e1 ++ " + " ++ show e2 ++ "}"
    show (EMinus e1 e2) = "{" ++ show e1 ++ " - " ++ show e2 ++ "}"
    show (EMult e1 e2) = "{" ++ show e1 ++ " * " ++ show e2 ++ "}"
    show (EDiv e1 e2) = "{" ++ show e1 ++ " / " ++ show e2 ++ "}"


------------------------------------------------------------------
lift0 :: a -> Maybe a
lift0 x = Just x

bind :: (a -> Maybe b) -> (Maybe a -> Maybe b)
bind f Nothing = Nothing
bind f (Just x) = f x

lift1 f ma = bind (\x -> Just (f x)) ma


-- Dever de casa: Definir a bind2 usando só a lift0 e bind, não pode usar Nothing, Just, etc

--bind2 :: (a -> (b -> Maybe c)) -> (Maybe a -> (Maybe b -> Maybe c))
--bind2 f ma mb = 

{- "Bind tem propriedades maravilhosas
1. Ela pode criar e propagar erros
2. eu posso criar escrever lift1 com bind
-}

--lift1 f ma = bind (lift0 . f) ma

------------------------------------------------------------------
{-
eval :: Exp -> Maybe Integer
eval (ENum n) = Just n
eval (ENeg e) = lift1 (\x -> -x) (eval e)
eval (EPlus e1 e2) = lift2 (+) (eval e1) (eval e2)
eval (EMinus e1 e2) = lift2 (-) (eval e1) (eval e2)
eval (EMult e1 e2) = lift2 (*) (eval e1) (eval e2)
eval (EDiv e1 e2) = lift2 (div) (eval e1) (eval e2)

e1 = ENeg (EDiv (ENum 10) (ENum 2))
-}
main = print ("Hello")
