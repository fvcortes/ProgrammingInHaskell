data Exp = ENum Integer
         | ENeg Exp
         | EPlus Exp Exp
         | EMinus Exp Exp
         | EMult Exp Exp
         | EDiv Exp Exp

instance Show Exp where
  show (ENum n) = show n
  show (ENeg e) = "-(" ++ show e ++ ")"
  show (EPlus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (EMinus e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (EMult e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (EDiv e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"


---------------------------------------------------------------------
unit :: a -> Maybe a
unit x = Just x



bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f = Nothing
bind (Just x) f = f x
---------------------------------------------------------------------

-- lift f ma = bind ma (\x -> unit (f x))
-- lift f ma = bind ma aux
--   where aux x = unit (f x)
lift f ma = bind ma (unit . f)


bind2 :: Maybe a -> Maybe b -> (a -> b -> Maybe c) -> Maybe c
bind2 ma mb f = bind ma (\a -> bind mb (\b -> f a b))

lift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
lift2 f ma mb = bind2 ma mb (\a b -> (unit (f a b)))


-- Mônada:
-- Um construtor de tipos M (e.g., Maybe)
-- Uma função 'unit :: a -> M a'
-- Uma função 'bind :: M a -> (a -> M b) -> M b'
-- (Não existe uma função com tipo 'M a -> a' !!)

---------------------------------------------------------------------



eval :: Exp -> Maybe Integer
eval (ENum n) = unit n

eval (ENeg e) = lift (\x -> -x) (eval e)

eval (EPlus e1 e2) = lift2 (+) (eval e1) (eval e2)

eval (EMinus e1 e2) = lift2 (-) (eval e1) (eval e2)

eval (EMult e1 e2) = lift2 (*) (eval e1) (eval e2)

eval (EDiv e1 e2) = bind2 (eval e1) (eval e2) aux
  where aux n1 0 = Nothing
        aux n1 n2 = unit (n1 `div` n2)


-- -(10/0)
e = EPlus (ENeg (EDiv (ENum 10) (ENum 0))) (ENum 23)

main = print (show e ++ " = " ++ show (eval e))

