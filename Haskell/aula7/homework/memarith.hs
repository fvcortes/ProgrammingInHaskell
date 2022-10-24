data Exp = ENum Integer
         | EVar String
         | EPlus Exp Exp
         | EMul Exp Exp
         | EInc String
         | EAssg String Exp
         | ESeq Exp Exp


instance Show Exp where
  show (ENum n) = show n
  show (EVar s) = s
  show (EPlus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (EMul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (EInc v) = v ++ "++ "
  show (EAssg v e) = "(" ++ v ++ " := " ++ show e ++ ")"
  show (ESeq e1 e2) = show e1 ++ "; " ++ show e2


---------------------------------------------------------------------
type Mem = String -> Integer

type M a = Mem -> (a, Mem)

---------------------------------------------------------------------
unit :: a -> M a
unit x = \m -> (x, m)



bind :: M a -> (a -> M b) -> M b
-- bind :: (Mem -> (a, Mem)) -> (a -> (Mem -> (b, Mem))) -> (Mem -> (b, Mem))
bind ma f = \m -> let (a, m') = ma m in
                    (f a) m'

---------------------------------------------------------------------
query :: String -> M Integer
query var = \m -> (m var, m)

inc :: String -> M Integer
-- inc :: String -> Mem -> (Integer, Mem)
inc var m = (m var, m')
  where m' = \var' -> if var' == var then m var' + 1 else m var'


---------------------------------------------------------------------

-- lift f ma = bind ma (\x -> unit (f x))
-- lift f ma = bind ma aux
--   where aux x = unit (f x)
lift f ma = bind ma (unit . f)


bind2 :: M a -> M b -> (a -> b -> M c) -> M c
bind2 ma mb f = bind ma (\a -> bind mb (\b -> f a b))

lift2 :: (a -> b -> c) -> M a -> M b -> M c
lift2 f ma mb = bind2 ma mb (\a b -> (unit (f a b)))


-- Mônada:
-- Um construtor de tipos M (e.g., Maybe)
-- Uma função 'unit :: a -> M a'
-- Uma função 'bind :: M a -> (a -> M b) -> M b'
-- (Não existe uma função com tipo 'M a -> a' !!)

---------------------------------------------------------------------



eval :: Exp -> M Integer

eval (ENum n) = unit n

eval (EVar var) = query var

eval (EPlus e1 e2) = lift2 (+) (eval e1) (eval e2)

eval (EInc var) = inc var



-- a := 10; b := 20; a + b
e = ESeq (EAssg "a" (ENum 10))
         (ESeq (EAssg "b" (ENum 20)) (EPlus (EVar "a") (EVar "b")))

emptyMem = \s -> 0

-- main = print (show e ++ " = " ++ show' (eval e emptyMem))
--   where show' (a,m) = show a

main = print (show e)


