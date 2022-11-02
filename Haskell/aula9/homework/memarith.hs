data Exp = ENum Integer
         | EVar String
         | EPlus Exp Exp
         | ESub Exp Exp
         | EMul Exp Exp
         | EInc String
         | EAssg String Exp
         | ESeq Exp Exp
         | EIf Exp Exp Exp
         | EWhile Exp Exp


instance Show Exp where
  show (ENum n) = show n
  show (EVar s) = s
  show (EPlus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (ESub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (EMul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (EInc v) = v ++ "++ "
  show (EAssg v e) = "(" ++ v ++ " := " ++ show e ++ ")"
  show (ESeq e1 e2) = show e1 ++ "; " ++ show e2
  show (EIf cond th el) = "if " ++ show cond ++ " then " ++
                          show th ++ " else " ++ show el
  show (EWhile cond body) = "while " ++ show cond ++ " do " ++ show body


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

assg :: String -> M Integer -> M Integer
assg var comp m = let (v, m') = comp m
                      m'' = \var' -> if var == var' then v else m' var'
                   in (v, m'')

---------------------------------------------------------------------

-- lift f ma = bind ma (\x -> unit (f x))
-- lift f ma = bind ma aux
--   where aux x = unit (f x)
lift f ma = bind ma (unit . f)


bind2 :: M a -> M b -> (a -> b -> M c) -> M c
bind2 ma mb f = bind ma (\a -> bind mb (\b -> f a b))

lift2 :: (a -> b -> c) -> M a -> M b -> M c
lift2 f ma mb = bind2 ma mb (\a b -> (unit (f a b)))


-- M�nada:
-- Um construtor de tipos M (e.g., Maybe)
-- Uma fun��o 'unit :: a -> M a'
-- Uma fun��o 'bind :: M a -> (a -> M b) -> M b'
-- (N�o existe uma fun��o com tipo 'M a -> a' !!)

---------------------------------------------------------------------


eval :: Exp -> M Integer

eval (ENum n) = unit n

eval (EVar var) = query var

eval (EPlus e1 e2) = lift2 (+) (eval e1) (eval e2)
eval (ESub e1 e2) = lift2 (-) (eval e1) (eval e2)
eval (EMul e1 e2) = lift2 (*) (eval e1) (eval e2)

eval (ESeq e1 e2) = lift2 seq (eval e1) (eval e2)
  where seq x y = y

eval (EAssg var e) = assg var (eval e)

eval (EInc var) = inc var

eval (EIf cond th el) = 
  bind (eval cond) (\c -> if c /= 0 then eval th else eval el)

eval (EWhile cond body) = w 0
  where w n = bind (eval cond) (\c -> 
                if c == 0 then unit n
                else bind (eval body) (\_ -> w (n + 1)))
                


-- n = 10; x = 1; while n do { n := n - 1; x = 2 * x; }; x
e =  ESeq (EAssg "n" (ENum 10))
    (ESeq (EAssg "x" (ENum 1))
    (ESeq (EWhile (EVar "n")
                  (ESeq (EAssg "n" (ESub (EVar "n") (ENum 1)))
                        (EAssg "x" (EMul (EVar "x") (ENum 2)))))
    (EVar "x")))

-- acc := 1; n := 5; (while (n > 0) then acc = acc * n; n = n - 1); acc;
efat = ESeq (EAssg "acc" (ENum 1)) 
      (ESeq (EAssg "n" (ENum 10)) 
      (ESeq (EWhile (EVar "n") 
                    (ESeq (EAssg "acc" (EMul (EVar "acc") (EVar "n"))) 
                          (EAssg "n" (ESub (EVar "n") (ENum 1)))))
            (EVar "acc")))

-- acc := 1; n := 2; m := 2; (while (m) do acc = acc * n; i = i - 1); acc;
epow = ESeq (EAssg "acc" (ENum 1)) 
      (ESeq (EAssg "n" (ENum 3)) 
      (ESeq (EAssg "m" (ENum 10)) 
      (ESeq (EWhile (EVar "m") 
                    (ESeq (EAssg "acc" (EMul (EVar "acc") (EVar "n"))) 
                          (EAssg "m" (ESub (EVar "m") (ENum 1)))))
            (EVar "acc"))))
emptyMem = \s -> 0

main :: IO ()
main = print (show epow ++ " = " ++ show' (eval epow emptyMem))
   where show' (a,m) = show a



