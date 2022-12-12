import Data.Char

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
  deriving Show

p_plus = 80
p_sub = 80
p_mul = 100
p_assg = 70
p_while = 40

p_seq = 60

p_if = 50


parshow :: Exp -> Bool -> String
parshow exp False = show' exp
parshow exp True = "(" ++ show' exp ++ ")"


show' :: Exp -> String
show' (ENum n) = show n
show' (EVar s) = s
show' (EPlus e1 e2) = "(" ++ show' e1 ++ " + " ++ show' e2 ++ ")"
show' (ESub e1 e2) = "(" ++ show' e1 ++ " - " ++ show' e2 ++ ")"
show' (EMul e1 e2) = "(" ++ show' e1 ++ " * " ++ show' e2 ++ ")"
show' (EInc v) = v ++ "++ "
show' (EAssg v e) = "(" ++ v ++ " .= " ++ show' e ++ ")"
show' (ESeq e1 e2) = show' e1 ++ "; " ++ show' e2
show' (EIf cond th el) = "if " ++ show' cond ++ " then " ++
                        show' th ++ " else " ++ show' el
show' (EWhile cond body) = "while " ++ show' cond ++ " do " ++ show' body


parCond :: Integer -> Integer -> String -> String
parCond p1 p2 exp = if p2 < p1 then "(" ++ exp ++ ")" else exp

parCondBin p1 p2 e1 e2 op = parCond p1 p2 (show'' e1 p2 ++ op ++ show'' e2 (p2 + 1))


show'' :: Exp -> Integer -> String
show'' (ENum n) _ = show n
show'' (EVar s) _ = s
show'' (EPlus e1 e2) p = parCondBin p p_plus e1 e2 " + "
show'' (ESub e1 e2) p = parCondBin p p_sub e1 e2 " - "
show'' (EMul e1 e2) p = parCondBin p p_mul e1 e2 " * "
show'' (ESeq e1 e2) p = parCondBin p p_seq e1 e2 "; "
show'' (EInc v) _ = v ++ "++ "
show'' (EAssg v e) p = parCond p p_assg (v ++ " := " ++ show'' e (p_assg + 1))
show'' (EIf cond th el) p = parCond p p_if ("if " ++ show'' cond p_if ++ " then " ++
                        show'' th p_if ++ " else " ++ show'' el (p_if + 1))
show'' (EWhile cond body) p = parCond p p_while ("while " ++ show'' cond p_while ++ " do "  ++ show'' body (p_while + 1))
---------------------------------------------------------------------
instance Num Exp where
  (+) = EPlus
  (*) = EMul
  (-) = ESub
  fromInteger = ENum
  abs = error "not implemented"
  signum = error "not implemented"

---------------------------------------------------------------------
(.=) = EAssg
(.|) = ESeq
v = EVar

infixr 2 .|
infixr 3 .= 
---------------------------------------------------------------------

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
                
testExpression expected actual = if expected == actual then "Passed" else "Failed"

showPar exp = show'' exp 0

printexp e expected = 
  let showParExp = showPar e in 
    do print e
       print showParExp
       print(testExpression expected showParExp)
 
e =  "n" .= 10 .|
     "x" .= 1 .|
     EWhile (v"n")
                  ("n" .= v"n" - 1 .|
                   "x" .= v"x" * 2) .|
    v"x"

esub = "n" .= (((10 - 20) - 30) - 40)
eplus = "n" .= 10 + (20 + (30 + 40))
emul = "n" .= 10 * (20 * (30 * 40))
ebin = "n" .= 10 * (20 + (30 - 40))
eassg = "n" .= 10 * ("x" .= 3 - 30)
eassg2 = "n" .= 5 * ("x" .= (10 - (20 - 30)))
eassg3 = "n" .= ("x" .= (10 - (20 - 30)))
eseq = "n" .= (10 .| "y" .= 20 + 10)
eseq2 = "n" .= (1 + 2 .| 3 + 4)
eseq3 = "n" .= 1 + 2 .| 3 + 4
eif = "n" .= EIf 0 1 2
eif2 = "n" .= EIf 0 1 (2 .| 3)
eif3 = "n" .= EIf 0 1 2 .| 3
ewhile = "n" .= EWhile 0 1
ewhile2 = "n" .= EWhile 0 (2 .| 3)
ewhile3 = "n" .= EWhile 0 2 .| 3
ewhileif = "n" .= EWhile 0 (EIf 0 1 2 .| 3)
ewhileif2 = "n" .= EWhile 0 (EIf 0 1 (2 .| 3))


expectedEsub = "n := 10 - 20 - 30 - 40"
expectedEplus = "n := 10 + (20 + (30 + 40))"
expectedEmul = "n := 10 * (20 * (30 * 40))"
expectedEbin = "n := 10 * (20 + (30 - 40))"
expectedEassg = "n := 10 * (x := 3 - 30)"
expectedEassg2 = "n := 5 * (x := 10 - (20 - 30))"
expectedEassg3 = "n := (x := 10 - (20 - 30))"
expectedEseq = "n := (10; y := 20 + 10)"
expectedEseq2 = "n := (1 + 2; 3 + 4)"
expectedEseq3 = "n := 1 + 2; 3 + 4"
expectedEif = "n := (if 0 then 1 else 2)"
expectedEif2 = "n := (if 0 then 1 else 2; 3)"
expectedEif3 = "n := (if 0 then 1 else 2); 3"
expectedEwhile = "n := (while 0 do 1)"
expectedEwhile2 = "n := (while 0 do 2; 3)"
expectedEwhile3 = "n := (while 0 do 2); 3"
expectedEwhileif = "n := (while 0 do (if 0 then 1 else 2); 3)"
expectedEwhileif2 = "n := (while 0 do if 0 then 1 else 2; 3)"

emptyMem = \s -> 0

main :: IO ()
main = do printexp esub expectedEsub
          printexp eplus expectedEplus
          printexp emul expectedEmul
          printexp ebin expectedEbin
          printexp eassg expectedEassg
          printexp eassg2 expectedEassg2
          printexp eassg3 expectedEassg3
          printexp eseq expectedEseq
          printexp eseq2 expectedEseq2
          printexp eseq3 expectedEseq3
          printexp eif expectedEif
          printexp eif2 expectedEif2
          printexp eif3 expectedEif3
          printexp ewhile expectedEwhile
          printexp ewhile2 expectedEwhile2
          printexp ewhile3 expectedEwhile3
          printexp ewhileif expectedEwhileif
          printexp ewhileif2 expectedEwhileif2