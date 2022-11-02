    

-- Mônada é um construtor de tipos M (e.g. Maybe)
-- Uma função 'unit :: a -> M a' joga valores para dentro desse mundo de Mônadas
-- Uma função 'bind :: M a -> (a -> M b) -> M b'
-- Não existe função 'M a -> a' !!

data Exp = ENum Integer
         | EVar String
         | EPlus Exp Exp
         | ESub Exp Exp
         | EMult Exp Exp
         | EInc String
         | EAssg String Exp
         | ESeq Exp Exp
         | EIf Exp Exp Exp
         | EWhile Exp Exp

instance Show Exp where
    show (ENum n) = show n
    show (EVar s) = s
    show (EPlus e1 e2) = "{" ++ show e1 ++ " + " ++ show e2 ++ "}"
    show (ESub e1 e2) = "{" ++ show e1 ++ " - " ++ show e2 ++ "}"
    show (EMult e1 e2) = "{" ++ show e1 ++ " * " ++ show e2 ++ "}"
    show (EInc v) = v ++ "++"
    show (EAssg v e) = "(" ++ v ++ " := " ++ show e ++ ")"
    show (ESeq e1 e2) = show e1 ++ ";" ++ show e2
    show (EIf cond th el) = "if " ++ show cond ++ " then " ++ show th ++ " else " ++ show el
    show (EWhile cond body) = "while " ++ show cond ++ " do " ++ show body

-------------------------------------------------
type Mem = String -> Integer
type M a = Mem -> (a, Mem)
-------------------------------------------------
unit :: a -> M a
unit x = \m -> (x, m)


bind :: M a -> (a -> M b) -> M b
bind ma f = \m -> let (a, m') = ma m in
                    (f a) m'

-------------------------------------------------
query :: String -> M Integer
query var = \m -> (m var, m)

inc :: String -> M Integer
inc var m = (m var, m')
    where m' = \var' -> if var' == var then m var' + 1 else m var'

assg :: String -> M Integer -> M Integer
assg var comp m = 
    let (a, m') = comp m
        m'' = \var' -> if var' == var then a else m' var' in 
            (a,m'')
    
-------------------------------------------------



-- lift f ma = bind ma (\x -> unit (f x))

lift1 f ma = bind ma (unit . f)

bind2 :: M a -> M b -> (a -> b -> M c) -> M c
bind2 ma mb f = bind ma (\a -> bind mb (\b -> f a b))

lift2 :: (a -> b -> c) -> (M a -> M b -> M c)
lift2 f ma mb = bind2 ma mb (\a b -> (unit (f a b)))

---------------------------------------------------------
-- Quero colocar variáveis, atribuição, incremento, etc.
-- Para isso , precisamos de uma memória 'Mem :: String -> Integer'

---------------------------------------------------------
eval :: Exp -> M Integer
eval (ENum n) = unit n
eval (EPlus e1 e2) = lift2 (+) (eval e1) (eval e2)
eval (ESub e1 e2) = lift2 (-) (eval e1) (eval e2)
eval (EMult e1 e2) = lift2 (*) (eval e1) (eval e2)
eval (EVar var) = query var
eval (EInc var) = inc var
eval (ESeq e1 e2) = lift2 aux (eval e1) (eval e2)
    where aux v1 v2 = v2
-- eval (EAssg var exp) = d
eval (EAssg var exp) = assg var (eval exp)

eval (EIf cond th el) = 
    -- bind exp foo => Avalia exp e chama essa função com o resultado dessa avaliação
    bind (eval cond) (\c -> if c /= 0 then eval th else eval el)
eval (EWhile cond body) = w 0
    where w n = bind (eval cond) (\c -> 
                if c == 0 then unit n else 
                bind (eval body) (\_ -> w (n + 1)))

e = EPlus (ENum 10) (ENum 23)

eassg = EAssg "x" (ENum 10)
eassg2 = EAssg "y" (ENum 20)
expt = ESeq (eassg) (eassg2)

e2 = EPlus (EVar "x") (EVar "y")

e3 = EPlus (EInc "x") (EInc "x")

-- a := 10; b := 20; a + b

eex = ESeq (EAssg "a" (ENum 10))
        (ESeq (EAssg "b" (ENum 20)) (EPlus (EVar "a") (EVar "b")))

-- if 10 then 20 else 30
exif = EIf (ENum 0) (ENum 20) (ENum 30)


-- b = 10; x = 0; while n do {(n:= n - 1; x = 2 * x;)}; x
ewhile = ESeq (EAssg "n" (ENum 10))
         (ESeq (EAssg "x" (ENum 1))
         (ESeq (EWhile (EVar "n")
                        (ESeq (EAssg "n" (ESub (EVar "n") (ENum 1)))
                              (EAssg "x" (EMult (EVar "x") (ENum 2)))))
        (EVar "x")))

emptyMem = \s -> 0


main = print (show ewhile ++ ":= " ++ show' (eval ewhile emptyMem))
    where show' (a,m) = show a
-- main = print (show expt ++ ":= " ++ show' (eval expt emptyMem))
--     where show' (a,m) = show a
-- main = print (show eassg ++ ":= " ++ show' (eval eassg emptyMem))
--     where show' (a,m) = show a
--main = print (show e3 ++ " = " ++ show' (eval e3 emptyMem))
--    where show' (a,m) = show a


-- Não vai ter aula semana que vem
-- Dever de casa, implementar o eval das expressões novas que a gnt já fez
-- Colocar um if
-- Colocar um while

-- Um exercício
-- Implementar dois programinhas que nem esse (ewhile)
-- implementar uma função fatorial e x^y