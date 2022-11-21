import Data.Char

newtype Parser a = Parser (String -> Maybe (a, String))
-- newtype Parser a = Parser (String -> Maybe (a, String))

apply :: Parser a -> (String -> Maybe (a, String))
apply (Parser f) = f


unitP :: a -> Parser a
unitP x = Parser (\s -> Just (x, s))

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP m f = Parser (\s -> aux (apply m s))
  where aux Nothing = Nothing
        aux (Just (x,s')) = apply (f x) s'

-- bind :: Parser a -> (a -> Parser b) -> Parser b
-- bind m f  = Parser (\s -> concat (map f' (apply m s)))
--    where f' (a, s) = apply (f a) s


instance Functor Parser where
  fmap f m = bindP m (unitP . f)


instance Applicative Parser where
  pure = unitP
  pf <*> px = bindP pf (\f -> bindP px (\x -> unitP (f x)))


instance Monad Parser where
  return = pure
  (>>=) = bindP
  
instance MonadFail Parser where
  fail _ = Parser (\s -> Nothing)


item :: Parser Char
item = Parser f
  where f [] = Nothing
        f (x:xs) = Just (x, xs)

-- (+++) :: Parser a -> Parser a -> Parser a
-- p +++ p' = Parser f
--   where f s = apply p s ++ apply p' s

orelse :: Parser a -> Parser a -> Parser a
p `orelse` p' = Parser f
  where f s = let l = apply p s in if null l then apply p' s else l


sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\c -> if p c then return c else fail "")

char :: Char -> Parser Char
char c = sat (== c)

pair :: Parser a -> Parser b -> Parser (a,b)
pair pa pb =
  do a <- pa
     b <- pb
     return (a,b)

optional :: Parser a -> Parser (Maybe a)
optional p = (p >>= (return . Just)) `orelse` return Nothing

many :: Parser a -> Parser [a]
many p = many1 p `orelse` return []

many1 :: Parser a -> Parser [a]
many1 p =
  do x <- p
     xs <- many p
     return (x:xs)

sp :: Parser ()
sp = many (sat isSpace) >> return ()

string :: String -> Parser ()
string [] = sp
string (x:xs) =
  do char x
     string xs
     return ()

strings :: [String] -> Parser String
strings [] = fail ""
strings (x:xs) = (string x >> return x) `orelse` strings xs

name :: Parser String
name =
  do first <- sat isAlpha
     rest <- many (sat isAlphaNum)
     sp
     return (first:rest)
----------------------------------------------------------------------
type Var = String
data Exp = ENum Integer
         | EVar Var
         | EPlus Exp Exp
         | ESub Exp Exp
         | EMul Exp Exp
         | EInc Var
         | EAssg Var Exp
         | ESeq Exp Exp
         | EIf Exp Exp Exp
         | EWhile Exp Exp

instance Show Exp where
  show (ENum n) = show n
  show (EVar s) = s
  show (EPlus e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (ESub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (EMul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (EInc v) = show v ++ "++ "
  show (EAssg v e) = "(" ++ show v ++ " := " ++ show e ++ ")"
  show (ESeq e1 e2) = show e1 ++ "; " ++ show e2
  show (EIf cond th el) = "if " ++ show cond ++ " then " ++
                          show th ++ " else " ++ show el
  show (EWhile cond body) = "while " ++ show cond ++ " do " ++ show body
---------------------------------------------------------------------

-- int = digit+
p_int :: Parser Exp
p_int =
  do n <- many1 (sat isDigit)
     sp
     return (ENum (read n))


-- var = alphanum+   (except reserved words)
p_var :: Parser Exp
p_var =
  do n <- name
     if (elem n rw) then fail ""
                    else return (EVar n)
  where
    rw = ["if", "then", "else", "while", "do"]

-- par = '(' exp ')'
p_par :: Parser Exp
p_par =
  do string "("
     x <- p_exp
     string ")"
     return x


p_primary :: Parser Exp
p_primary = p_int `orelse` p_var `orelse` p_par


-- app = primary+
-- p_app :: Parser Exp
-- p_app =
--   do exps <- many1 p_primary
--      return (foldl1 ExpApp exps)

buildBinExp :: Exp -> [(String, Exp)] -> Exp
buildBinExp e l = foldl f e l
  where
    f e (op, e') = (binOp op) e e'
    binOp :: String -> (Exp -> Exp -> Exp)
    binOp "+" = EPlus
    binOp "-" = ESub
    binOp "*" = EMul
    binOp ";" = ESeq

-- binExp elem ops = elem (ops elem)*
binExp :: Parser Exp -> [String] -> Parser Exp
binExp elem ops =
  do e <- elem
     es <- many (pair (strings ops) elem)
     return (buildBinExp e es)

term :: Parser Exp
term = binExp p_primary ["*", "/"]

p_sum :: Parser Exp       -- exp
p_sum = binExp term ["+", "-"]


p_arith = p_sum

-- if = 'if' exp 'then' exp 'else 'exp
p_if :: Parser Exp
p_if =
  do string "if"
     cond <- p_arith
     string "then"
     th <- p_exp1
     string "else"
     el <- p_exp1
     return (EIf cond th el)

p_while :: Parser Exp
p_while =
  do string "while"
     cond <- p_arith
     string "do"
     body <- p_exp1
     return (EWhile cond body)

p_seq = binExp p_exp1 [";"]



-- p_seq :: Parser Exp
-- p_seq =
--   do cmd1 <- p_exp
--      string ";"
--      cmd2 <- p_exp
--      return (ESeq cmd1 cmd2)

-- seq
-- p_seq :: Parser Exp
-- p_seq =
--   do exp <- p_exp
--      string ";"
--      exps <- many1 p_primary
--      return (ESeq exp (foldl1 ESeq exps))

p_assg :: Parser Exp
p_assg =
  do var <- name
     string ":="
     exp <- p_exp1
     return (EAssg var exp)     

p_inc :: Parser Exp
p_inc =
  do var <- name
     string "++"
     return (EInc var)

-- lambda = '\' name '->' exp
-- p_lambda :: Parser Exp
-- p_lambda =
--   do string "\\"
--      var <- name
--      string "->"
--      body <- p_exp
--      return (ExpLambda var p_ifbody)

-- let = 'letrec' name '=' '\' name '->' exp 'in' exp
-- p_let :: Parser Exp
-- p_let =
--   do string "letrec"
--      var <- name
--      string "="
--      string "\\"
--      var' <- name
--      string "->"
--      f <- p_exp
--      string "in"
--      bd <- p_exp
--      return (ExpLetrec var var' f bd)
     

p_exp1 :: Parser Exp
--p_exp = sp >> (p_let `orelse` p_lambda `orelse` p_if `orelse` p_arith)
--p_exp = sp >> (p_seq `orelse` p_if `orelse` p_while `orelse` p_primary)
p_exp1 = sp >> (p_while `orelse` p_if `orelse` p_assg `orelse` p_arith)

p_exp = p_seq
--p_exp = sp >> (p_seq `orelse` p_if `orelse` p_while `orelse` p_primary)
----------------------------------------------------------------------


------------------------- memarith ---------------------------------
---------------------------------------------------------------------

-- Memory type
type Mem = String -> Integer

type M a = Mem -> (a, Mem)

---------------------------------------------------------------------
-- unit & bind
unit :: a -> M a
unit x = \m -> (x, m)

bind :: M a -> (a -> M b) -> M b
-- bind :: (Mem -> (a, Mem)) -> (a -> (Mem -> (b, Mem))) -> (Mem -> (b, Mem))
bind ma f = \m -> let (a, m') = ma m in
                    (f a) m'
---------------------------------------------------------------------
-- auxiliar functions for expression evaluation
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
-- eval function
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
e1 = apply p_exp "n := 10; x := 1; while n do (n:= n - 1; x := 2 * x); x"

emptyMem = \s -> 0


prog = "while x do x := x + 1; x := x*4"
-- main
main :: IO ()
--main = print e1
--main = print "Hello"
--main = print (apply p_exp prog)
main = print (show e ++ " = " ++ show' (eval e emptyMem))
    where show' (a,m) = show a
          e = case e1 of
            Nothing -> ENum 0
            Just (e,_) -> e



-- type Mem = String -> Integer

-- --type M a = Mem -> (a, Mem)

-- newtype Memory a = Memory (Mem -> Maybe (a, Mem))

-- applyM :: Memory a -> (Mem -> Maybe (a, Mem))
-- applyM (Memory f) = f


-- unitM :: a -> Memory a
-- unitM x = Memory (\m -> Just (x, m))

-- bindM :: Memory a -> (a -> Memory b) -> Memory b
-- bindM m f = Memory (\m' -> aux (applyM m m'))
--   where aux Nothing = Nothing
--         aux (Just (x,m'')) = applyM (f x) m''

-- -- bind :: Parser a -> (a -> Parser b) -> Parser b
-- -- bind m f  = Parser (\s -> concat (map f' (apply m s)))
-- --    where f' (a, s) = apply (f a) s


-- instance Functor Memory where
--   fmap f m = bindM m (unitM . f)


-- instance Applicative Memory where
--   pure = unitM
--   pf <*> px = bindM pf (\f -> bindM px (\x -> unitM (f x)))


-- instance Monad Memory where
--   return = pure
--   (>>=) = bindM
  
-- instance MonadFail Memory where
--   fail _ = Memory (\s -> Nothing)

---------------------------------------------------------------------

--query :: String -> Memory Integer
--query var = \m -> (m var, m)

-- query :: String -> Memory Integer
-- query var = Memory (\m -> Just (m var, m))


--inc var = Memory (\m -> Just (m var, m'))
--    where m' = \var' -> if var' == var then m var' + 1 else m var'

--inc :: String -> Memory Integer
-- inc :: String -> Mem -> (Integer, Mem)
--inc var m = (m var, m')
--  where m' = \var' -> if var' == var then m var' + 1 else m var'

-- assg :: String -> Memory Integer -> Memory Integer
-- assg var comp m = let (v, m') = comp m
--                       m'' = \var' -> if var == var' then v else m' var'
--                    in (v, m'')

---------------------------------------------------------------------

-- lift f ma = bindM ma (unitM . f)

-- bind2 :: Memory a -> Memory b -> (a -> b -> Memory c) -> Memory c
-- bind2 ma mb f = bindM ma (\a -> bindM mb (\b -> f a b))

-- lift2 :: (a -> b -> c) -> Memory a -> Memory b -> Memory c
-- lift2 f ma mb = bind2 ma mb (\a b -> (unitM (f a b)))

-- ---------------------------------------------------------------------

-- eval :: Exp -> Memory Integer

-- eval (ENum n) = unitM n

-- eval (EVar var) = query var

-- eval (EPlus e1 e2) = lift2 (+) (eval e1) (eval e2)
-- eval (ESub e1 e2) = lift2 (-) (eval e1) (eval e2)
-- eval (EMul e1 e2) = lift2 (*) (eval e1) (eval e2)

-- eval (ESeq e1 e2) = lift2 seq (eval e1) (eval e2)
--   where seq x y = y

-- --eval (EAssg var e) = assg var (eval e)

-- --eval (EInc var) = inc var

-- eval (EIf cond th el) = 
--   bindM (eval cond) (\c -> if c /= 0 then eval th else eval el)

-- eval (EWhile cond body) = w 0
--   where w n = bindM (eval cond) (\c -> 
--                 if c == 0 then unitM n
--                 else bindM (eval body) (\_ -> w (n + 1)))


-- n = 10; x = 1; while n do { n := n - 1; x = 2 * x; }; x

