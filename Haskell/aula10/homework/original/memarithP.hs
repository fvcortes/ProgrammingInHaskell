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

{-
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
-}


--{-------------------------------------------------------------------


newtype Parser a = Parser (String -> Maybe (a, String))
-- newtype Parser a = Parser (String -> Maybe (a, String))

apply :: Parser a -> (String -> Maybe (a, String))
apply (Parser f) = f


unit :: a -> Parser a
unit x = Parser (\s -> Just (x, s))

bind :: Parser a -> (a -> Parser b) -> Parser b
bind m f  = Parser (\s -> aux (apply m s))
  where aux Nothing = Nothing
        aux (Just (x, s')) = apply (f x) s'


instance Functor Parser where
  fmap f m = bind m (unit . f)


instance Applicative Parser where
  pure = unit
  pf <*> px = bind pf (\f -> bind px (\x -> unit (f x)))


instance Monad Parser where
  return = unit
  (>>=) = bind
  fail _ = Parser (\s -> Nothing)


item :: Parser Char
item = Parser f
  where f [] = Nothing
        f (x:xs) = Just (x, xs)

-- (+++) :: Parser a -> Parser a -> Parser a
-- p +++ p' = Parser f
--   where f s = apply p s ++ apply p' s

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\c -> if p c then return c else fail "")

char :: Char -> Parser Char
char c = sat (== c)

pair :: Parser a -> Parser b -> Parser (a,b)
pair pa pb =
  do a <- pa
     b <- pb
     return (a,b)


orelse :: Parser a -> Parser a -> Parser a
p `orelse` p' = Parser f
  where f s = let l = apply p s in if null l then apply p' s else l


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


-- 3   x   (...)
-- 3 * x / (...)
--  3 * x / (...) + 4

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
term = binExp p_primary ["*"]

p_sum :: Parser Exp       -- exp
p_sum = binExp term ["+", "-"]

-- a + b + c + d   -->  (((a + b) + c) + d)
-- a ; b ; c ; d   -->  (((a ; b) ; c) ; d)

p_arith = p_sum


p_assg :: Parser Exp
p_assg =
  do var <- name
     string ":="
     e <- p_exp1
     return (EAssg var e)


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


-- while exp do exp
p_while :: Parser Exp
p_while =
  do string "while"
     cond <- p_arith
     string "do"
     body <- p_exp1
     return (EWhile cond body)


p_exp1 :: Parser Exp
p_exp1 = sp >> (p_while `orelse` p_if `orelse` p_assg `orelse` p_arith)

p_seq = binExp p_exp1 [";"]

{-
p_seq = 
  do c1 <- p_exp1
     rest <- many (string ";" >> p_exp1)
     return (foldr ESeq c1 rest)
-}


{-
p_seq = 
  do c1 <- p_exp1
     rest <- optional (string ";" >> p_seq)
     case rest of
       Nothing -> return c1
       Just sq -> return (ESeq c1 sq)
-}

p_exp = p_seq

--}-------------------------------------------------------------------


type Mem = String -> Integer

type M a = Mem -> (a, Mem)

---------------------------------------------------------------------
unitM :: a -> M a
unitM x = \m -> (x, m)




bindM :: M a -> (a -> M b) -> M b
-- bind :: (Mem -> (a, Mem)) -> (a -> (Mem -> (b, Mem))) -> (Mem -> (b, Mem))
bindM ma f = \m -> let (a, m') = ma m in
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
lift f ma = bindM ma (unitM . f)


bind2 :: M a -> M b -> (a -> b -> M c) -> M c
bind2 ma mb f = bindM ma (\a -> bindM mb (\b -> f a b))

lift2 :: (a -> b -> c) -> M a -> M b -> M c
lift2 f ma mb = bind2 ma mb (\a b -> (unitM (f a b)))


-- M�nada:
-- Um construtor de tipos M (e.g., Maybe)
-- Uma fun��o 'unit :: a -> M a'
-- Uma fun��o 'bind :: M a -> (a -> M b) -> M b'
-- (N�o existe uma fun��o com tipo 'M a -> a' !!)

---------------------------------------------------------------------


eval :: Exp -> M Integer

eval (ENum n) = unitM n

eval (EVar var) = query var

eval (EPlus e1 e2) = lift2 (+) (eval e1) (eval e2)
eval (ESub e1 e2) = lift2 (-) (eval e1) (eval e2)
eval (EMul e1 e2) = lift2 (*) (eval e1) (eval e2)

eval (ESeq e1 e2) = lift2 seq (eval e1) (eval e2)
  where seq x y = y

eval (EAssg var e) = assg var (eval e)

eval (EInc var) = inc var

eval (EIf cond th el) = 
  bindM (eval cond) (\c -> if c /= 0 then eval th else eval el)

eval (EWhile cond body) = w 0
  where w n = bindM (eval cond) (\c -> 
                if c == 0 then unitM n
                else bindM (eval body) (\_ -> w (n + 1)))
                

assert :: Bool -> a -> a
assert False x = error "Assertion failed"
assert _ x = x

-- n = 10; x = 1; while n do { n := n - 1; x = 2 * x; }; x
e1 =  apply p_exp "n := 10; x := 1; while n do (n := n - 1; x := 2 * x); x"


emptyMem = \s -> 0


main :: IO ()
main = print (show e ++ " = " ++ show' (eval e emptyMem))
   where show' (a,m) = show a
         e = case e1 of
             Just (e',"") -> e'
             _ -> ENum 0

-- main = print e1




