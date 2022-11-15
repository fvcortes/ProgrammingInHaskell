import Data.Char

newtype Parser a = Parser (String -> Maybe (a, String))
-- newtype Parser a = Parser (String -> Maybe (a, String))

applyP :: Parser a -> (String -> Maybe (a, String))
applyP (Parser f) = f


unitP :: a -> Parser a
unitP x = Parser (\s -> Just (x, s))

bindP :: Parser a -> (a -> Parser b) -> Parser b
bindP m f = Parser (\s -> aux (applyP m s))
  where aux Nothing = Nothing
        aux (Just (x,s')) = applyP (f x) s'

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
  where f s = let l = applyP p s in if null l then applyP p' s else l


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

--type M a = Mem -> (a, Mem)

--------------------------------------------------------------------- 

newtype Memory a = Memory (Mem -> Maybe (a, Mem))

applyM :: Memory a -> (Mem -> Maybe (a, Mem))
applyM (Memory f) = f


unitM :: a -> Memory a
unitM x = Memory (\m -> Just (x, m))

bindM :: Memory a -> (a -> Memory b) -> Memory b
bindM m f = Memory (\m' -> aux (applyM m m'))
  where aux Nothing = Nothing
        aux (Just (x,m'')) = applyM (f x) m''

-- bind :: Parser a -> (a -> Parser b) -> Parser b
-- bind m f  = Parser (\s -> concat (map f' (apply m s)))
--    where f' (a, s) = apply (f a) s


instance Functor Memory where
  fmap f m = bindM m (unitM . f)


instance Applicative Memory where
  pure = unitM
  pf <*> px = bindM pf (\f -> bindM px (\x -> unitM (f x)))


instance Monad Memory where
  return = pure
  (>>=) = bindM
  
instance MonadFail Memory where
  fail _ = Memory (\s -> Nothing)

---------------------------------------------------------------------

--query :: String -> Memory Integer
--query var = \m -> (m var, m)

query :: String -> Memory Integer
query var = Memory (\m -> Just (m var, m))


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

lift f ma = bindM ma (unitM . f)

bind2 :: Memory a -> Memory b -> (a -> b -> Memory c) -> Memory c
bind2 ma mb f = bindM ma (\a -> bindM mb (\b -> f a b))

lift2 :: (a -> b -> c) -> Memory a -> Memory b -> Memory c
lift2 f ma mb = bind2 ma mb (\a b -> (unitM (f a b)))

---------------------------------------------------------------------

eval :: Exp -> Memory Integer

eval (ENum n) = unitM n

eval (EVar var) = query var

eval (EPlus e1 e2) = lift2 (+) (eval e1) (eval e2)
eval (ESub e1 e2) = lift2 (-) (eval e1) (eval e2)
eval (EMul e1 e2) = lift2 (*) (eval e1) (eval e2)

eval (ESeq e1 e2) = lift2 seq (eval e1) (eval e2)
  where seq x y = y

--eval (EAssg var e) = assg var (eval e)

--eval (EInc var) = inc var

eval (EIf cond th el) = 
  bindM (eval cond) (\c -> if c /= 0 then eval th else eval el)

eval (EWhile cond body) = w 0
  where w n = bindM (eval cond) (\c -> 
                if c == 0 then unitM n
                else bindM (eval body) (\_ -> w (n + 1)))



main = print "Hello"