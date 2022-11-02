import Data.Char

newtype Parser a = Parser (String -> Maybe (a, String))
-- newtype Parser a = Parser (String -> Maybe (a, String))

apply :: Parser a -> (String -> Maybe (a, String))
apply (Parser f) = f


unit :: a -> Parser a
unit x = Parser (\s -> Just (x, s))

bind :: Parser a -> (a -> Parser b) -> Parser b
bind m f = Parser (\s -> aux (apply m s))
  where aux Nothing = Nothing
        aux (Just (x,s')) = apply (f x) s'

-- bind :: Parser a -> (a -> Parser b) -> Parser b
-- bind m f  = Parser (\s -> concat (map f' (apply m s)))
--    where f' (a, s) = apply (f a) s


instance Functor Parser where
  fmap f m = bind m (unit . f)


instance Applicative Parser where
  pure = unit
  pf <*> px = bind pf (\f -> bind px (\x -> unit (f x)))


instance Monad Parser where
  return = unit
  (>>=) = bind
  
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

data Exp = ExpK Integer          -- constants
         | ExpVar Var            -- variables
         | ExpAdd Exp Exp        -- e1 + e2
         | ExpSub Exp Exp        -- e1 - e2
         | ExpMul Exp Exp        -- e1 * e2
         | ExpDiv Exp Exp        -- e1 / e2
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3
         | ExpApp Exp Exp        -- e1 e2
         | ExpLambda Var Exp     -- \x -> e
         | ExpLetrec Var Var Exp Exp        -- letrec x=(\x'->e') in e
             deriving Show


-- int = digit+
p_int :: Parser Exp
p_int =
  do n <- many1 (sat isDigit)
     sp
     return (ExpK (read n))


-- var = alphanum+   (except reserved words)
p_var :: Parser Exp
p_var =
  do name <- name
     if (elem name rw) then fail ""
                       else return (ExpVar name)
  where
    rw = ["if", "then", "else", "letrec", "in"]


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
p_app :: Parser Exp
p_app =
  do exps <- many1 p_primary
     return (foldl1 ExpApp exps)


buildBinExp :: Exp -> [(String, Exp)] -> Exp
buildBinExp e l = foldl f e l
  where
    f e (op, e') = binOp op e e'
    binOp :: String -> (Exp -> Exp -> Exp)
    binOp "+" = ExpAdd
    binOp "-" = ExpSub
    binOp "*" = ExpMul
    binOp "/" = ExpDiv

-- binExp = elem (ops elem)*
binExp :: Parser Exp -> [String] -> Parser Exp
binExp elem ops =
  do e <- elem
     es <- many (pair (strings ops) elem)
     return (buildBinExp e es)

p_mul :: Parser Exp
p_mul = binExp p_app ["*", "/"]

p_sum :: Parser Exp
p_sum = binExp p_mul ["+", "-"]

p_arith = p_sum

-- if = 'if' exp 'then' exp 'else 'exp
p_if :: Parser Exp
p_if =
  do string "if"
     cond <- p_exp
     string "then"
     th <- p_exp
     string "else"
     el <- p_exp
     return (ExpIf cond th el)

-- lambda = '\' name '->' exp
p_lambda :: Parser Exp
p_lambda =
  do string "\\"
     var <- name
     string "->"
     body <- p_exp
     return (ExpLambda var body)

-- let = 'letrec' name '=' '\' name '->' exp 'in' exp
p_let :: Parser Exp
p_let =
  do string "letrec"
     var <- name
     string "="
     string "\\"
     var' <- name
     string "->"
     f <- p_exp
     string "in"
     bd <- p_exp
     return (ExpLetrec var var' f bd)
     

p_exp :: Parser Exp
p_exp = sp >> (p_let `orelse` p_lambda `orelse` p_if `orelse` p_arith)
----------------------------------------------------------------------

prog = "\\x -> letrec y = \\x -> x * 5 in if x then y a b else b * 25"

main = print (apply p_exp prog)


-- Exercício --->
-- Fazer uns programinhas para uma função de fatorial, e x^y
-- fat = "\n -> ..."

-- parser


