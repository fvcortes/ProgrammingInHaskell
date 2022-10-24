


instance Num Bool where
    (+) = (||)
    (*) = (&&)
    (-) True False = True
    (-) True True = False
    (-) False _ = False
    fromInteger 0 = False
    fromInteger _ = True
    abs x = x
    signum x = x

-- Exercício
-- Criar um tipo de vocês e implementar a classe num para isso
-- Maximo divisor comum
-- Implementar soma de fração, produto de fração, subtração de fração, fromInteger, abs, signum (ver os sinais de cada numero)
-- gcd - greater common divisor -> Lehmer's GCD algorithm

data Fracao = F Integer Integer
    
instance Show Fracao where 
    show (F x y) = show x ++ "/" ++ show y 

somafrac:: Fracao -> Fracao -> Fracao
somafrac (F a x) (F b y) = if x == y then F (a + b) (x) else F (a*(y `div` (gcd x y)) + b*(x `div` (gcd x y))) ((x `div` (gcd x y)) * y)

subfrac:: Fracao -> Fracao -> Fracao
subfrac (F a x) (F b y) = if x == y then F (a - b) (x) else F (a*(y `div` (gcd x y)) - b*(x `div` (gcd x y))) ((x `div` (gcd x y)) * y)



multifrac:: Fracao -> Fracao -> Fracao
multifrac (F a x) (F b y) = F (a*b) (x*y)

divfrac:: Fracao -> Fracao -> Fracao
divfrac (F a x) (F b y) = multifrac (F a x) (F y b)


instance Num Fracao where 
    (+) = somafrac
    (-) = subfrac
    (*) = multifrac
    fromInteger n = F n 1
    abs (F a x) = if a < 0 then if x < 0 then F ((-1)*a) ((-1)*x) else  F ((-1)*a) x else F a x
    signum (F a x) = (F (-1) (-1)) * (F a x)
{-


(+)

--somafrac:: Fracao -> Fracao -> Fracao
-- somafrac (F a x) (F b y) = F ( a*( y/(gcd x y) ) + b*( x/(gcd x y) ) ) ( x/(gcd x y) * y/((gcd x y)))

(-)
-- subfrac:: Fracao -> Fracao -> Fracao
-- somafrac (F a x) (F b y) = F ( a*( y/(gcd x y) ) - b*( x/(gcd x y) ) ) ( x/(gcd x y) * y/((gcd x y)))

(fromInteger)
fromInteger n = F n 1

(abs)
abs (F x y) = 

-}


f1 = F 1 6
f2 = F 2 10 

main = print (f1)
