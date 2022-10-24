-- O que tem de errado em:
{-
soma :: [a] -> a
soma [] = 0 -- zero atribui um tipo que nao pode ser compativel com a
soma (x:xs) = x + (soma xs) -- Nao sabemos o tipo de x, o operador + pode nao funcionar

-- redefinindo
Usa-se uma função de conversão e uma função de operador
soma :: (a -> a -> a) -> (Integer -> a) -> [a] -> a
soma op conv [] = conv 0
soma op conv (x:xs) = x `op` (soma op conv xs)

> soma (+) id [3,4,5]

-}

--data Interface a = I (a -> a -> a) (a -> a -> a) (Integer -> a)

class Interface a where
    add :: a -> a -> a
    prod :: a -> a -> a
    conv :: Integer -> a

--add (I x y z) = x
--prod (I x y z) = y
--conv (I x y z) = z

--soma :: Interface a -> [a] -> a
--soma it [] = conv it 0
--soma it (x:xs) = (add it) x (soma it xs)

soma :: Interface a => [a] -> a
soma [] = conv 0
soma (x:xs) = x `add` (soma xs)

{-
 é como se o haskell tivesse isso já declarado dentro dele
 class Num a where
    (+) :: a -> a -> a
    fromInteger :: a -> a -> a

soma :: Num a => [a] -> a
soma [] = fromInteger 0
soma (x:xs) = x `add` (soma xs)

Outra definição importante de haskell

Class Eq a where
    (==) :: a -> a -> Bool
    (<>) :: a -> a -> Bool

    x <> y = not (x == y)

Class show a where
    show :: a -> String
-}
instance Interface Integer where
    add = (+)
    prod = (*)
    conv = id

data Color = Red | Blue
    deriving (Eq, Show, Ord)

eqColor Red Red = True
eqColor Blue Blue = True
eqColor _ _ = False

-- instance Eq Color where
--     (==) = eqColor

-- instance Ord Color where
--     Red <= Blue = True
--     Blue <= Red = False
--     _ <= _ = True

-- Exemplo 1
--main = print(soma ([3,4,22]::[Integer]))

-- Exemplo 2
main = print (Red == Red)
