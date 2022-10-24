append :: [a] -> [a] -> [a]
append [] ys = ys -- regra 1
append (x:xs) ys = x : append xs ys



rev :: [a] -> [a]
rev [] = []
rev (x:xs) = append (rev xs) [x]
--[1,2,3,4] --> [4,3,2,1]

foo [] ys = ys
foo (x:xs) ys = foo xs (x:ys)

rev' xs = foo xs [] -- mais eficiente

-- Raciocínio equacional
-- [] ++ ys = ys?
--  sim pois você disse isso explicitamente
--
-- xs ++ [] = xs?
-- Dois casos a considerar
-- caso 1: xs = []
-- xs ++ [] = 
-- [] ++ [] = 
--  [] = xs

-- caso 2: xs = x:xs'
--  xs ++ [] = 
--  (x:xs') ++ [] = 
--  x : (xs' ++ []) = ??
-- Hipótese de indução: xs' ++ [] = xs' (HI)
-- x : (xs ++ []) = (HI) x : xs' = xs
------- Princípio da indução
-- P é verdade para a lista vazia
-- Se P é verdade para xs', então P é verdade para x:xs'
-----------------------------------------
-- Então P é veradade para toda lista xs.
-------------------------------------------------------
-- (Outra linguagem): while f(x) != 0 do {...}
-- Logo após o loop, f(x) é diferente de 0?
--      Não sabemos pois f(x) pode me dar outro valor quando chamado
--      mas em Haskell f(x) é o mesmo sempre para o mesmo x
-------------------------------------------------
-- Associatividade do append
-- xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
-- Prova por indução sobre xs:
-- caso 1: xs = [] (def)
-- xs ++ (ys ++ zs) = (def)
-- [] ++ (ys ++ zs) = (pela regra 1)
-- (ys ++ zs)
-- (xs ++ ys) ++ zs = (def) (vindo do outro lado)
-- ys ++ zs

-- Caso 2: xs = x:xs'
-- HI: xs' ++ (ys ++ zs) = (xs' ++ ys) ++ zs
-- xs ++ (ys ++ zs) = (def)
-- (xs:xs') ++ (ys ++ zs) = (pela regra 2)
-- x : (xs' ++ (ts ++ zs)) = (HI)
-- x : ((xs' ++ ys) ++ zs)

-- vindo do outro ado
-- (xs ++ ys) ++ zs = (def)
-- ((x:xs') ++ ys) ++ zs = (2)
-- (x: (xs' ++ ys)) ++ zs = (2)
-- x: ((xs' ++ ys) ++ zs) que é exatamente igual a prova vinda do primeiro lado anteriormente

sum1 :: [Integer] -> [Integer]
sum1 [] = []
sum1 (x:xs) = (x + 1) : sum1 xs

dobra :: [Integer] -> [Integer]
dobra [] = []
dobra (x:xs) = (2 * x) : dobra xs

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs

filtrapos :: [Integer] -> [Integer]
filtrapos [] = []
filtrapos (x:xs) = if x > 0 then x : filtrapos xs else filtrapos xs

filtra :: (a -> Bool) -> [a] -> [a]
filtra op [] = []
filtra op (x:xs) = if op x then x : filtra op xs else filtra op xs

quicks [] = []
quicks (x:xs) = quicks (filter (\y -> y < x) xs) ++
                (x : quicks (filter (\y -> y > x) xs))

soma [] = 0
soma (x:xs) = x + sum xs

prod [] = 1
prod (x:xs) = x * prod xs

fold':: (a -> b -> b) -> b -> [a] -> b
fold' op z [] = z
fold' op z (x:xs) = x `op` fold' op z xs

foldl':: (b -> a -> b) -> b -> [a] -> b
foldl' op acc [] = acc
foldl' op acc (x:xs) = foldl' op (acc `op` x) xs

-- Exercícios
-- 1) Escrever uma função len (comprimento de uma lista)
-- 2) Provar que len (xs ++ ys) = len xs + len ys
-- 3) Provar que len (rev xs) = len xs
-- *) Provar que rev [x] = [x]
-- 4) rev (xs ++ ys) = rev ys ++ rev xs
-- 5) rev (rev xs) = xs

-- 1)
len :: Num p => [a] -> p
len [] = 0
len (x:xs) = 1 + len xs

-- 2
{-
def: len (xs ++ ys) = len xs + len ys

caso 1: xs = []
len (xs ++ ys) = (def)
len ([] ++ ys) = (pela regra de append vazio)
len (ys) 
(Vindo do outro lado)
len xs + len ys = (def)
len [] + len ys = (def)
0 + len ys (...)
len (ys) = len ys <--
-----------------

caso 2: xs = (x:xs)
HI: len (xs' ++ ys) = len xs' + len ys
len (xs ++ ys) = (def)
len ( (x:xs') ++ ys ) = (...)
len ( x : (xs' ++ ys) ) = (def)
1 + len (xs' ++ ys) = (HI)
1 + len xs' + len ys <--
(outro lado)
len xs + len ys = (def)
len (x:xs') + len ys = (def)
1 + len xs' + len ys <--
--------------------
-}

--3 
{-
 len (rev xs) = len xs
 HI : len (rev xs') = len (xs')
 caso 1: xs = []
len (rev xs) = (def)
len (rev []) = (def)
len ([]) = (def)
0
(outro lado)
len xs = (def)
len [] = (def)
0
----
caso 2: xs = x:xs'
len (rev xs) = len xs = (def)
len ( rev (x:xs') ) = len (x:xs') = (def rev)
len (rev xs' ++ [x]) = (lemma len)
len (rev xs') + len [x] = (lemma len)
len (rev xs') + 1 + len [] = (def len)
len (rev xs') + 1 (HI)
len (xs') + 1 <---
(outr lado)
len (x:xs') = (def len)
1 + len (xs') = (arith)
len (xs') + 1 <---
-}

-- *
{-
rev [x] = [x] (def rev)
rev [] ++ [x] = (def rev)
[] ++ [x] = (def append)
[x]
-}

--4
{-
rev (xs ++ ys) = rev ys ++ rev xs
HI: rev(xs' ++ ys) = rev ys ++ rev xs'
caso 1: xs = (x:xs')
rev( (x:xs') ++ ys ) = rev ys ++ rev (x:xs') => (def append)
rev (x : (xs' ++ ys)) = (def rev)
( rev (xs' ++ ys) ) ++ [x] = (HI)
(rev ys ++ rev xs') ++ [x] <--
(outro lado)
rev ys ++ rev (x:xs') = (def rev)
rev ys ++ (rev xs' ++ [x]) = (regra associatividade do append)
(rev ys ++ rev xs') ++ [x] <--
------------------------
caso 2: xs = []
...
-}

{-
  Lemma 1 rev :  rev [x] = [x]
  rev (x:[]) = (x:[]) (regra 2 de rev)
  rev [] ++ [x] = (regra 1 de rev)
  [] ++ [x] = (regra 1 de append)
  [x]
-}
--5
{-
rev (rev xs) = xs
caso 1: xs = []
rev (rev []) = [] => (def)
rev ([]) = [] => (def)
[] = []
-------
caso 2: xs = (x:xs')
HI: rev (rev xs') = xs'
rev (rev (x:xs')) = (x:xs') = (regra 2 de rev)
rev (rev xs' ++ [x]) = (lemma rev)
rev [x] ++ rev (rev xs'  = (HI)
rev [x] ++ xs'  (lemma 1 rev)
[x] ++ xs' = (def de lista)
(x:[]) ++ xs' = (regra 2 do append)
x: ([] ++ xs') = (regra 1 do append)
x : xs' <--
(outro lado)
x:xs' <--
-}
-- last quicks (map sin [1..1000] )
main = print (rev' [1,2,3,4])
