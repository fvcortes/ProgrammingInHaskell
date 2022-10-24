flip' f = \x y -> f y x

f2 = flip' (++)

compo g f = \x -> g ( f x )

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x -> \y -> f y x
f3 = flip'' (++)

f4 = flip (++) "2" "3"

soma xs = if null xs then 0 else (head xs) + (soma (tail xs))

soma' [] = 0
soma' (h:t) = h + soma' t

len [] = 0
len (x:xs) = 1 + len xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = append (rev xs) [x]
--[1,2,3,4] --> [4,3,2,1]

main = print (rev [1,2,3,4])
