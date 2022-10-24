{-
Tipos algébricos
-}

data Result a = NotFound | Found a

data List a = Empty | Cons a (List a)
      deriving Show

type Map = List (String, Integer)

data Tree a = Empty' | Node (Tree a) a (Tree a)
      deriving Show
leaf x = Node Empty' x Empty'

t1 = (Node (leaf 10) 15 (leaf 20))

len :: List a -> Integer
len Empty = 0
len (Cons x xs) = 1 + len xs

size :: Tree a -> Integer
size Empty' = 0
size (Node t1 r t2) = size t1 + 1 + size t2

search :: Tree Integer -> Integer -> Bool
search Empty' _ = False
search (Node t1 r t2) x = if x == r then True 
                          else if x < r then search t1 x 
                                        else search t2 x

insert :: Integer ->  Tree Integer -> Tree Integer
insert a Empty' = leaf a
insert a n@(Node t1 r t2) = if a == r then n
                                 else if a < r then Node (insert a t1) r t2
                                               else Node t1 r (insert a t2)


l1 = Cons 10 (Cons 20 (Cons 30 Empty))

t2 = insert 10 Empty'
t3 = insert 20 (insert 5 (insert 10 Empty'))
t4 = insert 15 (insert 20 (insert 5 (insert 10 Empty')))

t5 = Empty'

traverse' :: Tree a -> [a]
traverse' Empty' = []
traverse' (Node t1 r t2) = traverse' t1 ++ [r] ++ traverse' t2

-- traverse mais eficiente
traverse'' Empty' xs = xs
traverse'' (Node t1 r t2) xs = traverse'' t1 (r : traverse'' t2 xs)

{-
Fazer um função traverse
traverse :: Tree a -> [a]
-}
main = print (traverse'' t4 [])