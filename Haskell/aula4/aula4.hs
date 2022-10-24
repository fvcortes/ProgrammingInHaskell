{-
Tipos algébricos
-}
data Bool' = False' | True'
              deriving Show
data Weekday = Monday 
             | Tuesday 
             | Wednesday 
             | Thursday
             | Friday
      deriving Show

data Point = P Double Double
    deriving Show

data ItemDesc = Cod Integer
              | Desc String
              | NoDesc
      deriving Show

data Result = Found Integer | NotFound
      deriving Show

data Result' a = Founda a | NotFounda


search' :: [(String, Integer)] -> String -> Result' Integer
search' [] _ = NotFounda
search' ((k,v):xs) y = if k==y then Founda v else search' xs y


search :: [(String, Integer)] -> String -> Result
search [] _ = NotFound
search ((k,v):xs) y = if k==y then Found v else search xs y

show' :: Result -> String
show' NotFound = "NotFound"
show' (Found x) = "result is " ++ (show x)

show'' :: Result -> String
show'' res = (case res of
                NotFound -> "not found"
                Found x -> "result is " ++ (show x)) ++ " is the truth"
type Map = [(String, Integer)]
show''' :: Map -> String -> String
show''' map key = case (search map key) of
                    NotFound -> "not found"
                    Found x -> "result is " ++ (show x)
i1 = Cod 12009
i2 = Desc "cereais"

getDesc :: ItemDesc -> String
getDesc (Desc s) = s
getDesc (Cod i) = "codigo " ++ (show i)
getDesc (NoDesc) = "Sem descricao"
getX :: Point -> Double
getX (P x y) = x

getY :: Point -> Double
getY (P x y) = y

distance :: Point -> Point -> Double
distance (P x1 y1) (P x2 y2) = sqrt ((dx * dx) + (dy *dy))
  where dx = x1 - x2
        dy = y1 - y2

foo:: Bool' -> Integer
foo False' = 0
foo True' = 1

showBool' :: Bool' -> String
showBool' False' = "False'"
showBool' True' = "True'"

(||.) :: Bool' -> Bool' -> Bool'
(||.) True' _ = True'
(||.) False' b = b

p:: Point
p = P 0.5 1.45

main = print (show''' [("a", 1), ("b", 2)] "b")