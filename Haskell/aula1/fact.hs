fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

sub :: Integer -> Integer -> Integer
sub x y = x - y

(&&.) :: Bool -> Bool -> Bool
True &&. x = x
False &&. x = False

(&&..) :: Bool -> Bool -> Bool
True &&.. True = True
True &&.. False = False
False &&.. True = False
False &&.. False = False

main = print (fact 5)
