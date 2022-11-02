import System.IO
import Data.Char

main = loop 0 0 0
loop lc wc cc =
    do e <- isEOF
       if e then print (lc,wc,cc)
            else do l <- getLine
                    loop (lc + 1) (wc + length (words l)) (cc + length l + 1)

wordc :: String -> Bool -> Integer
wordc "" False = 0
wordc "" True = 1
wordc (c:cs) False = if isAlphaNum c then wordc cs False else wordc cs True
wordc (c:cs) True = if isAlphaNum c then 1 + wordc cs False else wordc cs True


--main = print (wordc " abc  a def " False)
