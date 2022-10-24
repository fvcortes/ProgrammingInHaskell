import System.IO

main = loop 0 0
loop lc wc cc =
    do e <- isEOF
       if e then print (lc,cc)
            else do l <- getLine
                    loop (lc + 1) (wc + words l) (cc + length l + 1)

words :: String -> Integer
