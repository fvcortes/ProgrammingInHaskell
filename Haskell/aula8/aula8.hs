
-- putChar :: Char -> IO ()
-- getChar :: IO Char
x:: IO ()
x = putChar 'a'


foo :: IO ()
foo = let y = (x >>= (\u -> return ())) in y

putString :: String -> IO ()
putString "" =  return ()
--putString (c:cs) = putChar c >> putString cs
putString (c:cs) = do putChar c
                      putString cs

getString :: IO String
getString = 
    do c <- getChar
       if c == '\n' then return "\n"
                    else do cs <- getString
                            return (c:cs)
-- getString = 
--    getChar >>= (\c -> if c == '\n' then return "\n" 
--                                    else getString >>= (\cs -> return (c:cs)))
-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- a >> b = a >>= (\_ -> b)

main = getString >>= putString >> putString "fim\n"
-- main = getChar >>= putChar

-- main = foo
