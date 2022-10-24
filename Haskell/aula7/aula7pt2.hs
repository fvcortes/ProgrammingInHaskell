-- Mônada é um construtor de tipos M (e.g. Maybe)
-- Uma função 'unit :: a -> M a' joga valores para dentro desse mundo de Mônadas
-- Uma função 'bind :: M a -> (a -> M b) -> M b'
-- Não existe função 'M a -> a' !!

---------------------------
-- Outro mônada é 'listas'
--M a = [a]
unit :: a -> [a]
unit x = (x : nil)

bind :: [a] -> (a -> [b]) -> [b]
bind xs f = concat (map f xs)
-------------------------------

-------------------------------
-- Mônada de identidade
-- M a = a
-- unit = id
-- bind a f = f a
--------------------------------
main = print ("Hello")
