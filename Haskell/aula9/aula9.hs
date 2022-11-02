import Data.Char
-- Parser

newtype Parser a = Parser (String ->  [(a, String)])

apply :: Parser a -> (String -> [(a, String)])
apply (Parser f) st = f st

unit :: a -> Parser a
unit x = Parser (\s -> [(x,s)])

bind :: Parser a -> (a -> Parser b) -> Parser b
bind m f = Parser (\s -> concat (map f' (apply m s)))
    where f' (a,s) = apply (f a) s          


-- item é um parse de caracteres
-- Pergunta: por que o tipo de item é char e n string?

-- Exercício para casa: apaga o "+++" e reescreva o Parser usando Maybe ao invés de lista
-- Para Maybes, ou ele é Nothing ou ele é Just...
-- Se der Nothing eu faço isso, se der Just eu faço isso...
main = print "Hello"

