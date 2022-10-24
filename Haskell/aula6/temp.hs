---------------------------------------------------------------------
lift0 :: a -> Maybe a
lift0 x = Just x

bind :: (a -> Maybe b) -> Maybe a -> Maybe b
bind f Nothing = Nothing
bind f (Just x) = f x
---------------------------------------------------------------------

lift1 :: (a -> b) -> Maybe a -> Maybe b
lift1 f ma = bind (\x -> lift0 (f x)) ma



bind2 :: (x -> (y -> Maybe z)) -> (Maybe x -> (Maybe y -> Maybe z))
bind2 f mx my = bind (\x -> bind (\y -> f x y) my ) mx

bind2' :: Maybe x -> Maybe y -> (x -> y -> Maybe z) -> Maybe z
bind2' mx my f = bind mx (\x -> bind my (\y -> f x y)) 

-- bind2 f mx my = bind aux mx
--   where aux :: x -> Maybe z
--         aux x = ???
main = print ("Hello")