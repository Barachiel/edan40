module Utilities where

-- Returns output of 2 functions as a touple --
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Error handling? Safeguard? --
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- If first input is nothing, return second. Otherwise return first --

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Attempt to execute f(x), if Nothing, return x --

try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Applies function f recursively until the output is the same as the input --

fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Extract value from list via fractional index --

pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
