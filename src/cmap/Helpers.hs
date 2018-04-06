module Helpers where

type Id = Int

class Unwrap f where
    unwrap :: f a -> a

isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty _ = True

toList :: (a, a, a) -> [a]
toList (x, y, z) = x: y: z :[]

len :: Foldable t => t a -> Int
len = length

