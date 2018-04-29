module Helpers where

import Crypto.Hash (Digest)
import qualified Data.Map as M

type Id = Int
type Key = Digest

class Unwrap f where
    unwrap :: f a -> a

isNotEmpty :: [a] -> Bool
isNotEmpty [] = False
isNotEmpty _ = True

toList :: (a, a, a) -> [a]
toList (x, y, z) = x: y: z :[]

showAbbrev (k, v) = (take 7 $ show k, v)
