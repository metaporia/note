{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Abbrev where



import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.State

import VMap
import Helpers
import Select (Splittable)

-- | Maps shortened key to associated full-length digest.
data ShortKeys alg c = ShortKeys Int (M.Map c (Key alg))
    deriving (Eq, Show)

showShortKeys :: forall alg c. (HashAlg alg, Show c, Show alg) 
              => ShortKeys alg c -> String
showShortKeys (ShortKeys _ m) = "abbr:\n" ++ (go $ M.toList m)
    where go :: [(c, Key alg)] -> String
          go [] = []
          go ((c, k):xs) = show c ++ " -> " 
                        ++ take 8 (show k)
                        ++ "\n"
                        ++ go xs


newShortKeys n = ShortKeys n M.empty

instance Abbrev ShortKeys T.Text where 
    abbrev  k = state $ \(ShortKeys n m) -> 
        let m' = M.insert abbrev k m
            abbrev = T.pack . take n $ show k
         in (abbrev, ShortKeys n m')

    abbrev' (ShortKeys n m) k = 
        let abbr = T.pack . take n $ show k
            m' = M.insert abbr k m
         in (abbr, ShortKeys n m')
    alias a k = state $ \(ShortKeys n m) ->
        let m' = M.insert a k m
         in ((), ShortKeys n m')
    alias' (ShortKeys n m) alias key =
        let m' = M.insert alias key m
         in ShortKeys n m'
    lengthen  (ShortKeys n m) t = M.lookup t m
    newAbbrevStore = newShortKeys 
    getAbbrKeys (ShortKeys n m) = M.keys m

-- | Interface for key-shortening.
class Splittable c => Abbrev (a :: * -> * -> *) c where
    abbrev :: HashAlg alg 
           => Key alg -> State (a alg c) c
    abbrev' :: HashAlg alg 
            => a alg c -> Key alg -> (c, a alg c)
    alias :: HashAlg alg
          => c -> Key alg -> State (a alg c) ()
    alias' :: HashAlg alg
           => a alg c -> c -> Key alg -> a alg c
    lengthen :: HashAlg alg
             => a alg c  -> c -> Maybe (Key alg)
    newAbbrevStore :: Int -> a alg c 
    getAbbrKeys :: a alg c -> [c]
