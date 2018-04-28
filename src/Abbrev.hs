{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Abbrev where


import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.State

import VMap
import Helpers

-- | Maps shortened key to associated full-length digest.
data ShortKeys alg c = ShortKeys Int (M.Map c (Key alg))
    deriving (Eq, Show)

newShortKeys n = ShortKeys n M.empty

-- | Interface for key-shortening.
instance Abbrev ShortKeys T.Text where 
    abbrev  k = state $ \(ShortKeys n m) -> 
        let m' = M.insert abbrev k m
            abbrev = T.pack . take n $ show k
         in (abbrev, ShortKeys n m')
    alias a k = state $ \(ShortKeys n m) ->
        let m' = M.insert a k m
         in ((), ShortKeys n m')
    lengthen  (ShortKeys n m) t = M.lookup t m
    newAbbrevStore = newShortKeys 

--instance Abbrev Note T.Text where
--    abbrev k = state $ 
--        \(Note lnk vmap abbr svmap) -> 
--            let (shortened, abbr') = runState (abbrev k) abbr
--             in (shortened, Note lnk vmap abbr' svmap)
--    lengthen n@(Note lnk vmap abbr svmap) c = lengthen abbr c
--    newAbbrevStore = const newNote

class Abbrev (a :: * -> * -> *) c where
    abbrev :: HashAlg alg 
           => Key alg -> State (a alg c) c
    alias :: HashAlg alg
          => c -> Key alg -> State (a alg c) ()
    lengthen :: HashAlg alg
             => a alg c  -> c -> Maybe (Key alg)
    newAbbrevStore :: Int -> a alg c 

-- The next step, after 'load'--imagine the user has loaded several blobs, which they
-- would likely next wish to link together. If only they had access to such
-- an API--seems to be 'link'. (In actuality it would be more likely "show me
-- the names of the blobs I've loaded", but in any case--)

-- prototype global state structures
-- main generates the appropriate 'StateT s m a''s for as needed.

