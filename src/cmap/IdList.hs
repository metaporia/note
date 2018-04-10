module IdList where

import BlobId

import Crypto.Hash

newtype IdList alg = IdList [Digest alg] deriving (Eq)

--instance Read c => Read (IdList c) where
--    readsPrec _ [] = []
--    readsPrec _ s = [(IdList $ read s, [])]
--
--instance Show id => Show (IdList id) where
--    show (IdList ids) = show ids
--
--toBIds :: IdList id -> [BlobId id]
--toBIds (IdList bids) = bids

