module Handle where

import Data.Functor
--
-- `referent` marks max depth of tree--that is, depth = 1.
data Handle id = BlobId id | IdListId id deriving (Eq, Ord, Show)

instance Functor Handle where
    fmap f (BlobId id) = BlobId (f id)
    fmap f (IdListId id) = IdListId (f id)

-- trial 
data Referent = Blob | IdList deriving (Eq, Show)

class Handle' a where
    refersTo :: a -> Referent

data BlobId id = BlobId' id deriving (Eq, Show)
data IdListId id = IdListId' id deriving (Eq, Show)

instance Handle' (BlobId id) where
    refersTo _ = Blob

instance Handle' (IdListId id) where
    refersTo _ = IdList

