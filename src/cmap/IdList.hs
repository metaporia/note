module IdList where

import BlobId

newtype IdList id = IdList [BlobId id] deriving (Eq)

instance Read c => Read (IdList c) where
    readsPrec _ [] = []
    readsPrec _ s = [(IdList $ read s, [])]

instance Show id => Show (IdList id) where
    show (IdList ids) = show ids


