module Blob where

import Helpers (Unwrap(..))

newtype Blob c = Blob c deriving (Eq, Show)

instance Read c => Read (Blob c) where
    readsPrec _ [] = []
    readsPrec _ s = [(Blob $ read s, [])]

instance Unwrap Blob where
    unwrap (Blob c) = c


