module Blob where

newtype Blob c = Blob c deriving (Eq, Show)

instance Read c => Read (Blob c) where
    readsPrec _ [] = []
    readsPrec _ s = [(Blob $ read s, [])]


