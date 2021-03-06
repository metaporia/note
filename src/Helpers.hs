{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Helpers where

import Crypto.Hash (Digest, SHA1)
import qualified Data.Map as M

import Data.Aeson (ToJSON)

import Control.Exception

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

eitherToMaybe :: Either l r -> Maybe r
eitherToMaybe (Left l) = Nothing
eitherToMaybe (Right r) = Just r

maybeToEither :: Monoid l => Maybe r -> Either l r
maybeToEither (Just r) = Right r
maybeToEither Nothing = Left mempty

convertException :: Exception e => Either e a  -> Either String a
convertException (Right a) = Right a
convertException (Left err) = Left (displayException err)

fromRight :: Either a b -> b
fromRight (Right b) = b


