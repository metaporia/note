{-|
Module :: UI.Vi
Description: [Neo]Vi[m] user-interface API. 

Provides:

    * 'CursorPosn' : row and column number posn (vs the 'Int' indices used internally).
    * 'ViSel' : a 'CursorPosn'-based selection that respects newlines.
    * 'locate' : to fetch the key of the content in which a cursor is.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UI.Vi where

import Prelude hiding (insert, lookup, init)

import Crypto.Hash
import Text.RawString.QQ

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.String (IsString)

import Data.Functor.Identity
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Tuple (swap)

import Link
import qualified VMap as VM
import VMap
import Abbrev
import Helpers 
import Select
import Val

s = [r|aoeu
aoeu
aoeu
|]


data CursorPosn = 
    CursorPosn { row :: Int
               , col :: Int
               } deriving (Eq, Show)

data ViSel = 
    ViSel { open :: CursorPosn
          , close :: CursorPosn 
          } deriving (Eq, Show)


-- | Fetches the 'Key' associated with the content from which the 'CursorPosn'
-- location was requested. Naturally this requires access to the value maps
-- under 'Map'
--
-- Converts a 'CursorPosn' to an index.
cursorToIdx :: forall alg c. (Eq c, Splittable c, IsString c)
            => VMap alg c -> Key alg -> CursorPosn -> Maybe Int
cursorToIdx vm k cur = 
    let val = case lookup vm k of 
                Just (Blob _ c) -> posnToIdx c cur
                Just (Span k' sel') -> cursorToIdx vm k' cur
                Nothing -> Nothing

     in  undefined


posnToIdx :: (IsString c, Eq c, Splittable c) => c -> CursorPosn -> Maybe Int
posnToIdx s (CursorPosn row col)
    | null' s = Nothing
    | otherwise =
    let lns = splitOn' "\n" s
        idxBarLastLn = sum . map len $ take (row - 1) lns
        end = if len lns > (row - 1)
                 then Just $ lns !! (row - 1)
                 else Nothing
     in do lastLen <- fmap len end
           if col <= lastLen
              then Just(idxBarLastLn + col)
               else Nothing

-- | Convert selection based-on 'CursorPosn'--which respects newlines--to the
-- selection structure used internally; that is, 'Span', within which a
-- 'Selection' indexes into a 'Key's blob, treating newlines as totally
-- unremarkable characters to be given no special treatment.
fromViSel :: VMap alg c -> Key alg -> ViSel -> (Key alg, Selection)
fromViSel = undefined
        

toViSel :: VMap alg c -> Key alg -> Selection -> ViSel
toViSel = undefined

