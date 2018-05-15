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
import Select hiding (Start, End)
import Val

s' = [r|aoeu
aoeu
aoeu
|]


f = TIO.readFile "mock/paper3.md"
sidx = f >>= return . (\x -> posnToIdx' Start x  s)
eidx = f >>= return . (\x -> posnToIdx' End x  e)

s = (CursorPosn 216 1)
e = (CursorPosn 216 57)

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
--cursorToIdx :: forall alg c. (Eq c, Splittable c, IsString c)
--            => VMap alg c -> Key alg -> CursorPosn -> Maybe Int
--cursorToIdx vm k cur = 
--    case lookup vm k of 
--                Just (Blob _ c) -> posnToIdx c cur
--                Just (Span k' sel') -> cursorToIdx vm k' cur
--                Nothing -> Nothing

-- concretized for ease of prototyping
cursorToIdx' :: VMap SHA1 T.Text 
             -> Key SHA1 
             -> PosnType 
             -> CursorPosn 
             -> Maybe Int
cursorToIdx' vm k pt cur = 
    case lookup vm k of 
      Just (Blob _ c) -> posnToIdx' pt c cur
      Just (Span k' sel') -> cursorToIdx' vm k' pt cur
      Nothing -> Nothing

data PosnType = Start | End deriving (Eq, Show)

posnToIdx' :: PosnType -> T.Text -> CursorPosn -> Maybe Int
posnToIdx' pt s (CursorPosn row col)
  | T.null s = Nothing
  | otherwise =
      let lns = take row $ T.lines s
          count = T.length . T.unlines $ take (row - 1) lns
          lastLine = if len lns > (row - 1)
                        then Just $ last . take row $ lns
                        else Nothing

       in do lastLine' <- lastLine 
             if col <= len lastLine'
                then Just $ count + (case pt of
                                       Start -> col - 1
                                       End -> col)
                else Nothing

selLine :: Int -> Int -> Int -> (CursorPosn, CursorPosn)
selLine row colS colE = (CursorPosn row colS, CursorPosn row colE)


-- | Convert selection based-on 'CursorPosn'--which respects newlines--to the
-- selection structure used internally; that is, 'Span', within which a
-- 'Selection' indexes into a 'Key's blob, treating newlines as totally
-- unremarkable characters to be given no special treatment.
fromViSel :: VMap alg c -> Key alg -> ViSel -> (Key alg, Selection)
fromViSel = undefined
        

toViSel :: VMap alg c -> Key alg -> Selection -> ViSel
toViSel = undefined

