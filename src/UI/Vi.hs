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
import Note



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
locate :: VMap alg c -> CursorPosn -> Key alg
locate = undefined


-- | Convert selection based-on 'CursorPosn'--which respects newlines--to the
-- selection structure used internally; that is, 'Span', within which a
-- 'Selection' indexes into a 'Key's blob, treating newlines as totally
-- unremarkable characters to be given no special treatment.
fromViSel :: VMap alg c -> Key alg -> ViSel -> (Key alg, Selection)
fromViSel = undefined


toViSel :: VMap alg c -> Key alg -> Selection -> ViSel
toViSel = undefined


