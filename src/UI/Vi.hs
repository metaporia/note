{-|
Module :: UI.Vi
Description: [Neo]Vi[m] user-interface API. 

Provides 
* 'CursorPosn' : row and column number posn (vs the 'Int' indices used internally).
* 'ViSel' : a 'CursorPosn'-based selection that respects newlines.
* 'locate' : to fetch the key of the content in which a cursor is.
-}
module UI.Vi where

import VMap
import Select
import Helpers (Key)
import Val

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
