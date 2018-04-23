{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
import Crypto.Hash --(SHA1, hash, hashWith)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteArray 
import Data.ByteString.Conversion
--import Content
import Data.Maybe (fromJust)
import Prelude hiding (lookup, insert)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Select
import Helpers (Key)
import Map


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- slice refactor testing
tm = emptySHA1
m' :: (Map SHA1 T.Text, Key SHA1)
m' = insertRawBlob "Hello World!" tm
(m, k) = m'

s = Sel 3 8
(tm', k') = fromJust $ insertRawSpan k s m































---- testing
--(m, h) = fromJust $ insertBlob (toBlob "hello world" :: Content SHA1 T.Text) emptySHA1
--(m', h') = fromJust $ insertBlob (toBlob "hell" :: Content SHA1 T.Text) m
--(m'', h'') = fromJust $ insertBlob (toBlob "rld" :: Content SHA1 T.Text) m
--(m''', h''') = fromJust $ insertBlob (toBlob "o wo" :: Content SHA1 T.Text) m
--commutative = 
--    ( (Map $ ((M.union (hcmap m) (hcmap m'))
--                `M.union` (hcmap m'')) 
--                    `M.union` (hcmap m''')) == n''' )
--
--(n, i) = fromJust $ insertBlob (toBlob "hello world" :: Content SHA1 T.Text) emptySHA1
--(n', i') = fromJust $ insertBlob (toBlob "o wo" :: Content SHA1 T.Text) n
--(n'', i'') = fromJust $ insertBlob (toBlob "rld" :: Content SHA1 T.Text) n'
--(n''', i''') = fromJust $ insertBlob (toBlob "hell" :: Content SHA1 T.Text) n''

--idList = (toIdList [i''', i', i''])
--mapp = fromJust $ replaceBlob i idList  n'''
--text = "hello world" :: T.Text
--mapp' = fromJust $ selectFromBlobId i (Sel 4 8) n
--(mn, selDig) = fromJust $ selectFromBlob i (Blob text) (Sel 4 8) n
---- mapp == mapp'

--(ma, hash') = fromJust $ insertBlob (toBlob "hello world" :: Content SHA1 T.Text) emptySHA1
--t = sel "hello world" (Sel 3 8) :: (T.Text, T.Text, T.Text)
-- selectFrobBlob
--ret = selectFromBlob hash (fromJust (lookup m hash)) (Sel 3 8) m


