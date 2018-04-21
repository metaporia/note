{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
import HCMap
import Crypto.Hash --(SHA1, hash, hashWith)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteArray 
import Data.ByteString.Conversion
import Content
import Data.Maybe (fromJust)
import Prelude hiding (lookup, insert)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Select

import Blob

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- testing
(m, h) = fromJust $ insertBlob (toBlob "hello world" :: Content SHA1 T.Text) emptySHA1
(m', h') = fromJust $ insertBlob (toBlob "hell" :: Content SHA1 T.Text) m
(m'', h'') = fromJust $ insertBlob (toBlob "rld" :: Content SHA1 T.Text) m
(m''', h''') = fromJust $ insertBlob (toBlob "o wo" :: Content SHA1 T.Text) m
commutative = 
    ( (Map $ ((M.union (hcmap m) (hcmap m'))
                `M.union` (hcmap m'')) 
                    `M.union` (hcmap m''')) == n''' )

(n, i) = fromJust $ insertBlob (toBlob "hello world" :: Content SHA1 T.Text) emptySHA1
(n', i') = fromJust $ insertBlob (toBlob "o wo" :: Content SHA1 T.Text) n
(n'', i'') = fromJust $ insertBlob (toBlob "rld" :: Content SHA1 T.Text) n'
(n''', i''') = fromJust $ insertBlob (toBlob "hell" :: Content SHA1 T.Text) n''

idList = (toIdList [i''', i', i''])
mapp = fromJust $ replaceBlob i idList  n'''
text = "hello world" :: T.Text
mapp' = fromJust $ selectFromBlobId i (Sel 4 8) n
(mn, selDig) = fromJust $ selectFromBlob i (Blob text) (Sel 4 8) n
-- mapp == mapp'

(ma, hash') = fromJust $ insertBlob (toBlob "hello world" :: Content SHA1 T.Text) emptySHA1
--t = sel "hello world" (Sel 3 8) :: (T.Text, T.Text, T.Text)
-- selectFrobBlob
--ret = selectFromBlob hash (fromJust (lookup m hash)) (Sel 3 8) m

-- insertBlob
--
newtype HCMap' alg c = Map' { hcmap' :: M.Map (Key alg) (Content' alg c) } deriving (Eq)

tm = emptySHA1
f = TIO.readFile "specificity.md"
blob :: IO (Content' SHA1 T.Text)
blob = do
    b <- TIO.readFile "specificity.md"
    return $ toBlob' b

toBlob' :: c -> Content' alg c
toBlob' c = Blob' c

insertBlob' :: forall alg c. (HashAlg alg, HCMContent c)
            => Content' alg c -> HCMap' alg c -> Maybe (HCMap alg c, Key alg)
insertBlob' val m =
    let key :: Key alg
        key = hash (toByteString' val)
        m' :: M.Map (Key alg) (Content' alg c)
        m' = M.insert key val (hcmap' m)
     in undefined


type Key = Digest

data Content' alg c = Blob' c
                    | ASpan' (Key alg) Selection
                    deriving (Eq, Show)

