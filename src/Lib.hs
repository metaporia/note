{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import Prelude hiding (init, lookup, insert, span)

import qualified Data.Map as DM
import qualified Data.Map as M
import Crypto.Hash --(SHA1, hash, hashWith)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString.Conversion
--import Content
import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Select
import Helpers (Key)
import qualified VMap as VM
import VMap hiding (insert, emptySHA1)
import Link
import Data.Bifunctor
import Val
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Text.RawString.QQ
import Data.Tuple (swap)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- slice refactor testing
tm = VM.emptySHA1
m' :: (VMap SHA1 T.Text, Maybe (Key SHA1))
m' = insertRawBlob "Hello World!" tm
(m, k) = m'

s = Sel 3 8
(tm', k') = fromJust $ insertRawSpan (fromJust k) s m

mapInsert :: [T.Text] -> (VMap SHA1 T.Text, [Key SHA1])
mapInsert xs = 
    let seq = foldr (\(a, b) (as, bs) -> (a:as, b:bs)) ([],[]) 
        fold' (ms, ks) = (mconcat ms, catMaybes ks)
     in fold' $ seq $ map (flip insertRawBlob tm) xs

(mpp, subs) = mapInsert 
    [ "This is the shit"
     , "WOAH! another line. who've seen that coming?!"
     , "MOARRRR"
     , "AND MOERRRRRRRRR"
     ]
(mpp',obs) = mapInsert
    [ "AN object!!!"
    , "and antther"
    , "yet more obss"
    , "The fourth object lies here"
    ]

lmap = mpp <> mpp'
   
-- 'Links'/'Linker' Testing
le = emptySHA1

mkLinksTo :: Linker l alg => l alg -> [(Obj alg, Subj alg)] -> [l alg]
mkLinksTo linker xs = map (insert' linker) xs
    where insert' linker (to, from) = insertTo linker to from

mkLinksFrom :: Linker l alg => l alg -> [(Subj alg, Obj alg)] -> [l alg]
mkLinksFrom linker xs = map (insert' linker) xs
    where insert' linker (from, to) = insertFrom linker from to

lto' = mconcat $ mkLinksTo le $ map fromLink $ zip subs obs

toLink (a, b) = (Subj a, Obj b) 
fromLink (a, b) = (Obj a, Subj b) 

lfrom' :: Links SHA1
lfrom' = mconcat $ mkLinksFrom le $ map toLink $ zip obs subs




subVals :: [T.Text] -> [Key SHA1]
subVals = map (hash . toByteString') 

obVals :: [T.Text] -> [Key SHA1]
obVals = map (hash . toByteString') 

subKeys = map Subj $ subVals ["sub1", "two sub", "three sub"]
obKeys = map Obj $ obVals ["ob1", "two ob", "three ob"]
link' = foldr (\(s, o) m-> insert s o m) Link.empty $ (head subKeys, obKeys !! 2) : zip subKeys obKeys  

one = head (zip subKeys obKeys)
two = (head subKeys, obKeys !! 2)

l' = uncurry (\s o -> insert s o le) one
l'' = uncurry (\s o -> insert s o l') two

