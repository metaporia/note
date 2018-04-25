{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Map as DM
import Crypto.Hash --(SHA1, hash, hashWith)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString.Conversion
--import Content
import Data.Maybe (fromJust, catMaybes)
import Data.Monoid ((<>))
import Prelude hiding (lookup, insert, span)
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
import Control.Monad.Trans.Maybe
import Data.Functor.Identity


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
link = foldr (\(s, o) m-> insert m s o) Link.empty $ (head subKeys, obKeys !! 2) : zip subKeys obKeys  

one = head (zip subKeys obKeys)
two = (head subKeys, obKeys !! 2)

l' = uncurry (insert le) one
l'' = uncurry (insert l') two

-- UI.Vi pre-testing
f = TIO.readFile "specificity.md"
--v = fmap mkBlob f :: IO (Val SHA1 T.Text)
--v' = do
--    val <- v
--    return $ $ M.insert val M.empty
--
--span = do
--    (m, k) <- v'
--    return $ mkSpan k userSel0
--
userSel0 = Sel 0 178
--
--vmap' = do
--    s <- span
--    (m, k) <- v'
--    return $ fromJust $ M.insert s m
--
-- StateT :: (s -> m (a, s)) -> StateT s m a
-- StateT':: (VMap alg c-> IO (a, VMap alg c)) -> StateT' (VMap alg c) IO a

insertFromFile :: FilePath -> MsT (VMap SHA1 T.Text) IO (Key SHA1)
insertFromFile fp = MsT . MaybeT . StateT $ \m -> do
    f <- TIO.readFile fp
    let val = mkBlob f
    return . flipTuple $ VM.insert val m

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

vs = insertFromFile "specificity.md"


-- (>>=) :: m a -> (a -> m b) -> m b
--       :: StateT (VMap alg c) IO a -> (a -> StateT (VMap alg c) IO b) -> StateT (VMap alg c) IO b
--       :: MaybeT (StateT (VMap alg c) IO) a
--       -> (a -> MaybeT (StateT (VMap alg c) IO) b)
--       -> MaybeT (StateT (VMap alg c) IO) b
--
--MaybeT :: m (Maybe a) -> MaybeT m a
--


insertSpan :: (HashAlg alg, VMVal c) 
           => Selection -> Key alg -> MsT (VMap alg c) IO (Key alg)
insertSpan s k = mkMsT $ \m -> do
    return . flipTuple $ VM.insert (mkSpan k s) m

insertFromFileThenInsertSpan fp sel = runMsT (insertFromFile fp >>= insertSpan sel) VM.empty
t' fp sel = insertFromFile fp >>= insertSpan sel

maa = t' "specificity.md" userSel0

--g :: MsT (Key SHA1, VMap SHA1 T.Text) IO (Key SHA1) -> MsT (VMap SHA1 T.Text) IO (T.Text) 
--g mst = do
--    (k, m) <- get
--    case lookup m k of
--      Just (Blob _ b) -> return b
--      _               -> return ""
--
mst = insertFromFile "specificity.md"

newtype MsT s m a = MsT { getMsT :: MaybeT (StateT s m) a }
    deriving (Functor, Applicative, Monad, MonadState s)

type Ms s a = MsT s Identity a

mkMsT = MsT . MaybeT . StateT

runMs :: MsT s Identity a -> s -> Identity (Maybe a, s)
runMs mstio = (runStateT . runMaybeT $ getMsT mstio)

runMsT :: MsT s m a -> s -> m (Maybe a, s)
runMsT mstio = (runStateT . runMaybeT $ getMsT mstio)

-- | getMstT (x : MsT ...) :: s -> m (Maybe a, s)

-- | getMstT' (x : MsT' ...) :: 
--
newtype MsT' s m a = MsT' { getMsT' :: StateT s (MaybeT m) a }
    deriving (Functor, Applicative, Monad, MonadState s )

-- | Loads the contents of some source file into a 'VMap'. Returns originial
-- 'VMap' unchanged if the insertion fails.
load :: (VMVal c, HashAlg alg) 
     => c
     -> VMap alg c
     -> (Maybe (Key alg), VMap alg c)
load = undefined

