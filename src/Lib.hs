{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Prelude hiding (init, lookup, insert, span)

import qualified Data.Map as DM
import Crypto.Hash --(SHA1, hash, hashWith)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString.Conversion
--import Content
import Data.Maybe (fromJust, catMaybes)
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
-- insert
load :: (VMVal c, HashAlg alg) 
     => c
     -> VMap alg c
     -> (Maybe (Key alg), VMap alg c)
load c vmap = flipTuple $ insertRawBlob c vmap

-- | 'load' with 'State' -- Should I: 
--      i. make a transformer that /might/ work for multiple use-cases;
--
--     ii. write a product newtype which tuples up a 'VMap', a 'SelMap', and a
--         'Links' (read "link map" if inconsistent nomenclature irks you) into
--         one so that 'State' suffices for most high-level API calls as will
--         occur (here) in 'Lib'.

type VState alg c a = State (VMap alg c) a


load' :: (VMVal c, HashAlg alg)
      => c
      -> VState alg c (Maybe (Key alg)) --State (VMap alg c) (Maybe (Key alg))
load' = state . load

-- | Like 'load'', but loads content into empyt map. Use to initialize global
-- state.
init :: (VMVal c, HashAlg alg) => c -> VState alg c (Maybe (Key alg))
init  c = StateT $ 
    \s ->
        runStateT (load' c) s

spec = [r|
The specificity of being perks up its over-anthropomorphized head with an
expression approaching hopeful bewilderment, 'ooh, was that my name? do they
wanna talk to me? ooh ooh.' Then it notices it's self imposed limits (having
been made fully human to my mind, it is subject to the same conditions) and
thinks to itself, 'why do I think 'why do I think 'why do I think 'why do I
think '...ad infinitum''' until it notices the trend towards redundancy and has
the presence of mind to deduce that as an entity bound by consciousness, and
therefore perception as well, it must be a highly specific being comprised of
equally specific parts. At this point the very-nearly-human representation of
the specificity of being realizes that as a consciousness it is the product of
itself, and as such, it's limits are a product of it's own limits and the whole
she-bang is therefore self-inflicted (work that out. hah). Being human and
conscious, Specificity (lets name him so as to fully indulge his
anthropomorhpizaton) goes on to infer that his (lets further this metaphor with
some binary gender) specificity, as one of the root causes of the self-imposed
limitations upon his consciousness, is, in fact, an instance of a two generic
phenomenon (specificity of being and consciousness) and upon noticing that he
himself was title Specificity begins to wonder not only about the role his
consciousness plays in the limitation of his consciousness (all things human
comprise a subset of consciousness), but the role the specificity of being
plays in the mechanic by which consciousness limits itself. He begins to
exclaim "wait *I'm* Spec....". He's now, quite specifically dead. Not asleep or
knocked unconscious, but very very dead. Much to absolutely nobody's chagrin,
the cognitive overload of being doubly responsible for his predicament was too
much for Specificity to handle.  Not that he ever had the chance. The
subconscious of Specificity saw existential angst (try to concieve of the most
pain imaginable. Now magnify it because both you and I lack the insight and
experience to fathom such pain. We're too stupid) on a scale never beheld (that
is, never beheld and *survived* ) and simply gave up, overriding what should
have been Specificity's conscious decision to throw in the towel in the hopes
of protecting the delicate symbiotic balance struck by the various forms of
existential obfuscation -- it's a defense mechanism after all, except it killed
him. What does that say about life?
|]
