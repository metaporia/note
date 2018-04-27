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
    return . swap $ VM.insert val m

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
    return . swap $ VM.insert (mkSpan k s) m

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
newtype MsT' s m a = MsT' { getMsT' :: StateT s (MaybeT m) a }
    deriving (Functor, Applicative, Monad, MonadState s )

-- | 'load' with 'State' -- Should I: 
--
--      i. make a transformer that /might/ work for multiple use-cases;
--
--     ii. write a product newtype which tuples up a 'VMap', a 'SelMap', and a
--         'Links' (read "link map" if inconsistent nomenclature irks you) into
--         one so that 'State' suffices for most high-level API calls as will
--         occur (here) in 'Lib'.

type VState alg c a = State (VMap alg c) a


runVState :: StateT s Identity a -> s -> (a, s)
runVState vs = runIdentity . runStateT vs

-- | Loads the contents of some source file into a 'VMap'. Returns originial
-- 'VMap' unchanged if the insertion fails.
load :: (VMVal c, HashAlg alg)
      => c
      -> VState alg c (Maybe (Key alg)) --State (VMap alg c) (Maybe (Key alg))
load = state . (swap .) . insertRawBlob 
-- or, given:
(.*) = (.) . (.)
-- then, we have (yay for pointfree code!)
load' :: (VMVal c, HashAlg alg)
      => c
      -> VState alg c (Maybe (Key alg))
load' = state . (swap .* insertRawBlob)


type VSInternal' = (Maybe (Key SHA1), (VMap SHA1 T.Text))
-- | Like 'load'', but loads content into empty map. Use to initialize global
-- state.
initWith :: (VMVal c, HashAlg alg) => c -> (Maybe (Key alg), VMap alg c)
initWith  c =  runVState (load c) VM.empty

spec :: T.Text
spec = [r|
 specificity of being perks up its over-anthropomorphized head with an
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

blob :: T.Text
blob = "hello world"

b0 :: T.Text
b0 = "This is the end!"

b1 :: T.Text
b1 = T.drop 20 "aoeuscarohu arcoeu aorscueh aorsceh aosercuhaosrcuh"

b2' :: T.Text
b2' = "You hurt me, will not admit responsibility, involvement."


-- | Maps shortened key to associated full-length digest.
data ShortKeys alg c = ShortKeys Int (DM.Map c (Key alg))
    deriving (Eq, Show)

newShortKeys n = ShortKeys n M.empty

-- | Interface for key-shortening.
instance Abbrev ShortKeys T.Text where 
    abbrev  k = state $ \(ShortKeys n m) -> 
        let m' = M.insert abbrev k m
            abbrev = T.pack . take n $ show k
         in (abbrev, ShortKeys n m')
    lengthen  t = state $ \s@(ShortKeys n m) -> (M.lookup t m, s)
    newAbbrevStore = put . newShortKeys 


class Abbrev (a :: * -> * -> *) c where
    abbrev :: HashAlg alg 
           => Key alg -> State (a alg c) c
    lengthen :: HashAlg alg
             => c -> State (a alg c) (Maybe (Key alg))
    newAbbrevStore :: Int -> State (a alg c) ()

-- The next step, after 'load'--imagine the user has loaded several blobs, which they
-- would likely next wish to link together. If only they had access to such
-- an API--seems to be 'link'. (In actuality it would be more likely "show me
-- the names of the blobs I've loaded", but in any case--)

-- prototype global state structures
-- main generates the appropriate 'StateT s m a''s for as needed.

data Note alg c = 
    Note { getLinks :: Links alg 
         , getVMap :: VMap alg c
         , getAbbrev :: SelVMap alg
         } deriving (Eq, Show)

newNote :: Note alg c
newNote = Note Link.empty VM.empty newSelVMap

loadNS :: forall alg c. (VMVal c, HashAlg alg)
      => c
      -> StateT (Note alg c) Identity (Maybe (Key alg))
loadNS c = StateT  $ \note -> 
               let vm = getVMap note
                   links = getLinks note
                   abbrev = getAbbrev note
                   (mK, vm') = runVState (load c) vm
                in return (mK, Note links vm' abbrev)


-- | 'link', but wrapped for convenience in 'StateT (Note alg c) Identity ()'.
linkNS :: forall alg c. (VMVal c, HashAlg alg)
       => Subj alg
       -> Obj alg
       -> State (Note alg c) ()
linkNS s o = StateT $ \note ->  
    let links = getLinks note
        links' = snd $ runState (link s o) links
        abbrev = getAbbrev note
     in return ((), Note links' (getVMap note) abbrev)

link :: (Linker linker alg, Monoid (linker alg))
        => Subject alg
        -> Object alg
        -> State (linker alg) ()
link s o = insert' s o

--init :: State s a 
initT :: StateT s m a -> s -> m (a, s)
initT state s = runStateT state s

init :: State s a -> s -> (a, s)
init state s = runState state s

st :: State (Note SHA1 T.Text) (Maybe (Key SHA1))
st = loadNS blob *> loadNS spec *> loadNS "This is the end!"

st' :: State (Note SHA1 T.Text) ()
st' = StateT $ \note -> 
    let (mK, note') = runState (loadNS blob) note
        (mk0, note'') = runState (loadNS spec) note'
        (mK1, note''') = runState (loadNS b0) note''
        (mK2, note'''') = runState (loadNS b2') note'''
        ks = catMaybes [mK, mk0, mK1, mK2]
        s0 = Subj $ ks !! 0 -- 0 -> 2
        o0 = Obj $ ks !! 0 -- 0 -> 2
        o1 = Obj $ ks !! 1
        s1 = Subj $ ks !! 1
        s2 = Subj $ ks !! 2
        o2 = Obj $ ks !! 2
        lnkd :: State (Note SHA1 T.Text) () 
        lnkd = linkNS s0 o1 *> linkNS s2 o1 *> linkNS s2 o0 *> linkNS (Subj (ks!!3)) o0
        x = runState lnkd note'''
     in return $ runState lnkd note'''

-- 0 -> 1
-- 2 -> 1
-- 2  -> 0

-- loadNS blob >>= 
-- linkr :: Key alg -> Key alg -> StateT (Note SHA1 T.Text) ()
loadThenLink :: State (Note SHA1 T.Text) (Maybe (Key SHA1))
        -> State (Note SHA1 T.Text) (Maybe (Key SHA1))
        -> State (Note SHA1 T.Text) ()
loadThenLink s0 s1 = 
    StateT $ \note -> 
        let x :: a
            x = undefined
         in undefined
        


(mK', note) = init st' newNote
vmap = getVMap note
lnkr = getLinks note
--ks = appVM M.keys vmap
--k0 = ks !! 0
--k1 = ks !! 1
--k2 = ks !! 2

-- | Pretty-print 'VMap'
lsvm :: (VMVal c, HashAlg alg, Show c, Show alg) => Note alg c -> IO ()
lsvm = putStrLn . show' . getVMap

lslnk :: (VMVal c, HashAlg alg, Show c, Show alg) => Note alg c -> IO ()
lslnk = putStrLn . pshow . getLinks


-- | Pretty much useless.
lsvmS' :: (VMVal c, HashAlg alg, Show c, Show alg)
      => StateT (Note alg c) IO ()
lsvmS' = undefined

-- | 'Abbrev'/'State' wrapper.

