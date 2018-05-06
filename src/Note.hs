{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Note where

import Prelude hiding (insert, lookup, init)

import Crypto.Hash
import Text.RawString.QQ

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8,encodeUtf8)
import Data.String (IsString)
import Data.String.ToString (toString)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LIO
import qualified Data.ByteString as B
import Data.ByteString.Conversion

import qualified Data.ByteString.Base64 as Base64

import Data.Functor.Identity
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Tuple (swap)

import Data.Aeson ( ToJSON(..), FromJSON(..), Value(..), encode, decode, genericToEncoding
                  , toEncoding, defaultOptions, object, (.=))

import Data.ByteArray (convert, ByteArrayAccess)

import qualified Data.Binary as BIN

import GHC.Generics

import Link
import qualified VMap as VM
import VMap
import Abbrev
import Helpers
import Val
import Select
import UI.Vi

data Note alg c = 
    Note { getLinks :: Links alg 
         , getVMap :: VMap alg c
         , getAbbrev :: ShortKeys alg T.Text 
         , getSelVMap :: SelVMap alg
         } deriving (Eq, Show)

newNote :: Note alg c
newNote = Note Link.empty VM.empty (newAbbrevStore 7) newSelVMap

loadNS :: forall alg c. (VMVal c, HashAlg alg)
       => c
       -> StateT (Note alg c) Identity (Maybe (Key alg))
loadNS c = StateT  $ \note -> 
    let vm = getVMap note
        links = getLinks note
        abbrev = getAbbrev note
        (mK, vm') = runVState (load c) vm
     in return (mK, Note links vm' abbrev (getSelVMap note))



-- | 'link', but wrapped for convenience in 'StateT (Note alg c) Identity ()'.
linkNS :: forall alg c. (VMVal c, HashAlg alg)
       => Subj alg
       -> Obj alg
       -> State (Note alg c) ()
linkNS s o = StateT $ \note ->  
    let links = getLinks note
        links' = snd $ runState (link s o) links
        abbrev = getAbbrev note
     in return ((), Note links' (getVMap note) abbrev (getSelVMap note))

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

-- | Pretty-print 'VMap'
lsvm :: (VMVal c, HashAlg alg, Show c, Show alg) => Note alg c -> IO ()
lsvm = putStrLn . show' . getVMap

lslnk :: (VMVal c, HashAlg alg, Show c, Show alg) => Note alg c -> IO ()
lslnk = putStrLn . pshow . getLinks

lssm :: (VMVal c, HashAlg alg, Show c, Show alg) => Note alg c -> IO ()
lssm  = putStrLn . pprintSVM . getSelVMap


-- | Pretty much useless.
lsvmS' :: (VMVal c, HashAlg alg, Show c, Show alg)
       => StateT (Note alg c) IO ()
lsvmS' = undefined

-- | 'Abbrev'/'State' wrapper.
-- returns False if the it fails to load either full-length 'Key'.
linkAbbrevNS :: forall c. (VMVal c)
             => T.Text -- c is the shortened 'Key'.
             -> T.Text
             -> State (Note SHA1 c) Bool
linkAbbrevNS s o = state $ \note -> 
    let abbr = getAbbrev note
        tup = case (lengthen abbr s, lengthen abbr o) of
                (Just ks, Just ko) -> Just (ks, ko)
                (_, _) -> Nothing
             in if isJust tup 
                   then let (sk, ok) = fromJust tup
                            (_, note') = runState (linkNS (Subj sk) (Obj ok)) note
                         in (True, note')
           else (False, note)

-- | Adds new @abbrev -> full-length@ key entry to the given 'Note''s 'Abbrev'.
abbrevNS :: VMVal c 
         => Key SHA1
         -> State (Note SHA1 c) T.Text
abbrevNS k = state $ 
    \(Note lnk vm abbr selvm) ->
        let (shortened, abbr') = runState (abbrev k) abbr
         in (shortened, Note lnk vm abbr' selvm)

-- | Register @(alias, key)@ pair.
aliasNS :: VMVal c
        => T.Text
        -> Key SHA1
        -> State (Note SHA1 c) ()
aliasNS a k = state $ 
    \(Note lnk vm abbr selvm) ->
        let (_, abbr') = runState (alias a k) abbr
         in ((), Note lnk vm abbr' selvm)

-- | Links two shortened 'Key's. Returns 'False' if abbrev lookup fails.
aliasAbbrevNS :: T.Text
              -> T.Text
              -> State (Note SHA1 T.Text) Bool
aliasAbbrevNS alias' abbrev = state $
    \n@(Note lnk vm abbr selvm) ->
        let mK = lengthen abbr abbrev
            abbr' k = snd $ runState (alias alias' k) abbr
         in case mK of
              Just k -> (True, Note lnk vm (abbr' k) selvm)
              Nothing -> (False, n)

insertFromFile :: FilePath -> MsT (VMap SHA1 T.Text) IO (Key SHA1)
insertFromFile fp = MsT . MaybeT . StateT $ \m -> do
    f <- TIO.readFile fp
    let val = mkBlob f
    return . swap $ VM.insert val m



userSel0 = Sel 0 178

insertSpan :: (HashAlg alg, VMVal c)
           => (Key alg, Selection) -> State (Note alg c) (Maybe (Key alg))
insertSpan (k, s) = state $ \note@(Note lnk vm abbr sm) -> 
    case insertRawSpan k s vm of
      Just (vm', k') -> 
          let span = mkSpan k s
           in case registerSpanInsertion k' span sm of
                Just sm' -> (Just k', Note lnk vm' abbr sm')
                Nothing -> (Nothing, note)
      Nothing -> (Nothing, note)




-- 1 insertspan
-- 2 registerSpanInsertion

insertSpan' :: (HashAlg alg, VMVal c) 
            => Selection -> Key alg -> MsT (VMap alg c) IO (Key alg)
insertSpan' s k = mkMsT $ \m -> do
    return . swap $ VM.insert (mkSpan k s) m

insertFromFileThenInsertSpan fp sel = runMsT (insertFromFile fp >>= insertSpan' sel) VM.empty
t' fp sel = insertFromFile fp >>= insertSpan' sel

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
spec = [r|The specificity of being perks up its over-anthropomorphized head with an
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



(mK', note) = init (st' 
                *> abbrevNS k0 
                *> abbrevNS k1 
                *> abbrevNS k2 
                *> abbrevNS k3
                *> loadNS b2' 
                   ) newNote

vmap = getVMap note
lnkr = getLinks note
abbrev'' = getAbbrev note
ks = appVM M.keys vmap
k0 = ks !! 0
k1 = ks !! 1
k2 = ks !! 2
k3 = ks !! 3
(mks, note') = init (  linkAbbrevNS (T.pack . take 7 $ show k2) "ed464c0" 
                    *> aliasAbbrevNS "spec" (T.pack . take 7 $ show k2)
                    *> insertSpan (k2, Sel 5 7)
                    *> insertSpan (k2, Sel 1 800)
                    *> insertSpan (k2, Sel 3 10)
                    *> locateIdx' k2 4
                    ) note

lnkr' = getLinks note'
ls note = lsvm note *> lslnk note *> lssm note

locateIdx' :: (HashAlg alg, VMVal c)
           => Key alg -> Int -> State (Note alg c) (Maybe [Key alg])
locateIdx' k idx = state $ 
    \note@(Note lnk vm abbr sm) -> (locateIdx vm sm k idx, note)


-- Deref 'ShortKey' key @abbr@.
derefAbbrNS :: (VMVal c, HashAlg alg)
            => T.Text -> State (Note alg c) (Maybe c)
derefAbbrNS abbr = state $ 
    \n@(Note lnk vm ab sm) -> ((lengthen ab abbr) >>= deref vm, n)


cmds :: (Ord k, IsString k, VMVal c, HashAlg alg) =>  M.Map k (T.Text -> State (Note alg c) (Maybe c))
cmds = M.fromList [ ("deref", derefAbbrNS) ] -- add number of arguments expected to tuple (threeple, rly).

cmdLookup :: (Ord k, IsString k, VMVal c, HashAlg alg) => k -> T.Text -> Maybe (State (Note alg c) (Maybe c))
cmdLookup k t = M.lookup k cmds <*> pure t

runWith n state = runState state n

lookupRun cmd abbr note = cmdLookup cmd abbr >>= fst . runWith note

--extricate (Cmd cmd args) = 
    --    let (fn, nargs) = M.lookup cmd cmds'
--        args' = take 3 args
--     in if length args' >= nargs 
--           then f (args' !! 0)
--                  (args' !! 1)
--                  (args' !! 2)


