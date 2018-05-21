{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Lib where

import Prelude

import qualified Data.Map as M

import Crypto.Hash --(SHA1, hash, hashWith)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteArray() 
import Data.ByteString.Conversion()
--import Content
import qualified Data.Text as T 
import qualified Data.Text.IO as TIO

import Text.RawString.QQ

import Helpers

import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe

import Control.Monad.Trans.Either
import Control.Monad.Except

import Control.Exception

import Data.Functor.Identity
import Data.Functor (void)

import Data.Maybe (isJust, fromJust, catMaybes)
import Data.Monoid ((<>))

import Note (Note(..), newNote, ls)
import qualified Note
import UI.Types (NoteS, runWith', keyToText, derefAbbr, Note')
import UI.Vi
import Abbrev 
import VMap hiding (deref, getSpansOf)
import qualified VMap as VM
import Link hiding (pointsTo, isPointedToBy)
import qualified Link as Lnk
import Select hiding (Start, End)
import Val

type MonadNote m = ( Monad m
                   , MonadIO m
                   , MonadState Note' m
                   , MonadError String m
                   )

runWith = runWith'
run = runWith'

go' state = run (state *> ls') newNote
go_ state = void $ run (state *> ls') newNote
go state = run state newNote


              

ls' :: NoteS String ()
ls' = do n <- get
         liftIO $ sequence_ $ [ Note.lsvm
                              , Note.lslnk
                              , Note.lssm
                              , Note.lsabbr] 
                              <*> [n]
         put n
         return ()

paper = loadf' "mock/paper3.md" "paper"

onea = paper -- start is inclusive, end is exclusive
    *> uncurry (selectPosn "paper") (selLine 17 8 14)
    >>= aliasK "1a" 
    >> selectPosn "paper" (CursorPosn 216 1) (CursorPosn 216 57)
    >>= aliasK "c1"
    >> link "c1" "1a" 
    >> isPointedToBy "1a" 
    -- >> deref "c1a"

oneb = onea
    >> uncurry (selectPosn "paper") (selLine 17 28 38)
    >>= aliasK "1b"
    >> link "c1" "1b"
    >> deref "c1"

    -- >> uncurry (selectPosn "paper") (selLine 217 1 60)
    -- >>= aliasK "c1b"
n :: NoteS String (Note SHA1 T.Text)
n = do e <- liftIO $ go onea
       n <- get
       let t = fst $ fromRight e
       return n

keepValue :: Monad m => m a -> (a -> m b) -> m a
keepValue ma f = do
    a <- ma
    a <$ f a

keepValue' :: Monad m => m a -> (a -> m b) -> m a
keepValue' ma f = ma >>= \a -> a <$ f a

-- Internal API

-- | Binds @(alias :: T.Text)@ to @(key :: Key SHA1)@, and returns @alias@.
--aliasK :: T.Text -> Key SHA1 -> NoteS String T.Text
aliasK :: MonadNote m => T.Text -> Key SHA1 -> m T.Text
aliasK alias key = do
    n@(Note lnk vm abbr sm) <- get
    let abbr' = alias' abbr alias key
    put (Note lnk vm abbr' sm)
    return alias

linkK :: MonadNote m => Key SHA1 -> Key SHA1 -> m ()
linkK k k' = do
    n@(Note lnk vm abbr sm) <- get
    let lnkr' = Link.insert (Subj k) (Obj k') lnk
    put (Note lnkr' vm abbr sm)
    return ()

-- | Fetch a 'Key''s contents. 
--
-- NB: While 'EitherT' encodes the possiblitiy of failure into 'NoteS', this is
-- unrepresentative of the intended "deref invariant"--that is, 
--
--      * @k0 == hash $ deref k0@, and
--      * @deref k0 == Just v@.
--
-- Although the types do not reflect this (for the present), users should
-- remain cognizant of the more overarching, implicit intent.
--
-- tldr; if 'deref' returns a 'Nothing' something is /very/, /very/ wrong.
--
derefK :: MonadNote m => Key SHA1 -> m T.Text
derefK k = do
    n@(Note lnk vm abbr sm) <- get
    val <- liftEither $ 
        case VM.deref vm k of
          Just b -> Right b 
          Nothing -> Left "could not deref key"
    put n
    return val

-- | Wraps up two monadic actions into one: 
--  
--  1. loadf
--  2. aliasK
--
--  This provides a slightly higher-level means of loading up a text file, in
--  that it--by default--creates and exposes a human readable handle/alias
--  which references the contents at the given 'FilePath'.
loadf' :: MonadNote m
       => FilePath -> T.Text -> m T.Text
loadf' fp alias = loadf fp >>= aliasK alias

loadK :: MonadNote m
      => T.Text -> m (Key SHA1)
loadK t = do
    n@(Note lnk vm abbr sm) <- get
    let (vm', mK) = insertRawBlob t vm
    k <- liftEither $ case mK of
                        Just k' -> Right k'
                        Nothing -> Left "insertion failed."
    put (Note lnk vm' abbr sm)
    return k

load :: MonadNote m
     => T.Text -> m T.Text
load t = loadK t >>= abbr

lookupK :: MonadNote m => Key SHA1 -> m (Val SHA1 T.Text)
lookupK k =  do
    n@(Note lnk vm abbr sm) <- get
    val <- liftEither $ case VM.lookup vm k of
                          Just v  -> Right v 
                          Nothing -> Left "key not found in vmap"
    put (Note lnk vm abbr sm)
    return val

lookup :: MonadNote m => T.Text -> m (Val SHA1 T.Text)
lookup t = lengthen' t >>= lookupK

lengthen' :: MonadNote m => T.Text -> m (Key SHA1)
lengthen' a = do
    n <- get
    k <- liftEither $ 
        case lengthen (_getAbbrev n) a of
          Just k  -> Right k
          Nothing -> Left "no key assoc'd w given abbr"
    put n
    return k

-- | Fetch a 'Key''s contents, but from an 'Abbrev' ('ShortKey' a.t.m.).
deref :: MonadNote m => T.Text -> m T.Text
deref abbr = do
    n@(Note lnk vm abbr' sm) <- get
    k <- liftEither $ case lengthen abbr' abbr of
                        Just k -> Right k
                        Nothing -> Left "abbr not found in Abbrev"
    val <- liftEither $ case VM.deref vm k of
                          Just b  -> Right b
                          Nothing -> Left "deref failed"
    put n 
    liftIO $ TIO.putStrLn val
    return val

-- | Link two aliases or abbreviations.
link :: MonadNote m => T.Text -> T.Text -> m ()
link s o = do
    n@(Note lnk vm abbr sm) <- get
    st <- lengthenToEither abbr s
    ot <- lengthenToEither abbr o
    let lnkr' = Link.insert (Subj st) (Obj ot) lnk
    put (Note lnkr' vm abbr sm)
    return ()

lengthenToEither :: MonadNote m 
                 => ShortKeys SHA1 T.Text -> T.Text -> m (Key SHA1)
lengthenToEither abbr x = liftEither $
    case lengthen abbr x of
      Just y -> Right y 
      Nothing -> Left "abbr not found in Abbrev"

-- | Creat a new 'Val' from a 'FilePath'.
--loadf :: FilePath -> NoteS String (Key SHA1)
loadf :: MonadNote m
      => FilePath -> m (Key SHA1)
loadf fp = do
    eCont <- liftIO
        (try (TIO.readFile fp) :: IO (Either IOError T.Text))
    contents <- liftEither $ convertException eCont
    n@(Note lnk vm abbr sm) <- get
    let (vm', mK) = insertRawBlob contents vm
    k <- liftEither $ case mK of
                        Just k' -> Right k'
                        Nothing -> Left "insertion failed."
    put (Note lnk vm' abbr sm)
    return k

-- | Pretty print the state's 'VMap'.
lsvm :: MonadNote m => m T.Text
lsvm = do
    note <- get
    let s = show' $ _getVMap note
    put note
    return $ T.pack s

-- | Pretty print the state's 'Linker'.
lslnk :: MonadNote m => m T.Text
lslnk = do
    note <- get
    let s = pshow $ _getLinks note
    put note
    return $ T.pack s

-- | Pretty print the state's 'Abbrev.
lsabbr :: MonadNote m => m T.Text
lsabbr = do
    note <- get
    let s = show $ _getAbbrev note
    put note
    return $ T.pack s

-- | Shorten a key.
abbr :: MonadNote m
     => Key SHA1 -> m T.Text
abbr key = do
    n@(Note lnk vm abbr sm) <- get
    let (abr, abbr') = abbrev' abbr key 
    put (Note lnk vm abbr' sm)
    return abr

-- | Fetch keys of 'Vmap'.
vmkeys :: MonadNote m
       => m [Key SHA1]
vmkeys = do
    note <- get
    let ks = appVM M.keys $ _getVMap note
    put note
    return ks

-- | Fetch keys of 'Abbrev'.
abbrkeys :: MonadNote m
         => m [T.Text]
abbrkeys = do
    note <- get
    let ks = getAbbrKeys $ _getAbbrev note
    put note
    return ks

-- Looks up 'Subject's that link to the given 'Object'.
--
-- NB: Read as "fetch me the keys of 'Val's that point to /this/ --the
-- 'Object''s-- 'Key''s 'Val'."
isPointedToBy :: MonadNote m
              => T.Text -- ^ 'Subject' abbreviation. "What points to this?" is the question 
                  -- answered by 'linksto'.
        -> m [T.Text] 
isPointedToBy abbr = do
    note <- get
    key <- liftEither $ case lengthen (_getAbbrev note) abbr of
                          Just k -> Right k
                          Nothing -> Left "could not find key matching abbr"
    let subjs =  Lnk.isPointedToBy (_getLinks note) (Obj key)
    put note
    return $ map (\(Subj k) ->  T.pack . take 8 $ show k) subjs

-- | 'linksto' for 'Key's.
isPointedToByK :: MonadNote m => Key SHA1 -> m [Key SHA1]
isPointedToByK key = do
    note <- get
    let subjs =  Lnk.isPointedToBy (_getLinks note) $ Obj key
    put note
    return $ map (\(Subj k) -> k) subjs

-- | Looks up 'Object's linked to /from/ the given 'Subject'.
pointsTo :: MonadNote m => T.Text -> m [T.Text]
pointsTo abbr = do
    note <- get
    subj <- liftEither $ 
        case lengthen (_getAbbrev note) abbr of
          Just k -> Right $ Subj k
          Nothing -> Left "could not find key matching abbr"
    let objs =  Lnk.pointsTo (_getLinks note) subj
    put note
    return $ map (\(Obj k) -> T.pack . take 8 $ show k) objs

-- | 'linksfrom' for 'Key's.
pointsToK :: MonadNote m => Key SHA1 -> m [Key SHA1]
pointsToK key = do
    note <- get
    let subj = Subj key
        objs =  Lnk.pointsTo (_getLinks note) subj
    put note
    return $ map (\(Obj k) -> k) objs

-- | Applies 'select' to 'Key's.
selectK :: MonadNote m => Selection -> Key SHA1 -> m (Key SHA1)
selectK sel sourceKey = do
    (Note lnk vm abbr sm) <- get
    (vm', spanKey) <- liftEither $ 
        case insertRawSpan sourceKey sel vm of
          Just (vm', spanKey) -> Right (vm', spanKey) 
          Nothing -> Left "unable to deref span's target key"
    let sm' = registerSpanInsertion spanKey sourceKey sel sm
    put (Note lnk vm' abbr sm')
    return spanKey

-- | Apply, and register, a 'Selection' to the 'Val' of the 'Key' of the
-- given abbreviation, @a :: T.Text@. 
select :: MonadNote m => Selection -> T.Text -> m (Key SHA1)
select sel a  = do
    (Note lnk vm abbr sm) <- get
    sourceKey <- liftEither $
        case lengthen abbr a of
          Just k -> Right k
          Nothing -> Left "could not lengthen key"
    (vm', spanKey) <- liftEither $ 
        case insertRawSpan sourceKey sel vm of
          Just (vm', spanKey) -> Right (vm', spanKey) 
          Nothing -> Left "unable to deref span's target key"
    let sm' = registerSpanInsertion spanKey sourceKey sel sm
    put (Note lnk vm' abbr sm')
    return spanKey

-- | Like 'select', but additionally performs 'CursorPosn' to 'Selection'
-- conversion.
selectPosn :: MonadNote m 
           => T.Text -> CursorPosn -> CursorPosn -> m (Key SHA1)
selectPosn a s e = do
    (Note lnk vm abbr sm) <- get
    sourceKey <- liftEither $
        case lengthen abbr a of 
          Just k -> Right k
          Nothing -> Left "unable to deref span's target key"
    let convert pt' x = liftEither $ case cursorToIdx' vm sourceKey pt' x of
                          Just i -> Right i
                          Nothing -> Left "unable to convert 'CursorPosn' to 'Int'"
    s'' <- convert Start s
    e' <- convert End e
    selectK (Sel s'' e') sourceKey 

getSpansOfK :: MonadNote m 
            => Key SHA1 -- ^ bg blob
            -> m [Key SHA1] -- ^ spans on bg blob
getSpansOfK k = do
    n <- get
    ks <- liftEither $ VM.getSpansOf (_getSelVMap n) k
    return ks

