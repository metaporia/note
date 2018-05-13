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

import qualified Data.Map as M

import Crypto.Hash --(SHA1, hash, hashWith)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString.Conversion
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
import UI.Types (NoteS, runWith', keyToText)
import Abbrev 
import VMap hiding (deref)
import qualified VMap as VM
import Link
import Select



runWith = runWith'

run stateToRun state = runWith stateToRun state

go state = run (state *> ls') newNote
go_ state = void $ (run (state *> ls') newNote)


              

ls' :: NoteS String ()
ls' = do n <- get
         liftIO $ sequence_ $ [ Note.lsvm
                              , Note.lslnk
                              , Note.lssm
                              , Note.lsabbr] 
                              <*> [n]
         put n
         return ()

one = loadf "work" >>= aliasK "work"
two = loadf "specificity.md"  >>= aliasK "spec"

three = join $ liftM2 linkK (loadf "work") (loadf "specificity.md")

keepValue :: Monad m => m a -> (a -> m b) -> m a
keepValue ma f = do
    a <- ma
    a <$ f a

keepValue' :: Monad m => m a -> (a -> m b) -> m a
keepValue' ma f = ma >>= \a -> a <$ f a


-- Internal API

-- | Binds @alias :: T.Text@ to @key :: Key SHA1@, and returns @alias@.
aliasK :: T.Text -> Key SHA1 -> NoteS String T.Text
aliasK alias key = do
    n@(Note lnk vm abbr sm) <- get
    let abbr' = alias' abbr alias key
    put (Note lnk vm abbr' sm)
    return alias

linkK :: Key SHA1 -> Key SHA1 -> NoteS String ()
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
--      i) @k0 == hash $ deref k0@, and
--     ii) @deref k0 == Just v@.
--
-- Although the types do not reflect this (for the present), users should
-- remain cognizant of the more overarching, implicit intent.
--
-- tldr; if 'deref' returns a 'Nothing' something is /very/, /very/ wrong.
--
derefK :: Key SHA1 -> NoteS String T.Text
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
loadf' :: FilePath -> T.Text -> NoteS String T.Text
loadf' fp alias = loadf fp >>= aliasK alias

-- | Fetch a 'Key''s contents, but from an 'Abbrev' ('ShortKey' a.t.m.).
deref :: T.Text -> NoteS String T.Text
deref abbr = do
    n@(Note lnk vm abbr' sm) <- get
    k <- liftEither $ case lengthen abbr' abbr of
                        Just k -> Right k
                        Nothing -> Left "abbr not found in Abbrev"
    val <- liftEither $ case VM.deref vm k of
                          Just b -> Right b
                          Nothing -> Left "deref failed"
    put n 
    liftIO $ TIO.putStrLn val
    return val

link :: T.Text -> T.Text -> NoteS String ()
link s o = do
    n@(Note lnk vm abbr sm) <- get
    st <- liftEither $ maybeToEither $ lengthen abbr s
    ot <- liftEither $ maybeToEither $ lengthen abbr o
    liftIO $ print s >> print "\n" >> print o
    liftIO $ print st >> print "\n" >> print ot
    let lnkr' = Link.insert (Subj st) (Obj ot) lnk
    put (Note lnkr' vm abbr sm)
    return ()


loadf :: FilePath -> NoteS String (Key SHA1)
loadf fp = do
    eCont <- liftIO $ 
        (try (TIO.readFile fp) :: IO (Either IOError T.Text))
    contents <- liftEither $ convertException eCont
    n@(Note lnk vm abbr sm) <- get
    let (vm', mK) = insertRawBlob contents vm
    k <- liftEither $ case mK of
                        Just k' -> Right k'
                        Nothing -> Left "insertion failed."
    put (Note lnk vm' abbr sm)
    return k

lsvm :: NoteS String T.Text
lsvm = do
    note <- get
    let s = show' $ getVMap note
    put note
    return $ T.pack s

lslnk :: NoteS String T.Text
lslnk = do
    note <- get
    let s = pshow $ getLinks note
    put note
    return $ T.pack s

lsabbr :: NoteS String T.Text
lsabbr = do
    note <- get
    let s = show $ getAbbrev note
    put note
    return $ T.pack s

abbr :: Key SHA1 -> NoteS String T.Text
abbr key = do
    n@(Note lnk vm abbr sm) <- get
    let (abr, abbr') = abbrev' abbr key 
    put (Note lnk vm abbr' sm)
    return abr

vmkeys :: NoteS String [Key SHA1]
vmkeys = do
    note <- get
    let ks = appVM M.keys $ getVMap note
    put note
    return ks

abbrkeys :: NoteS String [T.Text]
abbrkeys = do
    note <- get
    let ks = getAbbrKeys $ getAbbrev note
    put note
    return $ ks

-- abbrs
linksto :: Key SHA1 -> NoteS String [T.Text]
linksto key = do
    note <- get
    let subjs =  linksTo (getLinks note) (Obj key)
    put note
    return $ map (\(Subj k) ->  take' 8 $ keyToText k) subjs

{-
-- keys
linkstoK :: ST -> NoteS String ST
linkstoK st = do
    note <- get
    key <- liftEither $ getKey st
    let obj = Obj key
        subjs =  linksTo (getLinks note) obj
    put note
    return . List $ map (\(Subj k) -> Key' . toAesonKey $ k) subjs

            
linksfrom :: ST -> NoteS String ST
linksfrom st = do
    note <- get
    abbr <- liftEither $ getAbbr st
    subj <- liftEither $ 
        case lengthen (getAbbrev note) abbr of
          Just k -> Right $ Subj k
          Nothing -> Left "could not find key matching abbr"
    let objs =  linksFrom (getLinks note) subj
    put note
    return . List $ map (\(Obj k) -> Abbr 
                                   . T.pack 
                                   . take 8 $ show k) objs

linksfromK :: ST -> NoteS String ST
linksfromK st = do
    note <- get
    key <- liftEither $ getKey st
    let subj = Subj key
        objs =  linksFrom (getLinks note) subj
    put note
    return . List $ map (\(Obj k) -> Key' . toAesonKey $ k) objs

-- | Adds 'Span'' to VMap, SelMap
selectK :: ST -> NoteS String ST
selectK st = do
    (sourceKey, sel) <- liftEither $ getSpan st
    (Note lnk vm abbr sm) <- get
    (vm', spanKey) <- liftEither $ 
        case insertRawSpan sourceKey sel vm of
          Just (vm', spanKey) -> Right (vm', spanKey) 
          Nothing -> Left "unable to deref span's target key"
    let sm' = registerSpanInsertion spanKey sourceKey sel sm
    put (Note lnk vm' abbr sm')

    return (Key' $ toAesonKey spanKey)
-- 1.  insertRawSpan k s
-- 2. registerSpanInsertion  spankey (Span k s)
-- 3. return (key' spankey)

-- | 'selectK' for 'Abbr'
select :: ST -> ST -> NoteS String ST
select st st' = do
    (Note lnk vm abbr sm) <- get
    a <- liftEither $ getAbbr st
    sel <- liftEither $ getSel' st'
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
    return (Key' $ toAesonKey spanKey)
-} 
