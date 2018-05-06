{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Note.Trans where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

import qualified Data.Text as T

import System.IO

import Crypto.Hash

import Data.Maybe (fromJust, fromMaybe)

import Data.Monoid ((<>))

import Control.Monad.Trans.Either
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState, get, put)

-- Intenal deps
import Note hiding (runWith)
import UI.Types as Types
import UI.Service
import VMap 


type Note' = Note SHA1 T.Text
type ST = ServiceTypes SHA1
-- transformer ordering case one
newtype NoteS' err a = NoteS' { getNoteS :: StateT Note' (EitherT err IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadState (Note SHA1 T.Text))

emptyNoteS' :: NoteS' err ()
emptyNoteS' = put newNote 


derefKey :: ST -> NoteS' String (Maybe (ServiceTypes SHA1))
derefKey st = do
    n@(Note lnk vm abbr sm) <- get
    let mK = getKey st
        bs = mK >>= fmap Blob' . deref vm 
    x <- liftIO $ putStrLn $ show bs
    put n
    return bs

