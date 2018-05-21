{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UI.Types where
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
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString as B
import Data.ByteString.Conversion hiding (List)

import qualified Data.ByteString.Base64 as Base64

import Data.Functor.Identity
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Tuple (swap)

import Data.Aeson ( ToJSON(..), FromJSON(..), Value(..), encode
                  , decode, genericToEncoding , toEncoding
                  , defaultOptions, object, (.=))

import Data.ByteArray (convert, ByteArrayAccess)
import qualified Data.ByteArray as BA

import qualified Data.Binary as BIN
import Data.Bits

import qualified Data.ByteString.Base64 as Base64

import Control.Monad.Trans.Either
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Identity
import Control.Monad.Except 

import Control.Exception

import Data.Monoid ((<>))

import GHC.Generics

import Lens.Micro
import Lens.Family.State hiding ((.=))
import Lens.Micro.TH

import Text.Printf
import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Numeric (showHex)

import Link hiding (pointsTo, isPointedToBy)
import qualified Link as Lnk
import qualified VMap as VM
import VMap
import qualified Abbrev as A
import Abbrev hiding (alias)
import Helpers 
import Val
import Select
import UI.Vi
import Note hiding (lsvm, lslnk, link)

type Note' = Note SHA1 T.Text
type ST = ServiceTypes SHA1
-- TODO add lenses to 'NoteS' et al
-- transformer ordering case one
newtype NoteS err a = NoteS { _getNoteS :: StateT Note' (EitherT err IO) a }
    deriving ( Functor, Applicative, Monad, MonadIO, MonadError err
             , MonadState (Note SHA1 T.Text))



emptyNoteS :: NoteS err ()
emptyNoteS = put newNote 
 
runNoteS :: NoteS err a-> Note' -> IO (Either err (a, Note'))
runNoteS = (runEitherT .) . runStateT . _getNoteS

runWith' m s = runNoteS m s

 

derefKey :: ST -> NoteS String (ServiceTypes SHA1)
derefKey st = do
    n@(Note lnk vm abbr sm) <- get
    k <- liftEither $ getKey st
    st' <- liftEither . fmap Blob' . deref' vm $ k
    put n
    return st'

apply0 = const

apply' :: (a -> b) -> [a] -> Either String b
apply' f xs = if not (null xs)
                then Right $ f (head xs)
                else Left "expected at least one argument"


apply2' :: (a -> a -> b) -> [a] -> Either String b
apply2' f xs = do
    xs' <- hasLen' xs 2
    return $ f (xs' !! 0) (xs' !! 1)


hasLen' :: [a] -> Int -> Either String [a]
hasLen' xs n = 
    let xs' = take n xs
     in if length xs >= n
           then Right xs'
           else Left $ "expected argument list of length " ++ show n
                       ++ "but instead received one of length " 
                       ++ show (length xs)

data ServiceTypes alg = Blob' T.Text
              | Span' (AesonKey alg) Selection
              | Key' (AesonKey alg)
              | Abbr T.Text
              | Sel' T.Text T.Text
              | Ls T.Text
              | Err T.Text -- expand to wrap err enum
              | Msg T.Text
              | List [ServiceTypes alg]
              | Nil
              deriving (Eq, Generic)

instance (HashAlg alg, Show alg) => Show (ServiceTypes alg) where
    show Nil = "Nil"
    show (List xs) = "List " ++ (show (map show xs))
    show (Msg t) = "Msg " ++ (T.unpack t)
    show (Ls t) = "Ls " ++ (T.unpack t)
    show (Blob' t) = "Blob' " ++ (T.unpack t)
    show (Abbr t) = "Abbr " ++ (T.unpack t)
               -- case b64toHex t of
               --       Right t -> "Abbr " ++ (take 7 t)
               --       Left err -> "Abbr err: "++err
    show (Err t) = "Err " ++ (T.unpack t)
    show (Span' ak sel) = "Span' " ++ (show ak) ++ show (sel)
    show (Key' ak) = show ak
    show (Sel' s e) = "Sel' " ++ (T.unpack s) ++ (' ':(T.unpack e))
                       
-- | show abbr in hex
b64toHex :: T.Text -> Either String String
b64toHex = fmap hex . Base64.decode . toByteString' 

hex :: B.ByteString -> String
hex = foldr showHex "" . B.unpack


instance FromJSON (ServiceTypes alg)
instance ToJSON (ServiceTypes alg) where
    toEncoding = genericToEncoding defaultOptions

getKey :: HashAlg alg => ServiceTypes alg -> Either String (Key alg)
getKey (Key' ak) = fromAesonKey ak
getKey _ = Left "Expected Key', found other variant."

getBlob :: ServiceTypes alg -> Either String T.Text
getBlob (Blob' t) = Right t 
getBlob _ = Left "Expected Blob', but found other variant."

getSpan :: HashAlg alg => ServiceTypes alg 
        -> Either String (Key alg, Selection)
getSpan (Span' ak sel) = fromAesonKey ak >>= \k-> return (k, sel)
getSpan _ = Left "Expected Span', found other variant."

getLs :: HashAlg alg => ServiceTypes alg -> Either String T.Text
getLs (Ls t) = Right t
getLs _ = Left "Expected Ls, but found other variant."

getErr :: HashAlg alg => ServiceTypes alg -> Either String T.Text
getErr (Err e) = Right e
getErr _ = Left "Expected Err, but found other variant."

getAbbr :: HashAlg alg => ServiceTypes alg -> Either String T.Text
getAbbr (Abbr e) = Right e
getAbbr _ = Left "Expected Msg, but found other variant."

getMsg :: HashAlg alg => ServiceTypes alg -> Either String T.Text
getMsg (Msg e) = Right e
getMsg _ = Left "Expected Msg, but found other variant."

getSel' :: HashAlg alg => ServiceTypes alg -> Either String Selection
getSel' (Sel' s e ) = Right (Sel (toInt s)  (toInt e))
    where toInt s = read (T.unpack s) :: Int
getSel'  _ =  Left "Expected Sel', but found other variant."


-- Remote API
ecmds :: (Ord k, IsString k) 
      =>  M.Map k ([ST] -> Either String (NoteS String ST))
ecmds = M.fromList [ ("derefK", apply' derefKey)
                   , ("loadf", apply' loadf)
                   , ("deref", apply' derefAbbr)
                   , ("abbr",  apply' abbr)
                   , ("lsvm",  const (return lsvm))
                   , ("lslnk", const (return lslnk))
                   , ("alias", apply2' alias)
                   , ("vmkeys", const (return vmkeys))
                   , ("abbrkeys", const (return abbrkeys))
                   , ("pointsto", apply'  pointsTo)
                   , ("pointstoK", apply' pointsToK)
                   , ("ispointedtoby", apply'  isPointedToBy)
                   , ("ispointedtobyK", apply' isPointedToByK)
                   , ("linkK", apply2' linkK) 
                   , ("link", apply2' link) 
                   , ("selectK", apply' selectK)
                   , ("select", apply2' select)]
-- TODO:
-- ▣  select <abbr> <posn> <posn>
-- □  'help <cmd>' | 'help' 
-- □  expose the below API w/o 'ST', for non-networked use. (see 

alias :: ST -> ST -> NoteS String ST
alias st st' = do
    aliass <- liftEither $ getAbbr st
    key <- liftEither $ getKey st'
    n@(Note lnk vm abbr sm) <- get
    let abbr' = alias' abbr aliass key
    put (Note lnk vm abbr' sm)
    return $ Msg "alias successfully registered"

linkK :: ST -> ST -> NoteS String ST
linkK st st' = do
    n@(Note lnk vm abbr sm) <- get
    s <- liftEither $ Subj <$> getKey st
    o <- liftEither $ Obj <$> getKey st'
    let lnkr' = Link.insert s o lnk
    put (Note lnkr' vm abbr sm)
    return $ Msg "link successfuly registered"

link :: ST -> ST -> NoteS String ST
link st st' = do
    n@(Note lnk vm abbr sm) <- get
    s <- liftEither $ getAbbr st
    o <- liftEither $ getAbbr st'
    let lengthen' a = liftEither $ 
            case A.lengthen abbr a of 
              Just k -> Right k 
              Nothing -> Left "no key matching abbr found"
    st <- lengthen' s
    ot <- lengthen' o
    liftIO $ print s >> print "\n" >> print o
    liftIO $ print st >> print "\n" >> print ot
                

    let lnkr' = Link.insert (Subj st) (Obj ot) lnk
    put (Note lnkr' vm abbr sm)
    return . Msg $ s <> " successfuly linked to " <> o



derefAbbr :: ST -> NoteS String ST
derefAbbr st = do
    n@(Note lnk vm abbr sm) <- get
    alias' <- liftEither $ getAbbr st
    k <- liftEither $ case A.lengthen abbr alias' of
                       Just k -> Right k
                       Nothing -> Left "abbr not found in Abbrev"
    val <- liftEither $ 
        case deref vm k of
          Just b -> Right b 
          Nothing -> Left "could not deref key"
    put n
    return (Blob' val)

loadf :: ST -> NoteS String ST
loadf st = do
    fp <- liftEither . fmap T.unpack $ getBlob st
    eCont <- liftIO $ 
        (try (TIO.readFile fp) :: IO (Either SomeException T.Text))
    contents <- liftEither $ convertException eCont
    n@(Note lnk vm abbr sm) <- get
    let (vm', mK) = insertRawBlob contents vm
        k = case mK of
              Just k' -> Right k'
              Nothing -> Left "insertion failed."
    st' <- liftEither $ (Key' . toAesonKey) <$> k
    put (Note lnk vm' abbr sm)
    return st'

lsvm :: NoteS String ST
lsvm = do
    note <- get
    let s = show' $ _getVMap note
    put note
    return . Ls $  T.pack s

lslnk :: NoteS String ST
lslnk = do
    note <- get
    let s = pshow $ _getLinks note
    put note
    return . Ls $ T.pack s

lsabbr :: NoteS String ST
lsabbr = do
    note <- get
    let s = show $ _getAbbrev note
    put note
    return . Ls $ T.pack s


runCmd :: Cmd -> Either String (NoteS String ST)
runCmd (Cmd cmd args) = 
    let f = M.lookup cmd ecmds
     in case f of 
          Just f' -> f' args
          Nothing -> Left "cmd not found."

abbr :: ST -> NoteS String ST
abbr st = do
    key <- liftEither $ getKey st
    n@(Note lnk vm abbr sm) <- get
    let (abr, abbr') = abbrev' abbr key 
    put (Note lnk vm abbr' sm)
    return (Abbr abr)

vmkeys :: NoteS String ST
vmkeys = do
    note <- get
    let x = appVM M.keys $ _getVMap note
    put note
    return . List $ map (Key' . toAesonKey) x

abbrkeys :: NoteS String ST
abbrkeys = do
    note <- get
    let x = getAbbrKeys $ _getAbbrev note
    put note
    return . List $ map Abbr x

-- abbrs
isPointedToBy :: ST -> NoteS String ST
isPointedToBy st = do
    note <- get
    abbr <- liftEither $ getAbbr st
    obj <- liftEither $ 
        case A.lengthen (_getAbbrev note) abbr of
          Just k -> Right $ Obj k
          Nothing -> Left "could not find key matching abbr"
    let subjs =  Lnk.isPointedToBy (_getLinks note) obj
    put note
    return . List $ map (\(Subj k) -> Abbr . take' 8 $ keyToText k) subjs

-- keys
isPointedToByK :: ST -> NoteS String ST
isPointedToByK st = do
    note <- get
    key <- liftEither $ getKey st
    let obj = Obj key
        subjs =  Lnk.isPointedToBy (_getLinks note) obj
    put note
    return . List $ map (\(Subj k) -> Key' . toAesonKey $ k) subjs

            
pointsTo :: ST -> NoteS String ST
pointsTo st = do
    note <- get
    abbr <- liftEither $ getAbbr st
    subj <- liftEither $ 
        case A.lengthen (_getAbbrev note) abbr of
          Just k -> Right $ Subj k
          Nothing -> Left "could not find key matching abbr"
    let objs =  Lnk.pointsTo (_getLinks note) subj
    put note
    return . List $ map (\(Obj k) -> Abbr 
                                   . T.pack 
                                   . take 8 $ show k) objs

pointsToK :: ST -> NoteS String ST
pointsToK st = do
    note <- get
    key <- liftEither $ getKey st
    let subj = Subj key
        objs =  Lnk.pointsTo (_getLinks note) subj
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
        case A.lengthen abbr a of
          Just k -> Right k
          Nothing -> Left "could not A.lengthen key"
    (vm', spanKey) <- liftEither $ 
        case insertRawSpan sourceKey sel vm of
          Just (vm', spanKey) -> Right (vm', spanKey) 
          Nothing -> Left "unable to deref span's target key"
    let sm' = registerSpanInsertion spanKey sourceKey sel sm
    put (Note lnk vm' abbr sm')
    return (Key' $ toAesonKey spanKey)
    
-- Encoding
newtype Result a = Result a deriving (Eq, Generic, Show)

instance ToJSON a => ToJSON (Result a) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Result a)


--instance ToJSON JSONByteString where
--    toJSON bs = String $ Base64.encode (getJBS bs)

newtype AesonKey alg = AesonKey { getAesonKey :: T.Text } 
    deriving (Eq, Generic)

instance (HashAlg alg, Show alg) => Show (AesonKey alg) where
    show ak = case fromAesonKey ak of
                Right k -> show k
                Left _ -> "invalid key"


instance ToJSON (AesonKey alg) where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (AesonKey alg)

keyToByteString :: Key alg -> B.ByteString
keyToByteString  = convert

byteStringToKey :: HashAlg alg => B.ByteString -> Maybe (Key alg)
byteStringToKey = digestFromByteString

keyToText :: Key alg -> T.Text
keyToText = decodeUtf8 . Base64.encode . keyToByteString

textToKey :: HashAlg alg => T.Text -> Maybe (Key alg)
textToKey =  join 
           . fmap byteStringToKey 
           . eitherToMaybe 
           . Base64.decode 
           . encodeUtf8

textToKey' :: HashAlg alg => T.Text -> Either String (Key alg)
textToKey' t =  (Base64.decode . encodeUtf8) t >>= maybeToEither 
                                                 . byteStringToKey  

ak = toAesonKey k0

toAesonKey :: HashAlg alg => Key alg -> AesonKey alg
toAesonKey = AesonKey . keyToText

fromAesonKey :: HashAlg alg => AesonKey alg -> Either String (Key alg)
fromAesonKey = textToKey' . getAesonKey

-- how do we convert @show (k :: Key SHA1)@ back to a Key?

data Cmd = Cmd T.Text [ServiceTypes SHA1] deriving (Eq, Generic, Show)

getCmd :: Cmd -> T.Text
getCmd (Cmd cmd _) = cmd

getArgs :: Cmd -> [ServiceTypes SHA1]
getArgs (Cmd _ args) = args


instance ToJSON Cmd where
    toJSON (Cmd cmd args) = object [ "type" .= String "command" 
                                   , "name" .= String cmd
                                   , "args" .= toJSON args
                                   ]

    toEncoding = genericToEncoding defaultOptions

instance FromJSON Cmd

hexStrToKey :: HashAlg alg => String -> Maybe (Key alg)
hexStrToKey s = conv s >>= digestFromByteString . B.pack



-- hex string to [Word8]
conv :: String -> Maybe [BIN.Word8]
conv = sequence . map (fmap (uncurry g) . getTwo) . chunksOf 2

-- where a and b are hex digits
g a b = let wa = shiftL (fromIntegral (digitToInt a) :: BIN.Word8) 4
            wb = fromIntegral (digitToInt b) :: BIN.Word8
         in wa .|. wb

getTwo :: [a] -> Maybe (a, a)
getTwo xs 
  | (length $ (take 2) xs) == 2 = Just (xs !! 0, xs !! 1)
  | otherwise = Nothing


pbits :: PrintfType r => r
pbits =  printf "%b\n"

-- Why will this only compile when preceding EOF?
makeLenses ''NoteS
