{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module UI.Service (serve, oas, module Types) where

import qualified Network.Socket as Sock
import Network.Socket.Options
import Network.Socket hiding (sendTo, send, recv)
import qualified Control.Exception as E
import qualified Network.Socket.ByteString.Lazy as NBL (send, sendAll, recv)
import Network.Socket.ByteString (send, sendAll, recv)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.ByteString.Conversion
import Data.ByteString.Builder

import System.Posix.Signals

import Crypto.Hash 

import qualified Data.Text as T
import GHC.Generics

import System.IO

import System.Exit
import Control.Concurrent ( forkIO, myThreadId, ThreadId, threadDelay
                          , newEmptyMVar, putMVar)
import qualified Control.Exception as E
import Control.Monad (forever, foldM)


import Data.Binary hiding (get, put)
import Data.Bits

import Text.Printf

import Data.Int (Int64)
import Data.Aeson hiding (encode, decode, Result)
import qualified Data.Aeson as A

import Data.Maybe (fromJust, fromMaybe)
import Data.Either

import Control.Monad.State
import Data.Monoid ((<>))
import Control.Exception hiding (Handler)
import Control.Monad.Except


-- Internal
import Note hiding ( runWith
                   , lsvm
                   , lslnk
                   , alias
                   )
import UI.Types as Types
import VMap (HashAlg)
import Helpers


chunk_size = 32768 -- 2 ^ 15
recv_timeout = 1000000

encode' :: ToJSON a => a -> BL.ByteString
encode' = A.encode

decode' :: FromJSON a => BL.ByteString -> Maybe a
decode' = A.decode

handleInterrupt :: ThreadId -> Socket -> Handler
handleInterrupt tid sock = Catch $ do 
    E.throwTo tid E.UserInterrupt
    close sock
    putStrLn "caught SIGINT; closed sock"

serve :: IO ()
serve = do
    tid <- myThreadId
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 10
    installHandler sigINT (handleInterrupt tid sock) Nothing
    -- this bad boy of a line does it all
    e <- runWith' (loop' sock note') newNote -- TODO: parameterize
    
    case e of
      Left err -> print err
      Right (_, finalState) -> print "receieved final state."

    close sock
            
loop' :: Socket -> Note' -> NoteS String ()
loop' sock  n = do
    (client, _ ) <- liftIO $ accept sock
    liftIO $ setRecvTimeout client recv_timeout
    (contents, len) <- liftIO $ recvAll client
    liftIO . putStrLn $ "received payload of length " ++ show len
    cmd <- liftEither . maybeToEither $ (A.decode contents :: Maybe Cmd)
    noteS <- liftEither $ runCmd cmd
    e <- liftIO $ runWith' noteS n
    case e of
      Left err -> do liftIO $print err
                     liftIO $ NBL.sendAll client . encode . A.encode . Err $ T.pack err
                     liftIO $ close client
                     liftIO $ putStrLn "looping newstate--lerr"
                     loop' sock n
      Right (st, n') -> do liftIO $ NBL.sendAll client . encode . A.encode $ st
                           liftIO $ close client
                           liftIO $ putStrLn "looping newstate"
                           let x = runWith' (loop' sock n) n
                           loop' sock n'

    liftIO $ close sock
    return ()


-- | Embed application of 'Cmd' inside 'NoteS' state.
handleCmd :: Either String Cmd -> NoteS String ST 
handleCmd eCmd = do
    n <- get
    cmd <- liftEither eCmd
    noteS <- liftEither $ runCmd cmd
    e <- liftIO $ runWith' noteS n
    (st, n') <- liftEither e
    put n'
    return st

showSock sock = do
    isc <- isConnected sock
    isb <- isBound sock
    isr <- isReadable sock
    isl <- isListening sock
    putStrLn $ show isc
    putStrLn $ show isb
    putStrLn $ show isr
    putStrLn $ show isl

contents = encode . A.encode $ Blob' "AOEuhaoeuaoeuaoeu"
cmd' = encode . A.encode $ Cmd "deref" [Blob' "spec"]

shutdownServer :: Socket -> IO ()
shutdownServer sock = NBL.sendAll sock (encode ("exit":: BL.ByteString) :: BL.ByteString)
        

-- client
openAndConn :: HostName -> String -> IO (Socket, SockAddr)
openAndConn host port = do
    -- host, port lookup. err or nonempty [AddrInfo].
    addrInfo <- getAddrInfo Nothing 
                            (Just host)
                            (Just port)
    let serverInfo = head addrInfo
        sockAddr = addrAddress serverInfo
    sock <- socket (addrFamily serverInfo)
                   (addrSocketType serverInfo)
                   (addrProtocol serverInfo)
    
    connect sock sockAddr
    return (sock, sockAddr)

sendCmd :: (Socket, SockAddr) -> Cmd -> IO (Maybe (ServiceTypes SHA1))
sendCmd (sock, _) cmd = do
    NBL.sendAll sock . encode $ A.encode cmd 
    putStrLn "sent!"
    (contents, _) <- recvAll sock
    BLC8.putStrLn contents
    close sock
    return (A.decode contents :: Maybe (ServiceTypes SHA1))

oas :: Cmd -> IO (Maybe (ServiceTypes SHA1))
oas cmd = do
    conn <- openAndConn "localhost" "4242"
    mST <- sendCmd conn cmd
    return mST


-- NB:
--
-- > (ntohl . decode . encode . htonl $ l) == l
--
pack' :: Binary a => a -> BL.ByteString
pack' payload = encode payload
    -- @ntohl . decode@ on the other side.


-- | A (hopefully) less moronic chunking strategy.
--
-- NB: 'mappend'ing 'Builder's has O(1) runtime!
recvAll :: Socket -> IO (BL.ByteString, Int64)
recvAll sock = do
    blen <- NBL.recv sock 8
    let len = fromIntegral (decode blen :: Int64)
    contents <- go sock len (lazyByteString "")
    return (contents, len)
    -- go sock len ""
    where go :: Socket -> Int64 -> Builder -> IO BL.ByteString
          go sock len buf = do
              chunk <- NBL.recv sock chunk_size
--             putStrLn "receiving..."
              let chunkLen = BL.length chunk
                  nextLen = len - chunkLen
                  buf' = buf <> (lazyByteString chunk)
              if nextLen > 0
                 then go sock nextLen buf' -- optimize?
                 else return (toLazyByteString buf')


unroll :: Word32 -> [Word8]
unroll x = map fromIntegral [ x .&. 0xff
                            , (x .&. 0xff00) `shiftR` 8
                            , (x .&. 0xff0000) `shiftR` 16 
                            , (x .&. 0xff000000) `shiftR` 24
                            ]
tuple :: [a] -> (a, a, a, a)
tuple as = (as !! 0, as!!1,as!!2,as!!3)

roll' = roll . tuple

roll :: (Word8, Word8, Word8, Word8) -> Word32
roll (w, x, y, z) = let ws :: [Word32]
                        ws =  [ fromIntegral w
                              , (fromIntegral x) `shiftL` 8
                              , (fromIntegral y) `shiftL` 16 
                              , (fromIntegral z) `shiftL` 24
                            ]
                         in foldr (.|.) 0x00000000 $ ws

pbits :: PrintfType r => r
pbits =  printf "%b\n"


encodingExample :: IO ()
encodingExample = do
    f <- B.readFile "specificity.md"
    let l = fromIntegral (B.length f) :: Word32 

    putStrLn $ "l = " ++ show l
    putStrLn $ "encode . htonl $ l = " ++ show (encode . htonl $ l)
    putStrLn $ "decode . encode . htonl $ l =" ++ show (decode . encode . htonl $ l :: Word32)
    putStrLn $ "ntohl . decode . encode . htonl $ l =" ++ show (ntohl . decode . encode . htonl $ l)

