{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module UI.Service where

import qualified Network.Socket as Sock
import Network.Socket hiding (sendTo, send, recv)
import qualified Control.Exception as E
import qualified Network.Socket.ByteString.Lazy as NBL (send, sendAll, recv)
import Network.Socket.ByteString (send, sendAll, recv)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.ByteString.Conversion
import Data.ByteString.Builder

import qualified Data.Text as T
import GHC.Generics

import System.IO

import Control.Concurrent (forkIO)
import Control.Monad (forever, foldM)

import Data.Binary
import Data.Bits

import Text.Printf

import Data.Int (Int64)
import Data.Aeson hiding (encode, decode)
import qualified Data.Aeson as A

import Data.Maybe (fromJust, fromMaybe)

import Control.Monad.State
import Data.Monoid ((<>))

import Note

chunk_size = 32768 -- 2 ^ 15

encode' :: ToJSON a => a -> BL.ByteString
encode' = A.encode

decode' :: FromJSON a => BL.ByteString -> Maybe a
decode' = A.decode


data Cmd = Cmd T.Text [T.Text] deriving (Eq, Generic, Show)

getCmd :: Cmd -> T.Text
getCmd (Cmd cmd _) = cmd

getArgs :: Cmd -> [T.Text]
getArgs (Cmd _ args) = args

cmd = Cmd "cmd" ["arg0", "arg1", "arg2"]

instance ToJSON Cmd where

    toJSON (Cmd cmd args) = object [ "type" .= String "command" 
                                   , "name" .= String cmd
                                   , "args" .= toJSON args
                                   ]

    toEncoding = genericToEncoding defaultOptions

instance FromJSON Cmd


-- | `note` service: 
--
-- * listen for content requests of the form @load <key> <db>@ which should
-- respond with the result of @deref db key@
--
-- * listen for location/context requests of the form @locate <key> <db>@ which
-- should respond with a list of keys (those possible aliases for the conctent
-- around the current cursor position).
-- 
--
-- `note` client:
--
-- For use in e.g., [neo]vim plugin.
--
-- * yield keys of currently viewed blob -- name the buffer? keep a dict?
--
--      - this is kind of tricky as there may not be /one/ key for a given
--      stream. perhaps a safe default would be to return the key of the
--      longest val (that is, of the Blob variant)--that's it: deref Spans until
--      a blob is retrieved, then return teh key to that blob. Thus, /any/ key
--      that references a part of the current content stream should suffice to
--      identitify the context, right?
--
--      - some indication of the scope of each span may help users pick which
--      fits their query.
--      
--      - the key(s) of the smallest span around the query cursorPosn should be
--      returned first
--
-- * alias (abbrev?) keys
--
-- * link aliases, keys
--
-- * create new blobs
--
-- * load blob
--
-- * preview associated blobs (w abbrev keys)
--
--      - this will also be tricky, but more due to the UI limitations of vim
--      coupled with the potentially large number of associated 'Val's, than to
--      some intrinsic property of the data structures involved.
--

server :: IO ()
server = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 10
    forever $ do
        (client, _) <- accept sock
        --clientHandle <- socketToHandle client ReadMode 
        --hSetBuffering clientHandle NoBuffering
        forkIO $ do
            (contents, len) <- recvAll client
            putStrLn $ "received payload of length " ++ show len
            case (A.decode contents :: Maybe Cmd) of
              Just cmd -> do BLC8.putStrLn contents 
                             print cmd
                             if getCmd cmd == "deref" 
                                then let s =  show . fst $ runState (derefAbbrNS (getArgs cmd !! 0)) note'
                                         arg0 = case null (getArgs cmd) of
                                                    False -> Just $ getArgs cmd !! 0
                                                    True -> Nothing
                                         t :: T.Text
                                         t = case lookupRun (getCmd cmd) (fromMaybe "" arg0) note' of
                                               Just x -> x
                                               Nothing -> "Lookup failed"
                                      in (print $ "abbr derefed. sending\n")
                                         *> NBL.send client (encode t) *> return ()
                                else return ()
              Nothing -> print contents
            bytes <- NBL.send client . encode $ "ack " `mappend` toByteString' len
            putStrLn ""
            close client
            --hClose clientHandle 
        

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


send' :: (Socket, SockAddr) -> IO (Socket, SockAddr)
send' (sock, sockAddr) = do
--    putStrLn "please enter '>host port'"
--    putStr ">"
--    ws <- words <$> getLine
--    let host = ws !! 0
--        port = ws !! 1
    --     kjj
    msg' <- B.readFile "specificity.md"
    let sendAll' sock msg = 
            do bytes <- NBL.send sock msg
               if bytes < (BL.length msg)
                  then sendAll' sock (BL.drop bytes msg) 
                  else return ()
    sendAll' sock (pack' msg')
    putStrLn "sent!"
    return (sock, sockAddr)


sendCmd :: (Socket, SockAddr) -> Cmd -> IO ()
sendCmd (sock, _) cmd = do
--    putStrLn "please enter '>host port'"
--    putStr ">"
--    ws <- words <$> getLine
--    let host = ws !! 0
--        port = ws !! 1
    --     kjj
    let enc = encode . A.encode
        sendAll' sock msg = 
            do bytes <- NBL.send sock msg
               if bytes < (BL.length msg)
                  then sendAll' sock (BL.drop bytes msg) 
                  else return ()
    sendAll' sock $ enc cmd
    putStrLn "sent!"
    let recv'' = do  blen <- NBL.recv sock 8
                     rest <- NBL.recv sock . fromIntegral $ (decode blen :: Int64)
                     return rest
--    first <- recv''
--    print first
--    second <- recv''
--    print second
    (contents, _) <- recvAll sock
    BLC8.putStrLn contents
    close sock

oas cmd = do
    conn <- openAndConn "localhost" "4242"
    sendCmd conn cmd

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

