{-# LANGUAGE OverloadedStrings #-}
module UI.Service where

import Network.Socket hiding (send)
import Network.Socket.ByteString (send)

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
-- 
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    conn <- accept sock
    runConn conn
    mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    send sock "Hello\n"
    close sock



