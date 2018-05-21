{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving      #-}
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import           Text.Hamlet (shamlet)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html (Html)
import           Yesod

import           Text.Markdown (markdown, def)

import           Data.Time.Clock.POSIX
import           Control.Monad.Except
import           Control.Concurrent.STM
import           Crypto.Hash (SHA1)



import           Lib
import           UI.Types (Note', NoteS, AesonKey)
import           Note hiding (lsvm)
import Parse (note'')


data App = App (TVar Note')
 
type Alias = Text

mkYesod "App" [parseRoutes|
/ HomeR GET
/time TimeR GET
/ls LsR GET
/deref/#Alias DerefR GET
/deref DerefRootR GET
|]
-- TODO add default/fallback route with REST API spec
--
-- □  write API descriptio□  n

instance Yesod App

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    provideRep $ return
        [shamlet|
            <p>Hello, my name is #{name} and I am #{age} years old.
        |]
    provideRep $ return $ object
        [ "name" .= name
        , "age" .= age
        ]
  where
    name = "Michael" :: Text
    age = 28 :: Int


-- | Consumes base-64 encoded SHA1 key, more or less equivalent to 'AesonKey'.
getDerefR :: Text -> Handler Html
getDerefR t = defaultDerefTemplate t <$> runA1 deref t

getDerefRootR :: Handler Html
getDerefRootR = do f <- liftIO $ TL.readFile "static/API"
                   return $ markdown def f

getTimeR :: Handler Text
getTimeR = do
    (App n) <- getYesod
    --x <- liftEither $ Right 3
    t <- liftIO $ getCurrentTime
    return . T.pack $ show t ++ "\n"

getLsR :: Handler Text 
getLsR = runA0 lsAll

-- | Plug in some 'NoteS' state operation, to a `runA*` function to generate
-- the (an/the?) associated 'Handler'.
runA0 :: NoteS String Text -> Handler Text
runA0 cmd = do
    -- n :: TVar Note'
    (App n) <- getYesod
    note <- liftIO . atomically $ readTVar n
    e <- liftIO $ run cmd note
    resp <- liftIO $ case e of
              Left err  -> return $ T.pack err
              Right (summary, note') -> atomically (modifyTVar n (const note'))
                                     >> return summary
    return (resp `T.append` "\n")

runA1 :: (a -> NoteS String Text) -> a -> Handler Text
runA1 cmd arg = do
    -- n :: TVar Note'
    (App n) <- getYesod
    note <- liftIO . atomically $ readTVar n
    e <- liftIO $ run (cmd arg) note
    resp <- liftIO $ case e of
              Left err  -> return $ T.pack err
              Right (summary, note') -> atomically (modifyTVar n (const note'))
                                     >> return summary
    return (resp `T.append` "\n")

defaultDerefTemplate :: Alias -> Text -> Html
defaultDerefTemplate k t = [shamlet|
<h1>DefaultDerefTemplate
<p>#{k}
<p>#{t}
|]


main :: IO ()
main = do
    n <- snd <$> note''
    newTVarIO n >>= warp 3000 . App
