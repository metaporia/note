{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import           Text.Hamlet (shamlet, shamletFile, hamlet, hamletFile)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze.Html (Html)
import           Yesod
import           Yesod.Static (staticDevel, Static)

import           Text.Markdown (markdown, def)

import           Data.Time.Clock.POSIX
import           Control.Monad.Except
import           Control.Concurrent.STM
import           Crypto.Hash (SHA1)

import Data.Monoid ((<>))

import           Lib
import           UI.Types (Note', NoteS, AesonKey)
import           Note hiding (lsvm)
import Parse (note'')


data App = App { getStatic :: Static 
               , getTVar :: (TVar Note')
               }
 
type Alias = Text

-- | dummy route renderer (for \@{..} in 'hamlet' QQs)
--
-- this won't break quickly if a decent 'Show' instance is 
-- derived for the 'Route' type you're /dying/ to implement
render :: Route' -> String
render = show 

-- NB: (it seems) the primary difference between 'hamlet' and 'shamlet' is
-- typed-URL interpolation, which IIRC depends upon a custom/derived path
-- rendering function. The dummy 'Route'' currently fools "shakespeare" into
-- silence (for the present).

data Route' = Home 
           | Deref
           | API
           | Ls
           | Time

instance Show Route' where
    show Home = "/"
    show Deref = "/deref"
    show API = "/API"
    show Ls = "/ls"
    show Time = "/time"

-- should mirror the contents of the (below) 'parseRoutes' macro invocation.
mkYesod "App" [parseRoutes|
/ HomeR GET
/time TimeR GET
/ls LsR GET
/deref/#Alias DerefR GET
/API APIDescriptionR GET
/ls/abbr AbbrR GET
/ls/abbr/preview AbbrTableR GET
/static StaticR Static getStatic
|]
-- TODO add default/fallback route with REST API spec
--
-- □  write API description

instance Yesod App

getHomeR' :: Handler TypedContent
getHomeR' = selectRep $ do
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

getAPIDescriptionR :: Handler Html
getAPIDescriptionR = do f <- liftIO $ TL.readFile "static/API"
                        return $ markdown def f

generateIndexHtml :: Handler Html
generateIndexHtml = do f <- liftIO $ TL.readFile "static/index.md"
                       return $ markdown def f


-- | Serve @/index.html@
getHomeR :: Handler Html
getHomeR = generateIndexHtml

getTimeR :: Handler Text
getTimeR = do
    (App _ n) <- getYesod
    --x <- liftEither $ Right 3
    t <- liftIO $ getCurrentTime
    return . T.pack $ show t ++ "\n"

getLsR :: Handler Text 
getLsR = runA0 lsAll

-- | Plug in some 'NoteS' state operation, to a `runA*` function to generate
-- the (an/the?) associated 'Handler'.
--
-- Updates program state if embedded 'NoteS' operation succeeds.
runA0 :: NoteS String Text -> Handler Text
runA0 cmd = do
    -- n :: TVar Note'
    (App _ n) <- getYesod
    note <- liftIO . atomically $ readTVar n
    e <- liftIO $ run cmd note
    resp <- liftIO $ case e of
              Left err  -> return $ T.pack err
              Right (summary, note') -> atomically (modifyTVar n (const note'))
                                     >> return summary
    return (resp `T.append` "\n")

-- | Like 'runA0' but for 'Html'. Wraps error(s) in 'defaultNoteSErrorTemplate'.
runA0' :: NoteS String Html -> Handler Html
runA0' cmd = do
    -- n :: TVar Note'
    (App _ n) <- getYesod
    note <- liftIO . atomically $ readTVar n
    e <- liftIO $ run cmd note
    resp <- liftIO $ case e of
              Left err  -> return . defaultNoteSErrorTemplate $ T.pack err
              Right (html, note') -> atomically (modifyTVar n (const note'))
                                  >> return html
    return resp


runA1 :: (a -> NoteS String Text) -> a -> Handler Text
runA1 cmd arg = do
    -- n :: TVar Note'
    (App _ n) <- getYesod
    note <- liftIO . atomically $ readTVar n
    e <- liftIO $ run (cmd arg) note
    resp <- liftIO $ case e of
              Left err  -> return $ T.pack err
              Right (summary, note') -> atomically (modifyTVar n (const note'))
                                     >> return summary
    return (resp `T.append` "\n")


getAbbrR :: Handler TypedContent
getAbbrR = selectRep $ do
    provideRep getAbbrText
    provideRep getAbbrHtml

getAbbrText :: Handler Text
getAbbrText = runA0 (T.intercalate "\n" <$> abbrkeys)

getAbbrHtml :: Handler Html
getAbbrHtml = runA0' (defaultListOfLinksTemplate <$> abbrkeys)

getAbbrTableR :: Handler Html
getAbbrTableR = do 
    (App _ n) <- getYesod
    note <- liftIO . atomically $ readTVar n
    e <- liftIO $ run abbrkeys note
    resp <- liftIO $ case e of
              Left err -> return 
                        . return 
                        . defaultNoteSErrorTemplate $ T.pack err
              Right (keys, note') -> return 
                                   . fmap prevsToHtml 
                                   . sequence $ derefToPair <$> keys
    resp
    

-- | consumes abbr and emits (abbr, dereffedVal)
derefToPair :: Text -> Handler (Text, Text)
derefToPair t = (,) t <$> runA1 deref t


prevsToHtml :: [(Text, Text)] -> Html
prevsToHtml previews = ($(hamletFile "static/defaultAbbrTableTemplate.hamlet")) render

defaultDerefTemplate :: Alias -> Text -> Html
defaultDerefTemplate k t = [shamlet|
<h1>DefaultDerefTemplate
<p>#{k}
<p>#{t}

<a href=/API>See API documentation, 
which is, BEWARE, a WIP.
|] 

defaultListOfLinksTemplate :: [Text] -> Html
defaultListOfLinksTemplate abbrs = [shamlet|
<h2> Dereferenceable key abbreviation links
$if null abbrs
    <p>No `Abbrevs`
$else
    <ul>
        $forall abbr <- abbrs
            <li><a href=#{T.append "/deref/" abbr}>#{abbr}
|]

defaultNoteSErrorTemplate :: Text -> Html
defaultNoteSErrorTemplate err = [hamlet|
<p>`note` error: #{err}

<a href=/API>See API documentation, which is, BEWARE, a WIP.
|] render

main :: IO ()
main = do
    s <- staticDevel "static"
    n <- snd <$> note''
    newTVarIO n >>= warp 3000 . App s
