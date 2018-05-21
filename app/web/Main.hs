{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
import           Data.Text (Text)
import qualified Data.Text as T
import           Yesod

import Data.Time.Clock.POSIX

import Lib
import UI.Types
import Note

data App = App Note'
    deriving (Show)
 


mkYesod "App" [parseRoutes|
/ HomeR GET
/deref DerefR GET
/time TimeR GET
|]
-- TODO add default/fallback route with REST API spec

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


getDerefR :: Handler Text
getDerefR = return "hello world!\n"

getTimeR :: Handler Text
getTimeR = do
    app <- getYesod
    t <- liftIO $ getCurrentTime
    return . T.pack $ show t ++ "\n"


main :: IO ()
main = warp 3000 $ App newNote
