{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
module Lib
    ( someFunc
    ) where

import ClassyPrelude hiding (Handler,pack,Builder)

import Network.Wai

import Web.Scotty (scotty,get,post,text,html,ActionM,header)
-- import Web.Scotty.Blaze

import Text.Digestive.Scotty (runForm)

import Text.Blaze.Renderer.Text (renderMarkup)

import qualified Control.Monad.State as MS

import           Blaze.ByteString.Builder (Builder)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)


import qualified Web.Spock as Spock
import           Web.Spock (spock,runSpock,SpockM,root)

import qualified Web.Spock.Config as SpockConfig
import           Web.Spock.Config (defaultSpockCfg, PoolOrConn( PCNoDatabase ))

import Data.IORef

import Servant
import GHC.Generics
import Data.Aeson

import Network.Wai.Handler.Warp (run)

import       Lucid.Base
import       Lucid.Html5
import       Data.Text

import Web.Spock.Lucid

import qualified Text.Blaze.Html  as Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Accounts.AccountViews
import Accounts.AccountForms

myScottyApp :: IO ()
myScottyApp = do
  scotty 3000 $ do
    get "/" $ do
      text "hello from scotty"
    get "/html" $ do
      html "<h1>hello world</h1>"
    get "/blaze" $ do
      html $ renderMarkup $ blazeHello "Scotty"
    get "/lucid" $ do
      html $ renderText $ lucidHello_ "Scotty"
    get "/login" $ do
      html $ renderText $ layoutLucid_ (loginView defaultLoginForm)
    post "/login" $ do
      (view,result) <- runForm "loginForm" loginForm
      case result of
        Just dto -> html "All Good"
        Nothing  -> html "bad"

data MySession = EmptySession
data MyAppState = DummyAppState

spockRoutes :: SpockM () MySession MyAppState ()
spockRoutes = do
  Spock.get root $
    Spock.text "hello from spock"
  Spock.get "/html" $
    Spock.html "<h1>Hello From Spock</h1>"
  Spock.get "/blaze" $
    Spock.html $ toStrict $ renderMarkup $ blazeHello "Spock"
  Spock.get "/lucid" $
    lucid $ lucidHello_ "Spock"

mySpockApp :: IO ()
mySpockApp = do
  spockConfig <- defaultSpockCfg EmptySession PCNoDatabase DummyAppState
  runSpock 3000 (spock spockConfig spockRoutes)


data MyMessage
  = MyMessage
  { message :: String
  }
  deriving (Generic,Show)

instance ToJSON MyMessage where
  toJSON (MyMessage msg) =
    object [ "message" .= msg ]

myMessage = MyMessage "Hello from Servant"

type MessageApi
  = "messages" :> Get '[JSON][MyMessage]

messageHandler :: Handler [MyMessage]
messageHandler =
  return [myMessage]

myApi = messageHandler

messagesProxy :: Proxy MessageApi
messagesProxy = Proxy

servantApp =
  run 3000 (serve messagesProxy myApi)

someFunc :: IO ()
someFunc = 
    -- putStrLn "Hello World"
    myScottyApp
    -- mySpockApp
    -- servantApp


layoutLucid_ :: Html () -> Html ()
layoutLucid_ content_ = do
  html_ $ do
    head_ $ do
      link_ [rel_ "stylesheet", href_ "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"]
      title_ "Layout"
    body_ $ do
      content_

lucidHello_ :: Text -> Html ()
lucidHello_ framework = do
  layoutLucid_ $ do
    h1_ $ toHtml ("Hello from Lucid and " <> framework)

layoutBlaze :: Blaze.Html -> Blaze.Html
layoutBlaze content =
  H.docTypeHtml $ do
    H.head $ do
      H.title "Layout"
    H.body $ do
      content

blazeHello :: Text -> Blaze.Html
blazeHello framework =
  layoutBlaze $ do
    H.h1 $ H.toHtml ("Hello from blaze and " <> framework)


