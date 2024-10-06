{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text, pack)
import Monomer
import TextShow
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when, unless, forever)
import Control.Monad.Fix (fix)
import Network.Socket.ByteString (recv, sendAll, send, sendMsg)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE

import qualified Monomer.Lens as L
import Monomer.Helper (catchAny)
import Data.Word (Word8)
import Data.Text.Lens

data AppModel = AppModel {
  _iptext :: Text,
  _message :: Text,
  _chat :: Text,
  _crutch :: String
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | SendMessages Text
  | GetMessages Socket
  | CreateRoom
  | JoinRoom Text
  | TextChanged Text
  | ShowMessage Text
  | ClearCrutch
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [ 
      hstack_ [sizeReqUpdater $ fixedToMaxH 10] [
        button "Create room" CreateRoom,
        spacer,
        textField_ iptext [placeholder "127 0 0 1"] `nodeKey` "iptext",
        spacer,
        button "Join" (JoinRoom (model ^. iptext))
      ],
      spacer,
      hstack[
        textAreaV_ (model ^. chat) TextChanged [readOnly, maxLines 1000]
      ],
      spacer,
      hstack_ [sizeReqUpdater $ fixedToMaxH 1] [
        textField_ message [placeholder "Message"] `nodeKey` "message",
        spacer,
        button "Send message" (SendMessages $ model ^. message)
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  GetMessages sock -> [Producer (getMessages sock model)]
  SendMessages msg -> [
    Model (model & crutch .~ (msg ^. from packed)),
    Model (model & chat .~ ((model ^. chat) <> msg) <> "\n")]
  ShowMessage text -> [Model (model & chat .~ ((model ^. chat) <> text) <> "\n")]
  CreateRoom -> [Producer $ doHost model]
  JoinRoom ip -> [Producer $ doClient model (ip ^. from packed)]
  TextChanged text -> []
  ClearCrutch -> [Model (model & crutch .~ "")]

doHost :: AppModel -> (AppEvent -> IO ()) -> IO ()
doHost model sendEvent = do
  sock <- socket AF_INET Stream 0
  bind sock (SockAddrInet 4242 0)
  listen sock 1

  accept sock >>= (\(sockOther, y) -> do 
    sendEvent (GetMessages sockOther)
    forever $ do
      when (model ^. crutch /= "") $ do 
        sendAll sock (C.pack (model ^. crutch ^. from packed))
        sendEvent ClearCrutch)


doClient :: AppModel -> String -> (AppEvent -> IO ()) -> IO ()
doClient model host sendEvent = do
  sock <- socket AF_INET Stream 0
  let ip = read <$> words host
  connect sock (SockAddrInet 4242 (tupleToHostAddress (ip!!0, ip!!1, ip!!2, ip!!3)))

  sendEvent (GetMessages sock)
  forever $ do
      when (model ^. crutch /= "") $ do 
        sendAll sock (C.pack (model ^. crutch ^. from packed))
        sendEvent ClearCrutch

getMessages :: Socket -> AppModel -> (AppEvent -> IO ()) -> IO ()
getMessages sock model event = do
  msg <- recv sock 1024
  unless (C.null msg) $ do
    event $ ShowMessage (pack (C.unpack msg))
  getMessages sock model event

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "HASKELLGRAM",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit
      ]
    model = AppModel { 
      _iptext = "",
      _message = "",
      _chat = "",
      _crutch = ""
    }
 