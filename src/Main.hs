{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Text (Text, pack)
import Monomer
import Network.Socket
import Control.Monad (when, unless)
import Control.Monad.Fix (fix)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import Data.Text.Lens
import qualified Monomer.Core.Lens as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data AppModel = AppModel {
  _iptext :: Text,
  _message :: Text,
  _chat :: Text,
  _isSocketCreated :: Bool
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | SendMessages Text
  | GetMessages Socket
  | CreateRoom
  | JoinRoom Text
  | TextChanged Text
  | ShowMessage Text
  | SendMessageToSocket Socket
  | ClearMessage
  | SocketCreated
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [ 
      hstack_ [sizeReqUpdater $ fixedToMaxH 10] [
        button "Create room" CreateRoom `styleBasic` [bgColor $ rgbHex "#66269a", border 0 $ rgbHex "#000", textColor $ rgbHex "#FFFFFF"],
        spacer,
        keystroke [("Enter", JoinRoom (model ^. iptext))] $ textField_ iptext [placeholder "127 0 0 1"] `styleBasic` [bgColor $ rgbHex "#303030", border 0 $ rgbHex "#000", textRight],
        spacer,
        button "Join" (JoinRoom (model ^. iptext)) `styleBasic` [bgColor $ rgbHex "#66269a", border 0 $ rgbHex "#000", textColor $ rgbHex "#FFFFFF"]
      ],
      spacer,
      hstack [
        textAreaV_ (model ^. chat) TextChanged [readOnly, maxLines 1000]
          `styleBasic` [textColor $ Color 255 255 255 1, height 500, bgColor $ rgbHex "#303030", border 0 $ rgbHex "#000"]
      ],
      spacer,
      hstack_ [sizeReqUpdater $ fixedToMaxH 1] [
        keystroke [("Enter", SendMessages $ model ^. message)] $ textField_ message [placeholder "Message"] `styleBasic` [bgColor $ rgbHex "#303030", border 0 $ rgbHex "#000"],
        spacer,
        button "Send message" (SendMessages $ model ^. message) `styleBasic` [bgColor $ rgbHex "#66269a", border 0 $ rgbHex "#000", textColor $ rgbHex "#FFFFFF"]
      ]
    ] `styleBasic` [bgColor $ rgbHex "#1D1D1D", padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> []
  GetMessages sock -> [Producer (getMessages sock model)]
  SendMessageToSocket sock -> [Producer (sendMessages sock model)]
  SendMessages msg -> [Model (model & message .~ msg <> "\n")]
  ShowMessage text -> [Model (model & chat .~ ((model ^. chat) <> text) <> "\n")]
  CreateRoom -> [
    Producer $ doHost model,
    Model $ model & isSocketCreated .~ True
    ]
  JoinRoom ip -> [Producer $ doClient model (ip ^. from packed)]
  TextChanged text -> []
  ClearMessage -> [Model (model & message .~ "")]
  SocketCreated -> [Model $ model & isSocketCreated .~ True]

doHost :: AppModel -> (AppEvent -> IO ()) -> IO ()
doHost model sendEvent = do
  unless (model ^. isSocketCreated) $ do 
    sendEvent $ ShowMessage "--> Waiting for connection... <--"
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet 4242 0)
    listen sock 1

    accept sock >>= (\(sockOther, y) -> do 
      sendEvent $ ShowMessage "--> Connected <--"
      sendEvent $ GetMessages sockOther
      sendEvent $ SendMessageToSocket sockOther)

doClient :: AppModel -> String -> (AppEvent -> IO ()) -> IO ()
doClient model host sendEvent = do
  unless (model ^. isSocketCreated) $ do 
    sendEvent $ ShowMessage "--> Connection... <--"
    let ip = read <$> words host
    if checkAddr ip then do
      sock <- socket AF_INET Stream 0
      connect sock (SockAddrInet 4242 (tupleToHostAddress (ip!!0, ip!!1, ip!!2, ip!!3)))

      sendEvent SocketCreated
      sendEvent $ ShowMessage "--> Connected <--"
      sendEvent $ GetMessages sock
      sendEvent $ SendMessageToSocket sock
    else sendEvent $ ShowMessage "--! Invalid address or you !--"

checkAddr :: [Word8] -> Bool
checkAddr ip 
  | length ip /= 4 = False
  | otherwise = isSupportedSockAddr $ SockAddrInet 4242 (tupleToHostAddress (ip!!0, ip!!1, ip!!2, ip!!3))

getMessages :: Socket -> AppModel -> (AppEvent -> IO ()) -> IO ()
getMessages sock model sendEvent = do
  msg <- recv sock 1024
  unless (C.null msg) $ do
    sendEvent $ ShowMessage $ T.decodeUtf8 msg
  sendEvent $ GetMessages sock

sendMessages :: Socket -> AppModel -> (AppEvent -> IO ()) -> IO ()
sendMessages sock model sendEvent = do
  unless (null (words $ model ^. message ^. from packed) || last (model ^. message ^. from packed) /= '\n') $ do
    sendAll sock $ T.encodeUtf8 $ pack $ init $ model ^. message ^. from packed
    sendEvent $ ShowMessage $ pack $ "You: " <> init (model ^. message ^. from packed)
    sendEvent ClearMessage
  sendEvent $ SendMessageToSocket sock

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "HASKELLGRAM",
      appWindowIcon "./assets/images/icon.png",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/mainfont.ttf",
      appInitEvent AppInit,
      appMaxFps 20,
      appWindowResizable False
      ]
    model = AppModel { 
      _iptext = "",
      _message = "",
      _chat = "",
      _isSocketCreated = False
    }