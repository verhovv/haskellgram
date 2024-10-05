{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import TextShow

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when, unless, forever)
import Control.Monad.Fix (fix)
import Network.Socket.ByteString (recv, sendAll, send)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE

import qualified Monomer.Lens as L
import Monomer.Helper (catchAny)
import Data.Word (Word8)

newtype AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Hello world",
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        button "Increase count" AppIncrease
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
  AppIncrease -> [Model (model & clickCount +~ 1)]

doHost :: IO ()
doHost = do
  sock <- socket AF_INET Stream 0
  bind sock (SockAddrInet 4242 0)
  listen sock 1

  accept sock >>= (\(sockOther, y) -> do 
    reader <- forkIO $ fix $ \loop -> do
        msg <- recv sockOther 1024
        unless (C.null msg) $ do
          C.putStrLn msg
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        msg <- C.getLine
        case msg of
             "quit" -> print "Bye!"
             _      -> sendAll sockOther msg >> loop
    killThread reader)

doClient :: String -> IO ()
doClient host = do
  sock <- socket AF_INET Stream 0
  let ip = read <$> words host
  connect sock (SockAddrInet 4242 (tupleToHostAddress (ip!!0, ip!!1, ip!!2, ip!!3)))

  reader <- forkIO $ fix $ \loop -> do
        msg <- recv sock 1024
        unless (C.null msg) $ do
          C.putStrLn msg
        loop
      
  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        msg <- C.getLine
        case msg of
             "quit" -> print "Bye!"
             _      -> sendAll sock msg >> loop
  killThread reader

main :: IO ()
main = do
  putStrLn "1. Host\n2. Client"
  inpt <- getLine
  case inpt of
    "1" -> doHost
    _   -> do
          putStrLn "Enter host ip"
          ip <- getLine
          doClient ip
        

  -- startApp model handleEvent buildUI config
  -- where
  --   config = [
  --     appWindowTitle "Hello world",
  --     appWindowIcon "./assets/images/icon.png",
  --     appTheme darkTheme,
  --     appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
  --     appInitEvent AppInit
  --     ]
  --   model = AppModel 0
 