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
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Network.Socket.ByteString (recv, sendAll)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE

import qualified Monomer.Lens as L
import Monomer.Helper (catchAny)

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


type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock
  forkIO (runConn conn chan msgNum)
  mainLoop sock chan $! msgNum + 1


runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Hi, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
             "quit" -> hPutStrLn hdl "Bye!"
             _      -> broadcast (name ++ ": " ++ line) >> loop

    killThread reader
    broadcast ("<-- " ++ name ++ " left.")
    hClose hdl  

conClient :: Socket -> Chan Msg -> IO ()
conClient sock chan = do
    reader <- forkIO $ fix $ \loop -> do
        msg <- recv sock 1024
        C.putStrLn msg
        loop

    sender <- forkIO $ fix $ \loop -> do
        msg <- C.getLine
        sendAll sock msg
        loop

    killThread reader
    killThread sender
          

main :: IO ()
-- server
-- main = do
--   sock <- socket AF_INET Stream 0
--   setSocketOption sock ReuseAddr 1
--   bind sock (SockAddrInet 4242 (tupleToHostAddress (127, 0, 0, 1)))
--   listen sock 5
--   chan <- newChan
--   _ <- forkIO $ fix $ \loop -> do
--     (_, _) <- readChan chan
--     loop
--   mainLoop sock chan 0

-- client
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  connect sock (SockAddrInet 4242 (tupleToHostAddress (192, 168, 148, 197)))
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  conClient sock chan

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
 