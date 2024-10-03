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
  putStrLn "Waiting for connection/users"
  print "main"
  conn <- accept sock
  print "accept sock"
  forkIO (runConn conn chan msgNum)
  print "running con"
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    print "connected"
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    putStrLn "Hi, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered chat.")
    putStrLn ("Welcome, " ++ name ++ "!")

    commLine <- dupChan chan
  
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ putStrLn line
        -- msg <- recv sock 1024
        -- B.putStrLn msg
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        -- line <- fmap init (hGetLine hdl)
        line <- getLine
        case line of
             "quit" -> print "Bye!"
             _      -> broadcast ("name" ++ ": " ++ line) >> loop

    killThread reader
    --sendAll sock (B.pack ("<-- " ++ "name" ++ " left."))
    hClose hdl  


-- main = runTCPClient "127.0.0.1" "3000" $ \s -> do
--     sendAll s "Hello, world!"
--     msg <- recv s 1024
--     putStr "Received: "
--     C.putStrLn msg

-- -- from the "network-run" package.
-- runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
-- runTCPClient host port client = withSocketsDo $ do
--     addr <- resolve
--     E.bracket (open addr) close client
--   where
--     resolve = do
--         let hints = defaultHints { addrSocketType = Stream }
--         NE.fromList <$> getAddrInfo (Just hints) (Just host) (Just port)
--     open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
--         connect sock $ addrAddress addr
--         return sock

main :: IO ()
-- host
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 5
  chan <- newChan
  _ <- forkIO $ fix $ \loop -> do
    (_, _) <- readChan chan
    loop
  mainLoop sock chan 0

-- client
-- main = do
--   sock <- socket AF_INET Stream 0
--   setSocketOption sock ReuseAddr 1
--   connect sock (SockAddrInet 4242 (tupleToHostAddress (127, 0, 0, 1)))
--   listen sock 5 -- ?
--   chan <- newChan
--   _ <- forkIO $ fix $ \loop -> do
--     (_, _) <- readChan chan
--     loop
--   mainLoop sock chan 0

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
 