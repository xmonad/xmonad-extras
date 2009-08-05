-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.EvalServer
-- Copyright   :  (c) 2009 Daniel Schoepe
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Daniel Schoepe <daniel.schoepe@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module allows controlling XMonad through Haskell expressions sent
-- via a socket.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.EvalServer (
                               -- * Usage
                               -- $usage

                               -- * Documentation
                               -- $documentation

                                initEVData
                               ,startServer
                               ,defaultServer
                               ,defaultServerConfig
                               ,evalEventHook
                               ,EvalServerData
                               ) where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar

import Data.Monoid

import System.IO

import XMonad.Actions.Eval
import XMonad

import Network

-- $usage
--
-- WARNING: This module will have the following issue if xmonad wasn't compiled with -threaded
-- (requires a modified xmonad-version): Expressions will only get evaluated when xmonad
-- receives an event, for example when the focus changes.
--
-- This module is highly experimental and might not work as expected or even cause deadlocks
-- when used with -threaded, due to the fact that xlib isn't reentrant.
--
-- This module lets you create a server that evaluates Haskell expressions in
-- the context of the currently running xmonad instance, which lets you control xmonad from
-- another process(e.g. a script).
-- To use this module add something like this to your xmonad.hs:
--
-- > import XMonad.Hooks.EvalServer
--
-- > main = do
-- >   evData <- initEVData
-- >   ..
-- >   xmonad $ .. $ defaultConfig {
-- >                   handleEventHook = evalEventHook defaultServerConfig evData
-- >                   startupHook = defaultServer evData 4242
-- >   ..
-- >   }
--
-- You can then send Haskell expressions that are to be evaluated over the socket.
-- Example using telnet:
--
-- > telnet localhost 4242
-- > windows $ W.view "1"
--

-- $documentation

-- | Data type for storing information such as the socket and received commands
data EvalServerData = EVData { evThreads :: MVar [(ThreadId,Handle)]
                             , evCommands :: MVar [(String,Handle)]
                             , evSocket :: MVar Socket }

-- | Creates the structure to store received commands and other data. A variable of this
-- type has to be passed to the other functions of this module.
initEVData :: MonadIO m => m EvalServerData
initEVData = liftIO $ liftM3 EVData (newMVar []) newEmptyMVar newEmptyMVar -- not so pretty, but fits on one line

-- | Creates a server listening on a TCP socket with the given port number.
defaultServer :: MonadIO m => EvalServerData -> PortNumber -> m ()
defaultServer cv = startServer cv . PortNumber

-- | Creates a server listening on the specified port(can also be a unix domain socket).
startServer :: MonadIO m => EvalServerData -> PortID -> m ()
startServer evdata port = liftIO $ do
  s <- listenOn port
  putMVar (evSocket evdata) s
  tid <- forkIO . forever $ accept s >>= clientThread evdata
  modifyMVar_ (evThreads evdata) $ return . ((tid,stdout):)
  return ()

-- | Default config to evaluate the received expressions
defaultServerConfig :: EvalConfig
defaultServerConfig = defaultEvalConfig { handleError = return . show }

-- | This event hook causes commands to be executed when they are received.
evalEventHook :: EvalConfig -> EvalServerData -> Event -> X All
evalEventHook evConfig evdata (ClientMessageEvent { ev_message_type = mt }) = do
  dpy <- asks display
  update <- io $ internAtom dpy "XMONAD_EVALSRV_UPD" False
  restrt <- io $ internAtom dpy "XMONAD_RESTART" False
  if mt == update
     then do
       cmds <- io . tryTakeMVar . evCommands $ evdata
       whenJust cmds $ mapM_ $ \(cmd,h) ->
          evalExpressionWithReturn evConfig cmd >>= io . hPutStrLn h
       return $ All False
     else if mt == restrt
             then shutdownServer evdata >> return (All True)
             else return $ All True
evalEventHook _ _ _ = return $ All True

shutdownServer :: MonadIO m => EvalServerData -> m ()
shutdownServer evdata = liftIO $ do
  -- we need to kill the reading thread first, otherwise hClose will block
  modifyMVar_ (evThreads evdata) $ (>> return []) . mapM_ (\(tid,h) -> killThread tid >> hClose h)
  modifyMVar_ (evSocket evdata) $ \s -> sClose s >> return s

-- | Handler for an individual client.
clientThread :: EvalServerData -> (Handle,HostName,PortNumber) -> IO ()
clientThread evdata (h,_,_) = do
  tid <- forkIO $ do
           hSetBuffering h LineBuffering
           forever $ hGetLine h >>= handleCommand h evdata
  modifyMVar_ (evThreads evdata) $ return . ((tid,h):)

-- | Handles a received command. TODO: Add a more elaborate protocol(e.g. one that allows shutting
-- down the server).
handleCommand :: Handle -> EvalServerData -> String -> IO ()
handleCommand h evdata cmd = openDisplay "" >>= \dpy -> do
  let cmds = evCommands evdata
  empt <- isEmptyMVar cmds
  if empt
     then putMVar cmds [(cmd,h)]
     else modifyMVar_ cmds (return . ((cmd,h):))
  -- normally we should use forkProcess here, but this doesn't work
  -- due to ghc issue 1185: http://hackage.haskell.org/trac/ghc/ticket/1185
  -- forkIO with -threaded could potentially cause problems, as the Xlib is
  -- not reentrant, so not using a -threaded version of xmonad and sending
  -- some event to the root window to have getEvent return might be preferable.
  forkIO $ do
    rootw <- rootWindow dpy $ defaultScreen dpy
    a <- internAtom dpy "XMONAD_EVALSRV_UPD" False
    allocaXEvent $ \e -> do
                  setEventType e clientMessage
                  setClientMessageEvent e rootw a 32 0 currentTime
                  sendEvent dpy rootw False structureNotifyMask e
                  sync dpy False
  return ()