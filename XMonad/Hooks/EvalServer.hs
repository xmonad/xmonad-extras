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

                                initCommands
                               ,startServer
                               ,defaultServer
                               ,CommandVar
                               ,defaultServerConfig
                               ,evalEventHook
                               ) where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar
import Control.Exception

import Data.Monoid

import System.IO

import XMonad.Actions.Eval
import XMonad

import Network

type CommandVar = MVar [(String,Handle)]

-- $usage
--
-- WARNING: This module will have issues if xmonad wasn't compiled with -threaded
-- (requires a modified xmonad-version): Expressions will only get evaluated when xmonad
-- receives an event, for example the focus changes.
--
-- This module lets you create a server that evaluates Haskell expression in
-- the context of the currently running xmonad, which lets you control xmonad from
-- another process(e.g. a script)
-- To use this module add something like this to your xmonad.hs:
--
-- > import XMonad.Hooks.EvalServer
-- > import Network
--
-- > main = do
-- >   cmdVar <- initCommands
-- >   socket <- defaultServer socket 4242
-- >   ..
-- >   xmonad $ .. $ defaultConfig {
-- >                   handleEventHook = evalEventHook defaultServerConfig cmdVar
-- >   ..
-- >   }
-- >   sClose socket -- if you forget this, the module will cease to work when xmonad
-- >                 -- is restarted.

-- You can then send Haskell expression that are to be evaluated over the socket.
-- Example using telnet:
-- > telnet localhost 4242
-- > windows $ W.view "1"

-- $documentation

-- | Creates an empty MVar to store received commands. A variable of this
-- type has to be passed to the other functions of this module.
initCommands :: MonadIO m => m CommandVar
initCommands = liftIO newEmptyMVar

-- | Creates a server listening on a TCP socket with the given port number.
defaultServer :: CommandVar -> PortNumber -> IO Socket
defaultServer cv = startServer cv . PortNumber

-- | Creates a server listening on the specified port(can also be a unix domain socket).
startServer :: CommandVar -> PortID -> IO Socket
startServer var port = do
  s <- listenOn port
  forkIO $
         bracket (return s) sClose
                 (\sock -> forever $ accept sock >>= forkIO . clientThread var)
  return s

-- | Default config to evaluate the received expressions
defaultServerConfig :: EvalConfig
defaultServerConfig = defaultEvalConfig { handleError = return . show }

-- | This event hook causes commands to be executed when they are received.
evalEventHook :: EvalConfig -> CommandVar -> Event -> X All
evalEventHook evConfig tCmds (ClientMessageEvent { ev_message_type = mt }) = do
  dpy <- asks display
  at <- io $ internAtom dpy "XMONAD_EVALSRV_UPD" False
  if (mt == at)
     then do
       cmds <- io . takeMVar $ tCmds
       forM_ cmds $ \(cmd,h) ->
          evalExpressionWithReturn evConfig cmd >>= io . hPutStrLn h
       return $ All False
     else return $ All True
evalEventHook _ _ _ = return $ All True

-- | Handler for an individual client.
clientThread :: CommandVar -> (Handle,HostName,PortNumber) -> IO ()
clientThread var (h,_,_) = do
  hSetBuffering h LineBuffering
  forever $ hGetLine h >>= handleCommand h var

-- | Handles a received command. TODO: Add a more elaborate protocol(e.g. one that allows shutting
-- down the server).
handleCommand :: Handle -> MVar [(String,Handle)] -> String -> IO ()
handleCommand h var cmd = openDisplay "" >>= \dpy -> do
  rootw <- rootWindow dpy $ defaultScreen dpy
  a <- internAtom dpy "XMONAD_EVALSRV_UPD" False
  empt <- isEmptyMVar var
  if empt
     then putMVar var [(cmd,h)]
     else modifyMVar_ var (return . ((cmd,h):))
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rootw a 32 0 currentTime
    sendEvent dpy rootw False structureNotifyMask e
    sync dpy False
