-- Utility for sending a command to xmonad and have
-- it immediately executed even when xmonad isn't built
-- with -threaded.
module Main () where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.Monoid
import Data.Word
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Network
import System.Console.GetOpt
import System.Environment
import System.IO

data Options = Options { optPort :: PortID
                       , optHost :: HostName
                       , optWait :: Bool
                       , optHelp :: Bool
                       }

defaultOptions :: Options
defaultOptions = Options { optPort = PortNumber 4242
                         , optHost = "localhost"
                         , optWait = False
                         , optHelp = False
                         }

readPort :: String -> Options -> Options
readPort str opts = opts { optPort = portNum }
    where portNum =  PortNumber . fromIntegral $ (read str :: Word16)

options :: [OptDescr (Endo Options)]
options = [ Option ['p'] ["port"]
                   (ReqArg (Endo . readPort) "<port>")
                   ("Port on which to connect. <port> is expected to be an integer"
                    ++ " between 0 and 65535. (Defaults to 4242)")
          , Option ['h'] ["host"]
                   (ReqArg (\s -> Endo $ \opts -> opts { optHost = s }) "<hostname>")
                   "Which host to connect to. (Defaults to \"localhost\")"
          , Option ['w'] ["wait"]
                   (NoArg . Endo $ \opts -> opts { optWait = True })
                   "Wait until the command is executed and print the result. (Default: False)"
          , Option []    ["help"]
                   (NoArg . Endo $ \opts -> opts { optHelp = True })
                   "Show usage information."
          ]

getOptions :: [String] -> IO (Options,String)
getOptions args =
    case getOpt Permute options args of
      (o,rest,[]) -> return (mconcat o `appEndo` defaultOptions, intercalate " " rest)
      (_,_,errs) -> ioError . userError $ concat errs ++ usageInfo header options

header :: String
header = "USAGE: xmonadcmd [OPTIONS] <string to send>"

sendCommand :: Options -> String -> IO ()
sendCommand opts cmd = openDisplay "" >>= \dpy -> do
  putStrLn cmd
  h <- connectTo (optHost opts) (optPort opts)
  hSetBuffering h LineBuffering
  hPutStrLn h cmd
  rootw <- rootWindow dpy $ defaultScreen dpy
  atom <- internAtom dpy "TEST" True
  forkIO $ allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rootw atom 32 0 currentTime
    sendEvent dpy rootw False structureNotifyMask e
    sync dpy False
  when (optWait opts) $ putStrLn =<< hGetLine h
  hClose h

main :: IO ()
main = do
  (opts,cmd) <- getOptions =<< getArgs
  if optHelp opts
     then putStrLn $ usageInfo header options
     else sendCommand opts cmd