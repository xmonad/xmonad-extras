{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP#-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Brightness
-- License     :  MIT
--
-- Stability   :  unstable
-- Portability :  unportable
--
-- Module to control the brightness of the screen in linux environments
--
-- [@Requirements@]
--     This module assumes that the following files exists:
--
--     * __\/sys\/class\/backlight\/intel_backlight\/max_brightness__
--
--     * __\/sys\/class\/backlight\/intel_backlight\/brightness__
--
--     Also, brightness should be updatable by changing the content of
--     __\/sys\/class\/backlight\/intel_backlight\/brightness__.
--
-- [@Permissions@]
--     To use this module, the owner of the __xmonad__ process will need to
--     have permission to write to __\/sys\/class\/backlight\/intel_backlight\/brightness__.
--     To achieve this, you can:
--
--     * Create a group with your user and root and give permissions to this
--     group to write to the file. I usually follow these steps:
--         
--         * Create a group named xmonad
--
--         > $ sudo groupadd xmonad
--
--         * Add user root and your user name to the group xmonad.
--
--         > $ sudo usermod -a -G xmonad root
--         > $ sudo usermod -a -G xmonad sibi
--
--         * The files under __\/sys__ are virtual. It's a RAM based filesystem through which you can access kernel data structures. The permission you give there won't persist after reboot. One of the way for persisting is creating a <https://unix.stackexchange.com/a/409780/29539 systemd script>:
--
--         > $ cat /etc/systemd/system/brightness.service
--         > [Unit]
--         > Description=Set brightness writable to everybody
--         > Before=nodered.service
--         > 
--         > [Service]
--         > Type=oneshot
--         > User=root
--         > ExecStart=/bin/bash -c "chgrp -R -H xmonad /sys/class/backlight/intel_backlight && chmod g+w /sys/class/backlight/intel_backlight/brightness"
--         > 
--         > [Install]
--         > WantedBy=multi-user.target
--         >
--         > $ sudo systemctl enable brightness.service
--         > $ sudo systemctl start brightness.service
--         > $ sudo systemctl status brightness.service
--
--
--     * Allow anyone to write the file through 646 permissions: __-rw-r--rw-__;
-- 
-----------------------------------------------------------------------------
module XMonad.Util.Brightness
    ( increase
    , decrease
    , change
    , setBrightness
    ) where

import XMonad
#if (MIN_VERSION_base(4,10,0))
import Data.Traversable (traverse)
#endif
import Prelude
import System.IO (hPutStrLn, stderr)
import Control.Monad (join)
import Data.Bifunctor (first)
import Control.Exception (try)
import Control.Applicative (liftA2)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString as BS

maxfile :: FilePath
maxfile = "/sys/class/backlight/intel_backlight/max_brightness"

currentfile :: FilePath
currentfile = "/sys/class/backlight/intel_backlight/brightness"

-- | Update brightness by +100
increase :: X ()
increase = liftIO $ change (+100) *> (pure ())

-- | Update brightness by -100
decrease :: X ()
decrease = liftIO $ change (+ (-100)) *> (pure ())

-- | Change brightness to a particular level
--
-- @since 0.13.4
setBrightness :: Int -> X ()
setBrightness level = liftIO $ change (\_ -> level) *> pure ()

-- | Perform all needed IO to update screen brightness
change :: (Int -> Int) -> IO (Either () ())
change f = do
  max <- getFromFile maxfile readInt
  current <- getFromFile currentfile readInt
  printError =<< apply (writeToFile currentfile) (liftA2 (guard f) max current)

apply :: (Int -> IO (Either String ())) -> Either String Int -> IO (Either String ())
apply f = fmap join . traverse f

guard :: (Int -> Int) -> Int -> Int -> Int
guard f max current
  | value > max = max
  | value < 0   = 0
  | otherwise = value
  where value = f current

readInt :: BS.ByteString -> Either String Int
readInt str = case (reads (unpack str)) of
                [(n, "\n")] -> Right n
                [(n, "")]   -> Right n
                _           -> Left "Could not parse string to int"

printError :: Either String e -> IO (Either () e)
printError es = either (\str -> hPutStrLn stderr str *> (return . Left $ ())) (\_ -> return . Left $ ()) es


getFromFile :: FilePath -> (BS.ByteString -> Either String a) -> IO (Either String a)
getFromFile filename fcast = fmap (fcast =<<) (try' $ BS.readFile filename)

writeToFile :: FilePath -> Int -> IO (Either String ())
writeToFile filename value = try' $ writeFile filename (show value)

try' :: forall a . IO a -> IO (Either String a)
try' x = fmap (first show) (try x :: IO (Either IOError a))
