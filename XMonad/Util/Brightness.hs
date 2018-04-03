{-# LANGUAGE ScopedTypeVariables #-}
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
--     group to write to the file;
--
--     * Allow anyone to write the file through 646 permissions: __-rw-r--rw-__;
-- 
-----------------------------------------------------------------------------
module XMonad.Util.Brightness
    ( increase
    , decrease
    , change
    ) where

import XMonad
import Data.Traversable (traverse)
import System.IO (hPutStrLn, stderr)
import Data.Bitraversable (bitraverse)
import Control.Monad (join)
import Data.Bifunctor (first)
import Control.Exception (try)
import Control.Applicative (liftA2)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString as BS

maxfile = "/sys/class/backlight/intel_backlight/max_brightness"
currentfile = "/sys/class/backlight/intel_backlight/brightness"

-- | Update brightness by +100
increase :: X ()
increase = liftIO $ change (+100) *> (pure ())

-- | Update brightness by -100
decrease :: X ()
decrease = liftIO $ change (+ (-100)) *> (pure ())

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
printError = bitraverse (hPutStrLn stderr) (pure . id)

getFromFile :: FilePath -> (BS.ByteString -> Either String a) -> IO (Either String a)
getFromFile filename fcast = fmap (fcast =<<) (try' $ BS.readFile filename)

writeToFile :: FilePath -> Int -> IO (Either String ())
writeToFile filename value = try' $ writeFile filename (show value)

try' :: forall a . IO a -> IO (Either String a)
try' x = fmap (first show) (try x :: IO (Either IOError a))
