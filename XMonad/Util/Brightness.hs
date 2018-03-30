{-# LANGUAGE ScopedTypeVariables #-}

-- | Module to control the brightness of the screen in linux environments
module XMonad.Util.Brightness
    ( increase
    , decrease
    , change
    ) where

import XMonad
import System.IO (hPutStrLn, stderr)
import Data.Either.Combinators (mapLeft, swapEither)
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

apply :: (Traversable m, Monad m, Monad f) => (a1 -> f (m a2)) -> m a1 -> f (m a2)
apply f = fmap join . sequence . fmap f

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
printError = fmap swapEither . sequence . swapEither . mapLeft (hPutStrLn stderr)

getFromFile :: FilePath -> (BS.ByteString -> Either String a) -> IO (Either String a)
getFromFile filename fcast = fmap (fcast =<<) (try' $ BS.readFile filename)

writeToFile :: FilePath -> Int -> IO (Either String ())
writeToFile filename value = try' $ writeFile filename (show value)

try' :: forall a . IO a -> IO (Either String a)
try' x = fmap (first show) (try x :: IO (Either IOError a))
