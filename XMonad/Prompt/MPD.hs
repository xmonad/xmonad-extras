{-# LANGUAGE PatternGuards, Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.MPD
-- Copyright   :  Daniel Schoepe <daniel.schoepe@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Daniel Schoepe <daniel.schoepe@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module lets you select songs and add/play songs with MPD by filtering
-- them by user-supplied criteria(E.g. ask for an artist, then for the album..)
--
-----------------------------------------------------------------------------

-- TODO: Add other backends besides mpd(e.g. amarok via dcop).

module XMonad.Prompt.MPD (-- * Usage
                          -- $usage
                          findMatching
                         ,addMatching
                         ,addAndPlay
                         ,RunMPD
                         ,findOrAdd
                         )  where
import Control.Monad
import Control.Applicative
import Data.Char
import Network.MPD
import System.IO
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Input
import Data.List (nub,isPrefixOf,findIndex)


-- $usage
--
-- To use this, import the following modules:
--
-- > import XMonad.Prompt.MPD
-- > import qualified Network.MPD as MPD
--
-- You can then use this in a keybinding, to filter first by artist, then by album and add 
-- the matching songs:
-- 
-- > addMatching MPD.withMPD defaultXPConfig [MPD.sgArtist, MPD.sgAlbum] >> return ()
--
-- If you need a password to connect to your MPD or need a different host/port, you can pass a
-- partially applied withMPDEx to the function:
--
-- > addMatching (MPD.withMPDEx "your.host" 666 (return $ Just $ "password")) ..
--

-- | Allows the user to supply a custom way to connect to MPD (e.g. partially applied withMPDEx).
type RunMPD = forall a . MPD a -> IO (Response a)

-- | Creates a case-insensitive completion function from a list.
mkComplLst :: [String] -> String -> IO [String]
mkComplLst lst s = return . filter (\s' -> map toLower s `isPrefixOf` map toLower s') $ lst

findMatching' :: XPConfig -> [Song] -> (Song -> String) -> X [Song]
findMatching' xp s m = do
  Just input <- inputPromptWithCompl xp "MPD" (mkComplLst . nub . map m $ s)
  return $ filter ((==input) . m) s

-- | Lets the user filter out non-matching with a prompt by supplied criteria.
findMatching :: RunMPD -> XPConfig -> [Song -> String] -> X [Song]
findMatching runMPD xp ms = do
  -- rs <- io (runMPD $ rights `fmap` listAllInfo "")
  -- workaround due to a bug in libmpd(reported and fixed in darcs).
  -- should be replaced by the line above when a new version is released.
  -- (last version with this bug: 0.3.1)
  rs <- io . runMPD . fmap concat $ mapM findArtist =<< listArtists
  case rs of
    Left e -> io (hPutStrLn stderr $ "MPD error: " ++ show e)  >> return []
    Right s -> foldM (findMatching' xp) s ms

-- | Determine playlist position of the song and add it, if it isn't present.
findOrAdd :: Song -> MPD PLIndex
findOrAdd s = playlistInfo Nothing >>= \pl -> do
  case findIndex ((== sgFilePath s) . sgFilePath) pl of
    Just i -> return . Pos . fromIntegral $ i
    Nothing -> ID <$> (addId . sgFilePath $ s)

-- | Add all selected songs to the playlist if they are not on it.
addMatching :: RunMPD -> XPConfig -> [Song -> String] -> X [PLIndex]
addMatching r xp ms = findMatching r xp ms >>= fmap (either (const []) id) . io . r . mapM findOrAdd

-- | Add matching songs and play the first one.
addAndPlay :: RunMPD -> XPConfig -> [Song -> String] -> X ()
addAndPlay r xp ms = addMatching r xp ms >>= io . r . play . Just . head >> return ()
