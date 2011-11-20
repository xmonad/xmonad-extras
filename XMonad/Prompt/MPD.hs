{-# LANGUAGE Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.MPD
-- Copyright   :  Daniel Schoepe <daniel.schoepe@googlemail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Daniel Schoepe <daniel.schoepe@googlemail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module lets the user select songs and have MPD add/play them by
-- filtering them by user-supplied criteria(E.g. ask for an artist, then for
-- the album..)
--
-----------------------------------------------------------------------------

module XMonad.Prompt.MPD (-- * Usage
                          -- $usage
                          findMatching
                         ,addMatching
                         ,addAndPlay
                         ,RunMPD
                         ,findOrAdd
                         )  where
import Control.Monad
import Data.Char
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import Network.MPD
import XMonad
import XMonad.Prompt
import Data.List as L (nub,isPrefixOf,find)

-- $usage
--
-- To use this, import the following modules:
--
-- > import XMonad.Prompt.MPD
-- > import qualified Network.MPD as MPD
--
-- You can then use this in a keybinding, to filter first by artist, then by
-- album and add the matching songs:
--
-- > addMatching MPD.withMPD defaultXPConfig [MPD.Artist, MPD.Album] >> return ()
--
-- That way you will first be asked for an artist name, then for an album by
-- that artist etc..
--
-- If you need a password to connect to your MPD or need a different host/port,
-- you can pass a partially applied withMPDEx to the function:
--
-- > addMatching (MPD.withMPDEx "your.host" 4242 "very secret") ..
--

-- | Allows the user to supply a custom way to connect to MPD (e.g. partially
-- applied withMPDEx).
type RunMPD = forall a . MPD a -> IO (Response a)

-- | A new prompt type since Prompt.Input causes problems when completing
-- strings with spaces in them
data MPDPrompt = MPDPrompt String

instance XPrompt MPDPrompt where
    showXPrompt (MPDPrompt s) = s ++ ": "
    nextCompletion = const getNextCompletion
    commandToComplete = const id

-- | Extracts the given metadata attribute from a Song
extractMetadata :: Metadata -> Song -> String
extractMetadata meta = fromMaybe "Unknown" . join . fmap listToMaybe .
                       M.lookup meta . sgTags

-- | Creates a case-insensitive completion function from a list.
mkComplLst :: [String] -> String -> IO [String]
mkComplLst lst s = return . filter isPrefix' $ lst
    where isPrefix' s' = map toLower s `isPrefixOf` map toLower s'

-- | Helper function for 'findMatching'
findMatching' :: XPConfig -> [Song] -> Metadata -> X [Song]
findMatching' _ [] _ = return []
findMatching' xp songs meta = do
  answer <- mkXPromptWithReturn (MPDPrompt (show meta)) xp
           (mkComplLst . nub . map (extractMetadata meta) $ songs)
           return
  case answer of
    Just input -> return $ filter ((==input) . extractMetadata meta) songs
    Nothing -> return []

-- | Lets the user filter out non-matching songs. For example, if given
-- [Artist, Album] as third argument, this will prompt the user for an
-- artist(with tab-completion), then for an album by that artist and then
-- returns the songs from that album.
findMatching :: RunMPD -> XPConfig -> [Metadata] -> X [Song]
findMatching runMPD xp metas = do
  resp <- io . runMPD . listAllInfo $ ""
  case resp of
    Left err -> trace ("XMonad.Prompt.MPD: MPD returned an error: " ++ show err)
                >> return []
    Right songs -> foldM (findMatching' xp) (rights songs) metas

-- | Determine playlist position of the song and add it, if it isn't present.
findOrAdd :: Song -> MPD Int
findOrAdd s = playlistInfo Nothing >>= \pl ->
  case L.find ((== sgFilePath s) . sgFilePath) pl of
    Just (Song { sgIndex = Just i }) -> return i
    _ -> flip addId Nothing . sgFilePath $ s

-- | Add all selected songs to the playlist if they are not in it.
addMatching :: RunMPD -> XPConfig -> [Metadata] -> X [Int]
addMatching runMPD xp metas = do
  matches <- findMatching runMPD xp metas
  fmap (either (const []) id) . io . runMPD . mapM findOrAdd $ matches

-- | Add matching songs and play the first one.
addAndPlay :: RunMPD -> XPConfig -> [Metadata] -> X ()
addAndPlay runMPD xp ms = do
  ids <- addMatching runMPD xp ms
  whenJust (listToMaybe ids) ((>> return ()) . io . runMPD . playId)