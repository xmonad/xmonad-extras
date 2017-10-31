{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
                          findMatching,
                          findMatchingWith,
                          addMatching,
                          addMatchingWith,
                          addAndPlay,
                          addAndPlayWith,
                          loadPlaylist,
                          loadPlaylistWith,
                          addAndPlayAny,
                          pickPlayListItem,
                          RunMPD,
                          findOrAdd
                         )  where
import Control.Monad
import Data.Char
import Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String
import Network.MPD
import XMonad hiding ((=?))
import XMonad.Prompt
import Data.List as L (find, isPrefixOf, nub)
import qualified Data.ByteString.Char8 as C

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
                       M.lookup meta . M.map (map toString) . sgTags

-- | Creates a case-insensitive completion function from a list.
mkComplLst :: (String -> String -> Bool) -> [String] -> String -> IO [String]
mkComplLst cmp lst s = return . filter matches $ lst
    where matches s' = map toLower s `cmp` map toLower s'

-- | Helper function for 'findMatching'
findMatching' :: (String -> String -> Bool) -> XPConfig -> [Song] -> Metadata
              -> X [Song]
findMatching' _ _ [] _ = return []
findMatching' cmp xp songs meta = do
  answer <- mkXPromptWithReturn (MPDPrompt (show meta)) xp
           (mkComplLst cmp . nub . map (extractMetadata meta) $ songs)
           return
  case answer of
    Just input -> return $ filter ((==input) . extractMetadata meta) songs
    Nothing -> return []

extractSongs :: [LsResult] -> [Song]
extractSongs = mapMaybe extractSong
    where extractSong (LsSong s) = Just s
          extractSong _ = Nothing

-- | Lets the user filter out non-matching songs. For example, if given
-- [Artist, Album] as third argument, this will prompt the user for an
-- artist(with tab-completion), then for an album by that artist and then
-- returns the songs from that album.
--
-- @since 0.13.2
findMatchingWith :: (String -> String -> Bool) -> RunMPD -> XPConfig
                 -> [Metadata] -> X [Song]
findMatchingWith matchFun runMPD xp metas = do
  resp <- io . runMPD . fmap extractSongs . listAllInfo $ ("" :: Path)
  case resp of
    Left err -> trace ("XMonad.Prompt.MPD: MPD returned an error: " ++ show err)
                >> return []
    Right songs -> foldM (findMatching' matchFun xp) songs metas

-- | Lets the user filter out non-matching songs. For example, if given
-- [Artist, Album] as third argument, this will prompt the user for an
-- artist(with tab-completion), then for an album by that artist and then
-- returns the songs from that album.
findMatching :: RunMPD -> XPConfig -> [Metadata] -> X [Song]
findMatching = findMatchingWith isPrefixOf

-- | Determine playlist position of the song and add it, if it isn't present.
findOrAdd :: Song -> MPD Int
findOrAdd s = playlistInfo Nothing >>= \pl ->
  case L.find ((== sgFilePath s) . sgFilePath) pl of
    Just (Song { sgIndex = Just i }) -> return i
    _ -> fmap unwrapId . flip addId Nothing . sgFilePath $ s
  where unwrapId (Id i) = i

-- | Add all selected songs to the playlist if they are not in it.
--
-- @since 0.13.2
addMatchingWith :: (String -> String -> Bool) -> RunMPD -> XPConfig
                -> [Metadata] -> X [Int]
addMatchingWith matchFun runMPD xp metas = do
  matches <- findMatchingWith matchFun runMPD xp metas
  fmap (either (const []) id) . io . runMPD . mapM findOrAdd $ matches

-- | Add all selected songs to the playlist if they are not in it.
addMatching :: RunMPD -> XPConfig -> [Metadata] -> X [Int]
addMatching = addMatchingWith isPrefixOf

-- | Add matching songs and play the first one.
--
-- @since 0.13.2
addAndPlayWith :: (String -> String -> Bool) -> RunMPD -> XPConfig
               -> [Metadata] -> X ()
addAndPlayWith matchFun runMPD xp ms = do
  ids <- addMatchingWith matchFun runMPD xp ms
  whenJust (listToMaybe ids) ((>> return ()) . io . runMPD . playId . Id)

-- | Add matching songs and play the first one.
addAndPlay :: RunMPD ->  XPConfig -> [Metadata] -> X ()
addAndPlay = addAndPlayWith isPrefixOf

-- | Load an existing playlist and play it.
--
-- @since 0.13.2
loadPlaylistWith :: (String -> String -> Bool) -> RunMPD ->  XPConfig -> X ()
loadPlaylistWith matchFun runMPD xp = do
  playlists <- fmap (either (const []) id) . io . runMPD $ listPlaylists
  mkXPrompt (MPDPrompt "Playlist: ") xp
    (mkComplLst matchFun . nub . map toString $ playlists)
    (\s -> do io $ runMPD $ do clear
                               load $ PlaylistName $ C.pack s
                               play Nothing
              return ())
  
-- | Load an existing playlist and play it.
loadPlaylist :: RunMPD ->  XPConfig -> X ()
loadPlaylist = loadPlaylistWith isPrefixOf

-- | Add songs which match all of the given words with regard to any
-- of the metadata.
--
-- @since 0.13.2
addAndPlayAny :: RunMPD -> XPConfig -> [Metadata] -> X ()
addAndPlayAny runMPD xp metas = do
  mkXPrompt (MPDPrompt "Search") xp
    (historyCompletionP (showXPrompt (MPDPrompt "Search: ") ==))
    (\s -> do io $ runMPD $ do
                clear
                songlists <- mapM (\t -> do
                                      sl <- mapM (\m -> search
                                                        (m =? fromString t))
                                            metas
                                      return $ concat sl) $ words s
                let songs = foldl L.intersect (head songlists) songlists
                fmap (either (const []) id) . io . runMPD . mapM findOrAdd $
                  songs
                play Nothing
              return ())


-- | Pick a song from the current playlist.
--
-- @since 0.13.2
pickPlayListItem :: RunMPD -> XPConfig -> X ()
pickPlayListItem runMPD xp = do
  mkXPrompt (MPDPrompt "Pick") xp
    (\s -> do pSongs <- io $ runMPD $ playlistSearch (Title =? fromString s)
              case pSongs of
                Left _ -> return []
                Right songs -> return $ take 100 $ nub $ map toString
                               $ concat $ catMaybes
                               $ map (M.lookup Title . sgTags) songs)
    (\s -> do io $ runMPD $ do
                pSongs <- io $ runMPD $ playlistSearch (Title =? fromString s)
                case pSongs of
                  Left _      -> return ()
                  Right songs -> case sgId $ head songs of
                                   Nothing    -> return ()
                                   Just theId -> playId theId
              return ())
