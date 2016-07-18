{-# LANGUAGE CPP #-}
-- boilerplate {{{
----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.Volume
-- Copyright    : (c) daniel@wagner-home.com
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : daniel@wagner-home.com
-- Stability    : unstable
-- Portability  : unportable
--
-- A minimal interface to the \"amixer\" command-line utility.
--
----------------------------------------------------------------------------
module XMonad.Actions.Volume (
    -- * Usage
    -- $usage

    -- * Common functions
    toggleMute,
    raiseVolume,
    lowerVolume,

    -- * Low-level interface
    getVolume,
    getMute,
    getVolumeMute,
    setVolume,
    setMute,
    setVolumeMute,
    modifyVolume,
    modifyMute,
    modifyVolumeMute,

    -- * Variants that take a list of channels
    defaultChannels,

    toggleMuteChannels,
    raiseVolumeChannels,
    lowerVolumeChannels,
    getVolumeChannels,
    getMuteChannels,
    getVolumeMuteChannels,
    setVolumeChannels,
    setMuteChannels,
    setVolumeMuteChannels,
    modifyVolumeChannels,
    modifyMuteChannels,
    modifyVolumeMuteChannels,

    defaultOSDOpts,
    osdCat
) where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import XMonad.Core
import Sound.ALSA.Mixer

{- $usage
You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.Volume

then add appropriate keybinds to adjust the volume; for example:

> , ((modMask x, xK_F8 ), lowerVolume 3 >> return ())
> , ((modMask x, xK_F9 ), raiseVolume 3 >> return ())
> , ((modMask x, xK_F10), toggleMute    >> return ())

For detailed instructions on editing your key bindings, see
"XMonad.Doc.Extending#Editing_key_bindings".
-}
-- }}}
-- API {{{
-- | Toggle mutedness on the default channels.  Returns 'True' when this attempts to mute the speakers and 'False' when this attempts to unmute the speakers.
toggleMute          :: MonadIO m => m Bool
-- | Raise the volume on the default channels the given number of percentage points.  Returns the volume it attempts to set.
raiseVolume         :: MonadIO m => Double -> m Double
-- | Lower the volume on the default channels the given number of percentage points.  Returns the volume it attempts to set.
lowerVolume         :: MonadIO m => Double -> m Double
-- | Get the geometric mean of the volumes on the default channels.
getVolume           :: MonadIO m => m Double
-- | Get the mutedness of the default channels.  Returns 'True' if any of the channels are muted, and 'False' otherwise.
getMute             :: MonadIO m => m Bool
-- | Get both the volume and the mutedness of the default channels.
getVolumeMute       :: MonadIO m => m (Double, Bool)
-- | Attempt to set the default channels to a volume given in percentage of maximum.
setVolume           :: MonadIO m => Double         -> m ()
-- | Attempt to set the muting on the default channels.
setMute             :: MonadIO m => Bool           -> m ()
-- | Attempt to set both the volume in percent and the muting on the default channels.
setVolumeMute       :: MonadIO m => Double -> Bool -> m ()
-- | Apply a function to the volume of the default channels, and return the modified value.
modifyVolume        :: MonadIO m => (Double         -> Double        ) -> m Double
-- | Apply a function to the muting on the default channels, and return the modified value.
modifyMute          :: MonadIO m => (Bool           -> Bool          ) -> m Bool
-- | Apply a function to both the volume and the muting of the default channels, and return the modified values.
modifyVolumeMute    :: MonadIO m => (Double -> Bool -> (Double, Bool)) -> m (Double, Bool)

toggleMute          = toggleMuteChannels       defaultChannels
raiseVolume         = raiseVolumeChannels      defaultChannels
lowerVolume         = lowerVolumeChannels      defaultChannels
getVolume           = getVolumeChannels        defaultChannels
getMute             = getMuteChannels          defaultChannels
getVolumeMute       = getVolumeMuteChannels    defaultChannels
setVolume           = setVolumeChannels        defaultChannels
setMute             = setMuteChannels          defaultChannels
setVolumeMute       = setVolumeMuteChannels    defaultChannels
modifyVolume        = modifyVolumeChannels     defaultChannels
modifyMute          = modifyMuteChannels       defaultChannels
modifyVolumeMute    = modifyVolumeMuteChannels defaultChannels

-- | Channels are what amixer calls \"simple controls\".  The most common ones are \"Master\", \"Wave\", and \"PCM\", so these are included in 'defaultChannels'.  It is guaranteed to be safe to pass channel names that don't exist on the default sound device to the *Channels family of functions.
defaultChannels :: [String]
defaultChannels = ["Master", "Wave", "PCM"]

toggleMuteChannels          :: MonadIO m => [String] -> m Bool
raiseVolumeChannels         :: MonadIO m => [String] -> Double -> m Double
lowerVolumeChannels         :: MonadIO m => [String] -> Double -> m Double
getVolumeChannels           :: MonadIO m => [String] -> m Double
getMuteChannels             :: MonadIO m => [String] -> m Bool
getVolumeMuteChannels       :: MonadIO m => [String] -> m (Double, Bool)
setVolumeChannels           :: MonadIO m => [String] -> Double         -> m ()
setMuteChannels             :: MonadIO m => [String] -> Bool           -> m ()
setVolumeMuteChannels       :: MonadIO m => [String] -> Double -> Bool -> m ()
modifyVolumeChannels        :: MonadIO m => [String] -> (Double         -> Double        ) -> m Double
modifyMuteChannels          :: MonadIO m => [String] -> (Bool           -> Bool          ) -> m Bool
modifyVolumeMuteChannels    :: MonadIO m => [String] -> (Double -> Bool -> (Double, Bool)) -> m (Double, Bool)

toggleMuteChannels  cs = modifyMuteChannels   cs not
raiseVolumeChannels cs = modifyVolumeChannels cs . (+)
lowerVolumeChannels cs = modifyVolumeChannels cs . (subtract)

getVolumeChannels     = liftIO . fmap fst . alsaGetAll
getMuteChannels       = liftIO . fmap snd . alsaGetAll
getVolumeMuteChannels = liftIO            . alsaGetAll

setVolumeChannels     cs v   = liftIO (alsaSetVolumeAll v   cs)
setMuteChannels       cs   m = liftIO (alsaSetMuteAll     m cs)
setVolumeMuteChannels cs v m = liftIO (alsaSetAll       v m cs)

modifyVolumeChannels = modify getVolumeChannels setVolumeChannels
modifyMuteChannels   = modify getMuteChannels   setMuteChannels
modifyVolumeMuteChannels cs = modify getVolumeMuteChannels (\cs' -> uncurry (setVolumeMuteChannels cs')) cs . uncurry
-- }}}
-- internals {{{
geomMean :: Floating a => [a] -> a
geomMean xs = product xs ** (recip . fromIntegral . length $ xs)

clip :: (Num t, Ord t) => t -> t
clip = min 100 . max 0

toRange :: (Integer, Integer) -> Double -> Integer
toRange (x, y) d = floor (d * (y' - x') / 100 + x')
  where x' = fromIntegral x
        y' = fromIntegral y
        
fromRange :: (Integer, Integer) -> Integer -> Double
fromRange (x, y) z = fromIntegral (z - x) / fromIntegral (y - x) * 100

modify :: Monad m => (arg -> m value) -> (arg -> value -> m ()) -> arg -> (value -> value) -> m value
modify get set cs f = do
    v <- liftM f $ get cs
    set cs v
    return v

withControl :: (Control -> IO a) -> [String] -> IO a
withControl f cs = withMixer "default" $ \mixer -> do 
  (control:_) <- catMaybes <$> mapM (getControlByName mixer) cs
  f control

alsaGetAll :: [String] -> IO (Double, Bool)
alsaGetAll = withControl $ \control -> (,) <$> alsaGetVolume control 
                                           <*> alsaGetMute control

alsaGetVolume :: Control -> IO Double
alsaGetVolume control = do
  let Just playbackVolume = playback $ volume control
      volChans = value playbackVolume
  range <- getRange playbackVolume
  vals <- mapM (\chan -> getChannel chan volChans) (channels volChans)
  return $ geomMean $ map (fromRange range . fromJust) vals

alsaGetMute :: Control -> IO Bool
alsaGetMute control = do
  let Just muteChans = playback $ switch control
  all id . map fromJust <$> mapM (\chan -> getChannel chan muteChans) (channels muteChans)

alsaSetVolumeAll :: Double -> [String] -> IO ()
alsaSetVolumeAll v = withControl (alsaSetVolume v)

alsaSetVolume :: Double -> Control -> IO () 
alsaSetVolume v control = do
  let Just playbackVolume = playback $ volume control
      volChans = value playbackVolume
  range <- getRange playbackVolume
  forM_ (channels volChans) $ \chan -> do 
    setChannel chan volChans (toRange range (clip v))

alsaSetMuteAll :: Bool -> [String] -> IO ()
alsaSetMuteAll m = withControl (alsaSetMute m)

alsaSetMute :: Bool -> Control -> IO ()
alsaSetMute m control = do
  let Just muteChans = playback $ switch control
  forM_ (channels muteChans) $ \chan -> setChannel chan muteChans m

alsaSetAll :: Double -> Bool -> [String] -> IO ()
alsaSetAll v m = withControl $ \control -> do
  alsaSetVolume v control
  alsaSetMute m control

-- | Helper function to output current volume via osd_cat.  (Needs the osd_cat executable).
-- The second parameter is passed True when the speakers are muted and should
-- return the options to pass to osd_cat.
osdCat :: MonadIO m => Double -> (Bool -> String) -> m ()
osdCat vol opts = do
  m <- getMute
  spawn $ "osd_cat -b percentage -P " ++ show (truncate vol :: Integer) ++ opts m

-- | Default options for displaying the volume.
defaultOSDOpts :: Bool -> String
defaultOSDOpts mute = "--align=center --pos=top --delay=1 --text=\"Volume" ++
                      (if mute then "[muted]\" " else "\" ") ++
                      "--font='-bitstream-bitstream vera sans-bold-r-*-*-10-*-*-*-*-*-*-*' " ++
                      "--outline=1"

-- }}}
