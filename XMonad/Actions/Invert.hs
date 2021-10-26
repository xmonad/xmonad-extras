{-# LANGUAGE OverloadedStrings #-}
-- | Invert individual window contents via compton/picom compositing manager.
--
-- @since 0.17.1
module XMonad.Actions.Invert 
  ( -- * Usage:
    -- $usage
    inversionStatus
  , invert
  ) where

import DBus
import DBus.Client
import Data.Maybe
import Data.Word
import Graphics.X11.Xlib.Display
import XMonad

-- $usage
-- To use, first import this module into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.Invert
--
-- Then add an appropriate mouse binding:
--
-- >     , ((modm, xK_i), withDisplay $ \dpy -> withFocused $ \w -> inversionStatus dpy w >>= \status -> invert dpy w $ not status)
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".

dpyName :: Display -> String
dpyName dpy = map replace $ displayString dpy where
  replace ':' = '_'
  replace '.' = '_'
  replace c = c

-- | Ask compton/picom the inverted status of the specified window
inversionStatus :: Display -> Window -> X Bool
inversionStatus dpy w =
  let mc = (methodCall "/" "com.github.chjj.compton" "win_get")
             { methodCallDestination = Just $ busName_ $ "com.github.chjj.compton." ++ dpyName dpy
             , methodCallBody = [toVariant (fromIntegral w :: Word32)
                                , toVariant ("invert_color_force" :: String)
                                ]
             }
  in io $ do client <- connectSession
             status <- call_ client mc
             disconnect client
             return $ (/= 0) $ fromJust $ (fromVariant :: Variant -> Maybe Word32) $ head $ methodReturnBody status

-- | Tell compton/picom to set the inverted status of the specified window
invert :: Display -> Window -> Bool -> X ()
invert dpy w status =
  let mc = (methodCall "/" "com.github.chjj.compton" "win_set")
             { methodCallDestination = Just $ busName_ $ "com.github.chjj.compton." ++ dpyName dpy
             , methodCallBody = [toVariant (fromIntegral w :: Word32)
                                , toVariant ("invert_color_force" :: String)
                                , toVariant ((if status then 1 else 0) :: Word32)
                                ]
             }
  in io $ do client <- connectSession
             callNoReply client mc
             disconnect client
