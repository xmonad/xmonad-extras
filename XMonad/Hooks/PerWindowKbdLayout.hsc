{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface, MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- GHC 6.10.4 complains about Foreign.C.Types, see Ticket #3419

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.PerWindowKbdLayout
-- Copyright   :  (c) Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Konstantin Sobolev <konstantin.sobolev@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A hook that remembers per-window keyboard layouts and switches them
-- on focus changes.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.PerWindowKbdLayout (
                                -- * Usage
                                -- $usage
                                perWindowKbdLayout) where

import Foreign
import Foreign.C.Types (CUChar,CUShort,CUInt(..),CInt(..))

import Control.Monad (when)
import Data.List (find)
import qualified Data.Map as M
import Data.Monoid (All(..))
import Data.Traversable (traverse)

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

#include <X11/XKBlib.h>

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.PerWindowKbdLayout
--
-- Then edit your @eventHook@ by adding 'perWindowKbdLayout', for example
--
-- > main = xmonad defaultConfig { handleEventHook = perWindowKbdLayout }

data XkbStateRec = XkbStateRec {
    group :: CUChar,
    locked_group :: CUChar,
    base_group :: CUShort,
    latched_group :: CUShort,
    mods :: CUChar,
    base_mods :: CUChar,
    latched_mods :: CUChar,
    locked_mods :: CUChar,
    compat_state :: CUChar,
    grab_mods :: CUChar,
    compat_grab_mods :: CUChar,
    lookup_mods :: CUChar,
    compat_lookup_mods :: CUChar,
    ptr_buttons :: CUShort
}

instance Storable XkbStateRec where
    sizeOf _ = (#size XkbStateRec)
    alignment _ = alignment (undefined :: CUShort)
    peek ptr = do
        r_group <- (#peek XkbStateRec, group) ptr
        r_locked_group <- (#peek XkbStateRec, locked_group) ptr
        r_base_group <- (#peek XkbStateRec, base_group) ptr
        r_latched_group <- (#peek XkbStateRec, latched_group) ptr
        r_mods <- (#peek XkbStateRec, mods) ptr
        r_base_mods <- (#peek XkbStateRec, base_mods) ptr
        r_latched_mods <- (#peek XkbStateRec, latched_mods) ptr
        r_locked_mods <- (#peek XkbStateRec, locked_mods) ptr
        r_compat_state <- (#peek XkbStateRec, compat_state) ptr
        r_grab_mods <- (#peek XkbStateRec, grab_mods) ptr
        r_compat_grab_mods <- (#peek XkbStateRec, compat_grab_mods) ptr
        r_lookup_mods <- (#peek XkbStateRec, lookup_mods) ptr
        r_compat_lookup_mods <- (#peek XkbStateRec, compat_lookup_mods) ptr
        r_ptr_buttons <- (#peek XkbStateRec, ptr_buttons) ptr
        return XkbStateRec {
            group = r_group,
            locked_group = r_locked_group,
            base_group = r_base_group,
            latched_group = r_latched_group,
            mods = r_mods,
            base_mods = r_base_mods,
            latched_mods = r_latched_mods,
            locked_mods = r_locked_mods,
            compat_state = r_compat_state,
            grab_mods = r_grab_mods,
            compat_grab_mods = r_compat_grab_mods,
            lookup_mods = r_lookup_mods,
            compat_lookup_mods = r_compat_lookup_mods,
            ptr_buttons = r_ptr_buttons
        }

foreign import ccall unsafe "X11/XKBlib.h XkbGetState"
    xkbGetState :: Display -> CUInt -> Ptr XkbStateRec -> IO CInt

foreign import ccall unsafe "XkbLockGroup" xkbLockGroup :: Display -> CUInt -> CUInt -> IO ()

type KbdLayout = Int

getKbdLayout :: Display -> IO KbdLayout
getKbdLayout d = alloca $ \stRecPtr -> do
    xkbGetState d (#const XkbUseCoreKbd) stRecPtr
    st <- peek stRecPtr
    return $ fromIntegral (group st)

setKbdLayout :: Display -> KbdLayout -> IO ()
setKbdLayout d l = xkbLockGroup d (#const XkbUseCoreKbd) $ fromIntegral l

data LayoutStorage = LayoutStorage (Maybe Window) (M.Map Window KbdLayout) deriving (Typeable,Read,Show)
instance ExtensionClass LayoutStorage where initialValue = LayoutStorage Nothing M.empty

perWindowKbdLayout :: Event -> X All
perWindowKbdLayout (DestroyWindowEvent {ev_window = w, ev_event_type = et}) = do
    when (et == destroyNotify) $
        XS.modify $ \(LayoutStorage mpf wtl) -> (LayoutStorage mpf (M.delete w wtl))
    return (All True)
perWindowKbdLayout _ = do
    mst <- gets (W.stack . W.workspace . W.current . windowset)
    traverse update $ W.focus `fmap` mst
    return (All True)

update :: Window -> X()
update foc = withDisplay $ \dpy -> do
    (LayoutStorage mpf wtl) <- XS.get
    curLayout <- io $ getKbdLayout dpy
    case mpf of
        Nothing ->
            XS.put (LayoutStorage (Just foc) (M.insert foc curLayout wtl))
        Just pf -> when (pf /= foc) $ do
            XS.put (LayoutStorage (Just foc) (M.insert pf curLayout wtl))
            io $ whenJust (M.lookup foc wtl) (setKbdLayout dpy)

-- vim:ft=haskell:ts=4:shiftwidth=4:softtabstop=4:expandtab:
