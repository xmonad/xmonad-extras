{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fcontext-stack=81 #-}
module XMonad.Config.Alt.Sample1 where
import XMonad.Config.Alt as C
import XMonad
import qualified XMonad.Hooks.ManageDocks
import qualified XMonad.Hooks.DynamicLog

ex1 = runConfig $ hBuild
    (C.modify Workspaces (++["lol","hi"]))
    (C.set ModMask mod4Mask)
    (C.set LayoutHook Full)
    (C.modify LayoutHook XMonad.Hooks.ManageDocks.avoidStruts)

ex2 = runConfig ex2'

ex2' = hEnd $ hBuild
      (C.statusBar "xmobar" XMonad.Hooks.DynamicLog.xmobarPP (\c -> (modMask c, xK_b)))
      (C.add LayoutHook Full) -- if this goes below the set, then you get
                              -- ex2'' :: IO (XConfig (ModifiedLayout AvoidStruts Tall))
                              --
                              -- instead of
                              --
                              -- ex2'' :: IO (XConfig (ModifiedLayout AvoidStruts (Choose Full Tall))
      (C.set LayoutHook (Tall 2 0.5 0.02))
      (C.avoidStruts) -- doesn't matter where this one goes
      (C.set LayoutHook (Tall 2 0.5 0.02))
      (C.set LayoutHook (Tall 2 0.5 0.02))

ex2'' = runConfig' defaultConfig ex2'



