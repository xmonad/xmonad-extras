{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module XMonad.Config.Alt.Sample where
import XMonad.Config.Alt as C
import XMonad
import qualified XMonad.Hooks.ManageDocks
import qualified XMonad.Hooks.DynamicLog

ex1, ex2 :: IO ()
ex1 = runConfig $ hBuild
    (C.modify Workspaces (++["lol","hi"]))
    (C.set ModMask mod4Mask)
    (C.modify LayoutHook XMonad.Hooks.ManageDocks.avoidStruts)

ex2 = runConfig ex2'

ex2' = hEnd $ hBuild
      (C.avoidStruts)
      (C.statusBar "xmobar" XMonad.Hooks.DynamicLog.xmobarPP (\c -> (modMask c, xK_b)))
      (C.set LayoutHook (Tall 2 0.5 0.02))

ex2'' = runConfig' defaultConfig ex2'
