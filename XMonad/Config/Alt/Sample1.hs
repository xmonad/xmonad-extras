{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fcontext-stack=81 #-}
module XMonad.Config.Alt.Sample1 where
import XMonad.Config.Alt as C
import XMonad
import qualified XMonad.Hooks.ManageDocks
import qualified XMonad.Hooks.DynamicLog

-- for an unknown reason ex1 and ex2 cannot coexist with ghc-7.0.2, previously
-- The same is still true for ghc 7.6


ex2 = runConfig ex2'

ex2' = hEnd $ hBuild
      (C.avoidStruts)
      (C.statusBar "xmobar" XMonad.Hooks.DynamicLog.xmobarPP (\c -> (modMask c, xK_b)))
      (C.set LayoutHook (Tall 2 0.5 0.02))

ex2'' = runConfig' defaultConfig ex2'
