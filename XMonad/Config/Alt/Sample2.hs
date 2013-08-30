{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fcontext-stack=81 #-}
module XMonad.Config.Alt.Sample2 where
import XMonad.Config.Alt as C
import XMonad
import qualified XMonad.Hooks.ManageDocks
import qualified XMonad.Hooks.DynamicLog

-- for an unknown reason ex1 and ex2 cannot coexist with ghc-7.0.2, previously
-- they did
ex1 = runConfig $ hBuild
    (C.modify Workspaces (++["lol","hi"]))
    (C.set ModMask mod4Mask)
    (C.set LayoutHook Full)
    (C.modify LayoutHook XMonad.Hooks.ManageDocks.avoidStruts)

