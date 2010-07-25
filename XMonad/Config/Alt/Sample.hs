module XMonad.Config.Alt.Sample where
import XMonad.Config.Alt as C
import XMonad
import XMonad.Hooks.ManageDocks

ex1 :: IO ()
ex1 = runConfig $ hBuild
    (C.modify Workspaces (++["lol","hi"]))
    (C.set ModMask mod4Mask)
    (C.modify LayoutHook avoidStruts)


