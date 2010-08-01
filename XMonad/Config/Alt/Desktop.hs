{-# LANGUAGE TemplateHaskell #-}
module XMonad.Config.Alt.Desktop where

import qualified XMonad as X
import qualified XMonad.Hooks.EwmhDesktops as E
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import XMonad.Config.Alt.Internal
import Control.Monad
import Control.Monad.Trans

$(decNat "avoidStrutsPrec" 1)
$(decNat "statusBarPrec" 2)
$(decNat "ewmhPrec" 2)

ewmh c = ins' ewmhPrec hTrue LayoutHook (liftM E.ewmh)

avoidStrutsOn a = ins' avoidStrutsPrec hTrue
                  (m Modify LayoutHook (ManageDocks.avoidStrutsOn a) =<<)

avoidStruts a = ins' avoidStrutsPrec hTrue
              (m Modify LayoutHook ManageDocks.avoidStruts =<<)
              a

statusBar cmd pp k = avoidStruts . ins' statusBarPrec hTrue 
                           (\c -> do
                               c' <- c
                               c'' <- liftIO $ DynamicLog.statusBar cmd pp k c'
                               return $ c'' { X.layoutHook = X.layoutHook c' }
                           )
                                                                   
                                                       
