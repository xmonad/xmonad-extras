{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Types defined here to avoid template haskell stage restrictions
module XMonad.Config.Alt.Types where
import XMonad
import Control.Monad.Writer


-- TH stage restriction otherwise
data Mode_ = Add_ | Modify_ | ModifyIO_ | Set_

type Warnings = [String] -> [String]

newtype Config a = Config (WriterT Warnings IO a)
    deriving (Monad, MonadIO, MonadWriter Warnings)

