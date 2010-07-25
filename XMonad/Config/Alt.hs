{- |

Module      :  XMonad.Config.Alt
Copyright   :  Adam Vogt <vogt.adam@gmail.com>
License     :  BSD3-style (see LICENSE)

Maintainer  :  Adam Vogt <vogt.adam@gmail.com>
Stability   :  unstable
Portability :  unportable

Alternative, more composable config.

For examples, refer to sources:

 * "XMonad.Config.Alt.Sample"

Implementation

 * "XMonad.Config.Alt.Internal"

-}
module XMonad.Config.Alt (module XMonad.Config.Alt.Internal, hBuild) where

import Data.HList (hBuild)
import XMonad.Config.Alt.Internal
