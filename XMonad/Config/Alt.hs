{- |

Module      :  XMonad.Config.Alt
Copyright   :  Adam Vogt <vogt.adam@gmail.com>
License     :  BSD3-style (see LICENSE)

Maintainer  :  Adam Vogt <vogt.adam@gmail.com>
Stability   :  unstable
Portability :  unportable

Alternative, more composable config.

This means the config can be assembled using pieces that encode:

 * correct composition when config options don't commute: @a (b conf)@ works, but @b (a (conf)@ is nonsense (ex. respecting layout hints and other layout modifiers).

 * features that must be enabled once-only

 * bundling multiple features in a single function: 'Config' captures IO

 * collect warnings (nothing uses this feature yet)


For examples, refer to sources:

 * "XMonad.Config.Alt.Sample"

Implementation

 * "XMonad.Config.Alt.Internal"

-}
module XMonad.Config.Alt (
  module XMonad.Config.Alt.Desktop,
  module XMonad.Config.Alt.Internal) where

import XMonad.Config.Alt.Internal
import XMonad.Config.Alt.Desktop
