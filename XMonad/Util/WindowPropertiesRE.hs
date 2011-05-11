{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.WindowPropertiesRE
-- Copyright   :  (c) 2011 Ilya Portnov <portnov84@rambler.ru>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ilya Portnov <portnov84@rambler.ru>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Similar to XMonad.Util.WindowProperties, but uses regular expressions matching
-- instead of exact match.
--
-----------------------------------------------------------------------------
module XMonad.Util.WindowPropertiesRE
  (PropertyRE (..),
   (~?),
   propertyToQueryRE, hasPropertyRE
  ) where

import Text.Regex.Posix ((=~))

import XMonad

import XMonad.Util.WindowProperties
import XMonad.Layout.LayoutBuilderP

-- | A wrapper for X.U.WindowProperties.Property.
-- Checks using regular expression.
data PropertyRE = RE Property
  deriving (Show,Read,Typeable)

-- | Regular expressions matching for ManageHooks
(~?) :: (Functor f) => f String -> String -> f Bool
q ~? x = fmap (=~ x) q

-- | Similar to XMonad.Util.WindowProperties.propertyToQuery, 
-- but uses regexp match instead of exact match
propertyToQueryRE :: Property -> Query Bool
propertyToQueryRE (Title s) = title ~? s
propertyToQueryRE (Resource s) = resource ~? s
propertyToQueryRE (ClassName s) = className ~? s
propertyToQueryRE (Role s) = stringProperty "WM_WINDOW_ROLE" ~? s
propertyToQueryRE (Machine s) = stringProperty "WM_CLIENT_MACHINE" ~? s
propertyToQueryRE (And p1 p2) = propertyToQueryRE p1 <&&> propertyToQueryRE p2
propertyToQueryRE (Or p1 p2) = propertyToQueryRE p1 <||> propertyToQueryRE p2
propertyToQueryRE (Not p) = not `fmap` propertyToQueryRE p
propertyToQueryRE (Const b) = return b

-- | Does given window have this property?
hasPropertyRE :: PropertyRE -> Window -> X Bool
hasPropertyRE (RE p) w = runQuery (propertyToQueryRE p) w

instance Predicate PropertyRE Window where
  alwaysTrue _ = RE (Const True)
  checkPredicate = hasPropertyRE

