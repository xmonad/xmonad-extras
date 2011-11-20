{-# LANGUAGE CPP #-}

module XMonad.Prompt.MPD.Compat (wrapId, unwrapId) where

#if MIN_VERSION_libmpd(0,7,0)

import Network.MPD (Id(..))
wrapId = Id
unwrapId (Id i) = i

#else

type Id = Int
wrapId = id
unwrapId = id

#endif

wrapId :: Int -> Id
unwrapId :: Id -> Int
