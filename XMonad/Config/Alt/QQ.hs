{-# LANGUAGE TemplateHaskell #-}

{- | Shorthand. The following are equivalent:

> hSucc (hSucc hZero)

> [$nat'| 2 |]

> $(nat 2)

-}
module XMonad.Config.Alt.QQ where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.HList


nat' :: QuasiQuoter
nat' = QuasiQuoter { quoteExp = \n -> nat (read n),
                     quotePat = error "XMonad.Config.Alt.QQ.nat'.quotePat: unimplemented"}

nat :: Int -> ExpQ
nat n = foldr appE [| hZero |] (replicate n [| hSucc |])

natTy :: Int -> TypeQ
natTy n = foldr appT [t| HZero |] (replicate n [t| HSucc |])

decNat :: String -> Int -> Q [Dec]
decNat t n = do
  d <- valD (varP (mkName t)) (normalB (nat n)) []
  s <- sigD (mkName t) (natTy n)
  return [s,d]
