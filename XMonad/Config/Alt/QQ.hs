{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Shorthand. The following are equivalent:

> hSucc (hSucc hZero)

> [$nat'| 2 |]

> $(nat 2)

This is probably redundant given "GHC.TypeLits". But for now HList
uses it's own 'HNat'.

-}
module XMonad.Config.Alt.QQ where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.HList.CommonMain
import Data.Char
import Text.ParserCombinators.ReadP

nat' :: QuasiQuoter
nat' = QuasiQuoter { quoteExp = \n -> nat (read n),
                     quotePat = error "XMonad.Config.Alt.QQ.nat'.quotePat: unimplemented",
                     quoteType = \n -> natTy (read n),
                     quoteDec = \s -> case readP_to_S parseDecNat s of
                                        [((v,n), "")] -> decNat v n
                                        _ -> fail ("XMonad.Config.Alt.QQ.nat.quoteDec cannot parse " ++ show s)
                  }


parseDecNat :: ReadP (String, Int)
parseDecNat = do
    skipSpaces
    v <- munch isAlpha
    skipSpaces
    char '='
    skipSpaces
    n <- munch isNumber
    skipSpaces
    eof
    return (v, read n)


nat :: Int -> ExpQ
nat n = foldr appE [| hZero |] (replicate n [| hSucc |])

natTy :: Int -> TypeQ
natTy n = foldr appT [t| HZero |] (replicate n [t| HSucc |])

decNat :: String -> Int -> Q [Dec]
decNat t n = do
  d <- valD (varP (mkName t)) (normalB (nat n)) []
  let ty = [t| Proxy $(natTy n) |]
  s <- sigD (mkName t) ty
  abbrev <- tySynD (mkName (headToUpper t)) [] ty
  return [s,d, abbrev]

headToUpper (x:xs) = toUpper x : xs
headToUpper [] = []

