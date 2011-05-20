{-# LANGUAGE
    OverlappingInstances
    ,EmptyDataDecls
    ,FlexibleContexts
    ,FlexibleInstances
    ,FunctionalDependencies
    ,GeneralizedNewtypeDeriving
    ,KindSignatures
    ,MultiParamTypeClasses
    ,NoMonomorphismRestriction
    ,ScopedTypeVariables
    ,TemplateHaskell
    ,TypeOperators
    ,TypeSynonymInstances
    ,UndecidableInstances
    ,ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures
    -fcontext-stack=81 #-}
-- I can't figure out an acceptable type for 'set' and similar:
-- ghc doesn't accept the type inferred by ghci

{- |

Module      :  XMonad.Config.Alt.Internal
Copyright   :  Adam Vogt <vogt.adam@gmail.com>
License     :  BSD3-style (see LICENSE)

Maintainer  :  Adam Vogt <vogt.adam@gmail.com>
Stability   :  unstable
Portability :  unportable

Import "XMonad.Config.Alt".
-}
module XMonad.Config.Alt.Internal (
    module XMonad.Config.Alt.QQ,

    -- * Running
    runConfig,
    runConfig',

    -- * Actions
    -- $actions
    set,
    add,
    modify,
    modifyIO,

    -- ** less useful
    modifyIO',
    insertInto,

    -- * Things to modify
    -- ** Special
    LayoutHook(LayoutHook),

    -- ** Others
    FocusFollowsMouse(FocusFollowsMouse),
    StartupHook(StartupHook),
    LogHook(LogHook),
    BorderWidth(BorderWidth),
    MouseBindings(MouseBindings),
    Keys(Keys),
    ModMask(ModMask),
    Workspaces(Workspaces),
    HandleEventHook(HandleEventHook),
    ManageHook(ManageHook),
    Terminal(Terminal),
    FocusedBorderColor(FocusedBorderColor),
    NormalBorderColor(NormalBorderColor),

    -- * Relatively private
    -- | You probably don't need these
    defaultPrec,

    -- ** Ordered Insertion into HLists like [(Nat,a)]
    insLt,
    insGeq,
    Ins2(..),
    Ins'(..),
    ins,

    -- ** Useful functions
    HCompose(hComp),
    Snd(Snd),
    HSubtract(hSubtract),
    HReplicateF(hReplicateF),
    HPred'(hPred'),

    -- ** For overloading
    Mode(..),
    Add(Add),
    Set(Set),
    Modify(Modify),
    ModifyIO(ModifyIO),

    Config(..),

    test,

    module Data.HList,
 ) where

import Control.Monad.Writer
import Data.Char
import Data.HList
import Language.Haskell.TH

import qualified XMonad as X
import XMonad.Config.Alt.Types
import XMonad.Config.Alt.QQ

-- * Class to write set / modify as functions
class Mode action field e x y | action field e x -> y, action field x y -> e where
    m :: action -> field -> e -> X.XConfig x -> Config (X.XConfig y)

-- * Actions for 'Mode'
data Add = Add -- ^ the 'Mode' instance combines the old value like  @new `mappend` old@
data Set = Set
data Modify = Modify
data ModifyIO = ModifyIO

$(decNat "defaultPrec" 4)

{- $actions

Use 'set', 'add', 'modify', 'modifyIO' for most predefined fields in 'XConfig'.

For constructing things to modify a config:

> insertInto action hold prec field v

 * @action@  is an instance of 'Mode' so you only need to write 'ModifyIO' to describe how to access this field.

 * @hold@    is 'HTrue' if you don't want to overwrite a preexisting value at the same @prec@. This is for things that should be applied once-only.

 * @field@   used with the 'Mode'

 * @v@       the value that is being updated (or a function if you use 'Modify' or similar)

-}
set f v = insertInto Set hFalse defaultPrec f v
add f v = insertInto Add hFalse defaultPrec f v
modify f v = insertInto Modify hFalse defaultPrec f v
modifyIO = modifyIO' hFalse defaultPrec

modifyIO' x = insertInto ModifyIO x

insertInto action hold prec f x = ins' prec hold (m action f x =<<)

-- | Represent setting layouts and layout modifiers
data LayoutHook = LayoutHook


instance Mode ModifyIO LayoutHook (l X.Window -> Config (m X.Window)) l m where
    m _ _ l c = do
        l' <- l $ X.layoutHook c
        return $ c { X.layoutHook = l' }

-- | 'Add' means something else for 'X.layoutHook' because there's no suitable
-- mempty for the general instance of 'X.LayoutClass'
instance (X.LayoutClass l X.Window, X.LayoutClass l' X.Window) =>
        Mode Add LayoutHook (l' X.Window) l (X.Choose l' l) where
    m _ _ l = \x -> return $ x { X.layoutHook = l X.||| X.layoutHook x }

instance (Read (l X.Window), X.LayoutClass l X.Window,
          Read (l' X.Window), X.LayoutClass l' X.Window) =>
        Mode Modify LayoutHook (l X.Window -> l' X.Window) l l' where
    m _ _ l = \x -> return $ x { X.layoutHook = l (X.layoutHook x) }

instance (X.LayoutClass l' X.Window) =>
        Mode Set LayoutHook (l' X.Window) l l' where
    m _ _ l = \x -> return $ x { X.layoutHook = l }






data Snd = Snd
instance Apply Snd (a, b) b where
    apply _ (_, b) = b


-- | like  @foldr (.) id@, but for a heteregenous list.
class HCompose l f | l -> f where
    hComp :: l -> f

instance HCompose HNil (a -> a) where
    hComp _ = id

instance HCompose r (a -> b) => HCompose ((b -> c) :*: r) (a -> c) where
    hComp (HCons g r) = g . hComp r



-- | The difference between HNats. Clamped to HZero
class HSubtract a b c | a b -> c where
    hSubtract :: a -> b -> c

instance (HNat a, HNat b, HSubtract a b c) => HSubtract (HSucc a) (HSucc b) c where
    hSubtract a b = hSubtract (hPred a) (hPred b)

instance HNat a => HSubtract a HZero a where
    hSubtract a _ = a

instance HSubtract HZero b HZero where
    hSubtract _ _ = hZero


class HNat n => HReplicateF n e l | n e -> l where
    hReplicateF :: n -> e -> l

instance HReplicateF HZero e HNil where
    hReplicateF _ _ = HNil

instance (Apply e x y, HReplicateF n e r) => HReplicateF (HSucc n) e ((HFalse, x -> y) :*: r) where
    hReplicateF n e = (hFalse, apply e) `HCons` hReplicateF (hPred n) e



-- | exactly like hPred, but accept HZero too
class HPred' n n' | n -> n' where
    hPred' :: n -> n'

instance HPred' HZero HZero where
    hPred' _ = hZero

instance HNat n => HPred' (HSucc n) n where
    hPred' = hPred

insLt n hold f l =
    l
     `hAppend`
    (hReplicateF ({-hPred' $ -} n `hSubtract` hLength l) Id)
     `hAppend`
    ((hold,f) `HCons` HNil)

insGeq n a f l =
    let (b,g) = hLookupByHNat n l
        h = hCond b (b,g) (a,f . g)
    in hUpdateAtHNat n h l

-- | utility class, so that we can use contexts that may not be satisfied,
-- depending on the length of the accumulated list.
class (HBool hold) => Ins2 b n hold f l l' | b n hold f l -> l' where
    ins2 :: b -> n -> hold -> f -> l -> l'

-- | when l needs to be padded with id
instance
     (-- HPred' a n',
      HLength l n,
      HSubtract a1 n a,
      -- HReplicateF n' Id l',
      HReplicateF a Id l',
      HAppend l l' l'',
      HAppend l'' (HCons (hold,e) HNil) l''1,
      HBool hold) =>
  Ins2 HTrue a1 hold e l l''1
 where ins2 _ = insLt

-- | when l already has enough elements, just compose. Only when the existing HBool is HFalse
instance
     (HLookupByHNat n l (t, a -> b),
      HUpdateAtHNat n z l l',
      HCond t (t, a -> b) (t1, a -> c) z,
      HBool t1) =>
  Ins2 HFalse n t1 (b -> c) l l'
 where ins2 _ = insGeq

class Ins' n hold f l l' | n hold f l -> l' where
    ins' :: n -> hold -> f -> l -> l'

instance (HLength l ll, HLt ll n b,  Ins2 b n hold f l l') => Ins' n hold f l l' where
    ins' = ins2 (undefined :: b)

{- | @ins n f xs@ inserts at index @n@ the function f, or extends the list @xs@
with 'id' if there are too few elements. This way the precedence is not
bounded.
-}
ins n e = ins' n hFalse (e =<<)

runConfig' defConfig x = do
    let Config c = hComp (hMap Snd (hComp (hEnd x) HNil)) (return defConfig)
    (a,w) <- runWriterT c
    print (w [])
    return a

--runConfig :: (X.LayoutClass l X.Window, Read (l X.Window)) => Config (X.XConfig l) -> IO ()
runConfig x = X.xmonad =<< runConfig' X.defaultConfig x

-- * Tests

data T1 a = T1 a deriving Show
data T2 a = T2 a deriving Show
data T3 a = T3 a deriving Show
data T3a a = T3a a deriving Show

data RunMWR = RunMWR
instance (Monad m, HCompose l (m () -> Writer w a)) => Apply RunMWR l (a, w) where
    apply _ x = runWriter $ hComp x (return ())
data Print = Print
instance Show a => Apply Print a (IO ()) where
    apply _ = print

data HHMap a = HHMap a
instance HMap f a b => Apply (HHMap f) a b where
    apply (HHMap f) = hMap f

{- | Verification that insertions happen in order

> (T1 (),"3")
> (T2 (T1 ()),"31")
> (T2 (T3 (T1 ())),"321")
> (T2 (T3a (T3 (T1 ()))),"3221")

-}
test :: IO ()
test = sequence_ $ hMapM Print $ hMap RunMWR $ hMap (HHMap Snd) $ hEnd $ hBuild
    test1_
    test2_
    test3_
    test3a_
  where
    test1_ = ins (undefined `asTypeOf` hSucc (hSucc (hSucc hZero))) (\x -> tell "3" >> return (T1 x)) hNil
    test2_ = ins (hSucc hZero) (\x -> tell "1" >> return (T2 x)) test1_
    test3_ = ins (hSucc (hSucc hZero)) (\x -> tell "2" >> return (T3 x)) test2_
    test3a_ = ins (hSucc (hSucc hZero)) (\x -> tell "2" >> return (T3a x)) test3_


{- Generated instances for monomorphic fields in 'X.XConfig'

Follows the style of:

> data FFM = FFM

> instance Mode ModifyIO FFM (Bool -> Config Bool) l l where
>     m _ _ f c = do
>         r <- f (X.fFM c)
>         return $ c { X.fFM = r }

And the same for Modify, Set

> instance (Fail (Expected String)) => Mode ModifyIO FFM y z w where
> instance (Fail (Expected String)) => Mode Modify FFM y z w where
> instance (Fail (Expected String)) => Mode Set FFM y z w where

The last set of overlapping instances exist to help type inference here:

> :t m ModifyIO NormalBorderColor
> m ModifyIO NormalBorderColor
>   :: (String -> Config String) -> XConfig x -> Config (XConfig x)

Otherwise it would just give you:

> m ModifyIO NormalBorderColor
>      :: Mode ModifyIO NormalBorderColor e x y =>
>          e -> XConfig x -> Config (XConfig y)

Which doesn't really matter overall since @x@ ends up fixed when you try
to run the config.

-}

-- | Improve error messages maybe.
data Expected a

$(fmap concat $ sequence
   [ do
        -- do better by using quoted names in the first place?
     let accessor = "X." ++ (case nameBase d of
                                x:xs -> toLower x:xs
                                _ -> [])
         acc = mkName accessor
     VarI _ (ForallT _ _ (_ `AppT` (return -> ty))) _ _ <- reify acc
     l <- fmap varT $ newName "l"

     let mkId action tyIn body = instanceD
                (return [])
                [t| $(conT ''Mode) $(conT action) $(conT d) $(tyIn) $l $l |]
                [funD 'm
                    [clause
                        [wildP,wildP]
                        (normalB body
                            )
                        []
                    ]
                ]
            `const` (action, tyIn) -- suppress unused var warning

     let fallback act = instanceD
            (sequence [classP ''Fail [[t| Expected $ty |]]])
            [t| $(conT ''Mode) $act $(conT d) $(varT =<< newName "x") $l $l |]
            [funD 'm [clause [] (normalB [| error "impossible to satisfy" |]) [] ]]
          `const` act              -- suppress unused var warning

     sequence $

      [fallback (conT n) | n <- [''ModifyIO, ''Modify, ''Set] ] ++

      [dataD (return []) d [] [normalC d []] []

      ,mkId ''ModifyIO [t| $ty -> Config $ty |]
                        [| \f c -> do
                                r <- f ($(varE acc) c)
                                return $(recUpdE
                                            [| c |]
                                            [fmap (\r' -> (acc,r')) [| r |]])
                                |]

      ,mkId ''Modify   [t| $ty -> $ty |]
                        [| \f c -> do
                                r <- return $ f ($(varE acc) c)
                                return $(recUpdE
                                            [| c |]
                                            [fmap (\r' -> (acc,r')) [| r |]])
                                |]

      ,mkId ''Set      [t| $ty |]
                        [| \f c -> do
                                return $(recUpdE
                                            [| c |]
                                            [fmap ((,) acc) [| f |]])
                                |]
      ]

    | d <- map mkName
           -- fields in XConf
           -- XXX make these ' versions so we can be hygenic
           ["NormalBorderColor",
            "FocusedBorderColor",
            "Terminal",
            -- "LayoutHook", -- types $l and $l change with updates
            "ManageHook",
            "HandleEventHook",
            "Workspaces",
            "ModMask",
            "Keys",
            "MouseBindings",
            "BorderWidth",
            "LogHook",
            "StartupHook",
            "FocusFollowsMouse"]
    ]
 )
