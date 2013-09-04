{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
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
    -- modifyIO',
    insertInto,

    -- * Fields
    -- $fields
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
    HSnd(HSnd),
    HSubtract,
    HReplicateF(hReplicateF),
    HPred',

    -- ** For overloading
    Mode(..),
    ModeAction(..),

    Config(..),

--     test,

    module Data.HList,
 ) where

import Control.Monad.Writer
import Data.Char
import Data.HList
import Language.Haskell.TH

import qualified XMonad as X
import XMonad.Config.Alt.Types
import XMonad.Config.Alt.QQ

{- | Class whose instances are used for 'add' 'set' 'modify' of an 'X.XConfig'@ layout@, which
can change the layout type. If we had lenses or other straightforward ways to adjust the entries
of 'X.XConfig', this class might be unnecessary. Without it, you would have to manually write out
things like:

> ins' defaultPrec hFalse (liftM (\c -> c{ layoutHook = avoidStruts (layoutHook c) }))

instead of

> modify LayoutHook avoidStruts

-}
class Mode (action :: ModeAction) field e x y | action field e x -> y, action field x y -> e where
    m :: Proxy action -> field -> e -> X.XConfig x -> Config (X.XConfig y)

-- | The data type for the first argument of a 'Mode' instance.
data ModeAction = Add -- ^  combines the old value like  @new `mappend` old@
    | Set | Modify | ModifyIO

$(decNat "defaultPrec" 4)

{- $actions

Use 'set', 'add', 'modify', 'modifyIO' for most predefined fields in 'XConfig'.

For constructing things to modify a config:

> insertInto action hold prec field v

 * @action@  is an instance of 'Mode' so you only need to write 'ModifyIO' to describe how to access this field.

 * @hold@    is @proxy :: Proxy True@ if you don't want to overwrite a preexisting value at the same @prec@. This is for things that should be applied once-only.

 * @field@   used with the 'Mode'

 * @v@       the value that is being updated (or a function if you use 'Modify' or similar)

-}

set f v      = insertInto defaultPrec hFalse (proxy :: Proxy Set)      f v
add f v      = insertInto defaultPrec hFalse (proxy :: Proxy Add)      f v
modify f v   = insertInto defaultPrec hFalse (proxy :: Proxy Modify)   f v
modifyIO f v = insertInto defaultPrec hFalse (proxy :: Proxy ModifyIO) f v

insertInto prec hold action field e l = ins' prec hold (m action field e =<<) l





-- | Represent setting layouts and layout modifiers
data LayoutHook = LayoutHook


instance Mode ModifyIO LayoutHook (l X.Window -> Config (m X.Window)) l m where
    m _ _ l c = do
        l' <- l $ X.layoutHook c
        return $ c { X.layoutHook = l' }

-- | 'Add' means something else for 'X.layoutHook' because there's no suitable
-- mempty for the general instance of 'X.LayoutClass'
instance (X.LayoutClass l w, X.LayoutClass l' w, w ~ X.Window) =>
        Mode Add LayoutHook (l' w) l (X.Choose l' l) where
    m _ _ l = \x -> return $ x { X.layoutHook = l X.||| X.layoutHook x }

instance (w ~ X.Window, Read (l w), X.LayoutClass l w,
          Read (l' w), X.LayoutClass l' w) =>
        Mode Modify LayoutHook (l w -> l' w) l l' where
    m _ _ l = \x -> return $ x { X.layoutHook = l (X.layoutHook x) }

instance (X.LayoutClass l' w, w ~ X.Window) =>
        Mode Set LayoutHook (l' w) l l' where
    m _ _ l = \x -> return $ x { X.layoutHook = l }






data HSnd = HSnd
instance ab ~ (a,b) => ApplyAB HSnd ab b where
    applyAB _ (_, b) = b

data Id = Id
instance (x~y) => ApplyAB Id x y where applyAB _ x = x


-- | The difference between HNats. Clamped to HZero
type family HSubtract (a :: HNat) (b :: HNat) :: HNat
type instance HSubtract (HSucc a) (HSucc b) = HSubtract a b
type instance HSubtract a HZero = a
type instance HSubtract HZero b = HZero

hSubtract :: Proxy a -> Proxy b -> Proxy (HSubtract a b)
hSubtract _ _ = undefined


class HReplicateF (n::HNat) e l | n e -> l where
    hReplicateF :: Proxy n -> e -> HList l

instance HReplicateF HZero e '[] where
    hReplicateF _ _ = HNil

instance (ApplyAB e x y, HReplicateF n e r) => HReplicateF (HSucc n) e ((Proxy False, x -> y) ': r) where
    hReplicateF n e = (hFalse, applyAB e) `HCons` hReplicateF (hPred n) e


-- | exactly like hPred, but accept HZero too
type family HPred' (n :: HNat) :: HNat
type instance HPred' (HSucc n) = n
type instance HPred' HZero = HZero


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
class Ins2 (b :: Bool) (n :: HNat) (hold :: Bool) f l l' | b n hold f l -> l',b n hold f l' -> l, b hold l l' -> f  where
    ins2 :: Proxy b -> Proxy n -> Proxy hold -> f -> HList l -> HList l'

-- | when l needs to be padded with id
instance (HReplicateF (HSubtract n (HLength l1)) Id l,
          HAppendList (HAppendList l1 l) '[(Proxy hold,t1)] ~ l2) =>
     Ins2 True n hold t1 l1 l2
 where ins2 _ = insLt

-- | when l already has enough elements, just compose. Only when the existing HBool is HFalse
instance (HUpdateAtHNat n e l, HLookupByHNat n l,
      HCond t (Proxy t, a -> b) (Proxy t1, a -> c) e,
      HLookupByHNatR n l ~ (Proxy t, a -> b),
      HUpdateAtHNatR n e l ~ l',
      bc ~ (b -> c)) =>
     Ins2 False n t1 bc l l'
 where ins2 _ = insGeq

class Ins' (n :: HNat) (hold :: Bool) f l l' | n hold f l -> l' where
    ins' :: Proxy n -> Proxy hold -> f -> HList l -> HList l'

instance ( HLt (HLength l) n ~ b,  Ins2 (HLt (HLength l) n) n hold f l l') => Ins' n hold f l l' where
    ins' = ins2 (undefined :: Proxy b)

-- ins' prec hold f l = ins2 ( hLt (hLength l) prec ) prec hold f l

{- | @ins n f xs@ inserts at index @n@ the function f, or extends the list @xs@
with 'id' if there are too few elements. This way the precedence is not
bounded.
-}
ins n e = ins' n hFalse (e =<<)

{- | like  @foldr (.) id@, but for a heteregenous list. This does the other
 order than hComposeList (which is weaker at type inference it seems, since
 the type of the id is needed to find the result...

 >>> hComposeList  ((+1) .*. (*2) .*. HNil) 2
 6

 >>> hComp ((+1) .*. (*2) .*. HNil) 2
 5
-}
class HCompose l f | l -> f where
    hComp :: HList l -> f

instance (a ~ a') => HCompose '[] (a -> a') where
    hComp _ = id

instance ((b -> c) ~ bc,  (a -> c) ~ ac, HCompose r (a -> b)) => HCompose (bc ': r) ac where
    hComp (HCons g r) = g . hComp r



type RunConfig l l' a t t2 t3 =
   (X.LayoutClass l a,
    HMapAux HSnd t2 t3,
    SameLength t2 t3,
    SameLength t3 t2,

    HCompose t (HList '[] -> HList t2),

    HCompose t3 (Config (X.XConfig l) -> Config (X.XConfig l'))
    )

runConfig' :: forall l l' a t t2 t3. RunConfig l l' a t t2 t3
    => X.XConfig l ->  HList t -> IO (X.XConfig l')
runConfig' defConfig x = do
    let Config c = hComp
            (applyAB (HMap HSnd) (hComp x HNil) :: HList t3)
            (return defConfig)

    (a,w) <- runWriterT c
    print (w [])
    return a

runConfig :: forall l l' a t t2 t3. (RunConfig l l' a t t2 t3,
    l ~ (X.Choose X.Tall (X.Choose (X.Mirror X.Tall) X.Full)),
    Read (l' X.Window), X.LayoutClass l' X.Window)
    => HList t -> IO ()
runConfig x = do
    let Config c = hComp
            (hMap HSnd (hComp x HNil) :: HList t3)
            (return X.defaultConfig)

    (a,w) <- runWriterT c
    print (w [])
    X.xmonad a

-- * Tests
{-

data T1 a = T1 a deriving Show
data T2 a = T2 a deriving Show
data T3 a = T3 a deriving Show
data T3a a = T3a a deriving Show

data RunMWR = RunMWR
instance (Monad m, HCompose l (m () -> Writer w a)) => ApplyAB RunMWR (HList l) (a, w) where
    -- type ApplyB RunMWR (HList l) = Just ... fundeps and AT's don't really mix
    -- type ApplyA RunMWR (a,w ) = Nothing
    applyAB _ x = runWriter $ hComp x (return ())
    -}

{- should be able to app (HMap (HMap f))
data HHMap a = HHMap a
instance HMap f a b => Apply (HHMap f) a b where
    apply (HHMap f) = hMap f
-}

{- | Verification that insertions happen in order

> (T1 (),"3")
> (T2 (T1 ()),"31")
> (T2 (T3 (T1 ())),"321")
> (T2 (T3a (T3 (T1 ()))),"3221")

-- broken. Fixing probably involves nasty type signatures like for set get modify etc.
test :: IO ()
test = sequence_ $ hMapM (HPrint `HComp` RunMWR) $ applyA' (HMap (HMap HSnd)) $ hEnd $ hBuild
    test1_
    test2_
    test3_
    test3a_
 where
    test1_ = ins (undefined `asTypeOf` hSucc (hSucc (hSucc hZero))) (\x -> tell "3" >> return (T1 x)) HNil
    test2_ = ins (hSucc hZero) (\x -> tell "1" >> return (T2 x)) test1_
    test3_ = ins (hSucc (hSucc hZero)) (\x -> tell "2" >> return (T3 x)) test2_
    test3a_ = ins (hSucc (hSucc hZero)) (\x -> tell "2" >> return (T3a x)) test3_
-}

{- $fields Generated instances for monomorphic fields in 'X.XConfig'

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
                [t| $(conT ''Mode) $(promotedT action) $(conT d) $(tyIn) $l $l |]
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

      [fallback (promotedT n) | n <- ['ModifyIO, 'Modify, 'Set] ] ++

      [dataD (return []) d [] [normalC d []] []

      ,mkId 'ModifyIO [t| $ty -> Config $ty |]
                        [| \f c -> do
                                r <- f ($(varE acc) c)
                                return $(recUpdE
                                            [| c |]
                                            [fmap (\r' -> (acc,r')) [| r |]])
                                |]

      ,mkId 'Modify   [t| $ty -> $ty |]
                        [| \f c -> do
                                r <- return $ f ($(varE acc) c)
                                return $(recUpdE
                                            [| c |]
                                            [fmap (\r' -> (acc,r')) [| r |]])
                                |]

      ,mkId 'Set      [t| $ty |]
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
