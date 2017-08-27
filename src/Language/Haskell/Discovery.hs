{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Language.Haskell.Discovery where

import Data.Typeable
import Data.Constraint
import Language.Haskell.TH
import Data.Maybe (mapMaybe)
import Data.Proxy


someDicts :: Name -> Q Exp
someDicts = fmap fst . dicts

dicts :: Name -> Q (Exp, Exp)
dicts name = do
  reify name >>= \case
    ClassI _ insts ->
      let okInsts = mapMaybe isFine insts
          size = numTypeVars $ head okInsts
       in pure ( ListE . fmap (getSomeDict size . getDict) $ okInsts
               , getWithDict size
               )
    _ -> error "must be used on a class"

isFine :: InstanceDec -> Maybe Type
isFine (InstanceD _ [] t _) = Just t
isFine _                    = Nothing

getDict :: Type -> Exp
getDict t = ConE 'Dict `SigE` (ConT ''Dict `AppT` t)

getSomeDict :: Int -> Exp -> Exp
getSomeDict 0 = id
getSomeDict 1 = AppE (ConE 'SomeDict1)
getSomeDict 2 = AppE (ConE 'SomeDict2)
getSomeDict 3 = AppE (ConE 'SomeDict3)
getSomeDict 4 = AppE (ConE 'SomeDict4)
getSomeDict 5 = AppE (ConE 'SomeDict5)
getSomeDict 6 = AppE (ConE 'SomeDict6)
getSomeDict 7 = AppE (ConE 'SomeDict7)
getSomeDict 8 = AppE (ConE 'SomeDict8)
getSomeDict _ = error "too many type variables!"

getWithDict :: Int -> Exp
getWithDict 0 = error "to do"
getWithDict 1 = VarE 'withSomeDict1
getWithDict 2 = VarE 'withSomeDict2
getWithDict 3 = VarE 'withSomeDict3
getWithDict 4 = VarE 'withSomeDict4
getWithDict 5 = VarE 'withSomeDict5
getWithDict 6 = VarE 'withSomeDict6
getWithDict 7 = VarE 'withSomeDict7
getWithDict 8 = VarE 'withSomeDict8
getWithDict _ = error "too many type variables!"

-- TODO(sandy): It would be cool if we could generate these just in time.
data SomeDict1 c where SomeDict1 :: Dict (c a)               -> SomeDict1 c
data SomeDict2 c where SomeDict2 :: Dict (c a b)             -> SomeDict2 c
data SomeDict3 c where SomeDict3 :: Dict (c a b d)           -> SomeDict3 c
data SomeDict4 c where SomeDict4 :: Dict (c a b d e)         -> SomeDict4 c
data SomeDict5 c where SomeDict5 :: Dict (c a b d e f)       -> SomeDict5 c
data SomeDict6 c where SomeDict6 :: Dict (c a b d e f g)     -> SomeDict6 c
data SomeDict7 c where SomeDict7 :: Dict (c a b d e f g h)   -> SomeDict7 c
data SomeDict8 c where SomeDict8 :: Dict (c a b d e f g h i) -> SomeDict8 c

doIt :: Name -> Q Exp
doIt name = do
  (ds, with) <- dicts name
  pure $ TupE [ds, with]

-- test = flip map $(dicts ''What1)
--      $ withSomeDict1
--      $ \(p :: Proxy a) -> what1 @a

withSomeDict1 :: (forall a. c a => Proxy a -> r) -> SomeDict1 c -> r
withSomeDict1 f (SomeDict1 (d :: Dict (c a))) =
  case d of Dict -> f $ Proxy @a

withSomeDict2 :: (forall a b. c a b => Proxy (a, b) -> r) -> SomeDict2 c -> r
withSomeDict2 f (SomeDict2 (d :: Dict (c a b))) =
  case d of Dict -> f $ Proxy @(a, b)

withSomeDict3 :: (forall a b d. c a b d => Proxy (a, b, d) -> r) -> SomeDict3 c -> r
withSomeDict3 f (SomeDict3 (d :: Dict (c a b d))) =
  case d of Dict -> f $ Proxy @(a, b, d)

withSomeDict4 :: (forall a b d e. c a b d e => Proxy (a, b, d, e) -> r) -> SomeDict4 c -> r
withSomeDict4 f (SomeDict4 (d :: Dict (c a b d e))) =
  case d of Dict -> f $ Proxy @(a, b, d, e)

withSomeDict5 :: (forall a b d e f. c a b d e f => Proxy (a, b, d, e, f) -> r) -> SomeDict5 c -> r
withSomeDict5 f (SomeDict5 (d :: Dict (c a b d e f))) =
  case d of Dict -> f $ Proxy @(a, b, d, e, f)

withSomeDict6 :: (forall a b d e f g. c a b d e f g => Proxy (a, b, d, e, f, g) -> r) -> SomeDict6 c -> r
withSomeDict6 f (SomeDict6 (d :: Dict (c a b d e f g))) =
  case d of Dict -> f $ Proxy @(a, b, d, e, f, g)

withSomeDict7 :: (forall a b d e f g h. c a b d e f g h => Proxy (a, b, d, e, f, g, h) -> r) -> SomeDict7 c -> r
withSomeDict7 f (SomeDict7 (d :: Dict (c a b d e f g h))) =
  case d of Dict -> f $ Proxy @(a, b, d, e, f, g, h)

withSomeDict8 :: (forall a b d e f g h i. c a b d e f g h i => Proxy (a, b, d, e, f, g, h, i) -> r) -> SomeDict8 c -> r
withSomeDict8 f (SomeDict8 (d :: Dict (c a b d e f g h i))) =
  case d of Dict -> f $ Proxy @(a, b, d, e, f, g, h, i)

numTypeVars :: Type -> Int
numTypeVars = subtract 1 . length . unapply

unapply :: Type -> [Type]
unapply (AppT a b) = unapply a ++ [b]
unapply a = [a]

