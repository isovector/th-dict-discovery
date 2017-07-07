{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module TH where

import Data.Constraint
import Language.Haskell.TH
import Data.Maybe (mapMaybe)
import Data.Proxy

dicts :: Name -> Q Exp
dicts name = do
  reify name >>= \case
    ClassI _ insts ->
      let okInsts = mapMaybe isFine insts
          size = numTypeVars $ head okInsts
       in pure . ListE . fmap (getSomeDict size . getDict) $ okInsts
    _ -> error "must be used on a class"

isFine :: InstanceDec -> Maybe Type
isFine (InstanceD _ [] t _) = Just t
isFine _ = Nothing

getDict :: Type -> Exp
getDict t = (ConE 'Dict `SigE` (ConT ''Dict `AppT` t))

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

data SomeDict1 c where SomeDict1 :: Dict (c a)               -> SomeDict1 c
data SomeDict2 c where SomeDict2 :: Dict (c a b)             -> SomeDict2 c
data SomeDict3 c where SomeDict3 :: Dict (c a b d)           -> SomeDict3 c
data SomeDict4 c where SomeDict4 :: Dict (c a b d e)         -> SomeDict4 c
data SomeDict5 c where SomeDict5 :: Dict (c a b d e f)       -> SomeDict5 c
data SomeDict6 c where SomeDict6 :: Dict (c a b d e f g)     -> SomeDict6 c
data SomeDict7 c where SomeDict7 :: Dict (c a b d e f g h)   -> SomeDict7 c
data SomeDict8 c where SomeDict8 :: Dict (c a b d e f g h i) -> SomeDict8 c

withSomeDict :: (forall a. c a => Proxy a -> r) -> SomeDict1 c -> r
withSomeDict f (SomeDict1 (d :: Dict (c a))) =
  case d of
    Dict -> f $ Proxy @a

class What1 a where
  what1 :: String

instance What1 Int where
  what1 = "int"

instance What1 Bool where
  what1 = "bool"

class What2 a b where
  what2 :: String

instance What2 Int Bool where
  what2 = "int bool"

numTypeVars :: Type -> Int
numTypeVars = subtract 1 . length . unapply

unapply :: Type -> [Type]
unapply (AppT a b) = unapply a ++ unapply b
unapply a = [a]

