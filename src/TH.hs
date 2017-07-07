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
    ClassI _ insts -> pure . ListE . fmap (getSomeDict . getDict) $ mapMaybe isFine insts
    _ -> error "must be used on a class"

isFine :: InstanceDec -> Maybe Type
isFine (InstanceD _ [] t _) = Just t
isFine _ = Nothing

getDict :: Type -> Exp
getDict t = (ConE 'Dict `SigE` (ConT ''Dict `AppT` t))

getSomeDict :: Exp -> Exp
getSomeDict = AppE (ConE 'SomeDict)

data SomeDict c where
  SomeDict :: Dict (c a) -> SomeDict c

withSomeDict :: (forall a. c a => Proxy a -> r) -> SomeDict c -> r
withSomeDict f (SomeDict (d :: Dict (c a))) =
  case d of
    Dict -> f $ Proxy @a

class What a where
  what :: String

instance What Int where
  what = "int"

instance What Bool where
  what = "bool"

