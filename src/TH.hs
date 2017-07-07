{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import Data.Constraint
import Language.Haskell.TH
import Data.Maybe (mapMaybe)

dicts :: Name -> Q Exp
dicts name = do
  reify name >>= \case
    ClassI _ insts -> ok $ head $ mapMaybe isFine insts
    _ -> error "must be used on a class"

isFine :: InstanceDec -> Maybe Type
isFine (InstanceD _ [] t _) = Just t
isFine _ = Nothing

ok :: Type -> Q Exp
ok t = pure $ ConE 'Dict `SigE` (ConT ''Dict `AppT` t)

class What a
instance What Int

