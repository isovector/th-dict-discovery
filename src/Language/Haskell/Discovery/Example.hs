{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Haskell.Discovery.Example where

import Data.Constraint
import Language.Haskell.Discovery
import Data.Proxy
import Data.Typeable


test :: [String]
test = flip map $(someDicts ''What1)
     $ withSomeDict1
     $ \(p :: Proxy a) -> what1 @a
