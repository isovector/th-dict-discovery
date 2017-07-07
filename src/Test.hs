{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test where

import Data.Constraint
import TH
import Data.Proxy
import Data.Typeable


test :: [String]
test = flip map $(dicts ''What1)
     $ withSomeDict1
     $ \(p :: Proxy a) -> what1 @a
