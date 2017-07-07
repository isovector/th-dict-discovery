{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test where

import Data.Constraint
import TH
import Data.Proxy


test :: [String]
test = flip map $(dicts ''What1)
     $ withSomeDict
     $ \(p :: Proxy a) -> what1 @a

test2 = $(dicts ''What2)
