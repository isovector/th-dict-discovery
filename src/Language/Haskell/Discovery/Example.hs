{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Haskell.Discovery.Example where

import Data.Constraint
import Language.Haskell.Discovery
import Data.Proxy
import Data.Typeable


mydicts = $(someDicts ''Show)

test :: [String]
test = flip map mydicts
     $ withSomeDict1
     $ \(p :: Proxy a) -> "hello"
