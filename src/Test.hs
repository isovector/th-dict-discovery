{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test where

import Data.Constraint
import TH


test :: [String]
test = flip map $(dicts ''What)
     $ withSomeDict
     $ \(d :: Dict (What a)) ->
       case d of
         Dict -> what @a
