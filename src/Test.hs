{-# LANGUAGE TemplateHaskell #-}

module Test where

import Data.Constraint
import TH


test :: Dict (What Int)
test = $(dicts ''What)
