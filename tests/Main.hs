module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Template
import Util

main = defaultMain [ templateGroup
                   , utilGroup
                   ]
