module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import CompressCss
import File
import Page
import Regex
import Template
import Util

-- | Run all tests.
main :: IO ()
main = defaultMain [ compressCssGroup
                   , fileGroup
                   , pageGroup
                   , regexGroup
                   , templateGroup
                   , utilGroup
                   ]
