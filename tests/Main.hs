module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import CompressCSS
import Context
import File
import Regex
import Template
import Util

main = defaultMain [ compressCSSGroup
                   , contextGroup
                   , fileGroup
                   , regexGroup
                   , templateGroup
                   , utilGroup
                   ]
