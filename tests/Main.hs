module Main where

import Control.Monad (mapM_)
import Test.QuickCheck

import Template
import Util

main = do
    runTests "Template" $ do
        quickCheck prop_template_encode_id
        quickCheck prop_substitute_id
        quickCheck prop_substitute_case1

    runTests "Util" $ do
        quickCheck prop_trim_length
        quickCheck prop_trim_id
        quickCheck prop_stripHTML_length
        quickCheck prop_stripHTML_id

  where
    runTests name action = do putStrLn name
                              action
