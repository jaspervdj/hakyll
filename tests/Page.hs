module Page 
    ( pageGroup
    ) where

import qualified Data.Map as M

import Control.Monad.Reader (runReaderT)
import Data.Binary
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Text.Hakyll.Internal.Page
import Text.Hakyll.Context
import Text.Hakyll.HakyllAction
import Text.Hakyll

-- Page test group.
pageGroup = testGroup "Page"
    [ testCase "test_readPage_1" test_readPage_1
    , testCase "test_readPage_2" test_readPage_2
    ]

-- | An abstract function to test page reading.
test_readPage :: FilePath           -- ^ Filename to give to the temporary file.
              -> String             -- ^ Content to put in the file.
              -> (Context -> Bool)  -- ^ Assertion to run on the result Context.
              -> IO Bool            -- ^ Result of the assertion.
test_readPage fileName content assertion = do
    temporaryDir <- getTemporaryDirectory
    let temporaryFile = temporaryDir </> fileName
    writeFile temporaryFile content
    page <- runReaderT (readPage temporaryFile) defaultHakyllConfiguration
    removeFile temporaryFile
    return $ assertion page

-- | readPage test case 1.
test_readPage_1 = test_readPage fileName content assertion @? "test_readPage_1"
  where
    fileName  = "test_readPage_1.markdown"
    content   = unlines [ "---"
                        , "author: Eric Cartman"
                        , "---"
                        , "This is a simple test."
                        ]
    assertion page = M.lookup "author" page == Just "Eric Cartman"

-- | readPage test case 2.
test_readPage_2 = test_readPage fileName content assertion @? "test_readPage_2"
  where
    fileName  = "test_readPage_2.txt"
    content   = unlines [ "--- someSection"
                        , "This is a section."
                        , "---"
                        , "This is the body."
                        ]
    assertion page =  M.lookup "someSection" page == Just "This is a section.\n"
                   && M.lookup "body" page == Just "This is the body.\n"
