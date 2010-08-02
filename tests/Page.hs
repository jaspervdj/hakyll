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
    , testCase "test_readPage_3" test_readPage_3
    , testCase "test_readPage_4" test_readPage_4
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
    page <- runDefaultHakyll (readPage temporaryFile)
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
    assertion page = M.lookup "author" (unContext page) == Just "Eric Cartman"

-- | readPage test case 2.
test_readPage_2 = test_readPage fileName content assertion @? "test_readPage_2"
  where
    fileName  = "test_readPage_2.txt"
    content   = unlines [ "--- someSection"
                        , "This is a section."
                        , "---"
                        , "This is the body."
                        ]
    assertion page =
        let m = unContext page
        in M.lookup "someSection" m == Just "This is a section.\n"
           && M.lookup "body" m == Just "This is the body.\n"

-- | readPage test case 3.
test_readPage_3 = test_readPage fileName content assertion @? "test_readPage_3"
  where
    fileName  = "test_readPage_3.txt"
    content   = unlines [ "No metadata here, sorry."
                        ]
    assertion page =
        M.lookup "body" (unContext page) == Just "No metadata here, sorry.\n"

-- | readPage test case 4.
test_readPage_4 = test_readPage fileName content assertion @? "test_readPage_4"
  where
    fileName  = "test_readPage_4.txt"
    content   = unlines [ "--- section"
                        , "This is a section."
                        , "---"
                        , "Header"
                        , "------"
                        , "The header is not a separate section."
                        ]
    assertion page = M.lookup "body" (unContext page) == Just body
    body = unlines [ "Header"
                   , "------"
                   , "The header is not a separate section."
                   ]
