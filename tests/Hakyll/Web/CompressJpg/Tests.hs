--------------------------------------------------------------------------------
module Hakyll.Web.CompressJpg.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (Assertion, assertBool)


--------------------------------------------------------------------------------
import           Hakyll.Web.CompressJpg
import           TestSuite.Util         (fromAssertions)
import qualified Data.ByteString.Lazy   as B


testJpg :: IO B.ByteString 
testJpg = B.readFile "tests/data/images/piccolo.jpg"

-- Test that the standard Image compressed to quality 25/100 is smaller
-- than the initial image
testCompressionFromImage :: Assertion
testCompressionFromImage = do
    image <- testJpg
    let initialSize = B.length image
        finalSize   = B.length $ compressJpg 25 image
    
    assertBool "Image was not compressed" (initialSize > finalSize)

--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Web.CompressJpg.Tests" $ concat
   [ fromAssertions "compressJpg" 
        [ testCompressionFromImage ] 
    ]
