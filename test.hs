{-# LANGUAGE BangPatterns #-}
import           Control.Monad          (forM)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8  as BS8
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Hashable          as DH
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Hakyll
import           System.FilePath        ((</>))

type FileHashes = Map Identifier String

mkFileHashes :: FilePath -> IO FileHashes
mkFileHashes dir = do
    allFiles <- getRecursiveContents (\_ -> return False) dir
    fmap Map.fromList $ forM allFiles $ \path0 -> do
        let path1 = dir </> path0
        !h <- hash path1
        return (fromFilePath path1, h)
  where
    hash :: FilePath -> IO String
    hash fp = (show . DH.hash) <$> BSL.readFile fp

main :: IO ()
main = hakyll $ do
    fileHashes <- preprocess (mkFileHashes "images")
    undefined
