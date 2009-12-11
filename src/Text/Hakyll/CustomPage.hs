module Text.Hakyll.CustomPage
    ( CustomPage,
      createCustomPage
    ) where

import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Control.Monad
import Text.Hakyll.Renderable

data CustomPage = CustomPage { url :: String,
                               dependencies :: [FilePath],
                               mapping :: [(String, Either String (IO B.ByteString))]
                             }

createCustomPage :: String
                 -> [FilePath]
                 -> [(String, Either String (IO B.ByteString))]
                 -> CustomPage
createCustomPage = CustomPage

instance Renderable CustomPage where
    getDependencies = dependencies
    getURL = url
    toContext page = do
        values <- mapM (either (return . B.pack) (>>= return) . snd) (mapping page)
        let keys = map (B.pack . fst) (mapping page)
        return $ M.fromList $ (B.pack "url", B.pack $ url page) : zip keys values 
