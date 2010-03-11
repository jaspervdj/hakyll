module Text.Hakyll.Internal.Template
    ( Template
    , fromString
    , readTemplate
    , substitute
    , regularSubstitute
    , finalSubstitute
    ) where

import qualified Data.Map as M
import Data.List (isPrefixOf)
import Data.Char (isAlphaNum)
import Data.Binary
import Control.Monad (liftM, liftM2, replicateM)
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import System.FilePath ((</>))
import Control.Monad.Reader (liftIO)

import Test.QuickCheck

import Text.Hakyll.Context (Context)
import Text.Hakyll.HakyllMonad (Hakyll)
import Text.Hakyll.Internal.Cache

-- | Datatype used for template substitutions.
data Template = Chunk String Template
              | Identifier String Template
              | EscapeCharacter Template
              | End
              deriving (Show, Read, Eq)

-- | Construct a @Template@ from a string.
fromString :: String -> Template
fromString [] = End
fromString string
    | "$$" `isPrefixOf` string = EscapeCharacter (fromString $ tail tail')
    | "$" `isPrefixOf` string = let (key, rest) = span isAlphaNum tail'
                                in Identifier key (fromString rest)
    | otherwise = let (chunk, rest) = break (== '$') string
                  in Chunk chunk (fromString rest)
  where
    tail' = tail string

-- | Read a @Template@ from a file. This function might fetch the @Template@
--   from the cache, if available.
readTemplate :: FilePath -> Hakyll Template
readTemplate path = do
    isCacheMoreRecent' <- isCacheMoreRecent fileName [path]
    if isCacheMoreRecent' then getFromCache fileName
                          else do content <- liftIO $ readFile path
                                  let template = fromString content
                                  storeInCache template fileName
                                  return template
  where 
    fileName = "templates" </> path

-- | Substitutes @$identifiers@ in the given @Template@ by values from the given
--   "Context". When a key is not found, it is left as it is. You can specify
--   the characters used to replace escaped dollars (@$$@) here.
substitute :: String -> Template -> Context -> String 
substitute escaper (Chunk chunk template) context =
    chunk ++ substitute escaper template context
substitute escaper (Identifier key template) context =
    replacement ++ substitute escaper template context
  where
    replacement = fromMaybe ('$' : key) $ M.lookup key context
substitute escaper (EscapeCharacter template) context =
    escaper ++ substitute escaper template context
substitute _ End _ = []

-- | @substitute@ for use during a chain. This will leave escaped characters as
--   they are.
regularSubstitute :: Template -> Context -> String
regularSubstitute = substitute "$$"

-- | @substitute@ for the end of a chain (just before writing). This renders
--   escaped characters.
finalSubstitute :: Template -> Context -> String
finalSubstitute = substitute "$"
    
instance Binary Template where
    put (Chunk string template) = put (0 :: Word8) >> put string >> put template
    put (Identifier key template) = put (1 :: Word8) >> put key >> put template
    put (EscapeCharacter template) = put (2 :: Word8) >> put template
    put (End) = put (3 :: Word8)

    get = do tag <- getWord8
             case tag of 0 -> liftM2 Chunk get get
                         1 -> liftM2 Identifier get get
                         2 -> liftM EscapeCharacter get
                         3 -> return End
                         _ -> error "Error reading template"

-- | Generate arbitrary templates from a given length.
arbitraryTemplate :: Int -> Gen Template
arbitraryTemplate 0 = return End
arbitraryTemplate length' = oneof [ do chunk <- chunk'
                                       Chunk chunk <$> template'
                                  , do key <- key'
                                       Identifier key <$> template'
                                  , EscapeCharacter <$> template'
                                  ]
  where
    template' = arbitraryTemplate (length' - 1)
    -- Generate keys.
    key' = do l <- choose (5, 10)
              replicateM l $ choose ('a', 'z')
    -- Generate non-empty chunks.
    chunk' = do string <- arbitrary
                let sanitized = filter (/= '$') string
                return $ if null sanitized then "foo"
                                           else sanitized

-- | Make @Template@ testable.
instance Arbitrary Template where
    arbitrary = choose (0, 20) >>= arbitraryTemplate

    shrink (Chunk chunk template) = [template, Chunk chunk End]
    shrink (Identifier key template) = [template, Identifier key End]
    shrink (EscapeCharacter template) = [template, EscapeCharacter End]
    shrink End = []
