module Text.Hakyll.Render 
    ( depends
    , render
    , renderWith
    , renderAndConcat
    , renderAndConcatWith
    , renderChain
    , renderChainWith
    , static
    , css
    ) where

import qualified Data.Map as M
import Data.List (isPrefixOf)
import Control.Monad (unless, liftM, foldM)
import Data.Char (isAlpha)
import Data.Maybe (fromMaybe)

import System.Directory (copyFile)
import System.IO

import Text.Hakyll.Context (Context, ContextManipulation)
import Text.Hakyll.Page
import Text.Hakyll.Renderable
import Text.Hakyll.File
import Text.Hakyll.CompressCSS

-- | Execute an IO action only when the cache is invalid.
depends :: FilePath -- ^ File to be rendered or created.
        -> [FilePath] -- ^ Files the render depends on.
        -> IO () -- ^ IO action to execute when the file is out of date.
        -> IO ()
depends file dependencies action = do
    valid <- isCacheValid (toDestination file) dependencies
    unless valid action

-- | Substitutes `$identifiers` in the given string by values from the given
--   "Context". When a key is not found, it is left as it is. You can here
--   specify the characters used to replace escaped dollars `$$`.
substitute :: String -> String -> Context -> String 
substitute _ [] _ = []
substitute escaper string context 
    | "$$" `isPrefixOf` string = escaper ++ substitute' (tail tail')
    | "$" `isPrefixOf` string = substituteKey
    | otherwise = (head string) : (substitute' tail')
  where
    tail' = tail string
    (key, rest) = break (not . isAlpha) tail'
    replacement = fromMaybe ('$' : key) $ M.lookup key context
    substituteKey = replacement ++ substitute' rest
    substitute' str = substitute escaper str context

-- | "substitute" for use during a chain.
regularSubstitute :: String -> Context -> String
regularSubstitute = substitute "$$"

-- | "substitute" for the end of a chain (just before writing).
finalSubstitute :: String -> Context -> String
finalSubstitute = substitute "$"

-- | Render to a Page.
render :: Renderable a
       => FilePath -- ^ Template to use for rendering.
       -> a -- ^ Renderable object to render with given template.
       -> IO Page -- ^ The body of the result will contain the render.
render = renderWith id

-- | Render to a Page. This function allows you to manipulate the context
--   first.
renderWith :: Renderable a
           => ContextManipulation -- ^ Manipulation to apply on the context.
           -> FilePath -- ^ Template to use for rendering.
           -> a -- ^ Renderable object to render with given template.
           -> IO Page -- ^ The body of the result will contain the render.
renderWith manipulation templatePath renderable = do
    handle <- openFile templatePath ReadMode
    templateString <- hGetContents handle
    seq templateString $ hClose handle
    context <- liftM manipulation $ toContext renderable
    -- Ignore $root when substituting here. We will only replace that in the
    -- final render (just before writing).
    let contextIgnoringRoot = M.insert "root" "$root" context
        body = regularSubstitute templateString contextIgnoringRoot
    return $ fromContext (M.insert "body" body context)

-- | Render each renderable with the given template, then concatenate the
--   result.
renderAndConcat :: Renderable a => FilePath -> [a] -> IO String
renderAndConcat = renderAndConcatWith id

-- | Render each renderable with the given template, then concatenate the
--   result. This function allows you to specify a "ContextManipulation" to
--   apply on every "Renderable".
renderAndConcatWith :: Renderable a
                    => ContextManipulation
                    -> FilePath
                    -> [a]
                    -> IO String
renderAndConcatWith manipulation templatePath renderables =
    foldM concatRender' [] renderables
  where
    concatRender' :: Renderable a => String -> a -> IO String
    concatRender' chunk renderable = do
        rendered <- renderWith manipulation templatePath renderable
        let body = getBody rendered
        return $ chunk ++ body

-- | Chain a render action for a page with a number of templates. This will
--   also write the result to the site destination. This is the preferred way
--   to do general rendering.
renderChain :: Renderable a => [FilePath] -> a -> IO ()
renderChain = renderChainWith id

-- | A more custom render chain that allows you to specify a
--   "ContextManipulation" which to apply on the context when it is read first.
renderChainWith :: Renderable a
                => ContextManipulation -> [FilePath] -> a -> IO ()
renderChainWith manipulation templates renderable =
    depends (getURL renderable) (getDependencies renderable ++ templates) $
        do initialPage <- liftM manipulation $ toContext renderable
           result <- foldM (flip render) (fromContext initialPage) templates
           writePage result

-- | Write a page to the site destination.
writePage :: Page -> IO ()
writePage page = do
    let destination = toDestination url
    makeDirectories destination
    writeFile destination body
  where
    url = getURL page
    -- Substitute $root here, just before writing.
    body = finalSubstitute (getBody page)
                           (M.singleton "root" $ toRoot url)

-- | Mark a certain file as static, so it will just be copied when the site is
--   generated.
static :: FilePath -> IO ()
static source = depends destination [source] action
  where
    destination = toDestination source
    action = do makeDirectories destination
                copyFile source destination

-- | Render a css file, compressing it.
css :: FilePath -> IO ()
css source = depends destination [source] css'
  where
    destination = toDestination source
    css' = do contents <- readFile source
              makeDirectories destination
              writeFile destination (compressCSS contents)
