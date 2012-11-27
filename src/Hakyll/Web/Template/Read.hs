--------------------------------------------------------------------------------
-- | Read templates in Hakyll's native format
module Hakyll.Web.Template.Read
    ( readTemplate
    ) where


--------------------------------------------------------------------------------
import           Data.List                    (isPrefixOf)


--------------------------------------------------------------------------------
import           Hakyll.Web.Template.Internal


--------------------------------------------------------------------------------
-- | Construct a @Template@ from a string.
readTemplate :: String -> Template
readTemplate = Template . readTemplate'
  where
    readTemplate' [] = []
    readTemplate' string
        | "$$" `isPrefixOf` string =
            Escaped : readTemplate' (drop 2 string)
        | "$" `isPrefixOf` string =
            case readKey (drop 1 string) of
                Just (key, rest) -> Key key : readTemplate' rest
                Nothing          -> Chunk "$" : readTemplate' (drop 1 string)
        | otherwise =
            let (chunk, rest) = break (== '$') string
            in Chunk chunk : readTemplate' rest

    -- Parse an key into (key, rest) if it's valid, and return
    -- Nothing otherwise
    readKey string =
        let (key, rest) = span validKeyChar string
        in if not (null key) && "$" `isPrefixOf` rest
            then Just (key, drop 1 rest)
            else Nothing

    validKeyChar x = x `notElem` ['$', '\n', '\r']
