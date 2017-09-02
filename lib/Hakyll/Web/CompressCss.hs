--------------------------------------------------------------------------------
-- | Module used for CSS compression. The compression is currently in a simple
-- state, but would typically reduce the number of bytes by about 25%.
{-# LANGUAGE PatternGuards #-}
module Hakyll.Web.CompressCss
    ( compressCssCompiler
    , compressCss
    ) where


--------------------------------------------------------------------------------
import           Data.List               (isPrefixOf)


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item


--------------------------------------------------------------------------------
-- | Compiler form of 'compressCss'
compressCssCompiler :: Compiler (Item String)
compressCssCompiler = fmap compressCss <$> getResourceString


--------------------------------------------------------------------------------
-- | Compress CSS to speed up your site.
compressCss :: String -> String
compressCss = compressSeparators . stripComments . compressWhitespace


--------------------------------------------------------------------------------
-- | Compresses certain forms of separators.
compressSeparators :: String -> String
compressSeparators [] = []
compressSeparators str
    | isConstant  = head str : retainConstants compressSeparators (head str) (drop 1 str)
    | isPrefixOf "calc( " str = "calc(" ++ compressCalcSeparators 1 (drop 6 str)
    | isPrefixOf "calc(" str = "calc(" ++ compressCalcSeparators 1 (drop 5 str)
    | stripFirst  = compressSeparators (drop 1 str)
    | stripSecond = compressSeparators (head str : (drop 2 str))
    | otherwise   = head str : compressSeparators (drop 1 str)
  where
    isConstant  = or $ map (isOfPrefix str) ["\"", "'"]
    stripFirst  = or $ map (isOfPrefix str) $ [";;", ";}"] ++ (map (\c -> " " ++ c) separators)
    stripSecond = or $ map (isOfPrefix str) $ map (\c -> c ++ " ") separators
    separators  = [" ", "{", "}", ":", ";", ",", ">", "+", "!"]

-- | Compresses separators when starting inside calc().
compressCalcSeparators :: Int -> String -> String
compressCalcSeparators 0 str = compressSeparators str
compressCalcSeparators depth str
  | stripFirst = compressCalcSeparators depth (tail str)
  | stripSecond = compressCalcSeparators depth (head str : (drop 2 str))
  | ('(' : xs) <- str = '(' : compressCalcSeparators (depth + 1) xs
  | isPrefixOf "calc( " str = compressCalcSeparators depth ("calc(" ++ (drop 6 str))
  | isPrefixOf "calc(" str = '(' : compressCalcSeparators (depth + 1) (drop 5 str)
  | (')' : xs) <- str = ')' : compressCalcSeparators (depth - 1) xs
  | otherwise = head str : compressCalcSeparators depth (tail str)
  where
    stripFirst = or $ map (isOfPrefix str) $ map (\c -> " " ++ c) ["*", "/", ")"]
    stripSecond = or $ map (isOfPrefix str) $ map (\c -> c ++ " ") ["*", "/", "("]

--------------------------------------------------------------------------------
-- | Compresses all whitespace.
compressWhitespace :: String -> String
compressWhitespace [] = []
compressWhitespace str
    | isConstant = head str : retainConstants compressWhitespace (head str) (drop 1 str)
    | replaceOne = compressWhitespace (' ' : (drop 1 str))
    | replaceTwo = compressWhitespace (' ' : (drop 2 str))
    | otherwise  = head str : compressWhitespace (drop 1 str)
  where
    isConstant = or $ map (isOfPrefix str) ["\"", "'"]
    replaceOne = or $ map (isOfPrefix str) ["\t", "\n", "\r"]
    replaceTwo = or $ map (isOfPrefix str) [" \t", " \n", " \r", "  "]

--------------------------------------------------------------------------------
-- | Function that strips CSS comments away.
stripComments :: String -> String
stripComments [] = []
stripComments str
    | isConstant          = head str : retainConstants stripComments (head str) (drop 1 str)
    | isPrefixOf "/*" str = stripComments $ eatComments $ drop 2 str
    | otherwise           = head str : stripComments (drop 1 str)
  where
    isConstant  = or $ map (isOfPrefix str) ["\"", "'"]
    eatComments str'
        | null str' = []
        | isPrefixOf "*/" str' = drop 2 str'
        | otherwise = eatComments $ drop 1 str'

--------------------------------------------------------------------------------
-- | Helper function to handle string constants correctly.
retainConstants :: (String -> String) -> Char -> String -> String
retainConstants f delim str
    | null str = []
    | isPrefixOf [delim] str = head str : f (drop 1 str)
    | otherwise = head str : retainConstants f delim (drop 1 str)

--------------------------------------------------------------------------------
-- | Helper function to determine whether a string is a substring.
isOfPrefix :: String -> String -> Bool
isOfPrefix = flip isPrefixOf
