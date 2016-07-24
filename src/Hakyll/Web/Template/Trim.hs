--------------------------------------------------------------------------------
-- | Module for trimming whitespace.
module Hakyll.Web.Template.Trim
    ( trim
    ) where


--------------------------------------------------------------------------------
import           Data.Char                    (isSpace)
import           Data.List                    (dropWhileEnd)


--------------------------------------------------------------------------------
import           Hakyll.Web.Template.Internal


--------------------------------------------------------------------------------
trim :: [TemplateElement] -> [TemplateElement]
trim = cleanse . canonicalize


--------------------------------------------------------------------------------
cleanse :: [TemplateElement] -> [TemplateElement]
cleanse = recurse cleanse . process
    where process [] = []
          process (TrimR:Chunk str:ts) = Chunk (lstrip str):process ts
          process (Chunk str:TrimL:ts) = Chunk (rstrip str):process ts
          process (t:ts) = t:process ts

          lstrip = dropWhile isSpace
          rstrip = dropWhileEnd isSpace

--------------------------------------------------------------------------------
-- | Enforce the invariant that:
--
--     * Every 'TrimL' has a 'Chunk' to its left.
--     * Every 'TrimR' has a 'Chunk' to its right.
--
canonicalize :: [TemplateElement] -> [TemplateElement]
canonicalize = go
    where go t = let t' = redundant . swap $ dedupe t
                 in if t == t' then t else go t'


--------------------------------------------------------------------------------
-- | Remove the 'TrimR' and 'TrimL's that are no-ops.
redundant :: [TemplateElement] -> [TemplateElement]
redundant = recurse redundant . process
    where -- Remove the leading 'TrimL's.
          process (TrimL:ts) = process ts
          -- Remove trailing 'TrimR's.
          process ts = foldr trailing [] ts
              where trailing TrimR [] = []
                    trailing x xs = x:xs


--------------------------------------------------------------------------------
-- >>> swap $ [TrimR, TrimL]
-- [TrimL, TrimR]
swap :: [TemplateElement] -> [TemplateElement]
swap = recurse swap . process
    where process [] = []
          process (TrimR:TrimL:ts) = TrimL:process (TrimR:ts)
          process (t:ts) = t:process ts


--------------------------------------------------------------------------------
-- | Remove 'TrimR' and 'TrimL' duplication.
dedupe :: [TemplateElement] -> [TemplateElement]
dedupe = recurse dedupe . process
    where process [] = []
          process (TrimR:TrimR:ts) = process (TrimR:ts)
          process (TrimL:TrimL:ts) = process (TrimL:ts)
          process (t:ts) = t:process ts


--------------------------------------------------------------------------------
recurse :: ([TemplateElement] -> [TemplateElement])
        -> [TemplateElement]
        -> [TemplateElement]
recurse _ []     = []
recurse f (x:xs) = process x:recurse f xs
    where process y = case y of
                          If e tb eb -> If e (f tb) (f <$> eb)
                          For e t s  -> For e (f t) (f <$> s)
                          _          -> y

