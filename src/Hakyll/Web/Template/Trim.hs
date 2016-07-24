--------------------------------------------------------------------------------
-- | TODO
module Hakyll.Web.Template.Internal.Trim
    ( trim
    ) where


--------------------------------------------------------------------------------
import           Data.Char                    (isSpace)
import           Data.List                    (dropWhileEnd)


--------------------------------------------------------------------------------
import           Hakyll.Web.Template.Internal


--------------------------------------------------------------------------------
trim :: Template -> Template
trim = cleanse . canonicalize


--------------------------------------------------------------------------------
cleanse :: Template -> Template
cleanse = tmap (recurse cleanse . process)
    where process [] = []
          process (TrimR:Chunk str:ts) = Chunk (lstrip str):process ts
          process (Chunk str:TrimL:ts) = Chunk (rstrip str):process ts
          process (t:ts) = t:process ts

          lstrip = dropWhile isSpace
          rstrip = dropWhileEnd isSpace

--------------------------------------------------------------------------------
--
-- Invariant: Every TrimL should have a Chunk to its Left
--            Every TrimR should have a Chunk to its Right
--
--
-- Some initial implementation notes. Note: Not valid syntax etc.
--
--
--
--
--------------------------------------------------------------------------------
canonicalize :: Template -> Template
canonicalize = go
    where go t = let t' = redundant . swap . dedupe $ sink t
                 in if t == t' then t else go t'


--------------------------------------------------------------------------------
-- | 'redundant' removes the redundant 'TrimR's and 'TrimL's from the
-- 'Template's list of 'TemplateExpr's. It does _not_ recurse down the AST.
--
-- Note: Should _only_ be used on the top level 'Template'.
--
redundant :: Template -> Template
redundant = tmap (recurse redundant . process)
    where -- Remove the leading 'TrimL's.
          process (TrimL:ts) = process ts
          -- Remove trailing 'TrimR's.
          process ts = foldr trailing [] ts
              where trailing TrimR [] = []
                    trailing t ts = t:ts


--------------------------------------------------------------------------------
-- Commute:
--
--   List:
--
--      [t1, TrimR, TrimL, t2] = [t1, TrimL, TrimR, t2]
--
--   Rest should come for free provided Trim's are Sunk/Shifted etc I think.
--
swap :: Template -> Template
swap = tmap (recurse swap . process)
    where process [] = []
          process (TrimR:TrimL:ts) = TrimL:process (TrimR:ts)
          process (t:ts) = t:process ts


--------------------------------------------------------------------------------
--
-- Dedupe:
--
--   List:
--
--      [t1, TrimR, TrimR, t2] = [t1, TrimR, t2]
--
--      [t1, TrimL, TrimL, t2] = [t1, TrimL, t2]
--
--   If: Should come for free after Trim_'s have been sunk.
--
--      [t1, TrimR, If ex [TrimR, t] e, t2] = [t1, If ex [TrimR, t] e, t2]
--
--      [t1, If ex t [e, TrimL], TrimL, t2] = [t1, If ex t [e, TrimL], t2]
--
--      [t1, If ex [t, TrimL] Nothing, TrimL, t2] = [t1, If ex [t, TrimL] Nothing, t2]
--
--   For:
--
--      [t1, TrimR, For e [TrimR, b] sep, t2] = [t1, For e [TrimR, b] sep, t2]
--
--      [t1, For e b [sep, TrimL], TrimL, t2] = [t1, For e b [sep, TrimL], t2]
--
--      [t1, For e [b, TrimL] Nothing, TrimL, t2] = [t1, For e [b, TrimL] Nothing, t2]
--
dedupe :: Template -> Template
dedupe = tmap (recurse dedupe . process)
    where process [] = []
          process (TrimR:TrimR:ts) = process (TrimR:ts)
          process (TrimL:TrimL:ts) = process (TrimL:ts)
          process (t:ts) = t:process ts


--------------------------------------------------------------------------------
--
-- Sink:
--
--   If:
--
--      [t1, TrimR, If ex t e, t2] = [t1, If ex [TrimR, t] e, t2]
--
--      [t1, If ex t e, TrimL, t2] = if isJust e
--                                     then [t1, If ex t [e, TrimL], t2]
--                                     else [t1, If ex [t, TrimL] e, t2]
--
--   For:
--
--      [t1, TrimR, For e b sep, t2] = [t1, For e [TrimR, b] sep, t2]
--
--      [t1, For e b sep, TrimL, t2] = if isJust sep
--                                       then [t1, For e b [sep, TrimL], t2]
--                                       else [t1, For e [b, TrimL] sep, t2]
--
--
sink :: Template -> Template
sink = tmap (recurse sink . process)
    where process [] = []
          -- Right sink TrimR into If thenbody.
          process (TrimR:If e (Template tb) eb:ts)
              = If e (Template (TrimR:tb)) eb:process ts
          -- Left sink TrimL into If thenbody.
          process (If e (Template tb) Nothing:TrimL:ts)
              = If e (Template (tb ++ [TrimL])) Nothing:process ts
          -- Left sink TrimL into If elsebody.
          process (If e tb (Just (Template eb)):TrimL:ts)
              = If e tb (Just (Template (eb ++ [TrimL]))):process ts
          -- Right sink TrimR into For body.
          process (TrimR:For e (Template b) sep:ts)
              = For e (Template (TrimR:b)) sep:process ts
          -- Left sink TrimL into For body.
          process (For e (Template b) Nothing:TrimL:ts)
              = For e (Template (b ++ [TrimL])) Nothing:process ts
          -- Left sink TrimL into For sep.
          process (For e b (Just (Template sep)):TrimL:ts)
              = For e b (Just (Template (sep ++ [TrimL]))):process ts
          -- Otherwise move on.
          process (t:ts) = t:process ts


--------------------------------------------------------------------------------
tmap :: ([TemplateElement] -> [TemplateElement]) -> Template -> Template
tmap f (Template t) = Template (f t)


--------------------------------------------------------------------------------
recurse :: (Template -> Template) -> [TemplateElement] -> [TemplateElement]
recurse f []     = []
recurse f (x:xs) = process x:recurse f xs
    where process x = case x of
                          If e tb eb -> If e (f tb) (f <$> eb)
                          For e t s  -> For e (f t) (f <$> s)
                          _          -> x


--------------------------------------------------------------------------------
