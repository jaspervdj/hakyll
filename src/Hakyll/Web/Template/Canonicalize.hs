--------------------------------------------------------------------------------
-- | TODO
module Hakyll.Web.Template.Canonicalize
    ( canonicalize
    ) where


--------------------------------------------------------------------------------
import Hakyll.Web.Template.Internal


--------------------------------------------------------------------------------
--
-- Some initial implementation notes. Note: Not valid syntax etc.
--
--
-- Top level ONLY:
--    [TrimL, t, TrimR] = [t]
--
-- Dedupe:
--
--   List:
--
--      [t1, TrimR, TrimR, t2] = [t1, TrimR, t2]
--
--      [t1, TrimL, TrimL, t2] = [t1, TrimL, t2]
--
--   If:
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
-- Shift/Lift:
--
--   If:
--
--      If ex [t1, TrimR] (Just e) = If ex t1 [TrimR, e]
--
--      If ex [t1, TrimR] Nothing = [If ex t1 Nothing, TrimR]
--
--      If ex t [TrimL, e] = If ex [t, TrimL] e
--
--
--   For:
--
--      For e [t1, TrimR] (Just sep) = For e t1 [TrimR, sep]
--
--      For e [t1, TrimR] Nothing = For e t1 [TrimR, sep]
--
--      For e b [TrimL, sep] = For e [b, TrimL] sep
--
--
--
canonicalize :: Template -> Template
canonicalize = undefined
