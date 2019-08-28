-- | This module provides means for reading and applying 'Template's.
--
-- Templates are tools to convert items into a string. They are perfectly suited
-- for laying out your site.
--
-- Let's look at an example template:
--
-- > <html>
-- >     <head>
-- >         <title>My crazy homepage - $title$</title>
-- >     </head>
-- >     <body>
-- >         <div id="header">
-- >             <h1>My crazy homepage - $title$</h1>
-- >         </div>
-- >         <div id="content">
-- >             $body$
-- >         </div>
-- >         <div id="footer">
-- >             By reading this you agree that I now own your soul
-- >         </div>
-- >     </body>
-- > </html>
--
-- As you can see, the format is very simple -- @$key$@ is used to render the
-- @$key$@ field from the page, everything else is literally copied. If you want
-- to literally insert @\"$key$\"@ into your page (for example, when you're
-- writing a Hakyll tutorial) you can use
--
-- > <p>
-- >     A literal $$key$$.
-- > </p>
--
-- Because of it's simplicity, these templates can be used for more than HTML:
-- you could make, for example, CSS or JS templates as well.
--
-- Apart from interpolating @$key$@s from the 'Context' you can also
-- use the following macros:
--
-- * @$if(key)$@
--
-- > $if(key)$
-- >  <b> Defined </b>
-- > $else$
-- >  <b> Non-defined </b>
-- > $endif$
--
-- This example will print @Defined@ if @key@ is defined in the
-- context and @Non-defined@ otherwise. The @$else$@ clause is
-- optional.
--
-- * @$for(key)$@
--
-- The @for@ macro is used for enumerating 'Context' elements that are
-- lists, i.e. constructed using the 'listField' function. Assume that
-- in a context we have an element @listField \"key\" c itms@. Then
-- the snippet
--
-- > $for(key)$
-- >   $x$
-- > $sep$,
-- > $endfor$
--
-- would, for each item @i@ in 'itms', lookup @$x$@ in the context @c@
-- with item @i@, interpolate it, and join the resulting list with
-- @,@.
--
-- Another concrete example one may consider is the following. Given the
-- context
--
-- > listField "things" (field "thing" (return . itemBody))
-- >    (sequence [makeItem "fruits", makeItem "vegetables"])
--
-- and a template
--
-- >  I like
-- >  $for(things)$
-- >    fresh $thing$$sep$, and
-- >  $endfor$
--
-- the resulting page would look like
--
-- > <p>
-- >  I like
-- >
-- >   fresh fruits, and
-- >
-- >   fresh vegetables
-- > </p>
--
-- The @$sep$@ part can be omitted. Usually, you can get by using the
-- 'applyListTemplate' and 'applyJoinListTemplate' functions.
--
-- * @$partial(path)$@
--
-- Loads a template located in a separate file and interpolates it
-- under the current context.
--
-- Assuming that the file @test.html@ contains
--
-- > <b>$key$</b>
--
-- The result of rendering
--
-- > <p>
-- >   $partial("test.html")$
-- > </p>
--
-- is the same as the result of rendering
--
-- > <p>
-- >   <b>$key$</b>
-- > </p>
--
-- That is, calling @$partial$@ is equivalent to just copying and pasting
-- template code.
--
-- In the examples above you can see that the outputs contain a lot of leftover
-- whitespace that you may wish to remove. Using @'$-'@ or @'-$'@ instead of
-- @'$'@ in a macro strips all whitespace to the left or right of that clause
-- respectively. Given the context
--
-- > listField "counts" (field "count" (return . itemBody))
-- >    (sequence [makeItem "3", makeItem "2", makeItem "1"])
--
-- and a template
--
-- > <p>
-- >     $for(counts)-$
-- >       $count$
-- >       $-sep$...
-- >     $-endfor$
-- > </p>
--
-- the resulting page would look like
--
-- > <p>
-- >     3...2...1
-- > </p>
--
{-# LANGUAGE TemplateHaskell #-}
module Hakyll.Web.Template
    ( Template
    , templateBodyCompiler
    , templateCompiler
    , applyTemplate
    , loadAndApplyTemplate
    , applyAsTemplate
    , readTemplate
    , compileTemplateItem
    , unsafeReadTemplateFile
    , embedTemplate
    ) where


--------------------------------------------------------------------------------
import           Hakyll.Web.Template.Internal


--------------------------------------------------------------------------------
import           Data.FileEmbed               (embedFile)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Language.Haskell.TH          (Exp, Q)


--------------------------------------------------------------------------------
-- | Embed template allows you embed a template within the Haskell binary.
-- Example:
--
-- > myTemplate :: Template
-- > myTemplate = $(embedTemplate "test.html")
embedTemplate :: FilePath -> Q Exp
embedTemplate filePath = [|
    let source = T.unpack $ T.decodeUtf8 $(embedFile filePath) in
    case parseTemplateElemsFile filePath source of
        Left  err -> error err
        Right tpl -> template filePath tpl |]
