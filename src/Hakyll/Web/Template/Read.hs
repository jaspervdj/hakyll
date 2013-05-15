--------------------------------------------------------------------------------
-- | Read templates in Hakyll's native format
module Hakyll.Web.Template.Read
    ( readTemplate
    ) where


--------------------------------------------------------------------------------
import           Data.String                  (fromString)

--------------------------------------------------------------------------------
import           Hakyll.Web.Template.Internal


--------------------------------------------------------------------------------
-- | Construct a @Template@ from a string.
readTemplate :: String -> Template
readTemplate = fromString