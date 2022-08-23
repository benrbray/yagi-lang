module Util.PrettyPrint where

import Data.Text (Text)

------------------------------------------------------------

class PrettyPrint a where
  pretty :: a -> Text

  prettyPrint :: a -> IO ()
  prettyPrint = print . pretty