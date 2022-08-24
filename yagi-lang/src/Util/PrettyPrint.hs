module Util.PrettyPrint where

import Data.Text (Text)
import qualified Data.Text as T

------------------------------------------------------------

class PrettyPrint a where
  pretty :: a -> Text

  prettyPrint :: a -> IO ()
  prettyPrint = putStrLn . T.unpack . pretty