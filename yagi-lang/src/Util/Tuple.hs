module Util.Tuple where

import Control.Monad

------------------------------------------------------------

toFst :: (a -> b) -> a -> (b,a)
toFst = ((,) =<<)

toSnd :: (a -> b) -> a -> (a,b)
toSnd = ap (,)

