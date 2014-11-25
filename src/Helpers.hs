module Helpers where

import Control.Monad
import Control.Monad.Trans.Maybe

findM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM f = runMaybeT . msum . map (MaybeT . f)
