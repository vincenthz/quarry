module Storage.HashFS.Utils where

import qualified Control.Exception as E

catchIO :: IO a -> IO (Maybe a)
catchIO f = E.catch (Just `fmap` f) toNothing
  where toNothing :: E.IOException -> IO (Maybe a)
        toNothing _ = return Nothing
