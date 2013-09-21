module Tools.Quarry.Cache
    ( CacheTable
    , emptyCache
    , withCache
    ) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import Control.Monad.Trans

type CacheTable a b = MVar (M.Map a b)

emptyCache :: MonadIO m => m (CacheTable a b)
emptyCache = liftIO $ newMVar M.empty

withCache :: (Ord a, MonadIO m) => CacheTable a b -> a -> m (Maybe b) -> m (Maybe b)
withCache t k f = do
    table <- liftIO $ readMVar t
    case M.lookup k table of
        Nothing -> do mv <- f
                      case mv of
                          Nothing -> return mv
                          Just v  -> liftIO $ modifyMVar_ t (return . M.insert k v) >> return mv
        Just v  -> return $ Just v
