{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Storage.HashFS.Monad
    ( HashFS
    , run
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Reader
import Storage.HashFS.Types

-- | HashFS Monad, contains the configuration of the filesystem
newtype HashFS h a = HashFS { runHashFS_ :: ReaderT (HashFSConf h) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (HashFSConf h))

-- | Run the HashFS Monad
run :: HashFS h a -> HashFSConf h -> IO a
run m cfg = runReaderT (runHashFS_ m) cfg
