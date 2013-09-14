{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Tools.Quarry.Monad
    where

import Control.Applicative
import Control.Monad.Reader
import Tools.Quarry.Types

-- | Quarry Monad
newtype QuarryM a = QuarryM { runQuarryM :: ReaderT QuarryConfig IO a }
    deriving (Functor, Applicative, Monad, MonadReader QuarryConfig, MonadIO)

runQuarry :: QuarryConfig -> QuarryM a -> IO a
runQuarry conf f = runReaderT (runQuarryM f) conf
