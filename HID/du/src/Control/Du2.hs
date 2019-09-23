{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Control.Du2 where

import           Data.Du
import           Control.Monad
import           Control.Monad.Writer
import           Control.Monad.Reader
import           Control.Monad.State

newtype Du2M s a = Du2M
  { runDu2 :: ReaderT DuConfig (WriterT (DuLog s) (StateT (DuState s) IO)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader DuConfig
             , MonadWriter (DuLog s)
             , MonadState (DuState s)
             )

runApp :: Du2M s a -> DuConfig -> s -> IO (a, DuLog s)
runApp app config init = evalStateT
    (runWriterT (runReaderT (runDu2 app) config))
    (DuState 0 (basePath config) init)
