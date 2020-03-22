#!/usr/bin/env stack
-- stack runghc --resolver lts-15.4 --install-ghc --package fsnotify
{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import           System.FSNotify

main =
    withManager $ \mgr -> do
        -- start a watching job (in the background)
        watchDir
          mgr          -- manager
          "."          -- directory to watch
          (const True) -- predicate
          print        -- action

        -- sleep forever (until interrupted)
        forever $ threadDelay 1000000

