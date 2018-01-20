{-# LANGUAGE TemplateHaskell,BangPatterns,PatternGuards,DeriveDataTypeable #-}

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Backend.P2P
import Control.Monad (forever, mapM_)

main = do
  [host, port] <- getArgs

  backend <- initializeBackend host port initRemoteTable
  node <- newLocalNode backend
  runProcess node $ forever $ do
    findPeers >>= mapM_ $ \peer -> nsend peer "echo-server" "hello!"
