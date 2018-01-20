{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Transport where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List
import Control.Monad.Except
import Data.Maybe
import Data.List
import Types

import qualified Control.Distributed.Backend.P2P as P2P
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay)
--import qualified Control.Distributed.Static.RemoteTable as RemoteTable
{-------------------------------------------------------------------------------
This file contains all methods for the p2p communication, protocols excluded.
All calls to those protocols are made in this file though. This can be seen as
the main methods for communication, ensuring that the shared files are the same
using bidirectional transformations.
-------------------------------------------------------------------------------}

{-- Types assignations
hostName :: HostName
hostName = "192.168.1.1"

serviceName :: ServiceName
serviceName = ""
-}

main = P2P.bootstrap "localhost" "9001" [P2P.makeNodeId "localhost:9000"] $ do
    liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes
    P2P.nsendPeers "myService" ("some", "message")
