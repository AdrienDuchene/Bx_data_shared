{-# LANGUAGE TemplateHaskell,BangPatterns,PatternGuards,DeriveDataTypeable #-}
module Transport
    (
    -- setting connection parameters
    setHostName,
    setPort,
    --launch Network
    launchChordDefault,
    launchChord,
    -- send and receive Data
    sendAllDefault,
    fetchAllDefault,
    sendData,
    fetchData
    ) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node -- (initRemoteTable)
import Control.Distributed.Process.MonadBaseControl()
import Control.Monad.Base
import System.Exit
import Network.Transport.TCP
import Network.Socket

import Control.Monad (liftM)
import Data.Typeable
import Control.Monad.IO.Class (liftIO)

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import qualified Control.Exception as Ex

import qualified Data.Map as Map
import Data.List (foldl')

import Data.Digest.Pure.SHA
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO (hGetLine, stdin)
-- for helper/debug
import qualified Data.List as List
import System.Random (randomRIO)
import Data.Ratio
import Data.Maybe (fromJust)

import qualified Data.HashTable.IO as HT

import qualified Control.Distributed.Process.DHT.Chord as Chord
import qualified Control.Distributed.Process.DHT.DHash as DHash

import Control.Distributed.Process.DHT.Chord
import Control.Distributed.Process.DHT.DHash

import Control.Distributed.Backend.P2P (makeNodeId)
import Config

path::String
path = "../Tests/peer2.txt"
--path = "../../../../../../Dropbox/BiGUL/test.txt"
--path = "../../../../Dropbox/BiGUL/test.txt"

setPort :: String -> ServiceName
setPort port = port

setHostName :: String -> HostName
setHostName host = host

defaultServiceName::ServiceName
defaultServiceName = "8199"

defaultHostName::HostName
defaultHostName = "192.168.193.138"



peers = ["192.168.193.139:8199"]

launchChordDefault peers = do
  Right transport <- createTransport defaultHostName defaultServiceName (\serviceName -> (defaultHostName, serviceName)) defaultTCPParameters
  let rtable = Chord.__remoteTable . Chord.__remoteTableDecl $ initRemoteTable
  self <- newLocalNode transport rtable -- ^ this is ourselves
  runProcess self $ do
      bootstrap initState self (map makeNodeId (peers :: [String])) -- ^ Start a _new_ chord ring containing only us.
      --spawnLocal randomFinds -- ^ do some random lookups in the chord ring at intervals, just for debug
      ht <- liftIO $ HT.new -- ^ make a new empty hashtable, if we want we can use a non empty table, eg the one from last time the client run.
      spawnLocal $ initBlockStore ht -- ^ spawn the block store. this one handles puts, gets and deletes
      say "Network operations done"

launchChord peers host port = do
  Right transport <- createTransport (host :: HostName) (port :: ServiceName) (\serviceName -> ((host :: HostName), (serviceName :: ServiceName))) defaultTCPParameters
  let rtable = Chord.__remoteTable . Chord.__remoteTableDecl $ initRemoteTable
  self <- newLocalNode transport rtable -- ^ this is ourselves
  runProcess self $ do
      bootstrap initState self (map makeNodeId (peers :: [String])) -- ^ Start a _new_ chord ring containing only us.
      --spawnLocal randomFinds -- ^ do some random lookups in the chord ring at intervals, just for debug
      ht <- liftIO $ HT.new -- ^ make a new empty hashtable, if we want we can use a non empty table, eg the one from last time the client run.
      spawnLocal $ initBlockStore ht -- ^ spawn the block store. this one handles puts, gets and deletes
      say "Network operations done"


sendAllDefault data2send peers = do
  Right transport <- createTransport defaultHostName defaultServiceName (\serviceName -> (defaultHostName, serviceName)) defaultTCPParameters
  let rtable = Chord.__remoteTable . Chord.__remoteTableDecl $ initRemoteTable
  self <- newLocalNode transport rtable -- ^ this is ourselves
  runProcess self $ do
      bootstrap initState self (map makeNodeId (peers :: [String])) -- ^ Start a _new_ chord ring containing only us.
      --spawnLocal randomFinds -- ^ do some random lookups in the chord ring at intervals, just for debug
      ht <- liftIO $ HT.new -- ^ make a new empty hashtable, if we want we can use a non empty table, eg the one from last time the client run.
      spawnLocal $ initBlockStore ht -- ^ spawn the block store. this one handles puts, gets and deletes
      say "launched chord"
      liftIO $ threadDelay 30000000
      sendData data2send
      liftIO $ threadDelay 9000000
      say "Network operations done"

fetchAllDefault keylist peers = do
  Right transport <- createTransport defaultHostName defaultServiceName (\serviceName -> (defaultHostName, serviceName)) defaultTCPParameters
  let rtable = Chord.__remoteTable . Chord.__remoteTableDecl $ initRemoteTable
  self <- newLocalNode transport rtable -- ^ this is ourselves
  let mainPid = getSelfPid
  runProcess self $ do
        bootstrap initState self (map makeNodeId (peers :: [String])) -- ^ Start a _new_ chord ring containing only us.
        --spawnLocal randomFinds -- ^ do some random lookups in the chord ring at intervals, just for debug
        ht <- liftIO $ HT.new -- ^ make a new empty hashtable, if we want we can use a non empty table, eg the one from last time the client run.
        spawnLocal $ initBlockStore ht -- ^ spawn the block store. this one handles puts, gets and deletes
        say "launched chord"
        let x = read keylist :: [Integer]
        liftIO $ threadDelay 5000000
        st <- getState
        resp <- getObject x (r st) :: Process (Maybe String)
        liftBase $ writeFile directory (maybeToString resp)
        say "leaving chord"
        liftIO $ threadDelay 200000
        return ()




sendData :: String -> Process ()
sendData "" = do say "Cannot send empty data set"
sendData data2send = do
  holder <- putObject data2send
  say "This is the key of the shared data : "
  say . show . (map fst) $ holder

fetchData::String -> Process ()
fetcData "" = do say "Cannot fetch object with empty key List"
fetchData keyList = do
  let x = read keyList :: [Integer]
  st <- getState
  resp <- getObject x (r st) :: Process (Maybe String)
  let str = maybeToString resp
  say $ show str

maybeToString :: (Maybe String) -> String
maybeToString str = case str of
  Just x -> x
  otherwise -> ""

writeData :: (Maybe String) -> IO (String)
writeData (Just str) = do
  print ("coucou Just str")
  print (str)
  x <- writeFile directory str
  return "Wrote file : Done"
writeData Nothing = do
  print ("coucour NOTHING")
  x <- writeFile directory "Nothing"
  return "Wrote file : Done"
-- {{{ userInput
-- | debug function, reads a 0.[0-9] number from command line and runs findSuccessors on it in the DHT
userInput :: String -> Process ()
userInput f = do line <- liftIO $ hGetLine stdin --reads a line on keyboard and puts it in line
                 st <- getState -- gets the state of the current node
                 let x = 2^160 :: Integer
                   --fm :: Integer -> Double
                   --fm = fromRational . (% x)
                   --sh = (take 5) . show
                 case (take 3 line) of
                    "ptf" -> do holder <- putObject f
                                say . show . (map fst) $ holder
                    "ptk" -> do holder <- putObject (drop 4 line )
                                say . show . (map fst) $ holder
                    "put" -> do holder <- putObject (drop 4 line)--puts into the DHT
                                say . show . (map fst) $ holder
                    "get" -> do resp <- getObject ((read (drop 4 line)) :: [Integer]) (r st) :: Process (Maybe String)-- gathers all chuncks and decode them
                                say $ show resp
                    "fnd" -> do let num = truncate ((read (drop 4 line)) * (fromInteger x)) :: Integer -- num contains the nearest integer of the result of calculation.
                                tmp_howMany <- liftIO $ hGetLine stdin -- gets the number to find the key "howMany"
                                let howMany = read tmp_howMany :: Int
                                succ <- Chord.findSuccessors num howMany
                                say $ show . (map (cNodeId)) $ succ
                    "del" -> do let num = ((read (drop 4 line)) :: Integer)
                                succ <- deleteBlock num
                                say $ "Trying to delete: " ++ (show num)
                    "sta" -> do st <- getState
                                say (show st)
                    "id" -> do st <- getState
                               say $ show . cNodeId . self $ st
                    _ -> return ()
                 userInput f
-- }}}

initState = Chord.NodeState {
          self = undefined
        , fingerTable = Map.empty -- ^ The fingerTable
        , blockDir = "/tmp/"
        , predecessor = undefined
        , timeout = 1000 -- ^ The timout latency of ping
        , m = 160 -- ^ The number of bits in a key, ususaly 160
        , r = 5 -- ^ the number of successors
        , b = 0 -- ^ the nuber of replicas
        , blockSize = 255 -- ^ the number of bytes a block is
        }

-- {{{ randomFinds
-- | This is a debug function, it periodically requests to know the sucessor of a random key in the ring
randomFinds = do
  liftIO $ threadDelay 8000000 -- 8 sec
  st <- getState
  key <- liftIO $ randomRIO (1, 2^(m st)) :: Process Integer
  succ <- findSuccessors key (r st)
  let x = 2^(m $ st) :: Integer
      --fm :: Integer -> Double
      --fm = fromRational . (% x)
      --sh = (take 5) . show
  --say $ (show . cNodeId . self $ st) ++ " says succ " ++ (show key) ++ " is " ++ (concatMap (show . cNodeId) succ)
  randomFinds
-- }}}
