{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module MainU where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import GHC.Generics
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Data.Maybe
import Data.Foldable
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Config
import Types
import Helpers
import Instructions
import ToTableSync
import TranslationU
import SynchronizeTable
import TableSyncParser
import Keys
import ColFirst
import Transport


setup = do
    putStrLn "Which Database do you want to use ? "
    dBName <- getLine
    conn <- connName dBName
    req <- (prepare conn ("CREATE TABLE " ++ mapTableName ++ " (a VARCHAR(200), b VARCHAR(200));"))
    execute req []
    commit conn
    disconnect conn
    print "Setup done"

selCol :: [String] -> IO String
selCol cols = do
    if ((length cols)>1)
    then do
        putStrLn "Selection column ? "
        print (cols)
        getLine
    else do
        print ("Selection column is " ++ (head cols))
        return (head cols)

main = do
    putStrLn "Which Database do you want to use ? "
    dBName <- getLine
    putStrLn "Push or pull data ?"
    a <- getLine
    if ((a=="push") || (a=="Push"))
    then mainGet dBName
    else if ((a=="pull") || (a=="Pull"))
        then mainPut dBName
        else main

mainGet :: String -> IO()
mainGet dBName = do
        conn <- connName dBName
        tables <- getTables conn
        putStrLn "Please enter the list of keys to retrieve :"
        putStrLn "Warning : order matters ! "
        keys <- getLine
        fetchAllDefault keys defaultPeer
        putStrLn "Which Table do you want to use ? (':q' to quit)"
        print tables
        tableN <- getLine
        if (tableN==":q")
        then do print "Quit"
        else do
            if not(tableN `elem` tables)
                then do mainGet dBName

                else do
                    conn1 <- clone conn
                    table <- getTable conn1 tableN
                    putStrLn "Select columns you want to synchronize "
                    print (getColsT table)

                    c <- getLine
                    cols <- return (words c)
                    condCol <- (selCol cols)

                    putStrLn "Condition ? (For example : '==10') ('*' for all)"
                    conds <- getLine

                    putStrLn "Writable (T/F) ? "
                    bool <- getLine
                    b <- if (bool=="T")
                            then return True
                            else return False

                    putStrLn "Do you want to synchronize this data (Y/N) ?"
                    primary <- primaryKey dBName tableN

                    --primary can be multiple
                    rearrTable <- return (fromJust (get (colF (head primary)) table))
                    tableToSync <- return (fromJust (get(tableXTableSync cols condCol (toFct conds) b) rearrTable))
                    print tableToSync
                    sync <- getLine
                    if (sync=="Y" || sync=="y")
                        then do

                            conn2 <- clone conn
                            a_bU <- (defineUniversal 0 conn2 mapTableName tableToSync )

                            s <- parse directory

                            universal <- return (fromJust (put (translateTableSync a_bU) s [tableToSync]))
                            --stock in a file
                            writeFile directory (showList_ universal)

                            let x = sendData (showList_ universal)

                            print universal
                            print ()
                            mainGet dBName

                        else do mainGet dBName


mainPut :: String -> IO()
mainPut dBName = do

        putStrLn "Please enter the list of keys to retrieve :"
        putStrLn "Warning : order matters ! "
        keys <- getLine
        fetchAllDefault keys defaultPeer

        ss <- parse directory
        conn <- connName dBName
        a_bL <- sequence (map (\s -> (defineUniversal 1 conn mapTableName s)) ss)
        disconnect conn

        a_b <- return (removeDuplicates (concat a_bL))

        noUniversalL <- return (fromJust (get (translateTableSync a_b) ss))

        forM_ noUniversalL $ \noUniversal -> do
            tableN <- return ((\(TableSync x _ _) -> x) noUniversal)
            conn2 <- clone conn
            table <- (getTable conn2 tableN)
            disconnect conn2

            cols <- return ((\(TableSync _ cols _) -> cols) noUniversal)
            conds <- return ((\(TableSync _ _ (xs)) -> map fst xs) noUniversal)
            cond <- return (map setCondType (map head conds))

            boolean <- return ((\(TableSync _ _ ((x,b):_)) -> b) noUniversal)

            rearrTable <- return (fromJust (get (colF (head cols)) table))
            tableToSync <- return (fromJust (put(tableXTableSync cols (head cols) (`elem` cond) boolean) rearrTable noUniversal))
            tSync <- return (fromJust (put (colF (head cols)) table tableToSync))

            conn3 <- clone conn
            synchronizeTable conn3 tSync
            disconnect conn3

        print "DB synchronize"
