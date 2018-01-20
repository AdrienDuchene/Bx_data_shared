{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Main where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import GHC.Generics
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Data.Maybe
import Config
import Types
import Helpers
import Instructions
import ToTableSync
import TranslationU
import SynchronizeTable
import TableSyncParser

setup = do
    putStrLn "Which Database do you want to use ? "
    dBName <- getLine
    conn <- connName dBName
    req <- (prepare conn ("CREATE TABLE " ++ mapTableName ++ " (a VARCHAR(200), b VARCHAR(200));"))
    execute req []
    commit conn
    disconnect conn
    print "Setup done"

main = do
    putStrLn "Push or pull data ?"
    a <- getLine
    if ((a=="push") || (a=="Push"))
    then mainGet
    else if ((a=="pull") || (a=="Pull"))
        then mainPut
        else main

mainGet = do
        putStrLn "Which Database do you want to use ? "
        dBName <- getLine

        conn <- connName dBName
        tables <- getTables conn
        putStrLn "Which Table do you want to use ? "
        print tables

        tableN <- getLine
        conn1 <- clone conn
        table <- getTable conn1 tableN
        putStrLn "Select columns you want to synchronize "
        print (getColsT table)

        c <- getLine
        cols <- return (words c)
        putStrLn "Selection column ? "
        print (cols)
        condCol <- getLine
        putStrLn "Condition ? (For example : ==10 )"
        conds <- getLine

        putStrLn "Writable (T/F) ? "
        bool <- getLine
        b <- if (bool=="T")
                then return True
                else return False

        putStrLn "Do you want to synchronize this data (Y/N) ?"
        tableToSync <- return (fromJust (get(tableXTableSync cols condCol (toFct conds) b) table))
        print tableToSync
        sync <- getLine
        if (sync=="Y" || sync=="y")
            then do

                conn2 <- clone conn
                a_bU <- (defineUniversal 0 conn2 mapTableName tableToSync )

                s <- parse directory

                universal <- return (fromJust (put (translateTableSync a_bU) s [tableToSync]))
                --stock in a file
                writeFile directory (show universal)
                print universal

            else do main


mainPut :: IO()
mainPut = do
        s <- parse directory
        ss <- return (head s)
        putStrLn "Which Database do you want to use ? "
        dBName <- getLine
        source <- return ((\(TableSync _ _ xs) -> xs) ss)

        conn <- connName dBName
        a_b <- (defineUniversal 1 conn mapTableName ss)
        disconnect conn

        noUniversalL <- return (fromJust (get (translateTableSync a_b) [ss]))
        noUniversal <- return (head noUniversalL)

        tableN <- return ((\(TableSync x _ _) -> x) noUniversal)
        conn2 <- clone conn
        table <- (getTable conn2 tableN)
        disconnect conn2

        cols <- return ((\(TableSync _ cols _) -> cols) noUniversal)
        conds <- return ((\(TableSync _ _ (xs)) -> map fst xs) noUniversal)
        cond <- return (map setCondType (map head conds))

        boolean <- return ((\(TableSync _ _ ((x,b):_)) -> b) noUniversal)

        tableToSync <- return (fromJust (put(tableXTableSync cols (head cols) (`elem` cond) boolean) table noUniversal))
        conn3 <- clone conn

        synchronizeTable conn3 tableToSync
        disconnect conn3

        print "DB synchronize"