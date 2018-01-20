{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module SynchronizeTable(synchronizeTable) where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import GHC.Generics
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Data.List
import Data.Maybe
import Types
import Instructions

createRequest :: ([SqlValue],[SqlValue]) -> Request
createRequest ([SqlNull],ys) = Insert ys
createRequest (xs,[SqlNull]) = Drop xs
createRequest (xs,ys) = if (xs/=ys) then (Update ys) else (NoRequest)


find1 :: [SqlValue] -> [[SqlValue]] -> [SqlValue]
find1 (x:xs) ((y:ys):yss) = if (x == y) then (y:ys)
                            else (find1 (x:xs) yss)

rearrange :: [[SqlValue]] -> [[SqlValue]] -> [([SqlValue],[SqlValue])]
rearrange [] [] = []
rearrange [] (y:ys) = ([SqlNull],y):(rearrange [] ys)
rearrange (x:xs) ys = if ((length (filter (== (head x)) (map head ys)) ) >= 1)
                        then (x,(find1 x ys)):(rearrange xs (delete (find1 x ys) ys))
                        else (x,[SqlNull]):(rearrange xs ys)

--separates requests
separateR :: ([[SqlValue]],[[SqlValue]]) -> [Request] -> ([[SqlValue]],[[SqlValue]])
separateR rqs [] = rqs
separateR (is,ds) ((Insert x):xs) = separateR ((x:is),ds) xs
separateR (is,ds) ((Drop x):xs) = separateR (is,(x:ds)) xs
separateR (is,ds) ((Update x):xs) = separateR ((x:is),(x:ds)) xs

-- the table is the modified table
-- table2 is the table that I want to modifie
-- primary is the name of the primary key for the table2
synchronizeTable :: Connection -> Table -> IO ()
synchronizeTable conn (Table tName tCol tContent) = do
        tUnmodified <- quickQuery' conn (selection tName) []

        rearrTables <- return (rearrange tUnmodified tContent)

        view <- return (map createRequest rearrTables)

        requests <- return (filter (\x -> x/=NoRequest) view)

        sortRequest <- return (separateR ([],[]) requests)
        insertRequest <- (insertR conn tName (length tCol))
        dropResquest <- (dropR conn tName (head tCol))

        -- the first SqlValue in a row need to be the primary key !
        executeMany dropResquest (map (\xs -> [head xs]) (snd sortRequest))
        executeMany insertRequest (fst sortRequest)
        commit conn
        disconnect conn


{-
TEST

restart :: IO ()
restart = do
        conn <- connDB
        r1 <- prepare conn ("DROP TABLE `checktable1`;")
        r2 <- prepare conn ("CREATE TABLE `checktable1` ( `Name` varchar(100) COLLATE utf8_bin NOT NULL, `Num` int(50) NOT NULL, PRIMARY KEY (Name) ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;")
        r3 <- prepare conn ("INSERT INTO `checktable1`(`Name`, `Num`) VALUES ('A',5)")
        execute r1 []
        commit conn
        execute r2 []
        execute r3 []
        commit conn
        print "restart"


Tables informations

tableConstruct :: [a]-> [[(b,xs)]]->[(a,[b])]
tableConstruct [] [] = []
tableConstruct (t:ts) (d:des) = (t,(map fst d)):(tableConstruct ts des)
tableConstruct _ _ = []

test :: IO ()
test  = do
        conn <- connDB
        ts <- (getTables conn)
        des <- (sequence (map (describeTable conn) ts))
        print (tableConstruct ts des)

--- OLD CODE ----

createRequest :: ([SqlValue],[SqlValue]) -> ([SqlValue],Request)
createRequest ([SqlNull],ys) = ([SqlNull], Insert ys)
createRequest (xs,[SqlNull]) = (xs,Drop xs)
createRequest (xs,ys) = if (xs/=ys) then (xs,Update ys) else (xs,NoRequest)

backRequest :: ([SqlValue],Request) -> ([SqlValue],[SqlValue])
backRequest ([SqlNull],Insert ys) = ([SqlNull],ys)
backRequest (xs,Drop ys) = (xs,[SqlNull])
backRequest (xs,Update ys) = (xs,ys)
backRequest (xs,NoRequest) = (xs,xs)
backRequest (_,_) = ([SqlNull],[SqlNull])

row :: BiGUL ([SqlValue],[SqlValue]) ([SqlValue],Request)
row = emb g p where
        g s = (createRequest s)
        p s v = backRequest v

-- view : couple of a table and list of string (request)
--The idea is to have two tables and a list of request
checkTable :: BiGUL [([SqlValue],[SqlValue])] [([SqlValue],Request)]
checkTable = Case [
            $(normalSV [p| [] |] [p| [] |] [p| [] |] )
                ==> $(update [p| [] |] [p| [] |] [d| |])
            ,$(normalSV [p| _:_ |] [p| _:_ |] [p| _:_ |])
                ==> $(update [p| x:xs |] [p| x:xs |] [d| x=row ; xs=checkTable|])
            ,$(adaptiveSV [p|_:_|] [p| [] |])
                ==> (\_ _ -> [])
            ,$(adaptiveSV [p| [] |] [p|_:_|])
                ==> (\_ (v:vs) -> (backRequest v):[])
            ]
-}