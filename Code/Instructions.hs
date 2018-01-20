{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Instructions where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import GHC.Generics
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Types
import Config

-- connection to my local database "bigul"
connDB :: IO Connection
connDB = do
    connectMySQL defaultMySQLConnectInfo { mysqlHost = "127.0.0.1", mysqlDatabase = "bigul"}

-- connection to my local database with a name
connName :: String ->  IO Connection
connName name = do
    connectMySQL defaultMySQLConnectInfo { mysqlHost = mySqlhost, mysqlUser = mySqlUser, mysqlPassword = mySqlPwd, mysqlDatabase = name}

selection :: String -> String
selection tableName = "SELECT * FROM " ++ tableName

-- return a String with x '?' (separate with a ',')
prepareElem :: Int -> String
prepareElem x = case x of
                1 -> "?"
                otherwise -> "?, " ++ (prepareElem (x-1))

--prepare statement for insert elements into a table
insertR :: Connection -> String -> Int -> IO Statement
insertR conn nameT xcol = prepare conn ("INSERT INTO " ++ nameT ++ " VALUES (" ++ (prepareElem xcol) ++ ")")

--prepare statement for drop elements into a table
dropR :: Connection -> String -> String  -> IO Statement
dropR conn nameT primary = prepare conn ("DELETE FROM " ++ nameT ++ " WHERE (" ++ primary ++" = ?)" )

-- transform a Table to an [[SqlValue]]
tableToSqlValue :: Table -> [[SqlValue]]
tableToSqlValue (Table name colName xs) = xs

-- transform an [[SqlValue]]  to a Table
sqlValueToTable :: String -> [String] -> [[SqlValue]] -> Table
sqlValueToTable name colName xs = (Table name colName xs)

--Get contents of a table with the Table
getContentT :: Table -> [[SqlValue]]
getContentT (Table _ _ xss) = xss

--Get columns of a table with the Table
getColsT :: Table -> [String]
getColsT (Table _ xs _) = xs

--Get contents of a table
getContent :: Connection -> String -> IO[[SqlValue]]
getContent conn tname = do
            quickQuery' conn (selection tname) []

getCols :: Connection -> String -> IO [String]
getCols conn tname = do
            des <- (describeTable conn tname)
            return (map fst des)

--Return a table
getTable :: Connection -> String -> IO Table
getTable conn tname = do
        cols <- getCols conn tname
        content <- (getContent conn tname)
        return (sqlValueToTable tname cols content)

--Return all DB informations
getDBTables :: Connection -> IO [Table]
getDBTables conn = do
    tables <- getTables conn
    sequence (map (getTable conn) tables)