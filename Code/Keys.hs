{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Keys where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import GHC.Generics
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Data.ByteString.Char8(pack)
import Data.Maybe
import Types
import Config

-- connection to my local database
connInf :: IO Connection
connInf = do
    connectMySQL defaultMySQLConnectInfo { mysqlHost = mySqlhost, mysqlUser = mySqlUser, mysqlPassword=mySqlPwd, mysqlDatabase = "information_schema"}

requestP ::String -> String
requestP db ="Select COLUMN_NAME " ++
            "FROM KEY_COLUMN_USAGE " ++
            "WHERE CONSTRAINT_SCHEMA = '" ++ db ++ "'" ++
            "AND CONSTRAINT_NAME = 'PRIMARY'"

requestF ::String -> String
requestF db ="Select COLUMN_NAME,REFERENCED_TABLE_NAME,REFERENCED_COLUMN_NAME " ++
            "FROM KEY_COLUMN_USAGE " ++
            "WHERE CONSTRAINT_SCHEMA = '" ++ db ++ "'" ++
            "AND CONSTRAINT_NAME <> 'PRIMARY'" ++
            "AND REFERENCED_TABLE_NAME <> 'NULL'"

toSqlVal :: Key -> [SqlValue]
toSqlVal (PRIMARY x) = [(SqlByteString (pack x))]
toSqlVal (FOREIGN (x,(a,b))) = [(SqlByteString (pack x)),(SqlByteString (pack a)),(SqlByteString (pack b))]

tokey :: [SqlValue] -> Key
tokey (x:[]) = PRIMARY (fromSql x)
tokey (x:a:b:[]) = FOREIGN ((fromSql x),((fromSql a),(fromSql b)))

--Return primary key from a Table into String format
primaryKey ::String -> String -> IO[String]
primaryKey dbName table= do
        conn <- connInf
        p <- quickQuery' conn ((requestP dbName) ++ "AND TABLE_NAME = '" ++ table ++ "'") []
        return (map (\xs -> (fromSql (head xs))) p)

--Return all primary and foreign key from a Table into Key format
informationTable ::String -> String -> IO[Key]
informationTable dbName table= do
        conn <- connInf
        p <- quickQuery' conn ((requestP dbName) ++ "AND TABLE_NAME = '" ++ table ++ "'") []
        f <- quickQuery' conn ((requestF dbName) ++ "AND TABLE_NAME = '" ++ table ++ "'") []
        return (map tokey (p ++ f))


{-
OLD CODE

transform :: BiGUL Key [SqlValue]
transform = emb g p where
    g s = toSqlVal s
    p s v = tokey v

tokeys ::BiGUL [Key] [[SqlValue]]
tokeys = Case [
        $(normalSV [p|[]|] [p|[]|] [p|[]|])
            ==> $(update [p|[]|] [p|[]|] [d| |])
        ,$(normalSV [p| (PRIMARY _):_|] [|\(x:xs) -> Prelude.length x == 1|] [p| (PRIMARY _):_|])
            ==> $(update [p| x:xs |] [p| x:xs |] [d| x=transform; xs=tokeys|])
        ,$(normalSV [p| (FOREIGN _):_|] [|\(x:xs) -> Prelude.length x == 3|] [p| (FOREIGN _):_|])
            ==> $(update [p| x:xs |] [p| x:xs |] [d| x=transform; xs=tokeys|])
        ,$(adaptiveSV [p| _ |] [p| _:_ |])
            ==> (\ss (v:_) -> ((tokey v):ss) )
        ,$(adaptiveSV [p| _:_ |] [p| [] |])
            ==> (\_ _ -> [])
        ]

-}