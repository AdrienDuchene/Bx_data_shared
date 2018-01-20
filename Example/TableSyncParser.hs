{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module TableSyncParser where

import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Data.List.Split
import Data.ByteString.Char8 as ByteS (ByteString,pack)
import Data.Int
import Types

--drop the last element of the list
dropL :: [a] -> [a]
dropL [] = []
dropL xs = reverse(tail (reverse xs))

--Check if it's the end of the list
endList :: String -> Bool
endList s = ((head (reverse s))==']')

--Return the column name
getCols :: [String] -> [String]
getCols ((('['):x):xs) = x:(getCols xs)
getCols (x:[]) = [(dropL x)]
getCols (x:xs) = x:(getCols xs)

--Prepare the content for the parsing by split more elements
prepareContent :: [String] -> [String]
prepareContent [] = []
prepareContent ((('['):xs):xss) = xs:(prepareContent xss)
prepareContent (x:xs) = (splitOn "," x)++ (prepareContent xs)

--Create SqlValue Type
--TODO add type
toSqlValue :: String -> String -> SqlValue
toSqlValue "SqlByteString" x = (SqlByteString (pack x ))
toSqlValue "SqlString" x = (SqlString x)
toSqlValue "SqlInt32" x = (SqlInt32 (read x ::Int32))
toSqlValue "SqlInt64" x = (SqlInt64 (read x ::Int64))
toSqlValue "SqlInteger" x = (SqlInteger (read x ::Integer))

--return all SqlValue remaining
getSqlValue :: [String] -> [SqlValue]
getSqlValue [] = []
getSqlValue (x:y:xs) = if (endList y)
                    then [(toSqlValue x (dropL y))]
                    else (toSqlValue x y):(getSqlValue xs)

--find the first boolean in the list
findB :: [String] -> Bool
findB [] = error "Parsing error"
findB ((('T'):('r'):('u'):('e'):(')'):_):_) = True
findB ((('F'):('a'):('l'):('s'):('e'):(')'):_):_) = False
findB (x:xs) = findB xs

--Return the content of a TableSync
getContent :: [String] -> ContentSync
getContent [] = []
getContent ((('('):('['):xs):ys:xss) = if (endList ys)
            then ((((toSqlValue xs (dropL ys)):[]),(findB xss)):(getContent xss))
            else ((((toSqlValue xs ys):(getSqlValue xss)),(findB xss)):(getContent xss))
getContent (x:xs) = getContent xs


toTableSync :: String -> IO TableSync
toTableSync s = do
            splitList <- return (tail(splitOn "\"" s))
            splits <- return (splitOn " " (concat splitList))
            name <- return (head splits)
            cols <- return (getCols ((\(a:b:_) -> (splitOn "," b)) splits))
            preparedContent <- return (prepareContent ((\(a:b:xs) -> xs) splits))
            content <- return (getContent preparedContent)
            return (TableSync name cols content)

parse :: String -> IO [TableSync]
parse path = do
        ls <- fmap lines (readFile path)
        x <- sequence (map toTableSync ls)
        return x