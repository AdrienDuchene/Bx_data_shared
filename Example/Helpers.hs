{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Helpers where

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List
import Control.Monad.Except
import Data.Maybe
import Data.List
import Data.Time.LocalTime
import Data.Char
import Types
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Data.ByteString.Char8 as ByteS (ByteString,concat,split,pack)
import qualified Data.Map as Map

{-------------------------------------------------------------------------------
	This file contains all the functions used to help other methods to work,
	 but not specifically related only to those methods.
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
                        OPERATIONS ON LISTS
-------------------------------------------------------------------------------}

--This method will return True if first parameter is contained in the second,
-- False otherwise.
containedIn::(Eq a)=> [a] -> [a] -> Bool
containedIn [] _ = True
containedIn (x:xs) ls = if(elem x ls) then True && containedIn xs ls
                          else False

-- returns the list of indexes of all elements in the first parameter in the
-- second parameter. I.e. if the element of first list is in the second list,
-- the index of the element in the second list is added to the list of indexes.
getIndexesList::(Eq a)=> [a] -> [a] -> [Int]
getIndexesList [] _ = []
getIndexesList _ [] = []
getIndexesList (selCol:selCols) cols = if((getIndex selCol cols) == -1)
    then getIndexesList selCols cols
    else (getIndex selCol cols):(getIndexesList selCols cols)

-- Returns the index of an element in the list. If not found, then -1
getIndex:: (Eq a)=> a -> [a] -> Int
getIndex l ls = case elemIndex l ls of
                            Nothing -> -1
                            Just i -> i


--Returns the element at a certain index of a List
getElemAtIndex:: ( Eq a) => Int -> [a] -> a
getElemAtIndex i ls = case elemAtIndex i ls of
  Nothing -> error("couldn't find elem")
  Just el -> el

-- Returns the Maybe element at a certain index of a list
elemAtIndex:: (Eq a) => Int-> [a] -> Maybe a
elemAtIndex _ [] = Nothing
elemAtIndex i ls =
  if(i<0) then Nothing
    else
      if(i >= (length ls)) then Nothing
        else
          Just (getElem i 0 ls)
          where
            getElem :: Int -> Int -> [a] -> a
            getElem i count (l:ls) =
              if(i==count) then l
                else
                  getElem i (count+1) ls

-- This method will return True if the value is at the given index in the list of list (in the deepest list), false otherwise
valueInDoubleList::(Eq a) => a -> [[a]] -> Int -> Bool
valueInDoubleList _ [] _ = False
valueInDoubleList val lss index = if(null (filter (\ls -> val == (getElemAtIndex index ls)) lss)) then
                                    False
                                  else True
-- Returns True if the value at the given index in the first list appears at the same index in the list of lists (in the deepest list)
valueInListAndListList::(Eq a) => [a] -> [[a]] -> Int -> Bool
valueInListAndListList [] _ _ = False
valueInListAndListList _ [] _ = False
valueInListAndListList xs lss index = if(null (filter (\ls -> (getElemAtIndex index xs) == (getElemAtIndex index ls)) lss)) then
                                    False
                                  else True

-- returns the list of elements of the second list not being in the first one
elemsNotInBoth :: (Eq a) => [a] -> [a] -> [a]
elemsNotInBoth [] bigLs = bigLs
elemsNotInBoth childLs [] = []
elemsNotInBoth childLs bigLs = removeListCols (getIndexesList childLs bigLs) 0 bigLs

--Removes an element from a list based on a list of indexes and a counter.
removeListCols ::[Int] -> Int -> [b] -> [b]
removeListCols [] _ cs = cs
removeListCols _ _ [] = []
removeListCols idxs count (c:cs) = if(elem count idxs)then removeListCols idxs (count+1) cs
                                    else (c:(removeListCols idxs (count+1) cs))
--Removes elements from a list of lists based on a list of indexes. The indexes
-- point at the deepest list.
removeListContent :: [Int] -> [[c]] -> [[c]]
removeListContent [] cs = cs
removeListContent _ [] = []
removeListContent idxs (c:cs) = (removeListCols idxs 0 c):(removeListContent idxs cs)

{-------------------------------------------------------------------------------
                          OPERATIONS ON SQLVALUE
-------------------------------------------------------------------------------}
-- Converts SqlValue to CondType
setCondType :: SqlValue -> CondType
setCondType sqlValue = case sqlValue of
  SqlByteString _ -> S (fromSql sqlValue)
  SqlString _ -> S (fromSql sqlValue)
  SqlInt32 _ -> I (fromSql sqlValue)
  otherwise -> S ""

defaultSqlValue :: SqlValue -> SqlValue
defaultSqlValue sqlValue = case sqlValue of
  SqlByteString _ -> defaultSQLByteString
  SqlInt32 _ -> defaultSQLInt
  otherwise -> SqlNull

toCondtype :: String -> CondType
toCondtype s = if (False `elem` (map isDigit s))
            then (S s)
            else (I (read s::Int))

toFct :: String -> (CondType -> Bool)
toFct (('='):('='):s) = (==(toCondtype s))
toFct (('/'):('='):s) = (/=(toCondtype s))
toFct (('>'):('='):s) = (>=(toCondtype s))
toFct (('<'):('='):s) = (<=(toCondtype s))
toFct (('<'):s) = (<(toCondtype s))
toFct (('>'):s) = (>(toCondtype s))
