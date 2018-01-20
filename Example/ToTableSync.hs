{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module ToTableSync where

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
import Types
import Helpers
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Data.ByteString.Char8 as ByteS (ByteString,concat,split,pack)
import qualified Data.Map as Map


{-
  This file contains all methods that are necessary to switch between Table and
  TableSync types.
  The GET way will go from Table to TableSync and will also
  select data from the Table according to the desires of the User (using
  terminal to give the parameters and functions needed). The data selection can
  be both on rows and columns.
  The PUT way will simply update the values in the Table from the ones contained
  in the TableSync.


  WARNING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  Both Source and View must be ordered in alphabetic order on the key. otherwise
  the function will not work.
-}

{-
  Function used to convert a list of Tables into a list of TableSync and
  vice-versa. The first argument "String" in all tuples is the name of the Tables
  to which the selection applies. Note that the description of the types is done
  with one Table to one TableSync.
-}
listTableXSync ::  [(String, ColName)] -- list of columns to select
                -> [(String, String)] -- column to verify the condition
                -> [(String, (CondType -> Bool))] -- condition
                -> [(String, Sync)] -- boolean to know if RW or RO
                -> BiGUL [Table] [TableSync]
listTableXSync [] _ _ _ = error("Nothing to select")
listTableXSync colNamess condCols rowFcts syncs = emb g p
  where
    g tables = map (\(Table n cols rows) ->
        case(get(tableXTableSync (getRightElem colNamess n) (getRightElem condCols n) (getRightElem rowFcts n) (getRightElem syncs n) ) (Table n cols rows)) of
          Just tableSync -> tableSync
          Nothing -> error("Failed to convert the Table named : " ++ n) ) tables
    p tables tableSyncs = map (\(TableSync n cols rows)->
        case (put(tableXTableSync (getRightElem colNamess n) (getRightElem condCols n) (getRightElem rowFcts n) (getRightElem syncs n) ) (head $ filter (\(Table tn _ _) -> tn == n) tables) (TableSync n cols rows))of
          Just table -> table
          Nothing -> error("Failed to convert the TableSync named : " ++ n)) tableSyncs


-- If the string parameter matches the string in the tuple, the first tuple matching is returned
getRightElem :: [(String, a)] -> String -> a
getRightElem [] _ = error("element not found in the list, no name matched")
getRightElem (l:ls) str = if((fst l) == str)then snd l else getRightElem ls str

   {-align
      -- selection condition (none, all Tables need to be translated)
      (\_ -> True)
      -- match between Table and TableSync (names are the same)
      (\(Table n _ _) (TableSync sn _ _) -> n == sn)
      -- how to update
      $(update
          [p|table|]
          [p|table|]
          [d|table = tableXTableSync |])
      -- create an item in the source from an item in the view
      (\(TableSync n _ _) -> Table n [] [])
      -- delete an item from the source list
      (\table -> Just table)-}


{-
  Method used to convert a single Table to a TableSync and vice-versa. In order
  to be working, the name of the Table MUST be the same as the name of the
  TableSync, otherwise an error is thrown. Also, if no column is selected, the
  method sends an error. Also, the tables can't be empty, by columns and rows.
-}
tableXTableSync::  ColName -- The list of columns selected
                  -> String --The column to satisfy the function here-after.
                  -> (CondType -> Bool) -- The function to select the rows to keep
                  -> Sync -- The boolean telling if R/O or R/W
                  -> BiGUL Table TableSync
tableXTableSync [] _ _ _= error("Nothing to select")
tableXTableSync selCols col rowsFct sync = Case[
      --Error cases
      $(normal [|\(Table n _ _) (TableSync sn _ _) -> n /= sn|] [p|Table _ [] []|])
        ==> error("Table names do not match, please check inputs"),
      $(normalSV [p|Table _ [] []|] [p|TableSync _ [] []|] [p|Table _ [] []|])
        ==> error("Tables have no content, please check input"),
      --Standard case
      $(normal [|\(Table n cols _) (TableSync sn _ _)-> (n == sn)&&(containedIn selCols cols)|] [|const True|])
        ==> $(update [p|t|] [p|t|]
            [d|t = ((selectCont rowsFct col) `Compose` (dropListCols selCols)) `Compose`(transformTable sync)|])
      ]


{-
  Function to select the content (i.e. select the rows) given Table(s).
  An emb function is used in order to retrieve indexes and be able to use
  the columns in the update function for the content.
-}
selectCont :: (CondType -> Bool)--The function to select the rows to keep
            -> String --The column to satisfy the function of selection
            -> BiGUL Table Table
selectCont rowsFct colSel = emb g p
                where
                  g (Table n cols conts) = Table n cols (case(get (selectContent (getIndex colSel cols) rowsFct) conts)of
                                                            Nothing -> error("An error occurred during the selection of the rows GET")
                                                            Just content -> content )
                  p (Table _ _ sconts) (Table n cols conts) = Table n cols (case (put (selectContent (getIndex colSel cols) rowsFct) sconts conts) of
                                                              Nothing -> error("An error occurred during the selection of the rows PUT")
                                                              Just content -> content )
{-
  Method used to select the rows to keep given content (in the get direction).
  Insertion of new rows is possible, but they should fit the condition. If they
  don't, an error will occur because the data consistency is not respected.
-}
selectContent ::  Int --The index of the column to satisfy condition below
              ->  (CondType -> Bool)--The function to select the rows to keep
              ->  BiGUL Content Content
selectContent index rowsFct = Case[
        $(normalSV [p|[]|] [p|[]|] [p|[]|])
          ==> $(update [p|[]|] [p|[]|] [d||]),
        -- the column on which the condition is applied is true in both source and view.
        $(normalSV [|\(row:rows) -> rowsFct $ setCondType (getElemAtIndex index row)|] [|\(row:rows) -> rowsFct $ setCondType (getElemAtIndex index row)|] [|const True|])
          ==> $(update [p|row:rows|] [p|row:rows|] [d|row = Replace ; rows = selectContent index rowsFct|]),
        -- the condition is not true on the source and that source is not in the view.
        $(normal [|\(row:rows) rowVs -> (not (rowsFct $ setCondType (getElemAtIndex index row))) && (notElem row rowVs)|] [|const True|])
          ==> $(update [p|_:rows|] [p|rows|] [d|rows = selectContent index rowsFct|]),
        -- the condition is not true on the source, but the row is in the view (has newly been inserted in the source, see adaptive).
        $(normal [|\(row:rows) rowVs -> (not (rowsFct $ setCondType (getElemAtIndex index row))) && (elem row rowVs)|] [|const True|])
          ==> $(update [p|row:rows|] [p|row:rows|] [d|row = Replace ; rows = selectContent index rowsFct|]),
        -- if all conditions above fail, and the view is not in the source, then source gets it.
        $(adaptive [|\rowSs (rowV:rowVs) -> notElem rowV rowSs |])
          ==> \rowSs (rowV:_) -> rowV:rowSs

        ]


{-------------------------------------------------------------------------------
                                    DROPPING
       This function will delete from the Table all the data at the indexes given in parameter.
       ColName parameter is the list of columns to KEEP
       DROP MANY COLUMNS
-------------------------------------------------------------------------------}

dropListCols :: ColName -- The list of columns to KEEP
             -> BiGUL Table Table
dropListCols colName = emb g p
                          where
                            g (Table n cols conts) =
                              Table n (removeListCols (getIndexesList (elemsNotInBoth colName cols) cols) 0 cols) (removeListContent (getIndexesList (elemsNotInBoth colName cols) cols) conts)
                            p (Table n cols conts) v = insertColumns (Table n cols conts) v (getIndexesList colName cols) 0


insertColumns :: Table -> Table -> [Int] -> Int -> Table
insertColumns (Table _ [] _) (Table n cols conts) _  _ = Table n cols conts
insertColumns (Table n cols conts) (Table _ [] _) _ _ = Table n cols conts
insertColumns (Table _ _ _) (Table n cols conts) [] _ = Table n cols conts
insertColumns (Table _ cols conts) (Table n scols sconts) idxs count = Table n (insertCol idxs 0 cols scols) (insertColDbl idxs conts sconts)

insertCol::(Eq b)=>[Int] -> Int -> [b] -> [b] -> [b]
insertCol [] _ _ scols = scols
insertCol _ _ [] [] = []
insertCol idxs i cols [] = if(i == 0) then cols -- this line FORBIDS TO DELETE ROWS
    else if(i< length cols) then
        (getElemAtIndex i cols):(insertCol idxs (i+1) cols [])
      else []
insertCol idxs i cols (scol:scols) = if(i < length cols)then
    if(elem i idxs)then
      scol:(insertCol idxs (i+1) cols scols)
    else (getElemAtIndex i cols):(insertCol idxs (i+1) cols (scol:scols))
  else
    []

insertColDbl ::(Eq SqlValue) => [Int] -> [[SqlValue]] -> [[SqlValue]] -> [[SqlValue]]
insertColDbl [] _ sconts = sconts
insertColDbl _ _ [] = []
insertColDbl _ [] sconts = sconts
insertColDbl idxs conts (scont:sconts) =
  (insertCol idxs 0 (giveRightRow conts scont idxs) scont):(insertColDbl idxs conts sconts)

giveRightRow:: (Eq SqlValue) => [[SqlValue]] -> [SqlValue] -> [Int] -> [SqlValue]
giveRightRow _ [] _ = []
giveRightRow [] scont _ = scont
giveRightRow rows scont idxs =
  if((filter (\x -> (head scont) == (getElemAtIndex (head idxs) x)) rows) == []) then
    --return scont modified to have the right length
    createDefaultRow (head rows) scont idxs 0
  else
    head $ filter (\x -> (head scont) == (getElemAtIndex (head idxs) x)) rows


createDefaultRow ::(Eq SqlValue) => [SqlValue] -> [SqlValue] -> [Int] -> Int -> [SqlValue]
createDefaultRow [] [] _ _ = []
createDefaultRow row [] idxs i =
  if(i==0) then []
  else if (i < length row)then
    (defaultSqlValue (getElemAtIndex i row)):(createDefaultRow row [] idxs (i+1))
  else []
createDefaultRow row (scont:sconts) idxs i =
  if(elem i idxs)then
    scont:(createDefaultRow row sconts idxs (i+1))
  else (defaultSqlValue (getElemAtIndex i row)):(createDefaultRow row (scont:sconts) idxs (i+1))



{-
  Method that changes the type from Table to TableSync in the GET direction and
  from TableSync to Table in the PUT direction.
-}
transformTable :: Sync -> BiGUL Table TableSync
transformTable sync = emb g p
      where
        g (Table n cols rows) = TableSync n cols (map (\row -> (row,sync)) rows)
        p s (TableSync sn scols srows) = Table sn scols (map (\(row,sync) -> row) srows)



{-------------------------------------------------------------------------------
                            DATA FOR TESTS
-------------------------------------------------------------------------------}

table :: Table
table = Table "Tracks1" ["Track", "Date", "Rating", "Album", "Quantity"] tableContent

tableContent :: Content
tableContent = [
    [SqlByteString "Lullaby",  SqlInt32 1989, SqlInt32 1, SqlByteString "Galore", SqlInt32 3],
    [SqlByteString "Lullaby",  SqlInt32 1989, SqlInt32 1, SqlByteString "Show"  , SqlInt32 3],
    [SqlByteString "Lovesong", SqlInt32 1989, SqlInt32 1, SqlByteString "Galore", SqlInt32 3],
    [SqlByteString "Lovesong", SqlInt32 1989, SqlInt32 1, SqlByteString "Disintegration" , SqlInt32 3],
    [SqlByteString "Trust",    SqlInt32 1992, SqlInt32 4, SqlByteString "Wish"  , SqlInt32 5]]


table2 :: Table
table2 = Table "Trak2" ["Track", "Date", "Rating", "Album", "Quantity"] [
    [SqlByteString "Sonne",  SqlInt32 1989, SqlInt32 1, SqlByteString "Seeman", SqlInt32 3],
    [SqlByteString "Rosenrot",  SqlInt32 1989, SqlInt32 1, SqlByteString "Seeman"  , SqlInt32 3],
    [SqlByteString "Mein Teil", SqlInt32 1989, SqlInt32 1, SqlByteString "Rammlied", SqlInt32 3],
    [SqlByteString "Sehnsucht", SqlInt32 1989, SqlInt32 1, SqlByteString "Sehnsucht" , SqlInt32 3],
    [SqlByteString "Amerika",    SqlInt32 1992, SqlInt32 4, SqlByteString "Wish"  , SqlInt32 5]]


droppedTable :: Table
droppedTable = Table "Tracks1" ["Track", "Date", "Album"] [
    [SqlByteString "Lullaby",  SqlInt32 1989, SqlByteString "Galore"],
    [SqlByteString "Lullaby",  SqlInt32 1989, SqlByteString "Show"  ],
    [SqlByteString "Lovesong", SqlInt32 1989, SqlByteString "Galore"],
    [SqlByteString "Lovesong", SqlInt32 1989, SqlByteString "Disintegration" ],
    [SqlByteString "T",    SqlInt32 1992, SqlByteString "Wih"  ]]

tableSync1 :: TableSync
tableSync1 = TableSync "Tracks1" ["Track", "Date", "Album"] [([SqlByteString "Trust", SqlInt32 1992, SqlByteString "W"  ], False), ([SqlByteString "Trust", SqlInt32 1990, SqlByteString "Rasmussen"  ], False)]

tableSync2 :: TableSync
tableSync2 = TableSync "Trak2" ["Track", "Album"] [([SqlByteString "Amerika", SqlByteString "Ame"  ], False)]


tableSyncContent :: ContentSync
tableSyncContent = [([SqlByteString "t", SqlInt32 1992, SqlByteString "Wish"  ], False)]

selCols :: ColName
selCols = ["Track", "Date", "Album"]

condCol :: String
condCol = "Track"


tableSource = Table "Tracks1" ["Track","Date","Album"] [[SqlByteString "Trust",SqlInt32 1992,SqlByteString "Wish"]]
tableSyncView = TableSync "Tracks1" ["Track","Date","Album"] [([SqlByteString "T",SqlInt32 1992,SqlByteString "Wish"],False)]

mainGetStr = print(get(tableXTableSync selCols "Track" (== (S "Trust")) False ) table)
mainGetStrTrace = print(getTrace(tableXTableSync selCols "Track" (== (S "Trust")) False ) table)
mainPutStr = print(put(tableXTableSync selCols "Track" (== (S "Trust")) False ) table tableSync1)
mainPutStrTrace = print(putTrace (tableXTableSync selCols "Track" (== (S "Trust")) False ) table tableSync1)

mainGetInt = print(get(tableXTableSync selCols "Date" (<= (I 1990)) False ) table)
mainGetIntTrace = print(getTrace(tableXTableSync selCols "Date" (<= (I 1991)) False ) table)
mainPutInt = print(put(tableXTableSync selCols "Date" (>= (I 1990)) False ) table tableSync1)
mainPutIntTrace = print(putTrace(tableXTableSync selCols "Date" (>= (I 1990)) False ) table tableSync1)


tableList :: [Table]
tableList = [table,table2]

tableSyncList :: [TableSync]
tableSyncList = [tableSync2, tableSync1]

colNamesList :: [(String, ColName)]
colNamesList = [("Tracks1", ["Track", "Date", "Album"]), ("Trak2", ["Track", "Album"])]

selColList :: [(String, String)]
selColList = [("Tracks1", "Track"), ("Trak2", "Track")]

rowFctList :: [(String, (CondType -> Bool))]
rowFctList = [("Tracks1",(== (S "Trust"))),("Trak2", (== (S "Amerika")))]

syncList :: [(String, Sync)]
syncList = [("Tracks1", False), ("Trak2", False)]

mainPutList = print(put(listTableXSync colNamesList selColList rowFctList syncList) tableList tableSyncList)
mainGetList = print(get(listTableXSync colNamesList selColList rowFctList syncList) tableList)

--tableXTableSyncPut = print(put(tableXTableSync False) tableSource tableSyncView)
--tableXTableSyncPutTrace = print(putTrace(tableXTableSync False) tableSource tableSyncView)
