{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module ColFirst where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import GHC.Generics
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Types

--put the head element in the end of the list
lastP :: [a] -> [a]
lastP (x:xs) = reverse (x:(reverse xs))

--put the last element first
firstP :: [a] -> [a]
firstP xs = (head (reverse xs)):(reverse(tail (reverse xs)))

rearrT :: BiGUL Table Table
rearrT = emb g p
        where
            g (Table n col cont) =
                (Table n (lastP col) (map (\row -> lastP row) cont))
            p (Table nS colS contS) (Table nV colV contV) =
                (Table nV (firstP colV) (map (\row -> firstP row) contV))

--Rearrange the table
--Place the String column in the first position of the table
colF :: String -> BiGUL Table Table
colF s = Case [
            $(normalSV [| (\(Table n col cont) -> (s==(head col)) ) |]
                       [| (\(Table n col cont) -> (s `elem` col) ) |]
                       [| (\(Table n col cont) -> (s `elem` col) ) |])
              ==> $(update [p| t |] [p| t |] [d| t=Replace |])
            ,$(normalSV [| (\(Table n col cont) -> (s/=(head col)) ) |]
                        [| (\(Table n col cont) -> (s `elem` col) ) |]
                        [| (\(Table n col cont) -> (s `elem` col) ) |])
              ==> rearrT `Compose` (colF s)
            ,$(adaptiveSV [p| _ |] [| (\(Table n col cont) -> (s/=(head col))) |])
              ==> error "The View is not correct"
            ]

--Take a list of string and rearrange a list of Table
--The order of the list of string need to correspond to the order of the Tables
colFirst :: [String] -> BiGUL [Table] [Table]
colFirst [] = Case [
                  $(normalSV [p| [] |] [p| [] |] [p| [] |])
                    ==> $(update [p| [] |] [p| [] |] [d| |])
                  ,$(adaptiveSV [p| _:_ |] [p| [] |])
                      ==> (\s _ -> [])
                  ]
colFirst (s:ss) = Case [
                  $(normalSV [p| [] |] [p| [] |] [p| [] |])
                      ==> $(update [p| [] |] [p| [] |] [d| |])
                  ,$(normalSV [p| _:_ |] [p| _:_ |] [p| _:_ |])
                      ==> $(update [p| x:xs |] [p| x:xs |] [d| x=(colF s); xs=(colFirst ss) |])
                  ,$(adaptiveSV [p| _:_ |] [p| [] |])
                      ==> (\s _ -> [])
                  ,$(adaptiveSV [p| [] |] [p| _:_ |])
                      ==> (\_ v -> v)
                  ]
{-
TEST
main = do
    conn1 <- connName "bigul"
    table <- getTable conn1 "checktable1"
    tableToSync <- return (fromJust (get(transformTable True) table))
    t <- return [tableToSync]
    print (get (colFirst ["idTest"]) t)
    ta <- return (fromJust (get (colFirst ["idTest"]) t))
    print (put (colFirst ["idTest"]) t ta)
-}