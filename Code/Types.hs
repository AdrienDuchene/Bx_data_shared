{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Types where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import GHC.Generics
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL

type TName = String
type ColName = [String]
type Content = [Row]
type ContentSync = [(Row,Sync)]
type Row = [SqlValue]
type Sync = Bool

data CondType = S String
              | I Int
              deriving(Show, Eq, Ord)

defaultSQLInt:: SqlValue
defaultSQLInt = SqlInt32 0

defaultSQLByteString::SqlValue
defaultSQLByteString = SqlByteString " "

--DB table
data Table = Table TName ColName Content
        deriving(Show, Eq)
deriveBiGULGeneric ''Table

--DB table with synchronize parameter
data TableSync = TableSync TName ColName ContentSync
        deriving(Show, Eq)
deriveBiGULGeneric ''TableSync

--DB request type
data Request = Insert [SqlValue]
             | Drop [SqlValue]
             | Update [SqlValue]
             | NoRequest
     deriving (Show, Eq)
deriveBiGULGeneric ''Request

--DB keys
data Key = PRIMARY String
         | FOREIGN (String,(String,String))
     deriving (Show, Eq, Ord)
deriveBiGULGeneric ''Key