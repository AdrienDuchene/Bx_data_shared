{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Config where

directory :: String
directory = "test.txt"

mapTableName :: String
mapTableName = "MapT"

userDefine :: Bool
userDefine = True

mySqlhost :: String
mySqlhost = "127.0.0.1"

mySqlUser :: String
mySqlUser = "root"

mySqlPwd :: String
mySqlPwd = ""