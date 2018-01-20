{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module TranslationU(translateTableSync,defineUniversal) where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List
import GHC.Generics
import Database.HDBC
import Database.HDBC.Types
import Database.HDBC.MySQL
import Data.List
import Data.List.Split as Split (splitOn)
import Data.ByteString.Char8 as ByteS (ByteString,concat,split,pack,unpack)
import Data.Maybe
import Config
import Test.RandomStrings
import Types
import Instructions
import SynchronizeTable

--put an element in the end of the list
lastP :: a -> [a] -> [a]
lastP x xs = reverse (x:(reverse xs))


reOrder :: [TableSync] -> [SqlValue] -> [TableSync]
reOrder xs id = (filter (\(TableSync n _ _) -> (head (reverse(splitOn "_" n)))==(fromSql (head(reverse id)))) xs)
                ++ (filter (\(TableSync n _ _) -> (head (reverse(splitOn "_" n)))/=(fromSql (head(reverse id)))) xs)

--join two sql value
joinSql :: [SqlValue] -> SqlValue
joinSql ((SqlByteString x):(SqlByteString y):[]) = (SqlByteString (ByteS.concat [x,"_",y]))
joinSql ((SqlString x):(SqlString y):[]) = (SqlString (x ++ "_" ++ y))
joinSql _ = SqlNull

--create the good id for an universal format
idSync :: SqlValue -> [[SqlValue]] -> SqlValue
idSync v [] = error "Translation error"
idSync v ((x:y:_):xss) = if (v==x)
                        then (joinSql (x:y:[]))
                        else (idSync v xss)


replaceWith :: [[SqlValue]] -> BiGUL String String
replaceWith mapT = emb g p
                where
                    g s = (findNoUniversal mapT s)
                    p s v = (findUniversal mapT v)

replaceRow :: [[SqlValue]] -> BiGUL (Row,Bool) (Row,Bool)
replaceRow mapT = emb g p
            where
                g ((s:ss),b) = (((toSql(findNoUniversal mapT (fromSql s))):ss),b)
                p (ss,bS) ((v:vs),bV) = if bS
                                        then (((toSql(findUniversal mapT (fromSql v))):vs)++(ss \\ vs),bV)
                                        else error("Data can't be modified \nYour data : "
                                                     ++ (show(v:vs,bV)  ++
                                                  "\nSynchronized data : "
                                                     ++ (show(ss,bS))))

--find the universal type from the noUniversal type
findUniversal :: [[SqlValue]] -> String -> String
findUniversal [] _ = ""
findUniversal ((x:y:_):ls) nNouniversal = if ((toSql nNouniversal)== x)
                                        then fromSql y
                                        else findUniversal ls nNouniversal

--find the noUniversal type from the universal type
findNoUniversal :: [[SqlValue]] -> String -> String
findNoUniversal [] _ = ""
findNoUniversal ((x:y:_):ls) universal = if ((toSql universal)== y)
                                        then fromSql x
                                        else findNoUniversal ls universal

--Universal format for two tables
uniName :: [[SqlValue]] -> BiGUL String String
uniName mapT = emb g p
                where
                g s = (findNoUniversal mapT (head (reverse(splitOn "_" s))))
                p s v = (v ++ "_" ++ (findUniversal mapT v))

uniCols2 :: [[SqlValue]] -> BiGUL [(Bool,String)] [String]
uniCols2 mapT = align
            -- not delected item
           (\(b,_) -> not b)
           --match between the source and the view
           (\(b,s) v -> (findUniversal mapT v)==s)
           --update function
           $(update [p| (_,x) |] [p| x |] [d| x=replaceWith mapT |])
           --create a view from a source
           (\v -> (False,(findUniversal mapT v)))
           --delete an item from the source list
           (\(b,s) -> Just (True,s))

uniCols :: [[SqlValue]] -> BiGUL [String] [String]
uniCols mapT = emb g p
            where
                g s = fromJust(get (uniCols2 mapT)  newS)
                    where
                        newS = map (\col -> (not((findNoUniversal mapT col)/=""),col)) s
                p s v = map (\(_,col) -> col) (fromJust (put (uniCols2 mapT) newS v))
                    where
                        newS = map (\col -> (False,col)) s

uniCont2 :: [[SqlValue]] -> BiGUL [(Bool,(Row,Sync))] ContentSync
uniCont2 mapT = align
            -- not delected item
           (\(b,_) -> not b)
           --match between the source and the view
           (\(b,((s:_),_)) ((v:_),_) -> (findUniversal mapT (fromSql v))==(fromSql s))
           --update function
           $(update [p| (_,x) |] [p| x |] [d| x=replaceRow mapT |])
           --create a view from a source
           (\((v:vs),b) -> (False,(((toSql (findUniversal mapT (fromSql v))):vs),b)))
           --delete an item from the source list
           (\(b,cont) -> Just (True,cont))

uniCont :: [[SqlValue]] -> BiGUL ContentSync ContentSync
uniCont mapT = emb g p
             where
                g s = fromJust(get (uniCont2 mapT)  newS)
                    where
                        newS = map (\(row,sync) -> (not((findNoUniversal mapT (fromSql (head row)))/=""),(row,sync))) s
                p s v = map (\(_,cont) -> cont) (fromJust (put (uniCont2 mapT) newS v))
                    where
                        newS = map (\cont -> (False,cont)) s



--translate a TableSync list into a TableSync list with an universal format
translateTableSync2 :: [[SqlValue]] -> BiGUL [(Bool,TableSync)] [TableSync]
translateTableSync2 mapT = align
                    -- not deleted item
                    (\(b,_) -> not b)
                    --match between the source and the view
                    (\(_,TableSync nS _ _) (TableSync nV _ _) -> (fromJust (get (uniName mapT) nS))==nV)
                    --update function
                    $(update [p| (_,(TableSync n cols cont)) |]
                             [p| (TableSync n cols cont) |]
                             [d| n=(uniName mapT); cols=(uniCols mapT) ; cont=(uniCont mapT)|])
                    --create a source from a view
                    (\(TableSync n col cont) -> (False,(TableSync (fromSql(idSync (toSql n) mapT)) [] [])))
                    --delete an item from the source list
                    (\(b,table) -> Just (True,table))

translateTableSync :: [[SqlValue]] -> BiGUL [TableSync] [TableSync]
translateTableSync mapT = emb g p
                    where
                        g s = fromJust(get (translateTableSync2 mapT)  newS)
                            where
                                newS = map (\(TableSync n cols cont) -> (not((findNoUniversal mapT (head (reverse(splitOn "_" n))))/=""),(TableSync n cols cont))) s
                        p s v = map (\(_,t) -> t) (fromJust (put (translateTableSync2 mapT) (map (\t -> (False,t)) s) v))


define :: [Bool] -> [SqlValue] -> [SqlValue]
define [] [] = []
define (x:xs) (y:ys) = if x
                    then (define xs ys)
                    else y:(define xs ys)

--Ask the user to create the link between the primary keys
link :: [SqlValue] -> IO [[SqlValue]]
link [] = return []
link (x:xs) = do
        s <- return ("What is the link of " ++ (show x) ++ " ?")
        putStrLn s
        r <- getLine
        rt <- return (toSql r)
        endOf <- (link xs)
        return ([x,rt]:endOf)

--check if the link exist in the mapping Table
--if not, return all the values with no links
checkLinkExist :: Int -> [[SqlValue]] -> [SqlValue] -> IO[SqlValue]
checkLinkExist i mappingT toCheck = do
            isDefine <- return (if (i==0) then (map (`elem` (map head mappingT)) toCheck)
                                      else (map (`elem` (map head (map reverse mappingT))) toCheck))
            return (define isDefine toCheck)

--generate random id for content and check if random string is already use or not
randomLinks :: [[SqlValue]] -> [SqlValue] -> IO[[SqlValue]]
randomLinks _ [] = return []
randomLinks mapT (x:xs) = do
            rand <- randomString (onlyAlpha randomASCII) 50
            randSql <- return (toSql rand)
            rest <- if (randSql `elem` (map head mapT))
                        then randomLinks mapT (x:xs)
                        else (randomLinks mapT xs)
            return ([x,randSql]:rest)

--Add link in the mapping table if needed
--Return the mapping table needed to do the conversion
defineUniversal :: Int -> Connection -> String -> TableSync -> IO [[SqlValue]]
defineUniversal i conn tMapName (TableSync n cols cont) = do
            nSql <- return (if (i==0) then (toSql n)
                                      else (toSql (head (reverse(splitOn "_" n)))))
            colsSql <- return (map toSql cols)
            contCheck <- return (map head (map fst cont))

            --get the mapping Tables
            mappingT <- (quickQuery' conn (selection tMapName) [])

            --All the content we need to define manually
            notDefine <- checkLinkExist i mappingT (nSql:colsSql)
            alsoNotDefine <- checkLinkExist i mappingT contCheck

            --For the content,the user can choose to define all relation or not with userDefine
            newLinks <- if (userDefine || (i==0))
                            then do link (notDefine ++ (alsoNotDefine))
                            else do
                                definedLink <- link notDefine
                                allLinks <- return (definedLink ++ mappingT)
                                randomDefinedLinks <- randomLinks allLinks alsoNotDefine
                                print randomDefinedLinks
                                return (definedLink ++ randomDefinedLinks)

            inserts <- return (if (i==0) then newLinks
                                      else (map reverse newLinks))

            --Add relation in the mappingTable
            insertRequest <- (insertR conn tMapName 2)
            executeMany insertRequest inserts
            commit conn

            mapT <- (quickQuery' conn (selection tMapName) [])
            return (if (i==0)
                    then  filter (\(x:y:_) -> x `elem` ((nSql:colsSql) ++ contCheck)) mapT
                    else  filter (\(x:y:_) -> y `elem` ((nSql:colsSql) ++ contCheck)) mapT)