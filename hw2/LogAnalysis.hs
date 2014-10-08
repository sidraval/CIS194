{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.List

parseInfo :: String -> LogMessage
parseInfo str = LogMessage Info (read (splitStr !! 1) :: TimeStamp) (intercalate " " $ drop 2 splitStr)
                where splitStr = words str

parseWarning :: String -> LogMessage
parseWarning str = LogMessage Warning (read (splitStr !! 1) :: TimeStamp) (intercalate " " $ drop 2 splitStr)
                where splitStr = words str

parseError :: String -> LogMessage
parseError str = LogMessage (Error (read (splitStr !! 1) :: Int)) (read (splitStr !! 2) :: TimeStamp) (intercalate " " $ drop 3 splitStr)
                where splitStr = words str


parseMessage :: String -> LogMessage
parseMessage ('I':str) = parseInfo ('I':str)
parseMessage ('W':str) = parseWarning ('W':str)
parseMessage ('E':str) = parseError ('E':str)
parseMessage x = Unknown x

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insertMe :: LogMessage -> MessageTree -> MessageTree
insertMe (Unknown _) messageTree = messageTree
insertMe logMessage Leaf = Node Leaf logMessage Leaf
insertMe logMessage (Node leftMessageTree currentLogMessage rightMessageTree)
 | logMessage `lessThan` currentLogMessage = Node (insertMe logMessage leftMessageTree) currentLogMessage rightMessageTree
 | otherwise = Node leftMessageTree currentLogMessage (insertMe logMessage rightMessageTree)

lessThan :: LogMessage -> LogMessage -> Bool
lessThan (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 < t2
lessThan _ _ = False

buildTree :: [LogMessage] -> MessageTree
buildTree [] = Leaf
buildTree (logMessage:[]) = insertMe logMessage Leaf
buildTree (logMessage:logMessages) = insertMe logMessage (buildTree logMessages)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf logMessage Leaf) = [logMessage]
inOrder (Node leftMessageTree currentLogMessage rightMessageTree) = inOrder leftMessageTree ++ [currentLogMessage] ++ inOrder rightMessageTree

getString :: LogMessage -> String
getString (Unknown str) = str
getString (LogMessage _ _ str) = str

getErrors :: [LogMessage] -> [LogMessage]
getErrors logMessages = filter isSevereError logMessages

isSevereError :: LogMessage -> Bool
isSevereError (Unknown _) = False
isSevereError (LogMessage (Error num) _ _) = num > 50
isSevereError (LogMessage _ _ _) = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = map getString (inOrder $ buildTree $ getErrors logMessages)


