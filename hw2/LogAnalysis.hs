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

insertLogMessage :: LogMessage -> MessageTree -> MessageTree
insertLogMessage logMessage Leaf = Node Leaf logMessage Leaf
insertLogMessage (Unknown _) x = x
insertLogMessage newLogMessage (Node leftMessageTree existingLogMessage rightMessageTree)
  | newLogMessage `lessThan` existingLogMessage = Node (insertLogMessage newLogMessage leftMessageTree) existingLogMessage rightMessageTree
  | otherwise = Node leftMessageTree existingLogMessage (insertLogMessage newLogMessage rightMessageTree)

lessThan :: LogMessage -> LogMessage -> Bool
lessThan (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 < t2
lessThan _ _ = False
