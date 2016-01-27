{-# OPTIONS_GHC -Wall #-}

{- Log Analysis
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf
 -
 - author: Andrei Alexandru
 -}


module LogAnalysis where

import Log

isUInt :: String -> Bool
isUInt [] = False
isUInt (x:[]) = x `elem` ['0'..'9']
isUInt (x:xs) = x `elem` ['0'..'9'] && isUInt xs

{-
begWith :: [String] -> String -> Bool
begWith [] _ = False
begWith (x:xs) toMatch = x == toMatch

getBegUInts :: [String] -> [Int]
getBegUInts [] = []
getBegUInts (x:xs)
  | isUInt x  = (read x :: Int) : getBegUInts xs
  | otherwise = []

begWithStringUInt :: [String] -> String -> Bool
begWithStringUInt [] _ = False
begWithStringUInt (_:[]) _ = False
begWithStringUInt (str : int : xs) toMatch = str == toMatch && isUInt int 
-}

isInfo :: [String] -> Bool
isInfo [] = False
isInfo (_:[]) = False
isInfo (x:y:_) = x == "I" && isUInt y

isWarning :: [String] -> Bool
isWarning [] = False
isWarning (_:[]) = False
isWarning (x:y:_) = x == "W" && isUInt y

isError :: [String] -> Bool
isError [] = False
isError (_:[]) = False
isError (_:_:[]) = False
isError (x:y:z:_) = x == "E" && isUInt y && isUInt z

testIfValid :: String -> Bool
testIfValid xs = let w = words xs in isInfo w || isWarning w || isError w

getType :: [String] -> MessageType
getType xs
  | isInfo xs = Info
  | isWarning xs = Warning
  | isError xs = (Error (read (xs !! 1)))

getTimeStamp :: [String] -> TimeStamp
getTimeStamp xs
  | isError xs = read $ xs !! 2
  | otherwise  = read $ xs !! 1

getMsg :: [String] -> String
getMsg xs
  | isError xs = unwords (drop 3 xs)
  | otherwise  = unwords (drop 2 xs)

parseMessage :: String -> LogMessage
parseMessage xs 
  | testIfValid xs = let w = words xs in LogMessage (getType w) (getTimeStamp w) (getMsg w)
  | otherwise      = Unknown xs

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (x:xs) = parseMessage x : parseLines xs

parse :: String -> [LogMessage]
parse xs = parseLines . lines $ xs
