{-# OPTIONS_GHC -Wall #-}

{- Log Analysis
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf
 -
 - author: Andrei Alexandru
 -}


module LogAnalysis where

import Log

-- | @isUInt s@ returns true if the string @s@ is an unsigned integer, false otherwise.
isUInt :: String -> Bool
isUInt [] = False
isUInt (x:[]) = x `elem` ['0'..'9']
isUInt (x:xs) = x `elem` ['0'..'9'] && isUInt xs

-- | @parseMessage' w@ parse the word list @w@ and returns a LogMessage
parseMessage' :: [String] -> LogMessage
parseMessage' ("I":y:xs)
  | isUInt y  = LogMessage Info (read y) (unwords xs)
  | otherwise = Unknown (unwords (y:xs))
parseMessage' ("W":y:xs)
  | isUInt y  = LogMessage Warning (read y) (unwords xs)
  | otherwise = Unknown (unwords (y:xs))
parseMessage' ("E":y:z:xs)
  | isUInt y && isUInt z = LogMessage (Error (read y))(read z) (unwords xs)
  | otherwise = Unknown (unwords (y:xs))
parseMessage' xs = Unknown (unwords xs)

-- | @parseMessage s@ parse the string @s@ and returns a LogMessage
parseMessage :: String ->LogMessage
parseMessage s = parseMessage' (words s)

-- | @parse s@ parse the text @s@ and returns a list of LogMessages
parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)
