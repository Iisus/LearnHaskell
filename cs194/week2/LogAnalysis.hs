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
  | otherwise            = Unknown (unwords (y:xs))
parseMessage' xs = Unknown (unwords xs)

-- | @parseMessage s@ parse the string @s@ and returns a LogMessage
parseMessage :: String ->LogMessage
parseMessage s = parseMessage' (words s)

-- | @parse s@ parse the text @s@ and returns a list of LogMessages
parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)


-- | @insert n t@ insert node LogMessage @n@ in the ordered MessageTree @t@, producing
--   a new ordered MessageTree. The ordering rules are: a node should be greater
--   at that the left subtree, but less than the right child node. Ordering is done
--   by timestamp
insert :: LogMessage -> MessageTree -> MessageTree
insert n@(LogMessage _ _ _) Leaf = Node Leaf n Leaf
insert n@(LogMessage _ insTs _) (Node lt cn@(LogMessage _ currTs _) rt)
  | insTs > currTs = Node lt cn (insert n rt)
  | otherwise      = Node (insert n lt) cn rt
insert _ t = t

-- | @build xs t@ helper function for @build@
build' :: [LogMessage] -> MessageTree -> MessageTree
build' [] t = t
build' (x:[]) t = insert x t
build' (x:xs) t = build' xs (insert x t)

-- | @build xs@ builds a MessageTree from the list of LogMessages @xs@
build :: [LogMessage] -> MessageTree
build xs = build' xs Leaf
