{-# OPTIONS_GHC -Wall #-}

{- Golf
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
 -
 - author: Andrei Alexandru
 -}


module Golf where

-- | @skips xs@ takes the list @xs@ and outputs a list of lists, by this rule:
-- | the first list in the output is the same as the input; the second list
-- | in the output contains every second element from the input, the nth list
-- | in the output contains every nth element from the input, for example:
-- | skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- |
-- | Implementation details: the input list @xs@ is zipped with a list of integers
-- | starting from 1, resulting a list of touples of type [(Int, a)]. Using a
-- | map over a list of integers starting from 1 to length xs, a filter is applied
-- | on every element from the list of touples to test if the first element from
-- | the touple is a multiple of the current element from the integer list. Using
-- | this, the correct touples for each index are extracted into a list. Using 
-- | another map function, the second element from the touples are extracted,
-- | givving the final result.
-- | Example: skips "abcd" -> zip -> [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
-- | -> map (filter ..) [1..length xs] -> [[(1, "a"), (2, "b"), (3, "c"), (4, "d")],
-- |                                       [(2, "b"), (4, "d")],
-- |                                       [(3, "c")],
-- |                                       [(4, "d")]]
-- | -> map (snd) result -> ["abcd", "bd", "c", "d"]
skips :: [a] -> [[a]]
skips xs = map (\ x ->
               map (snd) (filter (\ x' -> fst x' `mod` x == 0) (zip [1..] xs)))
               [1..length xs]
