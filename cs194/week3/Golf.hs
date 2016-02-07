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
-- | in the output contains every nth element from the input.
-- | Example: skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
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

-- | @localMaxima xs@ takes the list @xs@ and returns a list where each element
-- | is strictly greater than both the elements immediately before and after it.
-- | Example: localMaxima [2, 4, 3, 5] == [4]
-- | 
-- | Implementation details: the input list @xs@ is zipped in a list touples with
-- | 3 elements, each element containing 3 the neighbouring numbers from the list.
-- | A filter is applied on this list of touples, outputing a list which contains
-- | only touples with the local maxima (where the middle element is greater than
-- | the two sorrounding elements). Finally, using a map, a list is constructed by
-- | extracting the middle element from each touple.
-- | Example: [2, 4, 3, 5] --zip--> [(2, 4, 3), (4, 3, 5)] --filter--> [(2, 4, 3)]
-- | --map--> [4]
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\ (_, b, _) -> b)
                     (filter (\(a, b, c) -> a<b && b>c)
                            (zip3 xs (drop 1 xs) (drop 2 xs)))
