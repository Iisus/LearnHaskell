{-# OPTIONS_GHC -Wall #-}

{- Golf
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/03-rec-poly.pdf
 -
 - author: Andrei Alexandru
 -}


module Golf where

import Data.List

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
               map snd (filter (\ x' -> fst x' `mod` x == 0) (zip [1..] xs)))
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

-- | @countEach xs@ get the list of digits @xs@ (0-9) and returns a list with 10
-- | elements, each element being the number of apearances the current index have
-- | in the original list.
-- | Example [1, 1, 2, 1, 3, 2, 9, 9] -> [0, 3, 2, 1, 0, 0, 0, 0, 0, 2]
-- |
-- | Implementation details: for each element in the range 0..9, we filter out
-- | all the elements in the input list that are different, leaving only the
-- | ones that are the same, then insert at that position (map) the length of
-- | the filtered list.
countEach :: [Int] -> [Int]
countEach xs = map (\x -> length . filter (==x) $ xs) [0..9]

-- | @nrToStars xs@ gets the list of number of digits @xs@ and outputs a list of
-- | Strings.
-- | Example: [0, 3, 0, 1] -> ["   ", "***", "   ", "*"]
-- |
-- | Implementation details: For each number n it inserts n '*' and max - n ' ',
-- | where max is the maximum n in the list.
nrToStars :: [Int] -> [String]
nrToStars xs = map (\x -> replicate x '*' ++ replicate (maximum xs - x) ' ') xs

-- | @histogram xs@ gets the list of digits @xs@ (0-9) and retuns a histogram
-- | Example: [1, 3, 1, 5, 0, 5, 2, 1, 8]
-- |  *
-- |  *   *
-- | **** *  * 
-- | ==========
-- | 0123456789
-- |
-- | Implementation details: count how many times a digit appears and put that into a list.
-- | Transform that list into a list of strings where each string contains n stars, where
-- | n is the number that index appeared in the input list. Transpose, reverse that
-- | list and intercalate \n between lines for displaying.
histogram :: [Int] -> String
histogram xs = (unlines . reverse . transpose . nrToStars . countEach $ xs)
                ++ "==========\n0123456789\n"
