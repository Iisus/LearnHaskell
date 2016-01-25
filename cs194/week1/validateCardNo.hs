{- Validating Credit Card Numbers
 -
 - https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf
 -
 - author: Andrei Alexandru
 -
 - -}


{- Exercise 1 -}

-- toDigitsRev : take a positive Intger and return a list containing
--               the digits of that Integer reversed
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0     = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []


-- toDigit : take a positive Integer and return a list containing
--           the digits of that Integer
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


{- Exercise 2 -}

-- doubleEveryOtherBeg : take a list of Integers and doubles every second one,
--                       starting from the first
doubleEveryOtherBeg :: [Integer] -> [Integer]
doubleEveryOtherBeg [] = []
doubleEveryOtherBeg (x:[]) = [x]
doubleEveryOtherBeg (x:y:zs) = x : (y*2) : doubleEveryOtherBeg zs

-- doubleEveryOther : take a list of Integers and doubles every second one,
--                    starting from the last
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherBeg (reverse xs))


{- Exercise 3 -}

--sumDigits : take a list of positive Integers and returns the sum of the 
--            digits of every element in that list
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigitsRev x) + sumDigits xs


{- Exercise 4 -}

-- validate : takes an Integer and returns if that is a valid credit card no
--            it doubles every second digit from the right, sums all digits,
--            then if the result divides exactly by 10, it's valid
validate :: Integer -> Bool
validate n
  | n > 0     = sumDigits (doubleEveryOther (toDigits n) ) `mod` 10 == 0
  | otherwise = False

