-- CIS 194: Homework 1
-- Author: Tom Miller

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x > 0 = let digit = x `rem` 10
                  rest  = truncate $ (toRational x) / 10
              in  digit : toDigitsRev rest
    | otherwise = []

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = fst . foldr doubleEveryOther' ([], False) 
    where doubleEveryOther' x (xs, double) = if   double
                                             then (x*2:xs, False)
                                             else (x:xs, True)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate = (==0) . (`rem`10) . sumDigits . doubleEveryOther . toDigits
