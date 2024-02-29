--{-# ImportQualifiedPost #-}

import Data.Char
import Data.Set qualified as Set

--- todigits 1234 = [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits x  
    | x == 0 || x < 0 = [] 
    | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x == 0 || x < 0 = []
    | otherwise = x `mod` 10 : toDigitsRev (x `div` 10) 

reversed :: String-> String
reversed [] = []
reversed [x] = [x]
reversed xs = reverse xs


-- double every other number from right
-- [1,2,3,4] = [2,2,6,4]
-- [1,2,3] = [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] =[]
doubleEveryOther [x] =[x]
doubleEveryOther (x:y:zs) 
    | length (x:y:zs) `mod` 2 /= 0 = x : (y*2) : doubleEveryOther zs
    | otherwise = (x*2) : y :doubleEveryOther zs

--- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ys) 
    | toDigits x == [] = 0
    | toDigits x == [x] = x + sumDigits ys
    | (length $ toDigits x) >= 2 = sumDigits (toDigits x ++ ys)

areaCircle :: IO()
areaCircle = do
    let pi =3.14
        r =4
    print ( pi*r)

areaCircle' :: Float -> Float
areaCircle' r = 3.14 *r

volumeCyliner :: Float -> Float -> Float
volumeCyliner r h = areaCircle' r * h

volumeGreaterThan42 :: Float-> Float -> Bool
volumeGreaterThan42 r h = if ((volumeCyliner r h) >42) then True else False

=================================== Leap Year ==============================
A leap year (in the Gregorian calendar) occurs:

In every year that is evenly divisible by 4.
Unless the year is evenly divisible by 100, in which case it's only a leap year if the year is also evenly divisible by 400.
==========================================================================================================

isLeapYear :: Integer -> Bool
isLeapYear year 
    | year `mod` 4 == 0 && year `mod` 100 ==0 = year `mod` 400 == 0
    | year `mod` 4 == 0 && year `mod` 100 /=0 =True
    | otherwise = False

    
================================ Space Age ===================================
Given an age in seconds, calculate how old someone would be on:

Mercury: orbital period 0.2408467 Earth years
Venus: orbital period 0.61519726 Earth years
Earth: orbital period 1.0 Earth years, 365.25 Earth days, or 31557600 seconds
Mars: orbital period 1.8808158 Earth years
Jupiter: orbital period 11.862615 Earth years
Saturn: orbital period 29.447498 Earth years
Uranus: orbital period 84.016846 Earth years
Neptune: orbital period 164.79132 Earth years
So if you were told someone were 1,000,000,000 seconds old, you should be able to say that they're 31.69 Earth-years old.

If you're wondering why Pluto didn't make the cut, go watch this YouTube video.
Note: The actual length of one complete orbit of the Earth around the sun is closer to 365.256 days (1 sidereal year). 
The Gregorian calendar has, on average, 365.2425 days. While not entirely accurate, 365.25 is the value used in this exercise. 
See Year on Wikipedia for more ways to measure a year.
================================================================================================
data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune
    deriving Eq
    
ageOn :: Planet -> Float -> Float
ageOn planet seconds =  
  let earthOrbitalPeriod = 31557600.0
      orbitalPeriods = [(Mercury, 0.2408467), 
                          (Venus, 0.61519726), 
                          (Earth, 1.0), 
                          (Mars, 1.8808158), 
                          (Jupiter, 11.862615), 
                          (Saturn, 29.447498), 
                          (Uranus, 84.016846), 
                          (Neptune, 164.79132)]
      earthYears = seconds / earthOrbitalPeriod
      planetYears = case lookup planet orbitalPeriods of
                        Just orbitalPeriods -> earthYears / orbitalPeriods
                        Nothing -> error "Unknown planet"
    in planetYears
====================================== Pangram =====================================
Your task is to figure out if a sentence is a pangram.

A pangram is a sentence using every letter of the alphabet at least once. 
It is case insensitive, so it doesn't matter if a letter is lower-case (e.g. k) or upper-case (e.g. K).

For this exercise, a sentence is a pangram if it contains each of the 26 letters in the English alphabet.
===================================================================================

isPangram :: String -> Bool
isPangram text = 
    let lowerCase = map toLower text
    in all ( `elem` lowerCase) ['a'..'z']
