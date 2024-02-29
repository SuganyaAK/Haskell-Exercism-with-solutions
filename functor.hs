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

        -- sumDigits (toDigits x) 

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

isPangram :: String -> Bool
isPangram text = 
    let lowerCase = map toLower text
    in all ( `elem` lowerCase) ['a'..'z']

{-
maxConsumption :: IO ()
maxConsumption = do
    putStrLn "Enter the hourly consumption :"
    consumption <- getLine
    putStrLn "Enter hours of daily use : "
    hours <- getLine
    putStrLn "Enter max allowed consumption :"
    maxAllowed <- getLine
    let monthyConsumption = consumption* hours* 30
     return (monthlyConsumption)
        if (monthlyConsumption > maxAllowed ) 
            then putStr ("Monthly consumption is bigger than maximum allowed")
        else if (monthlyConsumption == maxAllowed ) 
            then putStr ("Monthly consumption is equal to maximum allowed")
        else (monthlyConsumption < maxAllowed ) 
            then putStr ("Monthly consumption is smaller than maximum allowed")
            -}

lengthWithFoldl :: [Int] -> Int
lengthWithFoldl xs = foldr (\_ n-> 1 + n) 0 xs