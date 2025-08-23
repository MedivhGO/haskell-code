module Some where

import Data.List (genericLength)

type Weekday = Int

type Year = Int

type Month = Int

type Day = Int

week' :: Year -> Day -> Weekday
week' y d = let y1 = y - 1 in (y1 + (div y1 4) - (div y1 100) + (div y1 400) + d) `mod` 7

isLeapYear :: Int -> Bool
isLeapYear y = (mod y 4 == 0) && (mod y 100 /= 0) || (mod y 400 == 0)

monthDays :: Year -> Month -> Int
monthDays y m
  | m == 2 = if not $ isLeapYear y then 28 else 29
  | elem m [1, 3, 5, 7, 8, 10, 12] = 31
  | elem m [4, 6, 9, 11] = 30
  | otherwise = error "invaid month"

accDays :: Year -> Month -> Day -> Int
accDays y m d
  | d > monthDays y m = error "invalid days"
  | otherwise = (sum $ take (m - 1) (map (monthDays y) [1 .. 12])) + d

-- contains6.hs
contains6 :: [String]
contains6 = filter (elem '6') (map show [1 .. 100])

-- contains6'.hs
contains6' :: [Int]
contains6' = map (\str -> read str :: Int) $ filter (elem '6') (map show [1 .. 100])

-- error
a = error "a is an error"

-- factorial.hs
factorial' :: Integer -> Integer
factorial' n
  | n < 0 = error "i is less than 0"
  | n == 0 = 1
  | otherwise = n * factorial' (n - 1)

-- fastFlib.hs
fastFib :: Int -> Int
fastFib n = fibfunc n 0 1
  where
    fibfunc 0 a _ = a
    fibfunc n a b = fibfunc (n - 1) b (a + b)

-- fibonacci.hs
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- function.hs
f :: (Num a) => a -> a
f x = 4 * x + 1

-- function1.hs
f' :: (Num a) => a -> a -> a
f' x y = 4 * x + 5 * y + 1

-- function2.hs
f'' :: (Num a) => a -> a
f'' y = 4 * 5 + 5 * y + 1

-- hailun.hs
s :: Double -> Double -> Double -> Double
s a b c = let p = (a + b + c) / 2 in sqrt (p * (p - a) * (p - b) * (p - c))

-- hailun1.hs
s' :: Double -> Double -> Double -> Double
s' a b c = sqrt (p * (p - a) * (p - b) * (p - c))
  where
    p = (a + b + c) / 2

-- if.hs
isTwo :: Int -> Bool
isTwo n = if n == 2 then True else False

-- mcf.hs
mc n
  | n > 100 = n - 10
  | otherwise = mc (mc (n + 11))

-- reverseSentence
-- reverseSentence::String->String
-- reverseSentence str = unwords (reverse (words str))--
-- reverseSentence str = unwords $ reverse $ words str--
-- reverseSentence = unwords $ reverse $ words

-- romeNatation.hs
romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

romeAmount :: [Int]
romeAmount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair :: [(Int, String)]
pair = zip romeAmount romeNotation

subtrahend :: Int -> (Int, String)
subtrahend n = head (dropWhile (\(a, _) -> a > n) pair)

convert :: Int -> (Int, String)
convert 0 = (0, "")
convert n =
  let (i, st) = subtrahend n
   in let (i', st') = convert (n - i) in (i', st ++ st')

-- selfinit.hs
initials :: String -> String -> String
initials a b = (show a) ++ (show b)

-- selftanhao.hs
tanhao :: (Integral a) => [a] -> a -> [a]
tanhao xs n = take (fromIntegral n) xs

-- sieve.hs
{-shaixuanfa-}
sieve :: (Integral a) => [a] -> [a]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes = sieve [2 ..]

-- tailre.hs
total' :: (Num a) => [a] -> a -> a
total' [] n = n
total' (x : xs) n = total' xs (n + x)

total :: (Num a) => [a] -> a
total xs = total' xs 0

-- undefined.hs
undefined :: a
undefined = error "Prelude;undefined"

-- values.hs
a1 :: Int
a1 = 5

b :: Bool
b = False

-- mygcd.hs
mygcd :: Int -> Int -> Int
mygcd x y = if y == 0 then x else mygcd y (mod x y)

-- power.hs
power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n - 1)

-- power1.hs
power1 :: Int -> Int -> Int
power1 _ 0 = 1
power1 x n
  | odd n = let p = power x ((n - 1) `div` 2) in x * p * p
  | otherwise = let p = power x (n `div` 2) in p * p

avg :: (Fractional a) => [a] -> a
avg xs = sum xs / genericLength xs
