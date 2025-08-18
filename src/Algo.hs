module Algo() where

-- quicksort.hs
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a|a <- xs,a <= x]
      biggerSorted = quicksort [a|a<-xs,a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- mygcd.hs
mygcd :: Int -> Int ->Int
mygcd x y = if y == 0 then x else mygcd y (mod x y)

-- reNoUpper.hs
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c|c <- st,c `elem` ['A'..'Z']]


-- power.hs
power :: Int -> Int -> Int
power _ 0 = 1
power x n = x * power x (n-1)

-- power1.hs
power1 :: Int -> Int -> Int
power1 _ 0 =1
power1 x n | odd n = let p = power x ((n-1) `div` 2) in x * p * p
           | otherwise = let p = power x (n `div` 2) in p * p

removeNonUppercase' st = [c|c<-st,c `elem` ['A'..'Z']]
