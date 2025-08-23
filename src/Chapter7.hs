module Chapter7 where

import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
