module Avg(avg) where

import Data.List(genericLength)

avg :: Fractional a => [a] -> a
avg xs = sum xs / genericLength xs