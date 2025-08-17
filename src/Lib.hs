module Lib
    (   hellworldFunc,
        printApplyTwice
    ) where


hellworldFunc :: IO ()
hellworldFunc = putStrLn "Hello, World!"

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

printApplyTwice :: IO ()
printApplyTwice = print (applyTwice (+1) (5 :: Integer))
