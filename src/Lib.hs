module Lib
    (   helloworldFunc,
        printApplyTwice,
        head'
    ) where


helloworldFunc :: IO ()
helloworldFunc = putStrLn "Hello, World!"

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

printApplyTwice :: IO ()
printApplyTwice = print (applyTwice (+1) (5 :: Integer))

head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x
