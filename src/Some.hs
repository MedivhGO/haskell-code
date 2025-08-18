module Some(
    head',
    describeList,
    describeList',
    chain,
    numLongChains,
    compareWithHundred,
    contains6,
    contains6',
    divideByTen,
    capital,
    elem',
) where

-- case1.hs
head' :: [a] -> a
head' xs = case xs of
    []    -> error "No head for empty lists!"
    (x:_) -> x

-- case2.hs
describeList :: [a] -> String
describeList xs = "The list is" ++ case xs of
    [] -> "empty."
    [_] -> "a singleton list."
    _  -> "a longer list."

-- case3.hs
describeList' :: [a] -> String
describeList' xs = "This list is" ++ what xs
    where what [] = "empty."
          what [_] = "a singleton list."
          what _ = "a longer list."

-- chain.hs
chain :: (Integral a) => a->[a]
chain 1 = [1]
chain n
      | even n = n : chain(n `div` 2)
      | odd n = n: chain (3 * n + 1)
      | otherwise = error "input must postive number"

numLongChains :: Int
numLongChains = length (filter isLong (map chain ([1..100] :: [Integer])))
                  where isLong xs = length xs > 15

-- compareWithHundered.hs
compareWithHundred :: (Num a,Ord a) => a -> Ordering
compareWithHundred  = compare 100

-- contains6.hs
contains6::[String]
contains6 = filter(elem '6')(map show [1..100])

-- contains6'.hs
contains6' :: [Int]
contains6' = map (\str->read str:: Int) $ filter (elem '6') (map show [1..100])

-- divideByTen.hs
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- doas.hs
capital :: String -> String
capital "" = "Empty string,whoops!"
capital all@(x:xs) = "The first letter of" ++ all ++ "is" ++ [x]

-- elem.hs
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' _ (x:xs)
    | a == x = True
    | otherwise = a `elem'` xs

-- elem2.hs
--左折叠实现 elem 函数
elem2 :: (Eq a) => a->[a] -> Bool
elem2 y ys = foldl(\acc x -> if x==y then True else acc) False ys

-- error
a = error "a is an error"

-- fact.hs
factorial :: (Integral a) => a -> a
factorial 0 = 0
factorial 1 = 1
factorial x = factorial (x-1) * x

-- factorial.hs
factorial' :: Integer -> Integer
factorial' n   | n < 0 = error "i is less than 0"
               | n == 0 = 1
               | otherwise = n*factorial'(n-1)

-- -- fastFlib.hs
-- fastFib = fst $ fibPair
--     where fibPair =

-- fibonacci.hs
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- flip2.hs
flip' :: (a->b->c) ->b->a->c
flip' f = \x y -> f y x

-- function.hs
f:: Num a=> a->a
f x = 4*x+1

-- function1.hs
f' :: Num a => a->a->a
f' x y = 4*x+5*y+1

-- function2.hs
f'' :: Num a => a->a
f'' y = 4 *5+5 *y +1

-- hailun.hs
s::Double -> Double -> Double ->Double
s a b c = let p = (a+b+c)/2 in sqrt (p*(p-a)*(p-b)*(p-c))

-- hailun1.hs
s' :: Double->Double->Double->Double
s' a b c = sqrt(p*(p-a)*(p-b)*(p-c))
         where p = (a+b+c) /2

-- if.hs
isTwo :: Int->Bool
isTwo n = if n==2 then True else False

-- isUpperAlphanum.hs
isUpperAlphaum :: Char -> Bool
isUpperAlphaum = (`elem` ['A'..'Z'])

-- Lambda.hs
addThree :: (Num a) => a->a->a->a
addThree = \x -> \y -> \z -> x+y+z

-- largestDivisible.hs
largestDivisible:: (Integral a) =>a
largestDivisible = head (filter p [100000,99999..])
                 where p x = x `mod` 3829 == 0

-- listCom.hs
boomBangs xs = [if x<10 then "BOOM!" else "BANG!"| x<-xs,odd x]

-- lucky.hs
lucky::(Integral a) => a-> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry,you're out of luck,pal!"

-- maximum.hs
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
        | x > maxTail = x
        | otherwise = maxTail
        where maxTail = maximum' xs

-- maximum1.hs
maximum1 :: (Ord a) => [a] -> a
maximum1 [] = error "maximum of empty list"
maximum1 [x] = x
maximum1 (x:xs) = max x  (maximum1 xs)

-- mcf.hs
mc n | n> 100 = n - 10
     | otherwise = mc (mc(n+11))

-- multiThree.hs
multThree :: (Num a) => a->a->a->a
multThree x y z = x * y * z

-- myCompare.hs
myCompare :: (Ord a) => a-> a->Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

-- numLongChains'.hs
numLongChains'::Int
numLongChains' = length(filter (\xs -> length xs > 15)(map chain[1..100]))

-- pmatch.hs
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- repeat.hs
repeat' :: a-> [a]
repeat' x = x:repeat' x

-- replicate.hs
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

-- reverse.hs
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- -- reverseSentence
-- reverseSentence::String->String
-- --reverseSentence str = unwords (reverse (words str))--
-- --reverseSentence str = unwords $ reverse $ words str--
-- reverseSentence = unwords $ reverse $ words

-- romeNatation.hs
romeNotation :: [String]
romeNotation =  ["M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"]

romeAmount :: [Int]
romeAmount = [1000,900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair:: [(Int,String)]
pair = zip romeAmount romeNotation

subtrahend :: Int -> (Int,String)
subtrahend n = head (dropWhile(\(a,_)->a >n)pair)

convert :: Int -> (Int,String)
convert 0 = (0,"")
convert n = let (i,st) = subtrahend n in
    let (i',st') = convert(n-i) in (i',st++st')

-- selfcylinder.hs
cylinder :: (RealFloat a) => a->a->a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi *r ^ 2
    in sideArea + 2 * topArea

-- selffst.hs
first :: (a,b,c) ->a
first (x,_,_) = x

second :: (a,b,c) -> b
second (_,y,_) = y

third :: (a,b,c) -> c
third (_,_,z) = z

-- selfguard.hs
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "a"
    | bmi <= 25.0 = "b"
    | bmi <= 30.0 = "c"
    | otherwise = "d"

-- selfinit.hs
initials :: String -> String -> String
initials a b = (show a) ++ (show b)

-- selfeng.hs
length''' :: (Num b) => [a] -> b
length''' [] = 0
length''' (_:xs) = 1 + length''' xs

-- selfsum.hs
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- selftanhao.hs
tanhao :: (Integral a) => [a] -> a -> [a]
tanhao xs n = take (fromIntegral n) xs

-- selfwhere.hs
bmiTell1 :: (RealFloat a) => a->a->String
bmiTell1 weight height
    | bmi <= skinny = "a"
    | bmi <= normal = "b"
    | bmi <= fat = "c"
    | otherwise = "d"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

-- selfwherefun.hs
calBmis :: (RealFloat a) => [(a,a)] -> [a]
calBmis xs = [bmi w h|(w,h)<-xs]
    where bmi weight height = weight / height ^ 2

-- -- sieve.hs
-- {-shaixuanfa-}
-- sieve :: (Integral a) => [a] -> [a]
-- sieve (p:xs) = p:sieve[x|x<-xs,x `mod` p /= 0]
-- primes = sieve [2..]

-- sum2.hs
sum2 :: (Num a) => [a] -> a
sum2 xs = foldl(\acc x -> acc + x) 0 xs

-- -- tailre.hs
-- total' :: Num a => [a] -> a -> a
-- total' [] n = n
-- total' (x:xs) n = total' xs (n+x)

-- total :: Num a => [a] -> a
-- total xs = total' xs 0

-- take.hs
take' :: (Num i,Ord i) => i -> [a] -> [a]
take' n _
      | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs

-- tellfirst.hs
tell :: (Show a) => [a] ->String
tell [] = "This list is empty"
tell (x:[]) = "This list has one element: " ++ show x
tell (x:y:[]) = "This list has two elements: "++ show x ++ "and" ++ show y
tell (x:y:_) = "This list is long.the first two elements are:" ++ show x ++ "and" ++ show y

-- types.hs
removeNonUppercase1 :: [Char] -> [Char]
removeNonUppercase1 st = [c|c <- st,c `elem` ['A'..'Z']]

addThree1 :: Int -> Int -> Int -> Int
addThree1 x y z = x+y+z

factorial1 :: Integer -> Integer
factorial1 n = product [1..n]

circumference :: Float -> Float
circumference r = 2 *pi *r

circumference' :: Double -> Double
circumference' r = 2*pi*r

-- undefined.hs
undefined :: a
undefined = error "Prelude;undefined"

-- values.hs
a1::Int
a1 = 5
b:: Bool
b = False

-- zip.hs
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs)(y:ys) = (x,y):zip' xs ys

-- zipWith.hs
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
