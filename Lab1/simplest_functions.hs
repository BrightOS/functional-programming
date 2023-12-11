module Main where
    main :: IO ()

    sumMy :: Num a => a -> a -> a
    sumMy a b = a + b
    
    productMy :: Num a => a -> a -> a
    productMy a b = a * b
    
    maxMy :: Ord a => a -> a -> a
    maxMy a b = if a > b then a else b
    
    minMy :: Ord a => a -> a -> a
    minMy a b = if a < b then a else b
    
    maximumMy :: Ord b => [b] -> b
    maximumMy xs = foldr max (head xs) (tail xs)
    
    minimumMy :: Ord b => [b] -> b
    minimumMy xs = foldr min (head xs) (tail xs)
    
    evenMy :: Integral a => a -> Bool
    evenMy x = x `mod` 2 == 0
    
    oddMy :: Integral a => a -> Bool
    oddMy x = x `mod` 2 /= 0
    
    gcdMy :: Integral t => t -> t -> t
    gcdMy a b = if b == 0 then a else gcdMy b (a `mod` b)

    lcmMy :: Integral a => a -> a -> a
    lcmMy a b = abs (a * b) `div` gcdMy a b

    powerMy :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
    powerMy base exp = if exp == 0 then 1 else base * powerMy base (exp - 1)

    -- Рекурсивные функции
    factMy :: Integer -> Integer
    factMy 0 = 1
    factMy n = n * factMy (n - 1)

    fibMy :: Integer -> Integer
    fibMy 0 = 0
    fibMy 1 = 1
    fibMy n = fibMy (n - 1) + fibMy (n - 2)

    main = do
        -- Примеры использования
        let maximumMyTest = maximumMy [1, 2, 3, 4]
        let factorialOf5 = factMy 5
        let fibonacciOf8 = fibMy 8

        print maximumMyTest
        print factorialOf5
        print fibonacciOf8