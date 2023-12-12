{-# OPTIONS_GHC #-}
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
    maximumMy [] = error "maximumMy empty list"
    maximumMy xs = foldl1 max xs
    
    minimumMy :: Ord b => [b] -> b
    minimumMy [] = error "minimumMy empty list"
    minimumMy xs = foldl1 min xs
    
    evenMy :: Integral a => a -> Bool
    evenMy x = x `mod` 2 == 0
    
    oddMy :: Integral a => a -> Bool
    oddMy x = x `mod` 2 /= 0
    
    gcdMy :: Integral t => t -> t -> t
    gcdMy a b = if b == 0 then a else gcdMy b (mod a b)

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

    -- Улучшенная функция фибоначчи
    fibMy' :: Integer -> Integer -> Integer -> Integer
    fibMy' 0 _ prevprev = prevprev
    fibMy' 1 prev _ = prev
    fibMy' n prev prevprev = fibMy' (n - 1) (prev + prevprev) prev

    -- Своя реализация для логических функций
    andMy :: [Bool] -> Bool
    andMy [] = True
    andMy (x:xs) = x && andMy xs

    orMy :: [Bool] -> Bool
    orMy [] = False
    orMy (x:xs) = x || orMy xs

    -- Своя реализация для списочных функций
    headMy :: [a] -> a
    headMy (x:xs) = x

    tailMy :: [a] -> [a]
    tailMy (x:xs) = xs
    
    lastMy :: [a] -> a
    lastMy xs = headMy (reverse xs)
    
    initMy :: [a] -> [a]
    initMy xs = reverse (tailMy (reverse xs))
    
    lengthMy :: Num a1 => [a2] -> a1
    lengthMy xs = sum (map (\_ -> 1) xs)
    
    indexMy :: [a] -> Int -> a
    indexMy xs n = xs !! n

    concatMy :: [a] -> [a] -> [a]
    concatMy xs ys = xs ++ ys
    
    takeMy :: (Ord t, Num t) => t -> [a] -> [a]
    takeMy n xs = if n <= 0 then [] else headMy xs : takeMy (n - 1) (tailMy xs)
    
    dropMy :: (Ord t, Num t) => t -> [a] -> [a]
    dropMy n xs = if n <= 0 then xs else dropMy (n - 1) (tailMy xs)
    
    reverseMy :: Foldable t => t a -> [a]
    reverseMy xs = foldl (\acc x -> x : acc) [] xs
    
    elemMy :: Eq a => a -> [a] -> Bool
    elemMy x [] = False
    elemMy x (y:ys) = (x == y) || elemMy x ys
    
    replicateMy :: (Ord t, Num t) => t -> a -> [a]
    replicateMy n x = takeMy n (repeat x)

    -- Дополнительные списочные операции
    lookupMy :: Eq a => a -> b -> [(a, b)] -> b
    lookupMy key defaultValue [] = defaultValue
    lookupMy key defaultValue ((k, v):kvs) =
        if key == k then v else lookupMy key defaultValue kvs

    substrMy :: [a] -> Int -> Int -> [a]
    substrMy xs start end = takeMy (end - start + 1) (dropMy start xs)

    strReplaceMy :: Eq a => [a] -> [a] -> [a] -> [a]
    strReplaceMy _ _ [] = []
    strReplaceMy find replace str@(x:xs) =
        if takeMy (lengthMy find) str == find
            then replace ++ strReplaceMy find replace (dropMy (lengthMy find) str)
            else [x] ++ strReplaceMy find replace xs

    elemIndicesMy :: Eq a => a -> [a] -> [Int]
    elemIndicesMy x xs = [i | (e, i) <- zip xs [0..], e == x]

    strPosMy :: Eq a => [a] -> [a] -> [Int]
    strPosMy find str = findIndices 0 str
        where
            findIndices :: Eq a => Int -> [a] -> [Int]
            findIndices _ [] = []  -- Базовый случай: пустая строка, завершаем поиск
            findIndices index xs
                | take (length find) xs == find = index : findIndices (index + 1) (drop (length find) xs)
                | otherwise = findIndices (index + 1) (tail xs)   

    main = do
        -- Примеры использования
        let maximumMyTest = maximumMy [1, 2, 3, 4]
        let factorialOf5 = factMy 5
        let fibonacciOf8 = fibMy 8
        let fibonacciOf8' = fibMy' 8 1 0

        let andMyTest = andMy [True, True, False]
        let orMyTest = orMy [True, False, False]

        let strPosMy = strPosMy "abracadabra" "ra"

        print maximumMyTest
        print factorialOf5
        print fibonacciOf8
        print fibonacciOf8'